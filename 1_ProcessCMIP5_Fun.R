#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# be carefull with encoding, please
# the issue with system language isn't solved yet
# the months' names are cyrillic as far
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#========================================================================
# Read CMIP5 data and calculates an area-average for a certain place
# and for a given season (here it's summer)
# The result is a file whith time-series
# An input file is required (set with InputFile_name)
# which following information: 
# Scenario_Name, Model_Name, Begin_Time, Dir_Name, Test_Dir_Name, File_Name
# In our case it's "Models_Details.txt"
#
# TO DO1: include a possibility to switch the season in the same file
# (n_season[K]<-06-01; ((month_i[i]=="Èþíü"|month_i[i]=="Èþëü")|(month_i[i]=="Èþëü"|month_i[i]=="Àâãóñò")))
# SeasonBeg_Canonical<-paste(SeasYear,"-06-01",sep="")
# SeasonEnd_Canonical<-paste(SeasYear,"-08-31",sep="")
#========================================================================
library("ncdf4")
FindCells<-function(ProcFile,DimCode="lon",coord_min,coord_max) {
	# Check names of the coordinates in the nc-file 
	# (E.g. CMMCC-CESM has "i" and "j" coordinates instead of "lat" and "lon" for historical run)
	if (!(any(names(file_nc_obj_procSsn$dim)=="lat"))) {
		msg<-paste("There is no dim with the name 'lat' in the nc-file '",
			file_nc_obj_procSsn["filename"][[1]],"'",sep="")
		stop(msg)
	}
	if (!(any(names(file_nc_obj_procSsn$dim)=="lon"))) {
		msg<-paste("There is no dim with the name 'lon' in the nc-file '",
			file_nc_obj_procSsn["filename"][[1]],"'",sep="")
		stop(msg)
	}
	# Check correctness of the set dimension code
	if ((DimCode!="lon") & (DimCode!="lat")) {
		stop("Wrong dimension name")
	} else {
		N_dim<-ProcFile$dim[[DimCode]]$len
		coord_1d_array<-ProcFile$dim[[DimCode]]$vals
		i_min<-which.min(abs(coord_1d_array-coord_min))
		i_max<-which.min(abs(coord_1d_array-coord_max))
	}
	return(data.frame(min=i_min,max=i_max,count=(i_max-i_min)))
}
# obtain a 3D array of parameters' values for a set coordinate box
GetField_byTime<-function(ProcFile,x_bnd,y_bnd) {
	# the target variable is the last one (temperature/precip/runoff etc)
	param_name<-ProcFile$var[[length(ProcFile$var)]]$name
	i_long<-FindCells(ProcFile=ProcFile,DimCode="lon",coord_min=min(x_bnd),coord_max=max(x_bnd))
	i_lat<-FindCells(ProcFile=ProcFile,DimCode="lat",coord_min=min(y_bnd),coord_max=max(y_bnd))
	ParamField<-ncvar_get(ProcFile, varid=param_name, start=c(i_long$min,i_lat$min,1),
	 	count=c(i_long$count,i_lat$count,-1))
	return(ParamField)
}
# extract array of time moments
# a supplementary function which is used in ExtractSeason
# @ProcFile is a netCFD file to be processed (should be opened first)
# TODO: 1900 change on a reference to the system time
ExtractTime<-function(ProcFile) {
	year<-ProcFile$dim$time$vals				# time in days
	time_units<-ProcFile$dim$time$units
	origin_time_string_tail<-strsplit(time_units,split="since")[[1]][2]
	# sometimes since is followed by some whitespaces or a tabulation
	N_time_begin<-trimws(x=origin_time_string_tail, which = "left")
	N_time_begin<-strsplit(N_time_begin,split=" ")[[1]][1]
	tau<-as.Date(year,origin=N_time_begin)					# time as a date
	month_i<-months(tau)									# a month corresponding to a date
	time_asDate<-strptime(tau,format="%Y-%m-%d")			# time as a beautiful date
	year_asDate<-time_asDate$year+1900						# correction of default year representation
	J<-length(ProcFile$dim$time$vals)							# number of time moments
	# extract the first simulation year year_0
	t_0<-as.Date(year[1],origin=N_time_begin)
	t_cal_0<-strptime(t_0,format="%Y-%m-%d")
	year_0<-t_cal_0$year+1900
	# extract the last simulation year year_0
	t_N<-as.Date(year[J],origin=N_time_begin)
	t_cal_N<-strptime(t_N,format="%Y-%m-%d")
	year_N<-t_cal_N$year+1900
	date_result<-list(firstYear=year_0,lastYear=year_N,numberOfMoments=J,
		year_byDates=year_asDate,months=month_i,tau_daysAsDate=tau)
	return(date_result)
}
#________________________________________________________________________________________________________________________
# Seasons should be set via months of the current year
# @ProcFile is a netCFD file to be processed (should be opened first)
ExtractSeason<-function(ProcFile,SeasonPeriods=c("Èþíü","Èþëü","Àâãóñò")) {
	# Note please that Russian names of the months were used
	# TO DO set months names as parameters
	benchmark<-c("ßíâàðü","Ôåâðàëü","Ìàðò","Àïðåëü","Ìàé","Èþíü",
		"Èþëü","Àâãóñò","Ñåíòÿáðü","Îêòÿáðü","Íîÿáðü","Äåêàáðü")
	if (any(unlist(is.na(match(x=SeasonPeriods,table=benchmark))))) {
		stop("Wrong name of the month. Please check spelling and run again")
	}
	else {
		time_inFile<-ExtractTime(ProcFile)
		k_year=time_inFile$firstYear	# current year
		N_years<-time_inFile$lastYear-time_inFile$firstYear+1
		N_moments<-time_inFile$numberOfMoments
		y_year<-array(NA,dim=N_years)
		y_year[1]=k_year
		K<-1 							# counter of years
		n_season<-0 					# counter of seasonal entries in a given year
		tau_season<-list() 				# array of dates (which entries represent the selected season)
		length(tau_season)<-N_years		# storage for time moments corresponding to a given season (N is the number of years)
		seasonal_year<-list() 			# array of dates (which entries represent the selected season)
		length(seasonal_year)<-N_years	# storage for time moments corresponding to a given season (N is the number of years)		
		index_season<-list() 			# indices of seasonal entries in the 3D array of the measured value
		length(index_season)<-N_years
		index_counter<-0
		# cycle along the whole set of time moments
		for (i in 1:N_moments) {		#for (i in 1:N_moments) 
			year_i<-time_inFile$year_byDates[i]	# a year corresponding to the current time moment
				if  (year_i>k_year) { 			# spring to the next year
					k_year<-year_i				# set a new current year value
					K=K+1						# counter of years
					y_year[K]<-year_i 			# set current year
					n_season[K]<-0				# number of entries for a given season by years
				}	
				if (any(unlist(time_inFile$months[i]==SeasonPeriods))) {
					n_season[K]<-n_season[K]+1
					index_counter<-index_counter+1
					if (is.null(seasonal_year[[K]])) {
						seasonal_year[[K]]<-y_year[K]
						} else	{
							seasonal_year[[K]][n_season[K]]<-y_year[K]
						}					
					if (is.null(tau_season[[K]])) {
						tau_season[[K]]<-time_inFile$tau_daysAsDate[i]
						} else	{
							tau_season[[K]][n_season[K]]<-time_inFile$tau_daysAsDate[i]
						}
					if (is.null(index_season[[K]])) {
						index_season[[K]]<-i
						} else	{
							index_season[[K]][n_season[K]]<-i
						}
				}
		}
	# exclude zero & empty elements
	# 1) an attempt to exlude NULL elements causes an error if there is no 
		if (any(unlist(lapply(FUN=is.null,tau_season)))) {
			stop("no season entries for the year ",y_year[which(n_season==0)]," ",sep="\t")
			n_season<-n_season[-which(n_season==0)]
			tau_season<-tau_season[-which(sapply(tau_season, is.null))]
		}
	# 2) and for n_season as well	
		if (any(unlist(lapply(FUN=is.null,index_season)))) {
			stop("no season entries for the year ",y_year[which(n_season==0)]," ",sep="\t")
			n_season<-n_season[-which(n_season==0)]
			index_season<-index_season[-which(sapply(index_season, is.null))]
		}
		# extend parameters' array for further interpolation
		# that means addition to the seasonal series one result from the left (i-1) and one result from the right (i+1)
		tau_Extded<-list()
		index_Extded<-list()
		TimeDiffSighn<-0
		for (i in 1:length(seasonal_year)) {
			index_Extded[[i]]<-index_season[[i]]
			# TODO: Check boundary cases!!!
			index_Extded[[i]]<-c(index_Extded[[i]][1]-1,unlist(index_Extded[[i]]),
				index_Extded[[i]][length(index_Extded[[i]])]+1)
			# requested index may be outside of an available range
			# check will be done
			if (max(index_Extded[[i]])>length(time_inFile$tau_daysAsDate)) { # too big value is requested
				TimeDiffSighn<-1
				tau_Extded[[i]]<-time_inFile$tau_daysAsDate[index_Extded[[i]][-length(index_Extded[[i]])]]
				index_Extded[[i]]<-index_Extded[[i]][-length(index_Extded[[i]])]
			}
			else {
				if (min(index_Extded[[i]])<1) { 							# too low value is requested
					TimeDiffSighn<-(-1)
					tau_Extded[[i]]<-time_inFile$tau_daysAsDate[index_Extded[[i]][-1]]
					index_Extded[[i]]<-index_Extded[[i]][-1]
				}	
					else {
						tau_Extded[[i]]<-time_inFile$tau_daysAsDate[index_Extded[[i]]]
					}
			}
		}
		result_list<-list(SeasonalYear=seasonal_year,SeasonalDates=tau_season,SeasonalIndices=index_season,
			SeasonalDates_Extded=tau_Extded,SeasonalIndices_Extded=index_Extded,flag=TimeDiffSighn)
		return(result_list)		
		# result_list<-list(SeasonalYear=seasonal_year,SeasonalDates=tau_season,SeasonalIndices=index_season,
		# 	SeasonalDates_Extded=tau_Extded,SeasonalIndices_Extded=index_Extded)
	}
}
#
# TODO 1: Months names & Seasonal dates are duplicates. Change input parameters
# TODO 2: Length of SesonInterval_Canonical should correspond to a given interval
ApproxForSeason<-function(ProcFile,x_bnd,y_bnd,
	SeasonPeriods=c("Èþíü","Èþëü","Àâãóñò"),
	FirstSeasonDay_date="-06-01",LastSeasonDay_date="-08-31",i_OfYear) {
	# TODO Check of correspondance between the Seasonal period and the Seasonal dates
	# read filed parameter data
	# 		type: that's a 3D matrix
	# NB x corresponds to the first dim, i.e. rows which are VERTICAL on the screen
	T_3D<-GetField_byTime(ProcFile,x_bnd=x_bnd,y_bnd=y_bnd) # 3D array
	# read time data
	# 		type: that's a list
	T_Seasonal<-ExtractSeason(ProcFile,SeasonPeriods=SeasonPeriods)
	#
	T_Seasonal$SeasonalDates[[i_OfYear]]
	LargeTau<-T_Seasonal$SeasonalDates_Extded[[i_OfYear]]
	TimeIndicesOfT<-T_Seasonal$SeasonalIndices_Extded[[i_OfYear]]
	SeasYear<-unique(T_Seasonal$SeasonalYear[[i_OfYear]])
	#
	SeasonBeg_Canonical<-paste(SeasYear,FirstSeasonDay_date,sep="")
	SeasonEnd_Canonical<-paste(SeasYear,LastSeasonDay_date,sep="")
	SesonInterval_Canonical<-seq.Date(from=as.Date(SeasonBeg_Canonical),
	 	to=as.Date(SeasonEnd_Canonical),length.out=20)
	# TODO Do not mix local & global variables!!! (LargeTau)
	SeasonAveraging<-function(i,j) {
		# if the parameter is NA for any value of TimeIndicesOfT, 
		# it means that it wasn't simulated at all, at any time moment
		if (is.na(T_3D[i,j,TimeIndicesOfT[1]])) {
			ApprVal<-NA
		}
		else {
			ApprVal<-approx(x=LargeTau,
				y=T_3D[i,j,TimeIndicesOfT],xout=SesonInterval_Canonical)
			return(mean(ApprVal$y))
		}	
	}
	lengthX<-length(T_3D[,1,1])
	lengthY<-length(T_3D[1,,1])
	resField<-matrix(NA,ncol=lengthY,nrow=lengthX)
	for (j in (1:lengthY)) {
		resField[,j]<-sapply(FUN=function(i) SeasonAveraging(i,j=j),X=1:lengthX)
	}
				#return(SeasonAveraging(i=1,j=1))
				#return(T_Seasonal$SeasonalDates[[2]])
				# return(SesonInterval_Canonical)
				#return(LargeTau)
				#return(str(T_3D))
				#return(T_3D[6,1,TimeIndicesOfT])
				#return(T_3D[,1,1])
				#return(T_3D[i=1,j=1,TimeIndicesOfT])
				#return(SeasonAveraging(i=1,j=1))
				#return(sapply(FUN=function(i) SeasonAveraging(i,j=1),X=1:6))
				#return(lengthX)
	return(z=resField)	# the result is the simplest matrix
}
#