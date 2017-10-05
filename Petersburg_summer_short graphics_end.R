#========================================================================
# Read CMIP5 data and calculates an area-average for a certain place
# and for a given season (here it's summer)
# The result is a file whith time-series
# An input file is required (set with InputFile_name)
# which following information: 
# Scenario_Name, Model_Name, Begin_Time, Dir_Name, Test_Dir_Name, File_Name
#
# TO DO1: include a possibility to switch the season in the same file
# (n_season[K]<-06-01; ((month_i[i]=="Èþíü"|month_i[i]=="Èþëü")|(month_i[i]=="Èþëü"|month_i[i]=="Àâãóñò")))
# SeasonBeg_Canonical<-paste(SeasYear,"-06-01",sep="")
# SeasonEnd_Canonical<-paste(SeasYear,"-08-31",sep="")
# TO DO2: it should be possible to process not only temperatures data (the key: varid='tas')
#========================================================================
rm(list=ls())
library("ncdf4")
InputFile_name<-"D://_CFD Data_//Data Processing//Scripts//Models_Details.txt"
ResultDir_name<-"D://_CFD Data_//Data Processing//Results//Test//"
#  INPUT of coordinates of the considered area
# 1) x is lontitude, y is latitude! (geografy has its own logic)
# 2) the considered area is rectangular for the sake of simplicity 
x_small<-30.0
x_big<-30.4
y_big<-59.8
y_small<-59.4
GeoRegion_name<-"Petersburg"
#	Read information needed to run the models
Model_Details<-read.table(InputFile_name,header = TRUE,stringsAsFactors=FALSE)
scenario_name<-Model_Details$Scenario_Name
model_name<-Model_Details$Model_Name
N_time_begin<-Model_Details$Begin_Time
dir_name<-Model_Details$Dir_Name
test_dir_name<-Model_Details$Test_Dir_Name
CMIP_file_name<-Model_Details$File_Name
file_name<-paste(dir_name,CMIP_file_name,sep="")
result_file_name<-paste(ResultDir_name,GeoRegion_name,"//",model_name,"_",scenario_name,"_summer.txt",sep="")
result_details_file_name<-paste(ResultDir_name,GeoRegion_name,"//",model_name,"_",scenario_name,"_summer_detai.txt",sep="")
result_details2_file_name<-paste(ResultDir_name,GeoRegion_name,"//",model_name,"_",scenario_name,"_summer_detai2.txt",sep="")
picture_file_name<-paste(ResultDir_name,GeoRegion_name,"//",model_name,"_",scenario_name,"_summer.png",sep="")
pdf_name<-paste(ResultDir_name,GeoRegion_name,"//",model_name,"_",scenario_name,"_summer_scatt.pdf",sep="")
info_file_name<-paste(model_name,"_",scenario_name,"_info.dat",sep="")
#
			#***************************
			#	TESTING
			#i_models<-1
			#***************************
#__________________________________________________________________________________
#	
#		CYCLE FOR PROCESSING of NetCFD Data
#__________________________________________________________________________________
# cycle along the models list
# the files of CMIP5 results are quite large
# we process them one-by-one
# the result of each cycle iteration is a result file with time-series obtained by a single model
			#***************************
			#	TESTING
			#***************************
			for (i_models in (4:4)) {
#for (i_models in seq(along.with=Model_Details$Model_Name)) {
	nc_open(file_name[i_models],verbose=FALSE)
	file_nc_obj<-nc_open(file_name[i_models])
	# attributes of the model file will be used to print actual simulation data in the result file 
	# it may be critical for check-up
	file_attributes<-ncatt_get(file_nc_obj,varid=0)
#________________________________________________________________________________________________________________________
#
#		work with spacial interpolation
#________________________________________________________________________________________________________________________
	#variables for coordinates cycle
	i_lat=1
	N_lon=file_nc_obj$dim$lon$len
	i_lat=1
	N_lat=file_nc_obj$dim$lat$len
	#for x_min: find the corresponding grid cell
	i_lon=1
	repeat {
		i_lon=i_lon+1
		if (i_lon>N_lon) {
		i_lon_min=-999
		break
		}
		if ((x_small<file_nc_obj$dim$lon$vals[i_lon-1])&(x_small>=file_nc_obj$dim$lon$vals[i_lon])) {
		i_lon_min=i_lon-1
		break
		}
		if ((x_small>=file_nc_obj$dim$lon$vals[i_lon-1])&(x_small<file_nc_obj$dim$lon$vals[i_lon])) {
		i_lon_min=i_lon-1
		break
		}
	}
	#for x_max: find the corresponding grid cell
	i_lon=1
	repeat {
		i_lon=i_lon+1
		if (i_lon>N_lon) {
		i_lon_max=-999
		break
		}
		if ((x_big<=file_nc_obj$dim$lon$vals[i_lon-1])&(x_big>file_nc_obj$dim$lon$vals[i_lon])) {
		i_lon_max=i_lon
		break
		}
		if ((x_big>file_nc_obj$dim$lon$vals[i_lon-1])&(x_big<=file_nc_obj$dim$lon$vals[i_lon])) {
		i_lon_max=i_lon
		break
		}
	}
	#for y_min: find the corresponding grid cell
	i_lat=1
	repeat {
		i_lat=i_lat+1
		if (i_lat>N_lat) {
		n_lat_min=-999
		break
		}
		if ((y_small<file_nc_obj$dim$lat$vals[i_lat-1])&(y_small>=file_nc_obj$dim$lat$vals[i_lat])) {
		i_lat_min=i_lat-1
		break
		}
		if ((y_small>=file_nc_obj$dim$lat$vals[i_lat-1])&(y_small<file_nc_obj$dim$lat$vals[i_lat])) {
		i_lat_min=i_lat-1
		break
		}
	}
	#for y_max: find the corresponding grid cell
	i_lat=1
	repeat {
		i_lat=i_lat+1
		if (i_lat>N_lat) {
		i_lat_max=-999
		break
		}
		if ((y_big<=file_nc_obj$dim$lat$vals[i_lat-1])&(y_big>file_nc_obj$dim$lat$vals[i_lat])) {
		i_lat_max=i_lat
		break
		}
		if ((y_big>file_nc_obj$dim$lat$vals[i_lat-1])&(y_big<=file_nc_obj$dim$lat$vals[i_lat])) {
		i_lat_max=i_lat
		break
		}
	}
	# handling single compatational cells (small area was set for computation)
	# checking for dimensions number for 
	if (i_lon_max==i_lon_min) {
		lon_count<-1
		delta_lon<-1
	} else {
		lon_count<-(i_lon_max-i_lon_min)
		delta_lon<-i_lon_max-i_lon_min
	}
	if (i_lat_max==i_lat_min) {
		lat_count<-1
		delta_lat<-1
	} else {
		lat_count<-(i_lat_max-i_lat_min)
		delta_lat<-i_lat_max-i_lat_min
	}
	# averaging across the area	
	T_x_y_t<-ncvar_get(file_nc_obj, varid='tas', start=c(i_lon_min,i_lat_min,1),count=c(lon_count,lat_count,-1))
	# functions ncvar_get() works differently for different dimensions
	if (any(c(delta_lon,delta_lat)==1)) {
		if (all(c(delta_lon,delta_lat)==1)) {
			T_tau<-T_x_y_t
			print("if0")
		} else {
			print("else1")	
			T_tau<-colMeans(T_x_y_t,dims=1)
		}
	} else {
		print("else2")
		T_tau<-colMeans(T_x_y_t,dims=2)
		}
#________________________________________________________________________________________________________________________
#
#		work with time
#________________________________________________________________________________________________________________________
	# array of time moments
	year<-file_nc_obj$dim$time$vals				# time in days
	tau<-as.Date(year,origin=N_time_begin[i_models])	# time as a date
	month_i<-months(tau)					# a month corresponding to a date
	time_asDate<-strptime(tau,format="%Y-%m-%d")			# time as a beautiful date
	year_asDate<-time_asDate$year+1900				# correction of default year representation
	# number of time moments
	J<-file_nc_obj$dim$time$len
	# calculation of the first simulation year year_0
	t_0<-as.Date(year[1],origin=N_time_begin[i_models])
	t_cal_0<-strptime(t_0,format="%Y-%m-%d")
	year_0<-t_cal_0$year+1900
	# calculation of the last simulation year year_0
	t_N<-as.Date(year[J],origin=N_time_begin[i_models])
	t_cal_N<-strptime(t_N,format="%Y-%m-%d")
	year_N<-t_cal_N$year+1900
	# number of modeled years
	N=year_N-year_0+1
	# counter of years
	k_year=year_0
	y_year<-array(-1,dim=N)
	y_year[1]=k_year
	K<-1
#________________________________________________________________________________________________________________________
#
#	transformation of a continous temperature array *tau* to seasonal mean
#________________________________________________________________________________________________________________________
	# counter of entries of the season
	n_season<-0
	tau_season<-list()
	length(tau_season)<-N	# storage for time moments corresponding to a given season (N is the number of years)
	T_season<-list()
	length(T_season)<-N	# storage for seasonal temperature values (N is the number of years)
	# cycle along the whole set of time moments
			#*********************************
			#	TESTING
			#*********************************
			#	for (i in 1:1) {
	for (i in 1:J) {
		year_i<-year_asDate[i]	# a year corresponding to the current time moment
		# spring to the next year
		if  (year_i>k_year) {
			k_year<-year_i	# set a new current year value
			K=K+1
			y_year[K]<-year_i
			# was it a mistake???
			# n_season[K]<-06-01			# number of entries for a given season by years
			n_season[K]<-0			# number of entries for a given season by years
			T_season[[K]]<-c(-999)	# temperatures corresponding to a given season by years
		}
		# month_i<-months(tau)
		if ((month_i[i]=="Èþíü"|month_i[i]=="Èþëü")|(month_i[i]=="Èþëü"|month_i[i]=="Àâãóñò")) {
			n_season[K]=n_season[K]+1
			T_season[[K]][n_season[K]]<-T_tau[i]
			if (is.null(tau_season[[K]])) {
				tau_season[[K]]<-tau[i]
			}	else	{
					tau_season[[K]][n_season[K]]<-tau[i]
				}
		}
	}
	#
	nc_close(file_nc_obj)
# let's exclude zero & empty elements
# 1) an attempt to exlude NULL elements causes an error if there is no 
# such elements. Additional check should be done before to exclude NULL elements
	if (any(lapply(FUN=is.null,tau_season))) {
		T_season<-T_season[-which(sapply(tau_season, is.null))]
		n_season<-n_season[-which(n_season==0)]
		tau_season<-tau_season[-which(sapply(tau_season, is.null))]
	}
# 2) the same check should be done for each vector: for T_season
	if (any(lapply(FUN=is.null,T_season))) {
		print("if_T works")
		tau_season<-tau_season[-which(sapply(T_season, is.null))]
		n_season<-n_season[-which(sapply(T_season, is.null))]
		T_season<-T_season[-which(sapply(T_season, is.null))]
	}
# 3) and for n_season as well	
	#
	if (any(n_season==0)) {
		print("if_n_season works")
		tau_season<-tau_season[-which(n_season==0)]
		T_season<-T_season[-which(n_season==0)]
		n_season<-n_season[-which(n_season==0)]
	}
	#
#**********************
# construct the whole list of the results
	SeasonalTemper<-mapply(tau_season,T_season,n_season, FUN=list, SIMPLIFY=FALSE)
# modify names of the list members
# nested items
	SeasonalTemper<-lapply(1:length(SeasonalTemper), 
     		function(i) setNames(SeasonalTemper[[i]],c("moments","T_season","n_season")))
# the upper fist level
	names(SeasonalTemper)<-paste("Seasons",c(1:length(SeasonalTemper)),sep="_")
#_____________________________________________________________________________________
#	Entries of each season correspond to different days for each year
#	(e.g. the first summer entry may be 1. or 25. June -- that makes difference!)
#	So, seasonal temperature should be interpolated between entries 
#	using the neighbour entries for each season (here the last spring and the first autumn days)
#
#	add boundary values from the left and the right to a vector of seasonal temperatures 
enlarge_TSeasn<-function(T_to_procc,i_s){
	i_SeasnBeg<-which.min(abs(T_to_procc[[i_s]][[2]][1]-T_tau))
	i_SeasnEnd<-which.min(abs(T_to_procc[[i_s]][[2]][length(T_to_procc[[i_s]])]-T_tau))
	#T_to_procc[[i_s]][[2]]<-c(T_tau[i_SeasnBeg-1],T_to_procc[[i_s]],T_tau[i_SeasnEnd+1])
	return(c(T_tau[i_SeasnBeg-1],T_to_procc[[i_s]][[2]],T_tau[i_SeasnEnd+1]))
}
#	add boundary values from the left and the right to a vector of seasonal moments 
enlarge_tauSeasn<-function(tau_to_procc,i_s){
	i_SeasnBeg<-which.min(abs(tau_to_procc[[i_s]][[1]][1]-tau))
	i_SeasnEnd<-which.min(abs(tau_to_procc[[i_s]][[1]][length(tau_to_procc[[i_s]])]-tau))
	return(c(tau[i_SeasnBeg-1],tau_to_procc[[i_s]][[1]],tau[i_SeasnEnd+1]))
}
#	interpolation of Seasonal temperatures and calculation of seasonal mean
Large_SeasonalTemp<-lapply(FUN=enlarge_TSeasn,T_to_procc=SeasonalTemper,
	X=seq(along.with=SeasonalTemper))
Large_Seasonaltau<-lapply(FUN=enlarge_tauSeasn,tau_to_procc=SeasonalTemper,
	X=seq(along.with=SeasonalTemper))
#
SeasTime<-lapply(FUN=function(i) as.Date(Large_Seasonaltau[[i]][1],origin=N_time_begin[i_models]),X=c(1:length(Large_Seasonaltau)))
SeasTime_format<-lapply(FUN=function(i) strptime(SeasTime[[i]][1],format="%Y-%m-%d"),X=c(1:length(Large_Seasonaltau)))
SeasYear<-sapply(FUN=function(i) SeasTime_format[[i]][1]$year+1900,X=c(1:length(Large_Seasonaltau)))
#
SeasonBeg_Canonical<-paste(SeasYear,"-06-01",sep="")
SeasonEnd_Canonical<-paste(SeasYear,"-08-31",sep="")
#
seq.Date(from=as.Date(SeasonBeg_Canonical[1]),to=as.Date(SeasonEnd_Canonical[1]),length.out=20)
SesonInterval_Canonical<-lapply(FUN=function(i) seq.Date(from=as.Date(SeasonBeg_Canonical[i]),
	to=as.Date(SeasonEnd_Canonical[i]),length.out=20),X=c(1:length(Large_Seasonaltau)))
#
SeasonTemp_Canonical<-lapply(FUN=function(i) approx(x=Large_Seasonaltau[[i]],
	y=Large_SeasonalTemp[[i]],xout=SesonInterval_Canonical[[i]]),X=c(1:length(Large_Seasonaltau)))
#__________________________________________________________________________________________________
#
#		work with mean values
#__________________________________________________________________________________________________
#
SeasonTemp_Mean<-sapply(FUN=function(i) mean(SeasonTemp_Canonical[[i]]$y),X=c(1:length(Large_Seasonaltau)))
Seasonal_Results<-mapply(SeasYear,Large_Seasonaltau,Large_SeasonalTemp, 
	SeasonTemp_Mean,FUN=list, SIMPLIFY=FALSE)
# rename nested items
	Seasonal_Results<-lapply(1:length(Seasonal_Results), 
     		function(i) setNames(Seasonal_Results[[i]],c("Year","SeasonMoments",
		"SeasonTemperEntries","MeanSeasTemper")))
# rename the upper fist level
	names(Seasonal_Results)<-paste("Seasons",c(1:length(SeasonalTemper)),sep="_")
head(Seasonal_Results,n=3)
Seasonal_Results<-list(Year=SeasYear,SeasonMoments=Large_Seasonaltau,
	SeasonTemperEntries=Large_SeasonalTemp,MeanSeasTemper=SeasonTemp_Mean)
#	construct parameters to output
#	check model's attributes
	Scenario_name<-file_attributes$experiment
	Model_name<-file_attributes$model_id
			#**********************
			#	TESTING
			#**********************
				#T_season_LgMatrix<-cbind(SeasYear,Model_name,Scenario_name,GeoRegion_name,
				#	Seasonal_Results$SeasonMoments[[1]][1],Seasonal_Results$SeasonMoments[[1]][2],Seasonal_Results$SeasonMoments[[1]][3],
				#	Seasonal_Results$SeasonTemperEntries[[1]][1],Seasonal_Results$SeasonTemperEntries[[1]][2],
				#	Seasonal_Results$SeasonTemperEntries[[1]][3],SeasonTemp_Mean)
#**********************
	T_season_Matrix<-cbind(SeasYear,Model_name,Scenario_name,GeoRegion_name,SeasonTemp_Mean)
	write.table(T_season_Matrix,file=result_file_name[i_models], quote = FALSE,
		row.names = FALSE,col.names=FALSE)
			#**********************
			#	TESTING
			#**********************
				#write.table(T_season_LgMatrix,file=result_details2_file_name[i_models], quote = FALSE,
				#	row.names = FALSE,col.names=FALSE)
			#**********************
				#
				#write.table(Seasonal_Results,file=result_details_file_name[i_models], quote = FALSE,
				#	row.names = FALSE,col.names=FALSE)
				#
#_______________________________________________________________________________________________
#		Graphical Part
#_______________________________________________________________________________________________
pdf(pdf_name[i_models])
	plot(x=SeasYear,y=SeasonTemp_Mean,t="l",col="red",xlab="Year",ylab="Seasonal Temperature, K",
		panel.first=grid(),lwd=2,main=paste(Model_name,", ",Scenario_name,sep=""))
	dev.off()
#******************
# TESTING
#******************
}

#******************
#_______________________________________________________________________________________________
