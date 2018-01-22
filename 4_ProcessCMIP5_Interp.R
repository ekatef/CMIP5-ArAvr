#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# be carefull with encoding, please
# the issue with system language isn't solved yet
# the months' names are cyrillic as far
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  INPUT of coordinates of the considered area
# 1) x is lontitude, y is latitude! (geografy has its own logic)
# 2) the considered area is rectangular for the sake of simplicity 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# following parameters sholud be set (that's done in the Config file)
# 	1) file_name names of the processed files (including the path if nessecary)
# 	2) CMIP5_dir_name is the directory with CMIP5 calculation files
# 	3) wd_name is the name of the working directory were results will be placed
library("fields")
library("maps")
# all_months_names<-c("Январь","Февраль","Март","Апрель","Май","Июнь","Июль","Август","Сентябрь","Октябрь","Ноябрь","Декабрь")
# # extract names of the files in CMIP5 directory
# # returns a vector of the files' names
# ListCMIPFiles<-function(CMIP5_DirName=CMIP5_dir_name,WDName=wd_name) {
# 	setwd(CMIP5_DirName)
# 	filesInDir_names<-dir()
# 	setwd(WDName)
# 	last_chars_in_file_names<-substr(filesInDir_names, nchar(filesInDir_names)-3+1, nchar(filesInDir_names))
# 	if (length(filesInDir_names)==0) {stop("The folder is empty. Check it please")}
# 	if (!any(last_chars_in_file_names==".nc")) {
# 		stop("No NetCFD files in the folder. Check please")
# 	} #else {n_files<-length(which(last_chars_in_file_names==".nc"))}
# 	filesInDir_names<-filesInDir_names[which(last_chars_in_file_names==".nc")]
# 	# the files' names don't include the path -- otherwise it would be too long
# 	return(filesInDir_names)
# }
# # look to the CMIP5 dir for the simulation files
# # return a dataframe with parameters of the models
# # TODO: Find duplicated files
# ExtractModels<-function(CMIP5_DirName=CMIP5_dir_name, WDName=wd_name) {
# 	# reading from the CMIP5 simulation file
# 	SimParam_df<-data.frame(FileName=character(),
# 		ModelName=character(),ScenarioName=character(),ParamName=character(),
# 		t0_internal=numeric(),tN_internal=numeric(),Origin_Time=numeric(),
# 		t0=numeric(),tN=numeric(),ArrayID=numeric(),year_0=numeric(),
# 		year_N=numeric(),month_0=numeric(),month_N=numeric(),
# 		stringsAsFactors=FALSE)
# 	filesInDir_names<-ListCMIPFiles(CMIP5_DirName=CMIP5_DirName,WDName)
# 	setwd(CMIP5_DirName)
# 	StatBar <- txtProgressBar(min = 1, max = length(filesInDir_names), style = 3)
# 	cat("Calculation of SimParam_df \n\r", sep = "")
# 	for (i in 1:length(filesInDir_names)) {
# 		setTxtProgressBar(StatBar, i,label = i)
# 		file_nc_obj<-nc_open(filesInDir_names[i],verbose=FALSE)
# 		# the target variable is the last one (temperature/precip/runoff etc)
# 		param_name<-file_nc_obj$var[[length(file_nc_obj$var)]]$name
# 		file_attributes<-ncatt_get(file_nc_obj,varid=0)
# 		model_name<-file_attributes$model_id
# 		scenario_name<-file_attributes$experiment_id
# 		time<-file_nc_obj$dim$time$vals
# 		begin_time<-time[1]
# 		end_time<-time[length(time)]
# 		origin_time_string_tail<-strsplit(file_nc_obj$dim$time$units,split="since")[[1]][2]
# 		# it may be a whitespace after "since" or some whitespaces or tab;
# 		# that's more reliable to delete all possible delimeters
# 		origin_time_string_tail<-trimws(x=origin_time_string_tail, which = "left")
# 		N_time_origin<-strsplit(origin_time_string_tail,split=" ")[[1]][1]
# 		tau_0<-begin_time
# 		tau_N<-end_time
# 		tau_0<-as.Date(begin_time,origin=N_time_origin)
# 		tau_N<-as.Date(end_time,origin=N_time_origin)
# 		time_0_asDate<-strptime(tau_0,format="%Y-%m-%d")			# time as a beautiful date
# 		year_0_asDate<-time_0_asDate$year+1900						# correction of default year representation
# 		time_N_asDate<-strptime(tau_N,format="%Y-%m-%d")			# time as a beautiful date
# 		year_N_asDate<-time_N_asDate$year+1900						# correction of default year representation
# 		month_0_asDate<-months(tau_0)
# 		month_N_asDate<-months(tau_N)
# 		# month_0_asDate<-file_nc_obj$dim$time$units
# 		# month_N_asDate<-tau_N
# 		# TO DO t0 and tN don't seem to be useful
# 		SimParam_df[i,]<-list(FileName=filesInDir_names[i],ModelName=model_name,ScenarioName=scenario_name,
# 			ParamName=param_name,
# 		 	t0_internal=begin_time,tN_internal=end_time,Origin_Time=N_time_origin,
# 		 	t0=tau_0,tN=tau_N,ArrayID=i,year_0=year_0_asDate,year_N=year_N_asDate,
# 		 	month_0=month_0_asDate,month_N=month_N_asDate)
# 		nc_close(file_nc_obj)
# 	}
# 	setwd(WDName)
# 	if (length(unique(SimParam_df$ScenarioName))!=1) {
# 		message("The models imply following parameters")
# 		print(SimParam_df[,c("ModelName","ScenarioName","year_0","year_N")])
# 		stop("The considered scenarios are different. Please check the processed files")
# 	}	
# 	if (length(unique(SimParam_df$ParamName))!=1) {
# 		message("The models contain following parameters")
# 		print(SimParam_df[,c("ModelName","ParamName","year_0","year_N")])
# 		stop("The considered parameters are different. Please check the processed files")
# 	}
# 	# all fileds except ArrayID [9th column] should be unique
# 	UniqueRows_length<-length(unique(SimParam_df[,-9])[,1])
# 	AllRows_length<-length(SimParam_df[,1])
# 	if (UniqueRows_length!=AllRows_length) {
# 		message(paste("The number of duplicated data files is",
# 			(AllRows_length-UniqueRows_length),sep=" "))
# 		stop("Duplicated Simulation File. Check processed files please")
# 	}
# 	return(SimParam_df)	
# }	
# ExtractGrid <- function(ProcFile,x_bnd,y_bnd) {
# 	i_lon_test_df<-FindCells(ProcFile = ProcFile,DimCode="lon",
# 		coord_min=min(x_bnd),coord_max=max(x_bnd))
# 	i_lon_range<-seq(from=i_lon_test_df$min,to=i_lon_test_df$max,length=i_lon_test_df$count)
# 	x_NCGrid<-ProcFile$dim[["lon"]]$vals[i_lon_range]
# 	i_lat_test_df<-FindCells(ProcFile=ProcFile,DimCode="lat",
# 		coord_min=min(y_bnd),coord_max=max(y_bnd))
# 	i_lat_range<-seq(from=i_lat_test_df$min,to=i_lat_test_df$max,length=i_lat_test_df$count)
# 	y_NCGrid<-ProcFile$dim[["lat"]]$vals[i_lat_range]
# 	return(list(GridLon=x_NCGrid,GridLat=y_NCGrid))	
# }
# RegridModel<-function(param_matrix,x_bnd,y_bnd,
# 	x_grid,y_grid,n_Regrid_Cells) {
# 	obj_test<-list(x=x_grid,y=y_grid,z=param_matrix)
# 	x_ToInt<-seq(from=min(x_bnd),to=max(x_bnd),length.out=n_Regrid_Cells)
# 	y_ToInt<-seq(from=min(y_bnd),to=max(y_bnd),length.out=n_Regrid_Cells)
# 	loc_test<- list(x=x_ToInt,y=y_ToInt)
# 	#test_int<-interp.surface(obj_test,loc_test)
# 	# TODO generalisation of interp.surface is needed process different x & y steps
# 	test_int<-interp.surface.grid(obj_test,loc_test)
# 	return(test_int)
# }
									# # 	#
									# # 	# strings/Long are x
									# # 	# coulmns/Lat are y
									# 	nLat_short<-seq(from=1,to=200,by=25)
									# 	nLong_short<-seq(from=1,to=200,by=5)
									# # 	MMEns_Delta_Short<-matrix(NA,ncol=length(nLong_short),nrow=length(nLat_short))
									# # 	str(MMEns_Delta_Short)
									# # 	#
									# # 	for (j in seq(along.with=nLat_short)) {
									# # 		MMEns_Delta_Short[j,]<-sapply(FUN=function(i) MMEns_Delta[nLat_short[j],nLong_short[i]],
									# # 					X=seq(along.with=nLong_short))
									# # 	}	
									# # #
									# 	x_ReGrid<-seq(from=x_RU_small,to=x_RU_big,length.out=200)
									# 	y_ReGrid<-seq(from=y_RU_small,to=y_RU_big,length.out=200)
									# 	write.table(file="Pr_delta.txt",as.data.frame(MMEns_Delta))
									# 	write.table(file="Pr_delta_shorten.txt",as.data.frame(MMEns_Delta_Short))
									# 	write.table(file="Lat_short.txt",as.data.frame(y_ReGrid[nLat_short]))
									# 	write.table(file="Long_short.txt",as.data.frame(x_ReGrid[nLong_short]))
									# 	#