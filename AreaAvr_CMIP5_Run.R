# TODO change input of locations' names
# InputFile_name<-"D://CMIP5//Models_Details.txt"
# ResultDir_name<-"D://_CFD Data_//Data Processing//Results//Test//"
#  INPUT of coordinates of the considered area
# 1) x is lontitude, y is latitude! (geografy has its own logic)
# 2) the considered area is rectangular for the sake of simplicity 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# following parameters sholud be set
# 	1) file_name names of the processed files (including the path if nessecary)
# 	2) CMIP5_dir_name is the directory with CMIP5 calculation files
# 	3) wd_name is the name of the working directory were results will be placed
#
all_months_names<-c("январь","‘евраль","ћарт","»юнь","»юль","јвгуст","—ент€брь","ќкт€брь","Ќо€брь","ƒекабрь")
# FirstSeasonDay_date<-"-06-01"
# LastSeasonDay_date<-"-08-31"
x_small<-27.5
x_big<-27.8
y_big<-53.9
y_small<-53.8
GeoRegion_name<-"Petersburg"
years_for_calcul<-2095:2105
# extract names of the files in CMIP5 directory
# returns a vector of the files' names
ListCMIPFiles<-function(CMIP5_DirName=CMIP5_dir_name,WDName=wd_name) {
	setwd(CMIP5_DirName)
	files_names<-dir()
	setwd(WDName)
	last_chars_in_file_names<-substr(files_names, nchar(files_names)-3+1, nchar(files_names))
	if (length(files_names)==0) {stop("The folder is empty. Check it please")}
	if (!any(last_chars_in_file_names==".nc")) {
		stop("No NetCFD files in the folder. Check please")
	} #else {n_files<-length(which(last_chars_in_file_names==".nc"))}
	files_names<-files_names[which(last_chars_in_file_names==".nc")]
	# the files' names don't include the path -- otherwise it would be too long
	return(files_names)
}
# look to the CMIP5 dir for the simulation files
# Returns a dataframe with parameters of the models
# TODO: Find duplicated files
ExtractModels<-function(CMIP5_DirName=CMIP5_dir_name,WDName=wd_name) {
	# reading from the CMIP5 simulation file
	SimParam_df<-data.frame(ModelName=character(),ScenarioName=character(),ParamName=character(),
		t0_internal=numeric(),tEnd_internal=numeric(),Origin_Time=numeric(),
		t0=numeric(),tN=numeric(),ArrayID=numeric(),year_0=numeric(),
		year_N=numeric(),month_0=numeric(),month_N=numeric(),
		stringsAsFactors=FALSE)
	files_names<-ListCMIPFiles(CMIP5_DirName,WDName)
	setwd(CMIP5_DirName)
	for (i in 1:length(files_names)) {
		file_nc_obj<-nc_open(files_names[i],verbose=FALSE)
		# the target variable is the last one (temperature/precip/runoff etc)
		param_name<-file_nc_obj$var[[length(file_nc_obj$var)]]$name
		file_attributes<-ncatt_get(file_nc_obj,varid=0)
		model_name<-file_attributes$model_id
		scenario_name<-file_attributes$experiment_id
		time<-file_nc_obj$dim$time$vals
		begin_time<-time[1]
		end_time<-time[length(time)]
		origin_time_string_tail<-strsplit(file_nc_obj$dim$time$units,split="since ")[[1]][2]
		N_time_origin<-strsplit(origin_time_string_tail,split=" ")[[1]][1]
		tau_0<-as.Date(begin_time,origin=N_time_origin)
		tau_N<-as.Date(end_time,origin=N_time_origin)
		time_0_asDate<-strptime(tau_0,format="%Y-%m-%d")			# time as a beautiful date
		year_0_asDate<-time_0_asDate$year+1900						# correction of default year representation
		time_N_asDate<-strptime(tau_N,format="%Y-%m-%d")			# time as a beautiful date
		year_N_asDate<-time_N_asDate$year+1900						# correction of default year representation
		month_0_asDate<-months(tau_0)
		month_N_asDate<-months(tau_N)
		SimParam_df[i,]<-list(ModelName=model_name,ScenarioName=scenario_name,ParamName=param_name,
		 	t0_internal=begin_time,tEnd_internal=end_time,Origin_Time=N_time_origin,
		 	t0=tau_0,tN=tau_N,ArrayID=i,year_0=year_0_asDate,year_N=year_N_asDate,
		 	month_0=month_0_asDate,month_N=month_N_asDate)

		nc_close(file_nc_obj)
	}
	setwd(WDName)
	if (length(unique(SimParam_df$ScenarioName))!=1) {
		message("The models imply following parameters")
		print(SimParam_df[,c("ModelName","ScenarioName","year_0","year_N")])
		stop("The considered scenarios are different. Please check the processed files")
	}	
	if (length(unique(SimParam_df$ParamName))!=1) {
		message("The models contain following parameters")
		print(SimParam_df[,c("ModelName","ParamName","year_0","year_N")])
		stop("The considered parameters are different. Please check the processed files")
	}
	# all fileds except ArrayID [9th column] should be unique
	UniqueRows_length<-length(unique(SimParam_df[,-9])[,1])
	AllRows_length<-length(SimParam_df[,1])
								# # two files with the same model name and the same begin year
								# # are supposed to be duplicates
								# DuplNamesInd_bool<-duplicated(SimParam_df$ModelName)
								# DuplNames<-unique(SimParam_df$ModelName[which(DuplNamesInd_bool)])
								# FirstEntrDuplNames<-match(x=DuplNames,table=SimParam_df$ModelName) # first entry
								# DuplNamesInd<-c(which(DuplNamesInd_bool),FirstEntrDuplNames)
								# DuplYrsInd_bool<-duplicated(SimParam_df$year_0)
								# DuplYrs<-unique(SimParam_df$year_0[which(DuplYrsInd_bool)])
								# FirstEntrDuplYrs<-match(x=DuplYrs,table=SimParam_df$year_0) # first entry
								# DuplYearsInd<-c(which(DuplYrsInd_bool),FirstEntrDuplYrs)
								# CommonModelsInd<-intersect(DuplNamesInd,DuplYearsInd)
								# #
								# unique(SimParam_df)
								# #ComEltInd<-intersect(which(DuplNames),which(DuplYrs)) # all entries apart the first one		
								# FirstIntrInd<-match(DuplNames[ComEltInd],DuplNames) # first entry
								# DuplYrsInd<-c(which(DuplYrsInd_bool),FirstEntrDuplYrs)
	if (UniqueRows_length!=AllRows_length) {
		message(paste("The number of duplicated data files is",
			(AllRows_length-UniqueRows_length),sep=" "))
		stop("Duplicated Simulation File. Check processed files please")
	}
	return(SimParam_df)	
}	
# assess to information about all realisation of the chosen model 
FindModelInfo<-function(SimParam_DF,i_model) {
	ModelNames_kit<-unique(SimParam_DF$ModelName)
	# ModelN_Entrees_kit<-array(dim=length(ModelNames_kit))
	# N_EntreesOfModel<-length(which(SimParam_DF$ModelName==ModelNames_kit[i_model]))
	# ModelN_Entrees_kit<-N_EntreesOfModel
	Model_Nth_df<-SimParam_DF[which(SimParam_DF$ModelName==ModelNames_kit[i_model]),]
	Model_Nth_df<-Model_Nth_df[order(Model_Nth_df$t0),]
	return(Model_Nth_df)
}
# the result is a matrix of spatial distribution
# of the parameter during the given years
ProcessModelledYears<-function(CMIP5_DirName=CMIP5_dir_name,WDName=wd_name,
	SimParam_DF,i_model,ProcYears,SeasonPeriods=c("»юнь","»юль","јвгуст"),
	FirstSeasonDay_date="-06-01",LastSeasonDay_date="-08-31",
	x_min=x_small,x_max=x_big,
	y_min=y_small,y_max=y_big) {
	ModelInfo_df<-FindModelInfo(SimParam_DF,i_model)
	#return(ModelInfo_df)
	attach(ModelInfo_df) # just for short
		# TODO: correct on for the case of overlap of the begin and the end years (e.g. end on Oct 2020, begin on Nov 2020)
		IDs_Of_ModelFiles<-sapply(FUN=function(i) ArrayID[(ProcYears[i]>=year_0)&(ProcYears[i]<=year_N)],
			X=seq(along.with=ProcYears))
		# remove dublicates
		IDs_Of_ModelFiles<-unique(IDs_Of_ModelFiles)
		# exclude NULL elements and unlist
		IDs_Of_ModelFiles<-unlist(Filter(IDs_Of_ModelFiles,f=length))
	detach(ModelInfo_df)
	ClimParam<-list()
	length(ClimParam)<-length(ProcYears)
	entries_counter<-1
	files_names<-ListCMIPFiles(CMIP5_DirName=CMIP5_DirName,WDName=WDName)
	setwd(CMIP5_DirName)
	for (i_ModelRun in (seq(along.with=IDs_Of_ModelFiles))) {
		file_nc_obj<-nc_open(files_names[IDs_Of_ModelFiles[i_ModelRun]],verbose=FALSE)
		testYr<-ExtractSeason(ProcFile=file_nc_obj,SeasonPeriods=SeasonPeriods)
		SimulatedYears<-sapply(FUN=function(i) unique(testYr$SeasonalYear[[i]]),
			X=1:length(testYr$SeasonalYear))
		# empty elements may appear in Indices_Of_year_InModelFile as not all ProcYears may
		# present in the current model realisation; exclude with the Filter 
		Indices_Of_year_InModelFile<-sapply(FUN=function(i) which(as.data.frame(SimulatedYears)==ProcYears[i]),
			X=1:length(ProcYears))
		Indices_Of_year_InModelFile<-unlist(Filter(Indices_Of_year_InModelFile,f=length))
		ClimParam[entries_counter:(entries_counter+length(Indices_Of_year_InModelFile)-1)]<-lapply(FUN=function(i) 
			ApproxForSeason(ProcFile=file_nc_obj,
			i_OfYear=Indices_Of_year_InModelFile[i],x_min=x_min,x_max=x_max,
			y_min=y_min,y_max=y_max,FirstSeasonDay_date=FirstSeasonDay_date,
			LastSeasonDay_date=FirstSeasonDay_date,
			SeasonPeriods=SeasonPeriods),X=seq(along.with=Indices_Of_year_InModelFile))
		entries_counter<-entries_counter+length(Indices_Of_year_InModelFile)	
		nc_close(file_nc_obj)
	}	
	setwd(WDName)
	if (length(ProcYears)!=(entries_counter-1)) {
		setwd(WDName) # back to the folder with source files
		stop(paste("Processed years ",ProcYears[1],"-",ProcYears[length(ProcYears)],
			"(which means ",length(ProcYears)," years) ","\n",
			"correspond to only ",entries_counter," available years",
			"simulated by the model ",unique(ModelInfo_df$ModelName),sep=""))
	}
		# FindModelInfo<-function(SimParam_DF,i_model)
	return(ClimParam)
}
I_ModelTest<-8
I_file<-10
#
setwd(wd_name)
CMIP5ModelsInfo_df<-ExtractModels(CMIP5_DirName=CMIP5_dir_name,WDName=wd_name)
print("CMIP5ModelsInfo")
print(str(CMIP5ModelsInfo_df))
SingleModel_df<-FindModelInfo(CMIP5ModelsInfo_df,i_model=I_ModelTest)
print(paste("SingleModel_df, i_model=",I_ModelTest,sep=" "))
print(SingleModel_df)
#
files_names<-ListCMIPFiles(CMIP5_DirName=CMIP5_dir_name,WDName=wd_name)
print(files_names[I_ModelTest])
#
							setwd(CMIP5_dir_name)
							file_nc_obj_test<-nc_open(files_names[I_file],verbose=FALSE)
							setwd(wd_name)
							Season_test<-ExtractSeason(ProcFile=file_nc_obj_test,SeasonPeriods=c("январь"))
							print(Season_test)
							# test_field<-GetField_byTime(ProcFile=file_nc_obj_test,x_min=x_small,x_max=x_big+5,
							# 	y_min=y_small,y_max=y_big+5)
							# print("Test Field")
							# print(test_field)
#
# print(SingleModel_df)
# ParamField<-ProcessModelledYears(CMIP5_DirName=CMIP5_dir_name,WDName=wd_name,
# 	SimParam_DF=CMIP5ModelsInfo_df,i_model=I_ModelTest,ProcYears=2023:2033,
# 	SeasonPeriods=c("ƒекабрь"),
# 	FirstSeasonDay_date="-12-01",LastSeasonDay_date="-12-31",
# 	x_max=x_big,x_min=x_small,
# 	y_max=y_big,y_min=y_small)
# print("ParamField")
# print(ParamField)