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
library("fields")
library("maps")
all_months_names<-c("Январь","Февраль","Март","Июнь","Июль","Август","Сентябрь","Октябрь","Ноябрь","Декабрь")
# FirstSeasonDay_date<-"-06-01"
# LastSeasonDay_date<-"-08-31"
# x_small<-27.5
# x_big<-27.8
# y_big<-53.9
# y_small<-53.8
# GeoRegion_name<-"Petersburg"
# years_for_calcul<-2095:2105
# extract names of the files in CMIP5 directory
# returns a vector of the files' names
ListCMIPFiles<-function(CMIP5_DirName=CMIP5_dir_name,WDName=wd_name) {
	setwd(CMIP5_DirName)
	filesInDir_names<-dir()
	setwd(WDName)
	last_chars_in_file_names<-substr(filesInDir_names, nchar(filesInDir_names)-3+1, nchar(filesInDir_names))
	if (length(filesInDir_names)==0) {stop("The folder is empty. Check it please")}
	if (!any(last_chars_in_file_names==".nc")) {
		stop("No NetCFD files in the folder. Check please")
	} #else {n_files<-length(which(last_chars_in_file_names==".nc"))}
	filesInDir_names<-filesInDir_names[which(last_chars_in_file_names==".nc")]
	# the files' names don't include the path -- otherwise it would be too long
	return(filesInDir_names)
}
# look to the CMIP5 dir for the simulation files
# Returns a dataframe with parameters of the models
# TODO: Find duplicated files
ExtractModels<-function(CMIP5_DirName=CMIP5_dir_name,WDName=wd_name) {
	# reading from the CMIP5 simulation file
	SimParam_df<-data.frame(FileName=character(),
		ModelName=character(),ScenarioName=character(),ParamName=character(),
		t0_internal=numeric(),tEnd_internal=numeric(),Origin_Time=numeric(),
		t0=numeric(),tN=numeric(),ArrayID=numeric(),year_0=numeric(),
		year_N=numeric(),month_0=numeric(),month_N=numeric(),
		stringsAsFactors=FALSE)
	filesInDir_names<-ListCMIPFiles(CMIP5_DirName,WDName)
	setwd(CMIP5_DirName)
	for (i in 1:length(filesInDir_names)) {
		file_nc_obj<-nc_open(filesInDir_names[i],verbose=FALSE)
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
		SimParam_df[i,]<-list(FileName=filesInDir_names[i],ModelName=model_name,ScenarioName=scenario_name,
			ParamName=param_name,
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
									# 
									# FindModelInfo<-function(SimParam_DF,i_model) {
									# 	ModelNames_kit<-unique(SimParam_DF$ModelName)
									# 	Model_Nth_df<-SimParam_DF[which(SimParam_DF$ModelName==ModelNames_kit[i_model]),]
									# 	Model_Nth_df<-Model_Nth_df[order(Model_Nth_df$t0),]
									# 	return(Model_Nth_df)
									# }
									# # the result is a matrix of spatial distribution
									# # of the parameter during the given years
									# # SimParam_DF as a parameter isn't very elegant
									# # however, ExtractModels() works very low, so it makes sence to extract a list just onece
									# ProcessModelledYears<-function(CMIP5_DirName=CMIP5_dir_name,WDName=wd_name,
									# 	SimParam_DF,i_model,ProcYears,SeasonPeriods=c("Июнь","Июль","Август"),
									# 	FirstSeasonDay_date="-06-01",LastSeasonDay_date="-08-31",x_bnd,y_bnd) {
									# 	ModelInfo_df<-FindModelInfo(SimParam_DF=SimParam_DF,i_model=i_model)
									# 	# return(ModelInfo_df)
									# 	attach(ModelInfo_df) # just for short
									# 		# TODO: correct on for the case of overlap of the begin and the end years (e.g. end on Oct 2020, begin on Nov 2020)
									# 		IDs_Of_ModelFiles<-sapply(FUN=function(i) ArrayID[(ProcYears[i]>=year_0)&(ProcYears[i]<=year_N)],
									# 			X=seq(along.with=ProcYears))
									# 		# remove dublicates
									# 		IDs_Of_ModelFiles<-unique(IDs_Of_ModelFiles)
									# 		# exclude NULL elements and unlist
									# 		IDs_Of_ModelFiles<-unlist(Filter(IDs_Of_ModelFiles,f=length))
									# 	detach(ModelInfo_df)
									# 	ClimParam<-list()
									# 	length(ClimParam)<-length(ProcYears)
									# 	entries_counter<-1
									# 	filesInDir_names<-ListCMIPFiles(CMIP5_DirName=CMIP5_DirName,WDName=WDName)
									# 	setwd(CMIP5_DirName)
									# 	for (i_ModelRun in (seq(along.with=IDs_Of_ModelFiles))) {
									# 		file_nc_obj<-nc_open(filesInDir_names[IDs_Of_ModelFiles[i_ModelRun]],verbose=FALSE)
									# 		testYr<-ExtractSeason(ProcFile=file_nc_obj,SeasonPeriods=SeasonPeriods)
									# 		SimulatedYears<-sapply(FUN=function(i) unique(testYr$SeasonalYear[[i]]),
									# 			X=1:length(testYr$SeasonalYear))
									# 		# find coordinates
									# 			# x
									# 		i_lon_test_df<-FindCells(ProcFile=file_nc_obj,DimCode="lon",
									# 			coord_min=min(x_bnd),coord_max=max(x_bnd))
									# 		i_lon_range<-seq(from=i_lon_test_df$min,to=i_lon_test_df$max,length=i_lon_test_df$count)
									# 		x_NCGrid<-file_nc_obj$dim[["lon"]]$vals[i_lon_range]
									# 			# y
									# 		i_lat_test_df<-FindCells(ProcFile=file_nc_obj,DimCode="lat",
									# 			coord_min=min(y_bnd),coord_max=max(y_bnd))
									# 		i_lat_range<-seq(from=i_lat_test_df$min,to=i_lat_test_df$max,length=i_lat_test_df$count)
									# 		y_NCGrid<-file_nc_obj$dim[["lat"]]$vals[i_lat_range]		
									# 		# empty elements may appear in Indices_Of_year_InModelFile as not all ProcYears may
									# 		# present in the current model realisation; exclude with the Filter 
									# 		Indices_Of_year_InModelFile<-sapply(FUN=function(i) which(as.data.frame(SimulatedYears)==ProcYears[i]),
									# 			X=1:length(ProcYears))
									# 		Indices_Of_year_InModelFile<-unlist(Filter(Indices_Of_year_InModelFile,f=length))
									# 		ClimParam[entries_counter:(entries_counter+length(Indices_Of_year_InModelFile)-1)]<-lapply(FUN=function(i) 
									# 			ApproxForSeason(ProcFile=file_nc_obj,
									# 			i_OfYear=Indices_Of_year_InModelFile[i],x_bnd=x_bnd,y_bnd=y_bnd,
									# 			FirstSeasonDay_date=FirstSeasonDay_date,
									# 			LastSeasonDay_date=FirstSeasonDay_date,
									# 			SeasonPeriods=SeasonPeriods),X=seq(along.with=Indices_Of_year_InModelFile))
									# 		entries_counter<-entries_counter+length(Indices_Of_year_InModelFile)	
									# 		nc_close(file_nc_obj)
									# 	}	
									# 	setwd(WDName)
									# 	if (length(ProcYears)!=(entries_counter-1)) {
									# 		setwd(WDName) # back to the folder with source files
									# 		stop(paste("Processed years ",ProcYears[1],"-",ProcYears[length(ProcYears)],
									# 			"(which means ",length(ProcYears)," years) ","\n",
									# 			"correspond to only ",entries_counter," available years",
									# 			"simulated by the model ",unique(ModelInfo_df$ModelName),"i_model",sep=""))
									# 	}
									# 		# FindModelInfo<-function(SimParam_DF,i_model)
									# 	return(list(Parameter=ClimParam,XGrid=x_NCGrid,YGrid=y_NCGrid))
									# }
ExtractGrid<-function(ProcFile,x_bnd,y_bnd) {
	i_lon_test_df<-FindCells(ProcFile=file_nc_obj_testSsn,DimCode="lon",
		coord_min=min(x_bnd),coord_max=max(x_bnd))
	i_lon_range<-seq(from=i_lon_test_df$min,to=i_lon_test_df$max,length=i_lon_test_df$count)
	x_NCGrid<-file_nc_obj_testSsn$dim[["lon"]]$vals[i_lon_range]
	i_lat_test_df<-FindCells(ProcFile=file_nc_obj_testSsn,DimCode="lat",
		coord_min=min(y_bnd),coord_max=max(y_bnd))
	i_lat_range<-seq(from=i_lat_test_df$min,to=i_lat_test_df$max,length=i_lat_test_df$count)
	y_NCGrid<-file_nc_obj_testSsn$dim[["lat"]]$vals[i_lat_range]
	return(list(GridLon=x_NCGrid,GridLat=y_NCGrid))	
}
RegridModel<-function(param_matrix,x_bnd,y_bnd,
	x_grid,y_grid,n_Regrid_Cells) {
	obj_test<-list(x=x_grid,y=y_grid,z=param_matrix)
	x_ToInt<-seq(from=min(x_bnd),to=max(x_bnd),length.out=n_Regrid_Cells)
	y_ToInt<-seq(from=min(y_bnd),to=max(y_bnd),length.out=n_Regrid_Cells)
	loc_test<- list(x=x_ToInt,y=y_ToInt)
	#test_int<-interp.surface(obj_test,loc_test)
	# TODO generalisation of interp.surface is needed process different x & y steps
	test_int<-interp.surface.grid(obj_test,loc_test)
	return(test_int)
}
#
x_RU_big<-200
x_RU_small<-20
y_RU_big<-80
y_RU_small<-40	
I_ModelTest<-3
I_file<-10
setwd(wd_name)
CMIP5ModelsInfo_df<-ExtractModels(CMIP5_DirName=CMIP5_dir_name,WDName=wd_name)
# print("CMIP5ModelsInfo")
# print(CMIP5ModelsInfo_df)
filesInDir_names<-ListCMIPFiles(CMIP5_DirName=CMIP5_dir_name,WDName=wd_name)
filesForProc_names<-CMIP5ModelsInfo_df$FileName[CMIP5ModelsInfo_df$year_0<2007 &
 CMIP5ModelsInfo_df$year_N>2055]
filesForProc_numbers<-sapply(FUN=function(i) which(filesInDir_names==filesForProc_names[i]),
	X=seq(along.with=filesForProc_names))
YearsRange1<-c(2007:2016)
YearsRange2<-c(2046:2055)
x_ReGrid<-seq(from=x_RU_small,to=x_RU_big,length.out=200)
y_ReGrid<-seq(from=y_RU_small,to=y_RU_big,length.out=200)
# works for temperature
#LimParam<-c(250,290)
# works for precipitation
LimParam<-c(0.5e-5,6e-5)
# works for runoff
#LimParam<-c(1e-6,15e-6)
MMEnsemble1<-list()
MMEnsemble2<-list()
mm_counter<-0
#for (I_file in filesForProc_numbers[-c(1:7)]) {
for (I_file in filesForProc_numbers) {
#for (I_file in filesForProc_numbers[1:3]) {
	mm_counter<-mm_counter+1	
	setwd(CMIP5_dir_name)
	file_nc_obj_testSsn<-nc_open(filesInDir_names[I_file],verbose=FALSE)
	paramForProc_name<-file_nc_obj_testSsn$var[[length(file_nc_obj_testSsn$var)]]$name
	fileForProc_attributes<-ncatt_get(file_nc_obj_testSsn,varid=0)
	modelForProc_name<-fileForProc_attributes$model_id
	scenarioForProc_name<-fileForProc_attributes$experiment_id
	#
	timeInFile_df<-ExtractTime(file_nc_obj_testSsn)
	yearsInFile<-unique(timeInFile_df$year_byDates)
	yearsToProc1_i<-which(yearsInFile %in% YearsRange1)
	yearsToProc2_i<-which(yearsInFile %in% YearsRange2)
	#
	test_grid<-ExtractGrid(ProcFile=file_nc_obj_testSsn_testSsn,x_bnd=c(x_RU_small,x_RU_big),
		y_bnd=c(y_RU_small,y_RU_big))
	test_YearsRange1<-lapply(FUN=function(i) ApproxForSeason(ProcFile=file_nc_obj_testSsn,
		x_bnd=c(x_RU_small,x_RU_big),y_bnd=c(y_RU_small,y_RU_big),
		SeasonPeriods=all_months_names,
		FirstSeasonDay_date="-01-01",LastSeasonDay_date="-12-31",i_OfYear=i),
		X=yearsToProc1_i)
	test_YearsRange1_Avr<-Reduce("+", test_YearsRange1) / length(test_YearsRange1)
	test_YearsRange1_regridded<-RegridModel(param_matrix=test_YearsRange1_Avr,x_bnd=c(x_RU_small,x_RU_big),
		y_bnd=c(y_RU_small,y_RU_big),x_grid=test_grid$GridLon,y_grid=test_grid$GridLat,
		n_Regrid_Cells=200)
	MMEnsemble1[[mm_counter]]<-test_YearsRange1_regridded$z
	test_YearsRange2<-lapply(FUN=function(i) ApproxForSeason(ProcFile=file_nc_obj_testSsn,
		x_bnd=c(x_RU_small,x_RU_big),y_bnd=c(y_RU_small,y_RU_big),
		SeasonPeriods=all_months_names,
		FirstSeasonDay_date="-01-01",LastSeasonDay_date="-12-31",i_OfYear=i),
		X=yearsToProc2_i)
	test_YearsRange2_Avr<-Reduce("+", test_YearsRange2) / length(test_YearsRange2)
	test_YearsRange2_regridded<-RegridModel(param_matrix=test_YearsRange2_Avr,x_bnd=c(x_RU_small,x_RU_big),
		y_bnd=c(y_RU_small,y_RU_big),x_grid=test_grid$GridLon,y_grid=test_grid$GridLat,
		n_Regrid_Cells=200)
	MMEnsemble2[[mm_counter]]<-test_YearsRange2_regridded$z
	nc_close(file_nc_obj_testSsn)
	setwd(wd_name)
	file_name1_pdf<-paste(modelForProc_name,"_",paramForProc_name,
		"_",YearsRange1[1],"to",YearsRange1[length(YearsRange1)],
		".pdf",sep="")
	output1_name<-paste(rd_name,file_name1_pdf,sep="")
	plotInfo1<-paste(YearsRange1[1],"to",YearsRange1[length(YearsRange1)],
		paramForProc_name,modelForProc_name,scenarioForProc_name,sep=" ")
	file_name2_pdf<-paste(modelForProc_name,"_",paramForProc_name,
		"_",YearsRange2[1],"to",YearsRange2[length(YearsRange2)],
		".pdf",sep="")
	output2_name<-paste(rd_name,file_name2_pdf,sep="")
	plotInfo2<-paste(YearsRange2[1],"to",YearsRange2[length(YearsRange2)],
		paramForProc_name,modelForProc_name,scenarioForProc_name,sep=" ")
	pdf(output1_name)
		PlotField(MatrixToPlot=test_YearsRange1_Avr,
			xToPlot=test_grid$GridLat,yToPlot=test_grid$GridLon,
			ZLimParam=LimParam,PlotTitule=plotInfo1,TCol=FALSE)
		PlotField(MatrixToPlot=test_YearsRange1_regridded$z,
			xToPlot=y_ReGrid,yToPlot=x_ReGrid,
			ZLimParam=LimParam,PlotTitule=plotInfo1,TCol=FALSE)	
	dev.off()
	pdf(output2_name)	
		PlotField(MatrixToPlot=test_YearsRange2_Avr,
			xToPlot=test_grid$GridLat,yToPlot=test_grid$GridLon,
			ZLimParam=LimParam,PlotTitule=plotInfo2,TCol=FALSE)
		PlotField(MatrixToPlot=test_YearsRange2_regridded$z,
			xToPlot=y_ReGrid,yToPlot=x_ReGrid,
			ZLimParam=LimParam,PlotTitule=plotInfo2,TCol=FALSE)	
	dev.off()
}	
MMEnsemble1_Avr<-Reduce("+", MMEnsemble1) / length(MMEnsemble1)
MMEnsemble2_Avr<-Reduce("+", MMEnsemble2) / length(MMEnsemble2)
MMEns_Delta<-(MMEnsemble2_Avr-MMEnsemble1_Avr)/MMEnsemble1_Avr
#
	ens_file_name1_pdf<-paste("MMEns","_",YearsRange1[1],"to",YearsRange1[length(YearsRange1)],
		".pdf",sep="")
	ens_output1_name<-paste(rd_name,ens_file_name1_pdf,sep="")
	plotInfo1<-paste(YearsRange1[1],"to",YearsRange1[length(YearsRange1)],
		paramForProc_name,"MMEns",scenarioForProc_name,sep=" ")
	ens_file_name2_pdf<-paste("MMEns","_",YearsRange2[1],"to",YearsRange2[length(YearsRange2)],
		".pdf",sep="")
	ens_output2_name<-paste(rd_name,ens_file_name2_pdf,sep="")
	plotInfo2<-paste(YearsRange2[1],"to",YearsRange2[length(YearsRange2)],
		paramForProc_name,"MMEns",scenarioForProc_name,sep=" ")
	pdf(ens_output1_name)
		PlotField(MatrixToPlot=MMEnsemble1_Avr,
			xToPlot=y_ReGrid,yToPlot=x_ReGrid,
			ZLimParam=LimParam,PlotTitule=plotInfo1,TCol=FALSE)	
	dev.off()
	pdf(ens_output2_name)	
		PlotField(MatrixToPlot=MMEnsemble2_Avr,
			xToPlot=y_ReGrid,yToPlot=x_ReGrid,
			ZLimParam=LimParam,PlotTitule=plotInfo2,TCol=FALSE)	
	dev.off()
	LimParam_perc<-c(-0.05,0.15)
	delta_file_pdf<-paste("MMEns_delta_",paramForProc_name,
		YearsRange1[1],"to",YearsRange1[length(YearsRange1)],
		"vs",YearsRange2[1],"to",YearsRange2[length(YearsRange2)],".pdf",sep="")
	delta_output_name<-paste(rd_name,delta_file_pdf,sep="")
	plotInfo_delta<-paste(YearsRange1[1],"-",YearsRange1[length(YearsRange1)],
		"vs",YearsRange2[1],"-",YearsRange2[length(YearsRange2)]," ",
		"MMEns_",paramForProc_name," delta(%)",scenarioForProc_name,sep="")
	pdf(delta_output_name)	
		PlotField(MatrixToPlot=MMEns_Delta,
			xToPlot=y_ReGrid,yToPlot=x_ReGrid,
			ZLimParam=LimParam_perc,PlotTitule=plotInfo_delta,TCol=FALSE,
			value_ofThickCount=0,NSign=2,NCLevels=20)	
	dev.off()
#
# print("str(MMEnsemble1)")
# print(str(MMEnsemble1))
# print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
# print("str(MMEnsemble2)")
# print(str(MMEnsemble2))