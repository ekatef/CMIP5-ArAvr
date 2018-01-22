library("fields")
library("maps")
# returns a vector of the .nc files' names from the CMIP5 folder
ListCMIPFiles<-function(CMIP5_Dir_Name = CMIP5_dir_name) {
	# setwd(CMIP5_Dir_Name)
	# filesInDir_names<-dir()
	# setwd(WDName)
	filesInDir_names <- list.files(CMIP5_Dir_Name)
	last_chars_in_file_names <- substr(filesInDir_names, nchar(filesInDir_names) - 3 + 1, 
		nchar(filesInDir_names))
	if (length(filesInDir_names) == 0) {stop("The folder is empty. Check it please")}
	if (!any(last_chars_in_file_names == ".nc")) {
		stop("No NetCFD files in the folder. Check please")
	} #else {n_files<-length(which(last_chars_in_file_names==".nc"))}
	filesInDir_names <- filesInDir_names[which(last_chars_in_file_names==".nc")]
	# the files' names don't include the path -- otherwise it would be too long
	return(filesInDir_names)
}
# look to the CMIP5 dir for the simulation files
# return a dataframe with parameters of the models
ExtractModels <- function(CMIP5_Dir_Name = CMIP5_dir_name) {
	# reading from the CMIP5 simulation file
	SimParam_df <- data.frame(FileName = character(),
		ModelName = character(), ScenarioName = character(), ParamName = character(),
		t0_internal = numeric(), tN_internal = numeric(), Origin_Time = numeric(),
		t0 = numeric(), tN = numeric(), ArrayID = numeric(), year_0 = numeric(),
		year_N = numeric(), month_0 = numeric(), month_N = numeric(),
		stringsAsFactors = FALSE)
	filesInDir_names <- ListCMIPFiles(CMIP5_Dir_Name = CMIP5_Dir_Name)
	# set an absolute path instead to change a working dir
	filesInDir_names_with_path <- paste(CMIP5_Dir_Name, "//", filesInDir_names, sep ="")
	# setwd(CMIP5_Dir_Name)
	StatBar <- txtProgressBar(min = 1, max = length(filesInDir_names), style = 3)
	cat("Calculation of SimParam_df \n\r", sep = "")
	for (i in 1:length(filesInDir_names)) {
		setTxtProgressBar(StatBar, i, label = i)
		file_nc_obj <- nc_open(filesInDir_names_with_path[i], verbose=FALSE)
		# the target variable is the last one (temperature/precip/runoff etc)
		param_name <- file_nc_obj$var[[length(file_nc_obj$var)]]$name
		file_attributes <- ncatt_get(file_nc_obj, varid=0)
		model_name <- file_attributes$model_id
		scenario_name <- file_attributes$experiment_id
		time <- file_nc_obj$dim$time$vals
		begin_time <- time[1]
		end_time <- time[length(time)]
		origin_time_string_tail <- strsplit(file_nc_obj$dim$time$units,split="since")[[1]][2]
		# it may be a whitespace after "since" or some whitespaces or tab;
		# that's more reliable to delete all possible delimeters
		origin_time_string_tail <- trimws(x=origin_time_string_tail, which = "left")
		N_time_origin <- strsplit(origin_time_string_tail, split=" ")[[1]][1]
		tau_0 <- begin_time
		tau_N <- end_time
		tau_0 <- as.Date(begin_time, origin=N_time_origin)
		tau_N <- as.Date(end_time, origin=N_time_origin)
		time_0_asDate <- strptime(tau_0, format="%Y-%m-%d")			# time as a beautiful date
		year_0_asDate <- time_0_asDate$year+1900						# correction of default year representation
		time_N_asDate <- strptime(tau_N,format="%Y-%m-%d")			# time as a beautiful date
		year_N_asDate <- time_N_asDate$year+1900						# correction of default year representation
		month_0_asDate <- months(tau_0)
		month_N_asDate <- months(tau_N)
		# month_0_asDate <- file_nc_obj$dim$time$units
		# month_N_asDate <- tau_N
		# TO DO t0 and tN don't seem to be useful
		SimParam_df[i,] <- list(FileName = filesInDir_names[i], ModelName = model_name, 
			ScenarioName = scenario_name, ParamName = param_name,
		 	t0_internal = begin_time, tN_internal = end_time, Origin_Time = N_time_origin,
		 	t0 = tau_0, tN = tau_N, ArrayID = i, year_0 = year_0_asDate, year_N = year_N_asDate,
		 	month_0 = month_0_asDate, month_N = month_N_asDate)
		nc_close(file_nc_obj)
	}
	# setwd(WDName)
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
	UniqueRows_length <- length(unique(SimParam_df[,-9])[,1])
	AllRows_length <- length(SimParam_df[,1])
	if (UniqueRows_length != AllRows_length) {
		message(paste("The number of duplicated data files is",
			(AllRows_length-UniqueRows_length),sep = " "))
		stop("Duplicated Simulation File. Check processed files please")
	}
	return(SimParam_df)	
}	
# check for model output splitted between different files
CheckModel <- function(ModelInfo_df, ModelName) {
	SingleModel_df <- ModelInfo_df[[ModelName]]
	FirstMoment <- SingleModel_df[["t0"]]
	LastMoment <- SingleModel_df[["tN"]]
	SingleModel_df <- SingleModel_df[order(SingleModel_df[["t0"]]), ]
	Intervals_between_TimeSpans <- 0 # distance between the end of one modelling interval and the begin of a new one
	if (length(LastMoment) > 1) {
		Intervals_between_TimeSpans <- c(0, (FirstMoment[-1] - LastMoment[-length(LastMoment)]))
	}
	if (any(Intervals_between_TimeSpans > 45)) {
		stop(paste("The interval betweem modelled time spans",
			"is more than 45 days. The problem Files are from", "\n", 
			SingleModel_df[which(Intervals_between_TimeSpans > 45)[1], "FileName"], "\n", " to ", "\n",
			SingleModel_df[which(Intervals_between_TimeSpans > 45)[length(which(Intervals_between_TimeSpans > 45))]+1, 
			"FileName"], "\n", "test",
			SingleModel_df[which(Intervals_between_TimeSpans > 45), "FileName"]))
	}
	return(SingleModel_df)            
}
# check that enough modelled data are available to cover a requested time period
# returns metadates only for those .nc files which include the requsted time range
# @DF a dataframe extracted by CheckModel()
# @Range_Begin & @Range_End are the strings in the format like "%d-%m"
# TODO: change seasons to months indices for consistency with SelectSeasons functions
CheckEnds <- function (DF, YearsRange, DateBeg, DateEnd) {
	# a trick with +/-30 is meant to supply neccecary data for further seasonal interpolation
	Range_Begin <- paste(DateBeg, min(YearsRange), sep="-")
	Range_Begin_date <- as.Date(strptime(Range_Begin, format="%d-%m-%Y")) - 30
	Range_End <- paste(DateEnd, max(YearsRange), sep="-")
	Range_End_date <- as.Date(strptime(Range_End, format="%d-%m-%Y")) + 30
	# check if there are enough modelled years to proceed requested years' range
	# the .nc files are still sorted by modelled dates; so, we just need to check end dates
	# TODO: solve 2034-FGOALS issue
	Begins_Ok_bool <- (Range_Begin_date >
		as.Date(DF$t0_internal[1], origin = DF$Origin_Time[1]))
	Ends_Ok_bool <- (Range_End_date <
		as.Date(DF$tN_internal[length(DF$tN_internal)], 
		origin = DF$Origin_Time[length(DF$Origin_Time)]))
	# for many .nc files for a period, at least one end of the modelled period
	# should be between the Range dates
	Cond1 <- ((as.Date(DF$t0_internal, origin = DF$Origin_Time)) > 
		(Range_Begin_date))&
		(as.Date(DF$t0_internal, origin = DF$Origin_Time)) < 
		(Range_End_date)
	Cond2 <- ((as.Date(DF$tN_internal, origin = DF$Origin_Time)) > 
		(Range_Begin_date))&
		(as.Date(DF$tN_internal, origin = DF$Origin_Time)) < 
		(Range_End_date)
		# long-timed .nc file contains both ends of the set interval
	Cond3 <- ((as.Date(DF$t0_internal, origin = DF$Origin_Time)) < 
		(Range_Begin_date))&
		(as.Date(DF$tN_internal, origin = DF$Origin_Time)) > 
		(Range_Begin_date)
	Cond4 <- ((as.Date(DF$t0_internal, origin = DF$Origin_Time)) < 
		(Range_End_date))&
		(as.Date(DF$tN_internal, origin = DF$Origin_Time)) > 
		(Range_End_date)
	if (!(Begins_Ok_bool)) {
		stop(paste("\n","Not enough modelled years:", "\n",
			"Model ", DF$ModelName, "\n", "the earliest modelled year is ", DF$year_0[1],
			"\n","the earliest requested year is ", min(YearsRange1), sep = ""))
	}
	if (!(Ends_Ok_bool)) {
		stop(paste("Not enough modelled years:", "\n",
			"Model ", DF$ModelName, "\n", "the latest modelled year is ", 
			DF$year_N[length(DF$year_N)],
			"\n","the final requested year is ", max(YearsRange1), sep = ""))
	}
	return(DF[which(Cond1|Cond2|(Cond3 & Cond4)), , drop = FALSE])
}
# @CMIP5_dir_name is the name of a dir with .nc files
# @wd_name is the name of a dir with the code to execute
# @File_To_Proc_Name is the name of .nc file
# @x_To_Proc_vct, y_To_Proc_vct are coordinates of the considered area
ReadModelFile <- function(CMIP5_Dir_Name, File_To_Proc_Name,
	X_To_Proc_vct, Y_To_Proc_vct) {
	#TO DO Check-ups!!!
	# setwd(CMIP5_dir_name)
	File_To_Proc_Name_full <- paste(CMIP5_Dir_Name, File_To_Proc_Name, sep ="")
 	file_nc_obj_procSsn <- nc_open(File_To_Proc_Name_full, verbose=FALSE)
	paramForProc_name <- file_nc_obj_procSsn$var[[length(file_nc_obj_procSsn$var)]]$name
	fileForProc_attributes<-ncatt_get(file_nc_obj_procSsn,varid=0)
	modelForProc_name <- fileForProc_attributes$model_id
	scenarioForProc_name<-fileForProc_attributes$experiment_id
	timeInFile_df <- ExtractRawTime(ProcFile = file_nc_obj_procSsn)
	proc_grid <- ExtractGrid(ProcFile = file_nc_obj_procSsn,x_bnd = X_To_Proc_vct,
		y_bnd = Y_To_Proc_vct)
	i_long_to_process <- FindCells(ProcFile = file_nc_obj_procSsn, DimCode = "lon",
		coord_min = min(X_To_Proc_vct), coord_max = max(X_To_Proc_vct))
	i_lat_to_process<-FindCells(ProcFile = file_nc_obj_procSsn,DimCode = "lat",
		coord_min = min(Y_To_Proc_vct), coord_max = max(Y_To_Proc_vct))
	T_3D_array_to_process<-ncvar_get(file_nc_obj_procSsn, varid=paramForProc_name, 
		start=c(i_long_to_process$min,i_lat_to_process$min,1),
		count=c(i_long_to_process$count,i_lat_to_process$count,-1))	
	nc_close(file_nc_obj_procSsn)
	# originally T_3D implies columns as latitude (y), that is T_3D[, y_i] corresponds to lat = y_i
	return(list(internal_time = timeInFile_df, grid = proc_grid, T_3D = T_3D_array_to_process))
}
# @ExtractedModels_df is the list of metadates as it's extracted by ExtractModels
# TODO: change SeasBeg/End to a months index
SelectAvailbleByTime <- function(ExtractedModels_df, YearsRange,
	DateBeg_dm, DateEnd_dm) {
	Model_Names <- unique(CMIP5ModelsInfo_df$ModelName)
	ModelsInfo_byModels <- split(x = ExtractedModels_df, f = as.factor(ExtractedModels_df$ModelName))
	Models_To_Process <- lapply(function(Z) CheckModel(ModelsInfo_byModels, Z), X = Model_Names)
	Models_To_Calcul <- vector(mode = "list", length = length(Models_To_Process))
	names(Models_To_Calcul) <- Model_Names
	for (i in (seq(along.with = Models_To_Process))) {
		Models_To_Calcul[[i]] <- CheckEnds(DF = Models_To_Process[[i, drop = FALSE]], 
			YearsRange = YearsRange, DateBeg = DateBeg_dm, DateEnd = DateEnd_dm)
		}
	return(Models_To_Calcul)	
	}
# @Models_To_Calcul_List is a list obtained by SelectAvailbleByTime()
StichModelledFiles <- function(CMIP5_Dir_Name, Models_To_Calcul_List, 
	ModelName, X_To_Proc_vct, Y_To_Proc_vct) {
	# along files for a certain model
	test_model <- lapply(function(i) ReadModelFile(CMIP5_Dir_Name = CMIP5_Dir_Name,
		File_To_Proc_Name = Models_To_Calcul_List[[ModelName]]$FileName[i], 
		X_To_Proc_vct = X_To_Proc_vct, Y_To_Proc_vct = Y_To_Proc_vct), 
		X = seq(Models_To_Calcul_List[[ModelName]]$FileName))
	# test_grid <- lapply(function(i) test_model[i]$grid, X = seq(along.with = test_model))
	# extract consequently nested items of a list by their names
	test_grid <- lapply(test_model, `[[`, "grid")
	test_time <- lapply(test_model, `[[`, "internal_time")
	test_T_3D <- lapply(test_model, `[[`, "T_3D")
	test_grid_lon <- lapply(test_grid, `[[`, "GridLon")
	test_grid_lat <- lapply(test_grid, `[[`, "GridLat")
	# check that all grids are the same
	# note: Reduce(identical, test_grid) returns FALSE if length(test_grid) > 2
	# so, special function is needed
	identicalValue <- function(x, y) { 
		if (identical(x,y)) return(x) 
		else {stop("The computational grids are different in different runs")}
	}	
	unique_grid_lon <- Reduce(identicalValue, test_grid_lon)
	unique_grid_lat <- Reduce(identicalValue, test_grid_lat)
	test_time_days <- lapply(test_time, `[[`, "time_nc")
	test_time_origin <- lapply(test_time, `[[`, "N_time_begin")
	# origins may be different for different time spans, so times in days and origin times are processed together
	test_to_date <- lapply(function (i) as.Date(test_time_days[[i]], 
		origin = test_time_origin[[i]]), X = seq(along.with = test_time_days))
	time_as_date_vct <- do.call(`c`, test_to_date)
	# print(str(time_as_date_vct))
	test_T_3D_combined <- do.call(abind, list(test_T_3D, along = 3))
	# result will be used as an input for
	# ApproxForSeason2(Dates_vct, SeasonPeriods = c(6L:8L),
	# YearVal, Grid_Lon, Grid_Lat, Param_3D)
	return(list(Modelled_Dates = time_as_date_vct, Grid_Lon = unique_grid_lon,
			Grid_Lat = unique_grid_lat, T_3D = test_T_3D_combined))
}
ExtractGrid <- function(ProcFile, x_bnd, y_bnd) {
	i_lon_test_df<-FindCells(ProcFile = ProcFile,DimCode="lon",
		coord_min=min(x_bnd),coord_max=max(x_bnd))
	i_lon_range<-seq(from=i_lon_test_df$min,to=i_lon_test_df$max,length=i_lon_test_df$count)
	x_NCGrid<-ProcFile$dim[["lon"]]$vals[i_lon_range]
	i_lat_test_df<-FindCells(ProcFile=ProcFile,DimCode="lat",
		coord_min=min(y_bnd),coord_max=max(y_bnd))
	i_lat_range<-seq(from=i_lat_test_df$min,to=i_lat_test_df$max,length=i_lat_test_df$count)
	y_NCGrid<-ProcFile$dim[["lat"]]$vals[i_lat_range]
	return(list(GridLon=x_NCGrid,GridLat=y_NCGrid))	
}
RegridModel <- function(param_matrix, x_bnd, y_bnd,
	x_grid, y_grid, n_Regrid_Cells) {
	obj_test <- list(x=x_grid, y=y_grid, z=param_matrix)
	x_ToInt <- seq(from = min(x_bnd), to = max(x_bnd), length.out=n_Regrid_Cells)
	y_ToInt <- seq(from = min(y_bnd), to = max(y_bnd), length.out=n_Regrid_Cells)
	loc_test <- list(x = x_ToInt, y = y_ToInt)
	#test_int<-interp.surface(obj_test,loc_test)
	# TODO generalisation of interp.surface is needed process different x & y steps
	test_int <- interp.surface.grid(obj_test, loc_test)
	return(test_int)
}