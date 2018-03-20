library("ncdf4")
library("fields")
library("maps")
library("abind")
# library("PCICt")
# returns a vector of the .nc files' names from the CMIP5 folder
ListCMIPFiles <- function(CMIP5_Dir_Name = CMIP5_dir_name) {
	filesInDir_names <- list.files(CMIP5_Dir_Name)
	last_chars_in_file_names <- substr(filesInDir_names, nchar(filesInDir_names) - 3 + 1, 
		nchar(filesInDir_names))
	if (length(filesInDir_names) == 0) {stop("The folder is empty. Check it please")}
	if (!any(last_chars_in_file_names == ".nc")) {
		stop("No NetCFD files in the folder. Check please")
	} #else {n_files<-length(which(last_chars_in_file_names==".nc"))}
	filesInDir_names <- filesInDir_names[which(last_chars_in_file_names == ".nc")]
	# the resulted files' names don't include the path to make check-up easy
	return(filesInDir_names)
}
# TODO ? should "proleptic_gregorian" be corrected?
# @N_time_origin is a string of "%Y-%m-%d" format which is usual for CMIP nc-files
TimeCorrections_in_days <- function(Date_to_Correct_days, 
	Original_nc_Calendar, Origin_Time) {
	Origin_Time <- strptime(Origin_Time, format = "%Y-%m-%d")	
	current_tau <- as.Date(Date_to_Correct_days, origin = Origin_Time)
	origin_year <- as.integer(format(Origin_Time, "%Y"))
	current_year <- as.integer(format(current_tau, "%Y"))
	correction_days <- 0
	if (Original_nc_Calendar %in% "noleap") {		
		correction_days <- floor((current_year - origin_year)/4)
	}
	if (Original_nc_Calendar %in% "365_day") {		
		correction_days <- floor((current_year - origin_year)*0.25)
	}
	if (Original_nc_Calendar %in% "360_day") {		
		correction_days <- floor((current_year - origin_year)*5.25)
	}
	return(correction_days)
}
# look to the CMIP5 dir for the simulation files
# return a dataframe with parameters of the models
ExtractModels <- function(CMIP5_Dir_Name = CMIP5_dir_name) {
	# reading from the CMIP5 simulation file
	SimParam_df <- data.frame(FileName = character(), ModelName = character(), 
		ScenarioName = character(), ParamName = character(), Calendar = character(),
		t0_internal = numeric(), tN_internal = numeric(), Origin_Time = character(),
		t0 = numeric(), tN = numeric(), ArrayID = numeric(), Year_0 = numeric(),
		Year_N = numeric(), Month_0 = numeric(), Month_N = numeric(),
		stringsAsFactors = FALSE)
	filesInDir_names <- ListCMIPFiles(CMIP5_Dir_Name = CMIP5_Dir_Name)
	# set an absolute path instead to change a working dir
	filesInDir_names_with_path <- paste(CMIP5_Dir_Name, "//", filesInDir_names, sep ="")
	# setwd(CMIP5_Dir_Name)
	StatBar <- txtProgressBar(min = 1, max = length(filesInDir_names), style = 3)
	cat("Calculation of SimParam_df \n\r", sep = "")
	for (i in 1:length(filesInDir_names)) {	
	# for (i in 1:2) { # testing statement
		setTxtProgressBar(StatBar, i, label = i)
		file_nc_obj <- nc_open(filesInDir_names_with_path[i], verbose=FALSE)
		# the target variable is the last one (temperature/precip/runoff etc)
		param_name <- file_nc_obj$var[[length(file_nc_obj$var)]]$name
		file_attributes <- ncatt_get(file_nc_obj, varid=0)
		model_name <- file_attributes$model_id
		scenario_name <- file_attributes$experiment_id
		time_attributes <- ncatt_get( file_nc_obj, varid = "time", attname=NA, verbose=FALSE)
		calendar_nc <- time_attributes$calendar
		# non-standard calendars require respective corrections
		t0_correction_days <- 0
		tN_correction_days <- 0
		time <- file_nc_obj$dim$time$vals
		begin_time <- time[1]
		end_time <- time[length(time)]
		origin_time_string_tail <- strsplit(file_nc_obj$dim$time$units,split="since")[[1]][2]
		# it may be a whitespace after "since" or some whitespaces or tab;
		# that's more reliable to delete all possible delimeters
		origin_time_string_tail <- trimws(x = origin_time_string_tail, which = "left")
		N_time_origin <- strsplit(origin_time_string_tail, split = " ")[[1]][1]
		# check for the data format
			first_4_chars <- sapply(FUN = function(i) substr(N_time_origin, start = i, stop = i), X = 1:4)
			fifth_char <- substr(N_time_origin, start = 5, stop = 5)
			# first four symbols should be digits
			test_4_numbers <- all(grepl("?[0-9]", first_4_chars))
			# fifth symbol should be "-" to match the data format used bellow for conversion
			delim_matches <- grepl("[-]", x = fifth_char)
			date_test_ok <- (test_4_numbers & delim_matches)
		if (!(date_test_ok)) {
			nc_close(file_nc_obj)
			stop(paste("Format of the original date in nc-file should be %Y-%m-%d",
			" Something wrong with the date formatting found in ", 
			filesInDir_names[i], sep = ""))}
						# tau_0 <- begin_time
						# tau_N <- end_time
		N_time_origin_as_Date <- strptime(N_time_origin, format = "%Y-%m-%d")
		tau_0 <- as.Date(begin_time, origin = N_time_origin_as_Date)
		tau_N <- as.Date(end_time, origin = N_time_origin_as_Date)
		t0_correction_days <- TimeCorrections_in_days(Date_to_Correct_days = begin_time, 
			Original_nc_Calendar = calendar_nc, Origin_Time = N_time_origin)			
		tN_correction_days <- TimeCorrections_in_days(Date_to_Correct_days = end_time,
			Original_nc_Calendar = calendar_nc, Origin_Time = N_time_origin)					
		tau_0 <- tau_0 + t0_correction_days
		tau_N <- tau_N + tN_correction_days
		year_0 <- as.integer(format(tau_0, "%Y"))
		year_N <- as.integer(format(tau_N, "%Y"))
		month_0_asDate <- months(tau_0)
		month_N_asDate <- months(tau_N)
		SimParam_df[i,] <- list(FileName = filesInDir_names[i], ModelName = model_name, 
					ScenarioName = scenario_name, ParamName = param_name, Calendar = calendar_nc,
				 	t0_internal = begin_time, tN_internal = end_time, Origin_Time = as.character(N_time_origin), # Origin_Time = N_time_origin,
				 	t0 = tau_0, tN = tau_N, ArrayID = i, Year_0 = year_0, Year_N = year_N,
				 	Month_0 = month_0_asDate, Month_N = month_N_asDate)
		nc_close(file_nc_obj)
	}
	if (length(unique(SimParam_df$ScenarioName))!=1) {
		message("The models imply following parameters")
		print(SimParam_df[ ,c("ModelName","ScenarioName","Year_0","Year_N"), drop = FALSE])
		stop("The considered scenarios are different. Please check the processed files")
	}	
	if (length(unique(SimParam_df$ParamName))!=1) {
		message("The models contain following parameters")
		print(SimParam_df[ ,c("ModelName","ParamName","Year_0","Year_N"), drop = FALSE])
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
# @ModelInfo_df is a adta frame of metadata for each model extracted by ExtractModels()
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
# @DF a dataframe extracted by by ExtractModels() and checked CheckModel()
# @MonthBegin & @MonthEnd are the indices on the months
# @MonthBegin should be >= @MonthEnd
CheckEnds <- function (DF, YearsRange, MonthBeg, MonthEnd) {
	acceptable_range <- 1L:12L
	if (!(MonthBeg %in% acceptable_range)) {stop(paste("The month index is outside the acceptable range: ",
			"MonthBeg = ", MonthBeg, sep = ""))}
	if (!(MonthEnd %in% acceptable_range)) {stop(paste("The month index is outside the acceptable range: ",
			"MonthEnd = ", MonthEnd, sep = ""))}	
	if (MonthBeg > MonthEnd) {stop(paste("Sorry, but MonthBeg should be less than MonthEnd. Set values are",
			"MonthBeg = ", MonthBeg, "MonthEnd = ", MonthEnd, sep = ""))}

	# check if there are enough modelled years to proceed requested years' range
	# the .nc files are still sorted by modelled dates; so, we just need to check end dates		
	Range_Begin <- paste("01", MonthBeg, min(YearsRange), sep="-")
	Range_End <- paste("01", MonthEnd, max(YearsRange), sep="-")	
	# a trick with +/-30 is meant to supply neccecary data for further seasonal interpolation	
	Range_Begin_date <- as.Date(strptime(Range_Begin, format = "%d-%m-%Y")) - 60
	Range_End_date <- as.Date(strptime(Range_End, format = "%d-%m-%Y")) + 60 # Range_End = 01 Dec -> all inside next Jan should be taken
	
	# corrections of the dates to account for different calendars
	begin_moment <- as.Date(DF$t0_internal[1], origin = DF$Origin_Time[1])
	end_moment <- as.Date(DF$tN_internal[length(DF$tN_internal)], 
		origin = DF$Origin_Time[length(DF$Origin_Time)])
	begin_correction <- TimeCorrections_in_days(Date_to_Correct_days = DF$t0_internal[1], 
		Original_nc_Calendar = DF$Calendar[1], Origin_Time = DF$Origin_Time[1])
	end_correction <- TimeCorrections_in_days(Date_to_Correct_days = DF$t0_internal[length(DF$Origin_Time)], 
		Original_nc_Calendar = DF$Calendar[length(DF$Origin_Time)], 
		Origin_Time = DF$Origin_Time[length(DF$Origin_Time)])
	begin_moment <- begin_moment + begin_correction
	end_moment <- end_moment + end_correction
	Begins_Ok_bool <- (Range_Begin_date > begin_moment)
	Ends_Ok_bool <- (Range_End_date < end_moment)
	if (!(Begins_Ok_bool)) {
		stop(paste0("\n","Not enough modelled years (begin):", "\n",
			"Model ", DF$ModelName, "\n", "the earliest modelled year is ", 
			DF$Year_0[1], "corresponding to the date ", begin_moment, " with a correction \n",
			"which is ", as.Date(DF$t0_internal[1], origin = DF$Origin_Time[1]), " without correction",
			"\n","the earliest requested year is ", min(YearsRange)))
	}
	if (!(Ends_Ok_bool)) {
		stop(paste0("Not enough modelled years (end):", "\n",
			"Model ", DF$ModelName, "\n", "the latest modelled year is ", 
			DF$Year_N[length(DF$Year_N)], " corresponding to the date ", end_moment, " with a correction \n",
			"which is ", as.Date(DF$tN_internal[length(DF$tN_internal)], 
				origin = DF$Origin_Time[length(DF$tN_internal)]), " without correction",
			"\n","the final requested year is ", max(YearsRange), "\n"))
	}	

	t0_vct <- as.Date(DF$t0_internal, origin = DF$Origin_Time)
	tN_vct <- as.Date(DF$tN_internal, origin = DF$Origin_Time)
	t0_correction <- sapply(FUN = function(i) TimeCorrections_in_days(Date_to_Correct_days = t0_vct[i], 
		Original_nc_Calendar = DF$Calendar[i], Origin_Time = DF$Origin_Time[i]), 
		X = seq(along.with = DF$t0_internal))
	tN_correction <- sapply(FUN = function(i) TimeCorrections_in_days(Date_to_Correct_days = tN_vct[i], 
		Original_nc_Calendar = DF$Calendar[i], Origin_Time = DF$Origin_Time[i]),
		X = seq(along.with = DF$tN_internal))
	t0_vct <- t0_vct + t0_correction
	tN_vct <- tN_vct + tN_correction
	# for many .nc files for a period, at least one end of the modelled period
	# should be between the Range dates
	Cond1 <- ((t0_vct > Range_Begin_date) & (t0_vct < Range_End_date))
	Cond2 <- ((tN_vct > Range_Begin_date) & (tN_vct < Range_End_date))
		# long-timed .nc file contains both ends of the set interval
	Cond3 <- ((t0_vct < Range_Begin_date) & (tN_vct > Range_Begin_date))
	Cond4 <- ((t0_vct < Range_End_date) & (tN_vct > Range_End_date))
	return(DF[which((Cond1|Cond2)|(Cond3 & Cond4)), , drop = FALSE])

}
# returns indices of the cells closest to the set boundary values of the coords
# @ProcFile is a processed nc file
FindCells <- function(ProcFile, DimCode = "lon", coord_min, coord_max) {
	if ((DimCode!="lon") & (DimCode!="lat")) {
		N_dim <- NA
		coord_1d_array <- NA
		i_min <- NA
		i_max <- NA
		warning("Wrong dimension name in FindCells()", immediate. = TRUE)
		# message("Wrong dimension name in FindCells()")
	} else {
		N_dim <- ProcFile$dim[[DimCode]]$len
		coord_1d_array <- ProcFile$dim[[DimCode]]$vals
		i_min <- which.min(abs(coord_1d_array-coord_min))
		i_max <- which.min(abs(coord_1d_array-coord_max))
	}
	return(data.frame(min = i_min, max = i_max,count = (i_max-i_min)))
}
# returns a time variable and an origin time from the processed nc file
# @ProcFile is a processed nc file
ExtractRawTime <- function(ProcFile) {
	time_nc<-ProcFile$dim$time$vals				# time in days
	time_units<-ProcFile$dim$time$units
	time_attributes <- ncatt_get(ProcFile, varid = "time", 
		attname=NA, verbose=FALSE)
	calendar_nc <- time_attributes$calendar
	origin_time_string_tail<-strsplit(time_units,split="since")[[1]][2]
	# sometimes since is followed by some whitespaces or a tabulation
	N_time_begin <- trimws(x=origin_time_string_tail, which = "left")
	N_time_begin <- strsplit(N_time_begin,split=" ")[[1]][1]
	# tau<-as.Date(time_nc,origin=N_time_begin)	# time as a date
	tau <- list(time_nc = time_nc, N_time_begin = N_time_begin,
		calendar_nc = calendar_nc)
	return(tau)
}
ExtractGrid <- function(ProcFile, x_bnd, y_bnd) {
	i_lon_test_df <- FindCells(ProcFile = ProcFile, DimCode = "lon",
		coord_min = min(x_bnd),coord_max = max(x_bnd))
	i_lon_range <- seq(from=i_lon_test_df$min,to = i_lon_test_df$max,length = i_lon_test_df$count)
	x_NCGrid <- ProcFile$dim[["lon"]]$vals[i_lon_range]
	i_lat_test_df <- FindCells(ProcFile=ProcFile,DimCode = "lat",
		coord_min = min(y_bnd),coord_max = max(y_bnd))
	i_lat_range <- seq(from = i_lat_test_df$min,to = i_lat_test_df$max,length = i_lat_test_df$count)
	y_NCGrid <- ProcFile$dim[["lat"]]$vals[i_lat_range]
	return(list(GridLon = x_NCGrid,GridLat = y_NCGrid))	
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
	fileForProc_attributes <- ncatt_get(file_nc_obj_procSsn,varid=0)
	modelForProc_name <- fileForProc_attributes$model_id
	scenarioForProc_name <- fileForProc_attributes$experiment_id
	timeInFile_df <- ExtractRawTime(ProcFile = file_nc_obj_procSsn)
		# check names of the coordinates in the nc-file 
	# (E.g. CMMCC-CESM has "i" and "j" coordinates instead of "lat" and "lon" for historical run)
	if (!(any(names(file_nc_obj_procSsn$dim) == "lat"))) {
		nc_close(file_nc_obj_procSsn)
		msg <- paste("There is no dim with the name 'lat' in the nc-file '",
			file_nc_obj_procSsn["filename"][[1]],"'",sep = "")
		stop(msg)
	}
	if (!(any(names(file_nc_obj_procSsn$dim) == "lon"))) {
		nc_close(file_nc_obj_procSsn)
		msg <- paste("There is no dim with the name 'lon' in the nc-file '",
			file_nc_obj_procSsn["filename"][[1]],"'",sep = "")
		stop(msg)
	}
	proc_grid <- ExtractGrid(ProcFile = file_nc_obj_procSsn,x_bnd = X_To_Proc_vct,
		y_bnd = Y_To_Proc_vct)	
	i_long_to_process <- FindCells(ProcFile = file_nc_obj_procSsn, DimCode = "lon",
		coord_min = min(X_To_Proc_vct), coord_max = max(X_To_Proc_vct))
	i_lat_to_process <- FindCells(ProcFile = file_nc_obj_procSsn,DimCode = "lat",
		coord_min = min(Y_To_Proc_vct), coord_max = max(Y_To_Proc_vct))
	T_3D_array_to_process<-ncvar_get(file_nc_obj_procSsn, varid=paramForProc_name, 
		start=c(i_long_to_process$min,i_lat_to_process$min,1),
		count=c(i_long_to_process$count,i_lat_to_process$count,-1))	
	nc_close(file_nc_obj_procSsn)
	# originally T_3D implies columns as latitude (y), that is T_3D[, y_i] corresponds to lat = y_i
	# that's extremelly inconvenient for further analysis -> transpose it with aperm(a, perm, ...)
	return(list(internal_time = timeInFile_df, grid = proc_grid, T_3D = aperm(a = T_3D_array_to_process, 
		perm = c(2, 1, 3)))) # the rows (dim = 2) & columns (dim = 2) were transposed; dim 3 wasn't changed
	# return(list(internal_time = timeInFile_df, grid = proc_grid, T_3D = T_3D_array_to_process))
}
# returns 
# @ExtractedModels_df is the list of metadates as it's extracted by ExtractModels
# @ YearsRange is a two-element vector
SelectAvailbleByTime <- function(ExtractedModels_df, YearsRange,
	MonthBeg, MonthEnd) {
	acceptable_range <- 1L:12L
	if (!(MonthBeg %in% acceptable_range)) {stop(paste("The month index is outside the acceptable range: ",
			"MonthBeg = ", MonthBeg, sep = ""))}
	if (!(MonthEnd %in% acceptable_range)) {stop(paste("The month index is outside the acceptable range: ",
			"MonthEnd = ", MonthEnd, sep = ""))}	
	if (MonthBeg > MonthEnd) {stop(paste("Sorry, but MonthBeg should be less than MonthEnd. Set values are",
			"MonthBeg = ", MonthBeg, "MonthEnd = ", MonthEnd, sep = ""))}	
	Model_Names <- unique(ExtractedModels_df$ModelName)
	ModelsInfo_byModels <- split(x = ExtractedModels_df, f = as.factor(ExtractedModels_df$ModelName))
	Models_To_Process <- lapply(function(Z) CheckModel(ModelsInfo_byModels, Z), X = Model_Names)
	Models_To_Calcul <- vector(mode = "list", length = length(Models_To_Process))
	names(Models_To_Calcul) <- Model_Names
	for (i in (seq(along.with = Models_To_Process))) {
		Models_To_Calcul[[i]] <- CheckEnds(DF = Models_To_Process[[i, drop = FALSE]], 
			YearsRange = YearsRange, MonthBeg = MonthBeg, MonthEnd = MonthEnd)
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
	# tau <- list(time_nc = time_nc, N_time_begin = N_time_begin,
	# 	calendar_nc = calendar_nc)
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
	test_time_origin <- lapply(test_time, `[[`, "N_time_begin")		# the result will be a single value
	test_time_calendar <- lapply(test_time, `[[`, "calendar_nc")	# the result will be a single value
	# origins may be different for different time spans, so times in days and origin times are processed together

	test_to_date <- lapply(function (i) as.Date(test_time_days[[i]], 
		origin = test_time_origin[[i]]), X = seq(along.with = test_time_days))
	time_as_date_vct <- do.call(`c`, test_to_date)	
	# TO DO: calendar correction!!!	
	# for calendar corrections
	time_corrections <- lapply(function(i) TimeCorrections_in_days(Date_to_Correct_days = test_time_days[[i]],
	Original_nc_Calendar = test_time_calendar[[i]], Origin_Time = test_time_origin[[i]]), 
	X = seq(along.with = test_time_days))
	time_corrections_vct <- do.call(`c`, time_corrections)
	time_corrected_vct <- time_as_date_vct + time_corrections_vct
	test_T_3D_combined <- do.call(abind, list(test_T_3D, along = 3))
	# result will be an input for
	# ApproxForSeason2(Dates_vct, SeasonPeriods = c(6L:8L),
	# YearVal, Grid_Lon, Grid_Lat, Param_3D)
	return(list(Modelled_Dates = time_as_date_vct, 
			time_Corrections = time_corrections_vct,
			RealCalendarTime = time_corrected_vct,
			Grid_Lon = unique_grid_lon,
			Grid_Lat = unique_grid_lat, T_3D = test_T_3D_combined))
}
# !!! x&y are transposed
# interp.surface() should be rewriten
RegridModel <- function(param_matrix, x_bnd, y_bnd,
	x_grid, y_grid, n_Regrid_Cells) {
	obj_test <- list(x = x_grid, y = y_grid, z = param_matrix)
	x_ToInt <- seq(from = min(x_bnd), to = max(x_bnd), length.out = n_Regrid_Cells)
	y_ToInt <- seq(from = min(y_bnd), to = max(y_bnd), length.out = n_Regrid_Cells)
	loc_test <- list(x = x_ToInt, y = y_ToInt)
	#test_int<-interp.surface(obj_test,loc_test)
	# TODO generalisation of interp.surface is needed process different x & y steps
	# return(obj_test)
	# return(list(x_to_int = x_ToInt, y_to_int = y_ToInt))
	test_int <- interp.surface.grid(obj_test, loc_test)
	return(test_int)
}