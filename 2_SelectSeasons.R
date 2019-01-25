# library("ncdf4")

ProcessSeasonByYears <- function(Avail_List, ModelName, 
	x_Range, y_Range, param_name,
	SeasonsToCalcul, YearsToCalcul, 
	n_cells){
 	StitchedData <- StichModelledFiles(CMIP5_Dir_Name = CMIP5_dir_name, 
 		Models_To_Calcul_List = Avail_List, ModelName = ModelName, 
 		X_To_Proc_vct = x_Range, Y_To_Proc_vct = y_Range, 
 		param_name = param_name)
 	x_grid_model <- StitchedData$Grid_Lon
 	y_grid_model <- StitchedData$Grid_Lat
 	# list of results (2D matrices) for a single year
 	res_list <- lapply(function(Z) ApproxForSeason2(Dates_vct = StitchedData$RealCalendarTime, 
 		SeasonPeriods = SeasonsToCalcul, YearVal = Z, Param_3D = StitchedData$T_3D), 
 		X = YearsToCalcul)
 	res_T_3D <- (Reduce(`+`, res_list)/length(res_list)) # returns gridded seasonal mean
 	# x&y corresponds to col&strings resp => x is y, y is x in the interp()
 	res_regrid <- RegridModel(param_matrix = res_T_3D, 
 		x_bnd = y_Range, y_bnd = x_Range,
		x_grid = y_grid_model, y_grid = x_grid_model, 
		n_Regrid_Cells = n_cells)
 	return(res_regrid)
 }

TsSeasonByYears <- function(Avail_List, ModelName, 
	x_Range, y_Range, param_name,
	SeasonsToCalcul, YearsToCalcul, 
	n_cells){
 	StitchedData <- StichModelledFiles(CMIP5_Dir_Name = CMIP5_dir_name, 
 		Models_To_Calcul_List = Avail_List, ModelName = ModelName, 
 		X_To_Proc_vct = x_Range, Y_To_Proc_vct = y_Range, param_name = param_name)
 	x_grid_model <- StitchedData$Grid_Lon
 	y_grid_model <- StitchedData$Grid_Lat
 	# list of results (2D matrices) for a single year
 	res_list <- lapply(function(Z) ApproxForSeason2(Dates_vct = StitchedData$RealCalendarTime, 
 		SeasonPeriods = SeasonsToCalcul, YearVal = Z, Param_3D = StitchedData$T_3D), 
 		X = YearsToCalcul)
 	# return(res_list)

 	res <- unlist(lapply(X = res_list, FUN = mean))
 	return(res)

 	# res_T_3D <- (Reduce(`+`, res_list)/length(res_list)) # returns gridded seasonal mean
 	# # x&y corresponds to col&strings resp => x is y, y is x in the interp()
 	# res_regrid <- RegridModel(param_matrix = res_T_3D, 
 	# 	x_bnd = y_Range, y_bnd = x_Range,
		# x_grid = y_grid_model, y_grid = x_grid_model, 
		# n_Regrid_Cells = n_cells)
 	# return(res_regrid)
 }
 #

 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 #					testing function
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# returns dates 
ExtractSeason_test <- function(Avail_List, ModelName, 
	x_Range, y_Range, param_name,
	SeasonsToCalcul, YearsToCalcul) {
		StitchedData <- StichModelledFiles(CMIP5_Dir_Name = CMIP5_dir_name, 
	 		Models_To_Calcul_List = Avail_List, ModelName = ModelName, 
	 		X_To_Proc_vct = x_Range, Y_To_Proc_vct = y_Range, param_name = param_name)
	ExtractSeason_end(Dates_vct = StitchedData$RealCalendarTime, 
		SeasonPeriods = SeasonsToCalcul)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 # @Calcul_df is a list as returns RegridModel(); that is [[i]] component of, e.g., AnnAv_YearsRange_0
Output_TabAndPlot <- function (CMIP5_df, Calcul_df, identif_text, RD_name,
	Years_Range_vct1, Years_Range_vct2, ParamLim_vct, TCol_Flag){
	Years_Range_vct1_string <- ifelse(missing(Years_Range_vct1), "", 
		paste("_", Years_Range_vct1[1], "-" , Years_Range_vct1[length(Years_Range_vct1)], sep = ""))
	Years_Range_vct2_string <- ifelse(missing(Years_Range_vct2), "", 
		paste("_", Years_Range_vct2[1], "-" , Years_Range_vct2[length(Years_Range_vct2)], sep = ""))	
	output_name <- paste(identif_text, "_" , unique(CMIP5_df$ParamName), "_",
			unique(CMIP5_df$ScenarioName), "_",
			"months_", SeasonsSet[1], "-", SeasonsSet[length(SeasonsSet)],
			Years_Range_vct1_string, Years_Range_vct2_string, sep= "")
	pdf_name <- paste(RD_name, output_name, ".pdf", sep = "")
	txt_name <- paste(RD_name, output_name, ".txt", sep = "")
	write.table(file = txt_name, Calcul_df, row.names = FALSE)
	if (missing(ParamLim_vct)) ParamLim_vct <- c(min(Calcul_df$z, na.rm = TRUE),
			max(Calcul_df$z, na.rm = TRUE))	
	if (missing(TCol_Flag)) {
		if (CMIP5_df$ParamName[i] == "tas") {
				TCol_Flag <- TRUE
		} else {TCol_Flag <- FALSE}
	}
	pdf(pdf_name, width=360/30, height = (180)/30)
	PlotFieldGlob(MatrixToPlot = Calcul_df$z, 
		xToPlot = Calcul_df$y, 
		yToPlot = Calcul_df$x,
		ZLimParam = ParamLim_vct, PlotInfo = "", 
		PlotTit_Text = output_name,TCol = TCol_Flag,
		value_ofThickCount = NULL, NSign = 3, NCLevels = 7)
	dev.off()
}















#~~~~~~~~~~~~~~~~~~ functions to work with nc data ~~~~~~~~~~~~~~~~~~~~~~~~
# @Dates_vct vector of the Date class, @SeasonPeriods is a month's index in a year
# returns an dataframe with dates and indices of entries corresponding to a certain season
ExtractSeason_end <- function(Dates_vct, SeasonPeriods = c(9L:11L)){
	acceptable_range <- 1L:12L
	if (!(all(SeasonPeriods %in% acceptable_range))) {stop(paste("The month index is outside the acceptable range: ",
			"SeasonPeriods = ", SeasonPeriods, sep = ""))}
	modelled_months <- as.integer(format(Dates_vct, "%m"))
	modelled_years <- as.integer(format(Dates_vct, "%Y"))
	set_of_years <- unique(modelled_years)
	modelled_month_years <- format(Dates_vct, "%Y-%m")
	modelled_moments_df <- data.frame(modelled_date = Dates_vct,
		month = modelled_months, year = modelled_years)
	# str(modelled_moments_df)
	# initialize a new data frame to store in your summed values
	seasonal_dates <- vector(mode = "list", length = length(set_of_years))
	seasonal_dates_extd <- vector(mode = "list", length = length(set_of_years))
	seasonal_ind <- vector(mode = "list", length = length(set_of_years))
	seasonal_ind_extd <- vector(mode = "list", length = length(set_of_years))
	seasonal_res <- list(SeasnlYears = set_of_years, SeasnlDates = seasonal_dates, 
		SeasnlDates_Extd = seasonal_dates_extd, SeasnlInd = seasonal_ind, 
		SeasnlInd_Extd = seasonal_ind_extd)
	for(i in 1:length(set_of_years)){
		seasonal_ind <- which((modelled_moments_df$year %in% set_of_years[i]) & 
					(modelled_moments_df$month %in% SeasonPeriods))
		# found seasonal entrie(s)
		if (length(seasonal_ind) > 0) {
			moments_in_season <- modelled_moments_df[seasonal_ind, ]
			seasonal_ind_extd <- seasonal_ind
			if (seasonal_ind[1] > 1) {
				seasonal_ind_extd <- c((seasonal_ind[1] - 1), seasonal_ind_extd)
			}
			if (seasonal_ind[length(seasonal_ind)] < length(modelled_moments_df$month)) { 
				seasonal_ind_extd <- c(seasonal_ind_extd, (seasonal_ind[length(seasonal_ind)] + 1))
			}
			moments_in_season <- modelled_moments_df[modelled_moments_df$year == set_of_years[i] & 
				modelled_moments_df$month %in% SeasonPeriods,]
			seasonal_res$SeasnlDates[[i]] <- moments_in_season
			seasonal_res$SeasnlInd[[i]] <- seasonal_ind	
			seasonal_res$SeasnlDates_Extd[[i]] <- modelled_moments_df[seasonal_ind_extd, ]	
			seasonal_res$SeasnlInd_Extd[[i]] <- seasonal_ind_extd			
		# no seasonal entries for a certain year	
		} else {
			seasonal_res$SeasnlDates[[i]] <- NA
			seasonal_res$SeasnlInd[[i]] <- NA	
			seasonal_res$SeasnlDates_Extd[[i]] <- NA
			seasonal_res$SeasnlInd_Extd[[i]] <- NA
		}
	}
	return(seasonal_res)
}
# returns a 2D field of a certain parameter averaged for a set period and for a given year
# @YearVal is a **single** value of a year to be processed!
# @Param_3D is a 3D array [along_y, along_x, along_tau]
# @Dates_vct vector of the Date class, @SeasonPeriods is a month's index in a year
ApproxForSeason2 <- function(Dates_vct, SeasonPeriods = c(6L:8L), YearVal, Param_3D) {
	acceptable_range <- 1L:12L
	if (!(all(SeasonPeriods %in% acceptable_range))) {stop(paste("The month index is outside the acceptable range: ",
			"SeasonPeriods = ", SeasonPeriods, sep = ""))}	
	SeasonalEntries <- ExtractSeason_end(Dates_vct = Dates_vct, 
		SeasonPeriods = SeasonPeriods)
	SeasonSeqBeg_string <- paste("01-", min(SeasonPeriods), "-", YearVal, sep = "")
	SeasonSeqBeg_date <- as.Date(SeasonSeqBeg_string, format = "%d-%m-%Y")

	# generate the last day of season by step 1 day back from the begin of the next season
	if (!(max(SeasonPeriods) %in% 12)) {
		SeasonSeqEnd_string <- paste("01-", (max(SeasonPeriods) + 1), "-", YearVal, sep = "")
	} else {
		SeasonSeqEnd_string <- paste("31-", max(SeasonPeriods), "-", YearVal, sep = "")
	}
	SeasonSeqEnd_date <- as.Date(SeasonSeqEnd_string, format = "%d-%m-%Y") - 1
	int_to_period <- seq.Date(from = SeasonSeqBeg_date, to = SeasonSeqEnd_date, 
		by = 1)
	# SeasonAveraging <- function(Array_to_Proc, i_inp, j_inp, k_inp, t_In, t_Out) {
	# 	ApprVal <- approx(x = t_In, y = Array_to_Proc[i_inp, j_inp, k_inp], xout = t_Out)
	# }	
	# $SeasnlYears contains unique years only => the which result will be integer (with 0 length is there are no entries)
	SeasnlYears <- SeasonalEntries[["SeasnlYears"]]
	i_OfYearsRange <- which(SeasnlYears %in% YearVal)
	if (length(i_OfYearsRange) != 0) {	
		int_from_period <- SeasonalEntries[["SeasnlDates_Extd"]][[i_OfYearsRange]][["modelled_date"]]
		k_to_interp <- SeasonalEntries[["SeasnlInd_Extd"]][[i_OfYearsRange]]
		lengthY <- length(Param_3D[ , 1, 1])
		lengthX <- length(Param_3D[1, , 1])
		resField <- matrix(NA_real_, ncol = lengthX, nrow = lengthY)
		res_test <- vector(mode = "list", lengthX)
		for (j in (1:lengthX)) {
		# for (j in (1:lengthX)) {
			# in some cases certain (x, y) aren't included into the computational domain (e.g. for runoff calcul)
			Y_to_calcul <- which(!is.na(Param_3D[ , j, 1]))	
			# it's possible that there is NA's only along the whole column => check is needed
			if (!length(Y_to_calcul) == 0) {
				resField[Y_to_calcul, j] <- sapply(FUN = function(i) mean(approx(x = int_from_period, 
					y = Param_3D[i, j, k_to_interp], xout = int_to_period)$y), X = Y_to_calcul)
			}
		}
		return(resField)	
	} else {return(NA)}
}
