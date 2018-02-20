# library("ncdf4")
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
