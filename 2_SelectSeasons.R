library("ncdf4")
#
FindCells <- function(ProcFile, DimCode="lon",coord_min,coord_max) {
	# check names of the coordinates in the nc-file 
	# (E.g. CMMCC-CESM has "i" and "j" coordinates instead of "lat" and "lon" for historical run)
	if (!(any(names(ProcFile$dim)=="lat"))) {
		msg<-paste("There is no dim with the name 'lat' in the nc-file '",
			ProcFile["filename"][[1]],"'",sep="")
		stop(msg)
	}
	if (!(any(names(ProcFile$dim)=="lon"))) {
		msg<-paste("There is no dim with the name 'lon' in the nc-file '",
			ProcFile["filename"][[1]],"'",sep="")
		stop(msg)
	}
	# check dimension code
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
#
ExtractRawTime<-function(ProcFile) {
	time_nc<-ProcFile$dim$time$vals				# time in days
	time_units<-ProcFile$dim$time$units
	origin_time_string_tail<-strsplit(time_units,split="since")[[1]][2]
	# sometimes since is followed by some whitespaces or a tabulation
	N_time_begin<-trimws(x=origin_time_string_tail, which = "left")
	N_time_begin<-strsplit(N_time_begin,split=" ")[[1]][1]
	# tau<-as.Date(time_nc,origin=N_time_begin)					# time as a date
	tau <- list(time_nc = time_nc, N_time_begin = N_time_begin)
	return(tau)
}
ExtractSeason_end <- function(Dates_vct, SeasonPeriods = c(9L:11L)){
	acceptable_range <- 1L:12L
	modelled_months <- as.integer(format(Dates_vct, "%m"))
	modelled_years <- as.integer(format(Dates_vct, "%Y"))
	set_of_years <- unique(modelled_years)
	modelled_month_years <- format(Dates_vct, "%Y-%m")
	modelled_moments_df <- data.frame(modelled_date = Dates_vct,
		month = modelled_months, year = modelled_years)
	str(modelled_moments_df)
	# initialize a new data frame to store in your summed values
	seasonal_dates <- vector(mode = "list", length = length(set_of_years))
	seasonal_dates_extd <- vector(mode = "list", length = length(set_of_years))
	seasonal_ind <- vector(mode = "list", length = length(set_of_years))
	seasonal_ind_extd <- vector(mode = "list", length = length(set_of_years))
	seasonal_res <- list(SeasnlYears = set_of_years, SeasnlDates = seasonal_dates, 
		SeasnlDates_Extd = seasonal_dates_extd, SeasnlInd = seasonal_ind, 
		SeasnlInd_Extd = seasonal_ind_extd)
	# run through a loop starting at your second year and ending at second last
	for(i in 1:length(set_of_years)){
	# for(i in 1:1) {	
		seasonal_ind <- which((modelled_moments_df$year %in% set_of_years[i]) & 
					(modelled_moments_df$month %in% SeasonPeriods))
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
# returns a 2D field of a certain parameter
# @YearVal is a **single** value of a year to be processed!
ApproxForSeason2<-function(Dates_vct, SeasonPeriods = c(6L:8L),
	YearVal, Grid_Lon, Grid_Lat, Param_3D) {
	SeasonalEntries <- ExtractSeason_end(Dates_vct = Dates_vct, 
		SeasonPeriods = SeasonPeriods)
	SeasonSeqBeg_string <- paste("01-", min(SeasonPeriods), "-", YearVal, sep = "")
	SeasonSeqBeg_date <- as.Date(SeasonSeqBeg_string, format = "%d-%m-%Y")
	# generate the last day of season by step 1 day back from the begin of the next season
	SeasonSeqEnd_string <- paste("01-", (max(SeasonPeriods) + 1), "-", YearVal, sep = "")
	SeasonSeqEnd_date <- as.Date(SeasonSeqEnd_string, format = "%d-%m-%Y") - 1
	int_to_period <- seq.Date(from = SeasonSeqBeg_date, to = SeasonSeqEnd_date, 
		by = 1)
	SeasonAveraging <- function(Array_to_Proc, i_inp, j_inp, k_inp, t_In, t_Out) {
		ApprVal <- approx(x = t_In, y = Array_to_Proc[i_inp, j_inp, k_inp], xout = t_Out)
	}	
	# $SeasnlYears contains unique years only => the which result will be integer (with 0 length is there are no entries)
	SeasnlYears <- SeasonalEntries[["SeasnlYears"]]
	i_OfYearsRange <- which(SeasnlYears %in% YearVal)
	if (length(i_OfYearsRange) != 0) {	
		int_from_period <- SeasonalEntries[["SeasnlDates_Extd"]][[i_OfYearsRange]][["modelled_date"]]
		k_to_interp <- SeasonalEntries[["SeasnlInd_Extd"]][[i_OfYearsRange]]
		lengthY <- length(Param_3D[ , 1, 1])
		lengthX <- length(Param_3D[1, , 1])
		resField <- matrix(NA, ncol = lengthX, nrow = lengthY)
		for (j in (1:lengthX)) {
			resField[, j] <- sapply(FUN = function(i) mean(approx(x = int_from_period, 
				y = Param_3D[i, j, k_to_interp], xout = int_to_period)$y), X = 1:lengthY) #lengthX)
		}
		return(resField)
	} else {return(NA)}
}
