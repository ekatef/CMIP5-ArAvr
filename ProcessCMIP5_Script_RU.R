#************************************
# the main computational file 		#
#************************************

# TODO Fix status bar error when processing a single model

# !!! ProcessCMIP5_Script will clean the workspace and set neccesary paths

run_from_scratch <- TRUE

if (run_from_scratch) {
	
	rm(list = ls())
	
	library(purrr)
	
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	source("0_ProcessCMIP5_Config.R")
	source("0_ProcessCMIP5_Config_local.R")
	source("1_ListModels.R")
	source("2_SelectSeasons.R")
	source("3_Plot_Calcul_Field.R")
	
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# (1)
	#------------
	# inspect model files in the dir
	# adjustment work is needed by first call of the working dir
	# (delete incomplete time periods, check modelled parameters etc.)
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# extract models info from current .nc dir; may take quite long
	CMIP5ModelsInfo_df <- ExtractModels(CMIP5_Dir_Name = CMIP5_dir_name,
		param_name = current_param)
	str(CMIP5ModelsInfo_df)
	
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# (2)
	#------------
	Avail_in_Year_0 <- SelectAvailbleByTime(ExtractedModels_df = CMIP5ModelsInfo_df, 
		YearsRange = YearsRange_check_0, MonthBeg = 1L, MonthEnd = 12L)
	Avail_in_Year <- SelectAvailbleByTime(ExtractedModels_df = CMIP5ModelsInfo_df, 
		YearsRange = YearsRange_check, MonthBeg = 1L, MonthEnd = 12L)
				print(paste("Available for years from the year ", 
					YearsRange_check[1], sep = ""))
				print(names(Avail_in_Year_0))
				print(paste("Available for years from the year ", 
					YearsRange_check[1], sep = ""))
				print(names(Avail_in_Year))
				# check that the models used for both periods are the same
				print("The model sets are identical for the both periods")
				print(identical(unique(names(Avail_in_Year_0)),
					unique(names(Avail_in_Year))))
	
	envolved_models <- unique(unlist(lapply(Avail_in_Year, `[[`, "ModelName")))
	print(envolved_models)
	
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# (3)
	#------------
	SeasonsByModels_YearsRange_0 <- vector(mode = "list", 
		length(names(Avail_in_Year_0)))
	names(SeasonsByModels_YearsRange_0) <- names(Avail_in_Year_0)
	print(str(SeasonsByModels_YearsRange_0))
	SeasonsByModels_YearsRange <- vector(mode = "list", 
		length(names(Avail_in_Year)))
	names(SeasonsByModels_YearsRange) <- names(Avail_in_Year)
	
	#--------------------------------------
	# define model set to proceed
	if (all(selected_names_to_proc == "ALL")){
		names_to_proc <- names(Avail_in_Year_0)
		names_to_proc_check <- names(Avail_in_Year)
	} else {
		names_to_proc <- names(Avail_in_Year_0)[names(Avail_in_Year_0) %in% selected_names_to_proc]
		names_to_proc_check <- names(Avail_in_Year)[names(Avail_in_Year) %in% selected_names_to_proc]
	}
	if (length(names_to_proc) != length(names_to_proc_check)) {
			stop("Different model sets for the considered periods")
		}
	#--------------------------------------
	
	#------------
	# (3a)
	#------------
	StatBar_0 <- txtProgressBar(min = 1, max = length(names_to_proc), style = 3)
	cat("Test time Calculations ", YearsRange_check_0[1], 
		" to ", YearsRange_check_0[length(YearsRange_check_0)],"  \n\r", sep = "")
	for (i in seq(along.with = names_to_proc)) {
		setTxtProgressBar(StatBar_0, i, label = i)
		SeasonsByModels_YearsRange_0[[i]] <- ExtractSeason_test(
				Avail_List = Avail_in_Year_0, 
				ModelName = names(Avail_in_Year_0)[which(names(Avail_in_Year_0) %in% names_to_proc[i])], 
				x_Range = c(x_RU_small, x_RU_big), 
				y_Range = c(y_RU_small, y_RU_big),
				SeasonsToCalcul = SeasonsSet, 
				YearsToCalcul = YearsRange_check_0,
				param_name = current_param
		)
	}
	close(StatBar_0)
	AnnAv_YearsRange_0 <- vector(mode = "list", length(names_to_proc))
	names(AnnAv_YearsRange_0) <- names_to_proc
	print(str(AnnAv_YearsRange_0))
	
	#------------
	# (3b)
	#------------
	StatBar <- txtProgressBar(min = 1, max = length(names_to_proc), style = 3)
	cat("Test time Calculations ", YearsRange_check[1], 
		" to ", YearsRange_check[length(YearsRange_check)],"  \n\r", sep = "")
	for (i in seq(along.with = names_to_proc)) {
		setTxtProgressBar(StatBar, i, label = i)
		SeasonsByModels_YearsRange[[i]] <- ExtractSeason_test(
				Avail_List = Avail_in_Year, 
				ModelName = names(Avail_in_Year)[which(names(Avail_in_Year) %in% names_to_proc[i])], 
				x_Range = c(x_RU_small, x_RU_big), 
				y_Range = c(y_RU_small, y_RU_big),
				SeasonsToCalcul = SeasonsSet, 
				YearsToCalcul = YearsRange_check,
				param_name = current_param
		)
	}
	close(StatBar)
	print(str(SeasonsByModels_YearsRange, 1))
	AnnAv_YearsRange <- vector(mode = "list", length(names_to_proc))
	names(AnnAv_YearsRange) <- names_to_proc
	print(str(AnnAv_YearsRange))
	
	#------------
	# (4)
	#------------
	#
	# @AnnAv_YearsRange_0[[i]] AND @AnnAv_YearsRange [[i]] are lists of seasonal
	# matrices of the parameter averaged across the given time periods
	# which are @YearsRange_check_0 AND @YearsRange_check respectively
	#
	#------------
	# (4a)
	#------------
	cat("Calculation of Annual Averages for the years ", YearsRange_check_0[1], 
		" to ", YearsRange_check_0[length(YearsRange_check_0)],"  \n\r", sep = "")
	for (i in seq(along.with = names_to_proc)) {
		setTxtProgressBar(StatBar_0, i, label = i)	
		model_name_to_calcul <- names(Avail_in_Year_0)[i] 
		# calculation
		AnnAv_YearsRange_0[[i]] <- ProcessSeasonByYears(
					Avail_List = Avail_in_Year_0, 
					ModelName = names(Avail_in_Year_0)[which(names(Avail_in_Year) %in% names_to_proc[i])],
					x_Range = c(x_RU_small, x_RU_big), 
					y_Range = c(y_RU_small, y_RU_big),
					SeasonsToCalcul = SeasonsSet, 
					YearsToCalcul = YearsRange_check_0, 
					n_cells = 200,
					param_name = current_param
				)
		#----------------------------------------------------------
		# output
		#---------------------------------------
		# Output_Tab(CMIP5_df = CMIP5ModelsInfo_df,
		# Calcul_df = AnnAv_YearsRange_0[[i]], 
		# identif_text = model_name_to_calcul, 
		# RD_name = rd_name, Years_Range_vct1 = YearsRange_check_0)
		#---------------------------------------
		# Output_TabAndPlot(CMIP5_df = CMIP5ModelsInfo_df,
		# 	Calcul_df = AnnAv_YearsRange_0[[i]], 
		# 	identif_text = model_name_to_calcul, 
		# 	RD_name = rd_name, Years_Range_vct1 = YearsRange_check_0)
		#----------------------------------------------------------
	}
	close(StatBar_0)
	print(str(AnnAv_YearsRange_0))
	
	#------------
	# (4b)
	#------------
	cat("Calculation of Annual Averages for the years ", YearsRange_check[1], 
		" to ", YearsRange_check[length(YearsRange_check)],"  \n\r", sep = "")
	for (i in seq(along.with = names_to_proc)) {
		setTxtProgressBar(StatBar, i, label = i)	
		model_name_to_calcul <- names(Avail_in_Year)[i] 
		# calculation
		AnnAv_YearsRange [[i]] <- ProcessSeasonByYears(
					Avail_List = Avail_in_Year, 
					ModelName = names(Avail_in_Year)[which(names(Avail_in_Year) %in% names_to_proc[i])],
					x_Range = c(x_RU_small, x_RU_big), 
					y_Range = c(y_RU_small, y_RU_big),
					SeasonsToCalcul = SeasonsSet, 
					YearsToCalcul = YearsRange_check, 
					n_cells = 200,
					param_name = current_param
				)
		#----------------------------------------------------------
		# output
		#---------------------------------------
		# Output_Tab(CMIP5_df = CMIP5ModelsInfo_df,
		# Calcul_df = AnnAv_YearsRange_0[[i]], 
		# identif_text = model_name_to_calcul, 
		# RD_name = rd_name, Years_Range_vct1 = YearsRange_check_0)	
		#---------------------------------------
		# Output_TabAndPlot(CMIP5_df = CMIP5ModelsInfo_df,
		# 	Calcul_df = AnnAv_YearsRange[[i]], 
		# 	identif_text = model_name_to_calcul, 
		# 	RD_name = rd_name, Years_Range_vct1 = YearsRange_check)
		#----------------------------------------------------------
	}
	close(StatBar)
	print(str(AnnAv_YearsRange))
	
	#------------
	# (5)
	#------------
	res_list_0 <- lapply(AnnAv_YearsRange_0, `[[`, "z")
	res_list <- lapply(AnnAv_YearsRange, `[[`, "z")
	# https://stackoverflow.com/a/18558250/8465924
	ens_0 <- Reduce("+", res_list_0) / length(res_list_0)
	ens <- Reduce("+", res_list) / length(res_list)
	delta <- ens - ens_0
	perc_delta <- delta/ens_0


	# confidence testing	
	delta_by_ens_list <- map2(res_list, res_list_0, `-`)
	# prepare matrix parameters
	n_mx <- c(nx = 200, ny = 200, nz = length(res_list_0))

	# the idea from https://stackoverflow.com/a/39351048
	sd_delta <- apply(
			array(unlist(delta_by_ens_list), 
				c(n_mx["nx"], n_mx["ny"], n_mx["nz"])), 
				c(1, 2), 
			sd
		)
	delta_t_test <- apply(
			array(unlist(delta_by_ens_list), 
				c(n_mx["nx"], n_mx["ny"], n_mx["nz"])), 
				c(1, 2), 
			p_t_test
		)
	max_estim <- apply(
			array(unlist(delta_by_ens_list), 
				c(n_mx["nx"], n_mx["ny"], n_mx["nz"])), 
				c(1, 2), 
			max_mean
		)	
	min_estim <- apply(
			array(unlist(delta_by_ens_list), 
				c(n_mx["nx"], n_mx["ny"], n_mx["nz"])), 
				c(1, 2), 
			min_mean
		)	
	
	# put everyting in lists
	ens_0_list <- list(
		x = AnnAv_YearsRange[[1]]$x,
		y = AnnAv_YearsRange[[1]]$y,
		z = ens_0
	)
	ens_list <- list(
		x = AnnAv_YearsRange[[1]]$x,
		y = AnnAv_YearsRange[[1]]$y,
		z = ens
	)
	delta_list <- list(
		x = AnnAv_YearsRange[[1]]$x,
		y = AnnAv_YearsRange[[1]]$y,
		z = delta
	)

	perc_delta <- delta/ens_0

	# if (current_param != "mrro") {
	# 	perc_delta <- delta/ens_0
	# } else {
	# 	perc_delta <- delta/ens_0
	# 	perc_delta[which(perc_delta > 0.3)] <- 0.3
	# 	perc_delta[which(perc_delta < -0.3)] <- -0.3

	# }

	perc_delta_list <- list(
		x = AnnAv_YearsRange[[1]]$x,
		y = AnnAv_YearsRange[[1]]$y,
		z = perc_delta
	)
	signif_list <- list(
		x = AnnAv_YearsRange[[1]]$x,
		y = AnnAv_YearsRange[[1]]$y,
		z = abs((2 * delta)/sd_delta)
	)
	p_t_list <- list(
		x = AnnAv_YearsRange[[1]]$x,
		y = AnnAv_YearsRange[[1]]$y,
		z = delta_t_test
	)
	max_perc_estim_list <- list(
		x = AnnAv_YearsRange[[1]]$x,
		y = AnnAv_YearsRange[[1]]$y,
		z = max_estim/ens_0
	)
	min_perc_estim_list <- list(
		x = AnnAv_YearsRange[[1]]$x,
		y = AnnAv_YearsRange[[1]]$y,
		z = min_estim/ens_0
	)	
}
#----------------------------------------------------------
# output
#---------------------------------------

	threshold_val <- 0.3

	if (current_param != "mrro") {
		perc_delta <- delta/ens_0
	} else {
		perc_delta <- delta/ens_0
		perc_delta[which(perc_delta > threshold_val)] <- threshold_val
		perc_delta[which(perc_delta < -threshold_val)] <- -threshold_val

		perc_delta_list <- list(
			x = AnnAv_YearsRange[[1]]$x,
			y = AnnAv_YearsRange[[1]]$y,
			z = perc_delta)

	}

# # works for rcp45, optimized ens
# col_adj_vct <- -c(9:10)
adj_set <- list(mean = -c(1:3), max = -c(1:3), min = -c(1, 10:11))
col_adj_vct <- adj_set[["min"]]


# TODO vectorize function calls

# the file name should be like that
# ens_0_long_format_rcp26_ens_max_sfcWind_rcp26_months_1-12_2007-2026_2045-2054
OutputLattPlot(res_dir = rd_name, spt_var = ens_0_list,
	yrs_range1 = YearsRange_check_0, 
	yrs_range2 = YearsRange_check,
	months_range = SeasonsSet,
	is_temper = it_is_temterature,
	is_long = FALSE,
	default_output = TRUE,
	identif_text = paste0(exp_id, "_", mod_set_id, "_", current_param))
OutputLattPlot(res_dir = rd_name, spt_var = ens_0_list,
	yrs_range1 = YearsRange_check_0, 
	yrs_range2 = YearsRange_check,
	months_range = SeasonsSet,
	is_temper = it_is_temterature,
	is_long = FALSE,
	default_output = TRUE,
	identif_text = paste0(exp_id, "_", mod_set_id, "_", current_param))
OutputLattPlot(res_dir = rd_name, spt_var = ens_list,
	yrs_range1 = YearsRange_check_0, 
	yrs_range2 = YearsRange_check,
	months_range = SeasonsSet,
	is_temper = it_is_temterature,
	is_long = FALSE,
	default_output = TRUE,
	identif_text = paste0(exp_id, "_", mod_set_id, "_", current_param))
OutputLattPlot(res_dir = rd_name, spt_var = delta_list,
	yrs_range1 = YearsRange_check_0, 
	yrs_range2 = YearsRange_check,
	months_range = SeasonsSet,
	is_temper = it_is_temterature,
	is_long = FALSE,
	default_output = TRUE, n_pal_adj = col_adj_vct,
	identif_text = paste0(exp_id, "_", mod_set_id, "_", current_param))
OutputLattPlot(res_dir = rd_name, spt_var = perc_delta_list,
	yrs_range1 = YearsRange_check_0, 
	yrs_range2 = YearsRange_check,
	months_range = SeasonsSet,
	is_temper = it_is_temterature,
	is_long = FALSE,
	default_output = TRUE, n_pal_adj = col_adj_vct,
	identif_text = paste0(exp_id, "_", mod_set_id, "_", current_param))

# add contours
OutputLattPlot(res_dir = rd_name, spt_var = ens_0_list,
	yrs_range1 = YearsRange_check_0, 
	yrs_range2 = YearsRange_check,
	months_range = SeasonsSet,
	is_temper = it_is_temterature,
	is_long = FALSE,
	default_output = TRUE,
	identif_text = paste0(exp_id, "_", mod_set_id, "_", current_param,
		"_cont"), 
	print_contour = TRUE)
OutputLattPlot(res_dir = rd_name, spt_var = ens_list,
	yrs_range1 = YearsRange_check_0, 
	yrs_range2 = YearsRange_check,
	months_range = SeasonsSet,
	is_temper = it_is_temterature,
	is_long = FALSE,
	default_output = TRUE,
	identif_text = paste0(exp_id, "_", mod_set_id, "_", current_param,
		"_cont"), 
	print_contour = TRUE)
OutputLattPlot(res_dir = rd_name, spt_var = delta_list,
	yrs_range1 = YearsRange_check_0, 
	yrs_range2 = YearsRange_check,
	months_range = SeasonsSet,
	is_temper = it_is_temterature,
	is_long = FALSE,
	default_output = TRUE,
	identif_text = paste0(exp_id, "_", mod_set_id, "_", current_param,
		"_cont"),
	 print_contour = TRUE, n_pal_adj = col_adj_vct)
OutputLattPlot(res_dir = rd_name, spt_var = perc_delta_list,
	yrs_range1 = YearsRange_check_0, 
	yrs_range2 = YearsRange_check,
	months_range = SeasonsSet,
	is_temper = it_is_temterature,
	is_long = FALSE,
	default_output = TRUE,
	identif_text = paste0(exp_id, "_", mod_set_id, "_", current_param,
		"_cont"),
	 print_contour = TRUE, n_pal_adj = col_adj_vct)
OutputLattPlot(res_dir = rd_name, spt_var = signif_list,
	yrs_range1 = YearsRange_check_0, 
	yrs_range2 = YearsRange_check,
	months_range = SeasonsSet,
	is_temper = FALSE,
	is_long = FALSE,
	default_output = TRUE,
	identif_text = paste0(exp_id, "_", mod_set_id, "_", current_param,
		"_signif_delta"),
	 print_contour = FALSE)
OutputLattPlot(res_dir = rd_name, spt_var = p_t_list,
	yrs_range1 = YearsRange_check_0, 
	yrs_range2 = YearsRange_check,
	months_range = SeasonsSet,
	is_temper = it_is_temterature,
	is_long = FALSE,
	default_output = TRUE,
	identif_text = paste0(exp_id, "_", mod_set_id, "_", current_param))
OutputLattPlot(res_dir = rd_name, spt_var = max_perc_estim_list,
	yrs_range1 = YearsRange_check_0, 
	yrs_range2 = YearsRange_check,
	months_range = SeasonsSet,
	is_temper = it_is_temterature,
	is_long = FALSE,
	default_output = TRUE, n_pal_adj = col_adj_vct,
	identif_text = paste0(exp_id, "_", mod_set_id, "_", current_param))
OutputLattPlot(res_dir = rd_name, spt_var = min_perc_estim_list,
	yrs_range1 = YearsRange_check_0, 
	yrs_range2 = YearsRange_check,
	months_range = SeasonsSet,
	is_temper = it_is_temterature,
	is_long = FALSE,
	default_output = TRUE, n_pal_adj = col_adj_vct,
	identif_text = paste0(exp_id, "_", mod_set_id, "_", current_param))
#---------------------------------------

#---------------------------------------
# Output_Tab(CMIP5_df = CMIP5ModelsInfo_df,
# 	Calcul_df = ens_0_list, 
# 	identif_text = "abs_more_full_ens2_estim_75to05", 
# 	RD_name = rd_name, 
# 	Years_Range_vct1 = YearsRange_check_0)

# Output_Tab(CMIP5_df = CMIP5ModelsInfo_df,
# 	Calcul_df = ens_list, 
# 	identif_text = "abs_more_full_ens2_estim_75to05", 
# 	RD_name = rd_name, 
# 	Years_Range_vct2 = YearsRange_check)

# Output_Tab(CMIP5_df = CMIP5ModelsInfo_df,
# 	Calcul_df = delta_list, 
# 	identif_text = "delta_more_full_ens2_estim_75to05", 
# 	RD_name = rd_name, 
# 	Years_Range_vct1 = YearsRange_check_0,
# 	Years_Range_vct2 = YearsRange_check)
# Output_Tab(CMIP5_df = CMIP5ModelsInfo_df,
# 	Calcul_df = perc_delta_list, 
# 	identif_text = "perc_delta_more_full_ens2_estim_75to05", 
# 	RD_name = rd_name, 
# 	Years_Range_vct1 = YearsRange_check_0,
# 	Years_Range_vct2 = YearsRange_check)
#---------------------------------------

#---------------------------------------
Output_LongTab(
	CMIP5_df = CMIP5ModelsInfo_df,
	Calcul_df = ens_0_list, 
	identif_text = paste0("ens_0_long_format_", exp_id, "_", mod_set_id, "_", 
		current_param), 
	RD_name = paste0(rd_name, "/"), 
	Years_Range_vct1 = YearsRange_check_0,
	Years_Range_vct2 = YearsRange_check
)

Output_LongTab(
	CMIP5_df = CMIP5ModelsInfo_df,
	Calcul_df = ens_list, 
	identif_text = paste0("ens_long_format_", exp_id, "_", mod_set_id, "_", 
		current_param), 
	RD_name = paste0(rd_name, "/"), 
	Years_Range_vct1 = YearsRange_check_0,
	Years_Range_vct2 = YearsRange_check
)

Output_LongTab(
	CMIP5_df = CMIP5ModelsInfo_df,
	Calcul_df = delta_list, 
	identif_text = paste0("delta_long_format_", exp_id, "_", mod_set_id, "_", 
		current_param), 
	RD_name = paste0(rd_name, "/"), 
	Years_Range_vct1 = YearsRange_check_0,
	Years_Range_vct2 = YearsRange_check
)

Output_LongTab(
	CMIP5_df = CMIP5ModelsInfo_df,
	Calcul_df = perc_delta_list, 
	identif_text = paste0("perc_delta_long_format_", exp_id, "_", mod_set_id, "_", 
		current_param), 
	RD_name = paste0(rd_name, "/"), 
	Years_Range_vct1 = YearsRange_check_0,
	Years_Range_vct2 = YearsRange_check
)

Output_LongTab(
	CMIP5_df = CMIP5ModelsInfo_df,
	Calcul_df = min_perc_estim_list, 
	identif_text = paste0("min_perc_estim_list_", exp_id, "_", mod_set_id, "_", 
		current_param), 
	RD_name = paste0(rd_name, "/"), 
	Years_Range_vct1 = YearsRange_check_0,
	Years_Range_vct2 = YearsRange_check
)

Output_LongTab(
	CMIP5_df = CMIP5ModelsInfo_df,
	Calcul_df = max_perc_estim_list, 
	identif_text = paste0("max_perc_estim_list_", exp_id, "_", mod_set_id, "_", 
		current_param), 
	RD_name = paste0(rd_name, "/"), 
	Years_Range_vct1 = YearsRange_check_0,
	Years_Range_vct2 = YearsRange_check
)

Output_LongTab(
	CMIP5_df = CMIP5ModelsInfo_df,
	Calcul_df = signif_list, 
	identif_text = paste0("signif_list_", exp_id, "_", mod_set_id, "_", 
		current_param), 
	RD_name = paste0(rd_name, "/"), 
	Years_Range_vct1 = YearsRange_check_0,
	Years_Range_vct2 = YearsRange_check
)

Output_LongTab(
	CMIP5_df = CMIP5ModelsInfo_df,
	Calcul_df = p_t_list, 
	identif_text = paste0("p_t_list_", exp_id, "_", mod_set_id, "_", 
		current_param), 
	RD_name = paste0(rd_name, "/"), 
	Years_Range_vct1 = YearsRange_check_0,
	Years_Range_vct2 = YearsRange_check
)

#---------------------------------------

# # rainbow plots ------------------------
# Output_TabAndPlot(CMIP5_df = CMIP5ModelsInfo_df,
# 	Calcul_df = ens_0_list, 
# 	identif_text = paste0("abs_rainbow_", exp_id, "_", mod_set_id, "_",
# 		current_param), 
# 	RD_name = paste0(rd_name, "/"), 
# 	Years_Range_vct1 = YearsRange_check_0)

# Output_TabAndPlot(CMIP5_df = CMIP5ModelsInfo_df,
# 	Calcul_df = ens_list, 
# 	identif_text = paste0("abs_rainbow_", exp_id, "_", mod_set_id, "_",
# 		current_param), 
# 	RD_name = paste0(rd_name, "/"), 
# 	Years_Range_vct2 = YearsRange_check)

# Output_TabAndPlot(CMIP5_df = CMIP5ModelsInfo_df,
# 	Calcul_df = delta_list, 
# 	identif_text = paste0("delta_rainbow_", exp_id, "_", mod_set_id, "_",
# 		current_param), 
# 	RD_name = paste0(rd_name, "/"), 
# 	Years_Range_vct1 = YearsRange_check_0,
# 	Years_Range_vct2 = YearsRange_check)
# Output_TabAndPlot(CMIP5_df = CMIP5ModelsInfo_df,
# 	Calcul_df = perc_delta_list, 
# 	identif_text = paste0("perc_delta_rainbow_", exp_id, "_", mod_set_id, "_",
# 		current_param), 
# 	RD_name = paste0(rd_name, "/"), 
# 	Years_Range_vct1 = YearsRange_check_0,
# 	Years_Range_vct2 = YearsRange_check)
#----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~