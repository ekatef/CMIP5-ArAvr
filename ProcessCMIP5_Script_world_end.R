# !!! ProcessCMIP5_Script will clean the workspace and set neccesary paths
rm(list = ls())
Sys.setenv(LANGUAGE='en')

#  wd_name should be repeated by starting with scratch
# # TODO: parametrisation
# wd_name <- "name_the_folder_with_R_code"
# setwd(wd_name) # for the first run due to rm in the beginning

# # actual config file
# source("0_ProcessCMIP5_Config_....R")
source("1_ListModels.R")
source("2_SelectSeasons.R")
source("3_Plot_Calcul_Field.R")


x_RU_big <- 360
x_RU_small <- 0
y_RU_big <- 90
y_RU_small <- -90
YearsRange_check_0 <- 2007:2016
YearsRange_check <- 2045:2054

# process all years: calculates seasonal mean for each model and the set years
# & redridn the result on the "standard" mesh
SeasonsSet <- c(1L:12L)
calcul_param = "tas"

CMIP5ModelsInfo_df <- ExtractModels(CMIP5_Dir_Name = CMIP5_dir_name,
	param_name = calcul_param)
str(CMIP5ModelsInfo_df)

Avail_in_Year_0 <- SelectAvailbleByTime(ExtractedModels_df = CMIP5ModelsInfo_df, 
	YearsRange = YearsRange_check_0, MonthBeg = 1L, MonthEnd = 12L)
Avail_in_Year <- SelectAvailbleByTime(ExtractedModels_df = CMIP5ModelsInfo_df, 
	YearsRange = YearsRange_check, MonthBeg = 1L, MonthEnd = 12L)
			print(paste("Available for years from the year ", YearsRange_check_0[1], sep = ""))
			print(names(Avail_in_Year_0))
			print(paste("Available for years from the year ", YearsRange_check[1], sep = ""))
			print(names(Avail_in_Year))
			# check that the models used for both periods are the same
			print("The model sets are identical for the both periods")
			print(identical(unique(names(Avail_in_Year_0)),unique(names(Avail_in_Year))))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SeasonsByModels_YearsRange_0 <- vector(mode = "list", length(names(Avail_in_Year_0)))
names(SeasonsByModels_YearsRange_0) <- names(Avail_in_Year_0)
print(str(SeasonsByModels_YearsRange_0))
SeasonsByModels_YearsRange <- vector(mode = "list", length(names(Avail_in_Year)))
names(SeasonsByModels_YearsRange) <- names(Avail_in_Year)


StatBar0 <- txtProgressBar(min = 1, max = length(Avail_in_Year_0), style = 3)
cat("Calculation of Annual averages for the years ", YearsRange_check_0[1], 
	" to ", YearsRange_check_0[length(YearsRange_check_0)],"  \n\r", sep = "")

for (i in seq(along.with = names(Avail_in_Year_0)[1:2])) {
	setTxtProgressBar(StatBar0, i, label = i)
	SeasonsByModels_YearsRange_0[[i]] <- ExtractSeason_test(Avail_List = Avail_in_Year_0, 
		ModelName = names(Avail_in_Year_0)[i], 
		x_Range = c(x_RU_small, x_RU_big), y_Range = c(y_RU_small, y_RU_big),
		YearsToCalcul = YearsRange_check_0)
}

AnnAv_YearsRange_0 <- vector(mode = "list", length(names(Avail_in_Year_0)))
names(AnnAv_YearsRange_0) <- names(Avail_in_Year_0)
print(str(AnnAv_YearsRange_0))
AnnAv_YearsRange <- vector(mode = "list", length(names(Avail_in_Year)))
names(AnnAv_YearsRange) <- names(Avail_in_Year)

StatBar0 <- txtProgressBar(min = 1, max = length(Avail_in_Year_0), style = 3)
cat("Calculation of Annual averages for the years ", YearsRange_check_0[1], 
	" to ", YearsRange_check_0[length(YearsRange_check_0)],"  \n\r", sep = "")
for (i in seq(along.with = names(Avail_in_Year_0)[1:2])) {
	setTxtProgressBar(StatBar0, i, label = i)	
	model_name_to_calcul <- names(Avail_in_Year_0)[i] 
	# calculation
	AnnAv_YearsRange_0 [[i]] <- ProcessSeasonByYears(Avail_List = Avail_in_Year_0, 
		ModelName = names(Avail_in_Year_0)[i],
		x_Range = c(x_RU_small, x_RU_big), y_Range = c(y_RU_small, y_RU_big),
		YearsToCalcul = YearsRange_check_0, n_cells = 200)
	# output
	Output_TabAndPlot(CMIP5_df = CMIP5ModelsInfo_df,
		Calcul_df = AnnAv_YearsRange_0[[i]], 
		identif_text = model_name_to_calcul, 
		RD_name = rd_name, Years_Range_vct1 = YearsRange_check_0)
}
print(str(AnnAv_YearsRange_0))

StatBar <- txtProgressBar(min = 1, max = length(Avail_in_Year), style = 3)
cat("Calculation of Annual averages for the years ", YearsRange_check[1], 
	" to ", YearsRange_check[length(YearsRange_check)],"  \n\r", sep = "")
for (i in seq(along.with = names(Avail_in_Year))) {
	setTxtProgressBar(StatBar, i, label = i)	
	model_name_to_calcul <- names(Avail_in_Year)[i] 
	# calculation
	AnnAv_YearsRange [[i]] <- ProcessSeasonByYears(Avail_List = Avail_in_Year, 
	ModelName = names(Avail_in_Year)[i],
	x_Range = c(x_RU_small, x_RU_big), y_Range = c(y_RU_small, y_RU_big),
	YearsToCalcul = YearsRange_check[1], n_cells = 200)
	# output
	Output_TabAndPlot(CMIP5_df = CMIP5ModelsInfo_df,
		Calcul_df = AnnAv_YearsRange[[i]], 
		identif_text = model_name_to_calcul, 
		RD_name = rd_name, Years_Range_vct2 = YearsRange_check)
}
print(str(AnnAv_YearsRange))
# Year_0 is an arbitary choise; it sould be simply Year, too
i_seq_delta <- seq(along.with = names(Avail_in_Year_0))
delta <- lapply(function(i) list(x = AnnAv_YearsRange_0[[i]]$x,
	y = AnnAv_YearsRange_0[[i]]$y,
	z = (AnnAv_YearsRange[[i]]$z - AnnAv_YearsRange_0[[i]]$z)), 
	X = i_seq_delta)
str(delta)
StatBar <- txtProgressBar(min = 1, max = length(delta), style = 3)
cat("Delta output", "  \n\r", sep = "")
for (i in i_seq_delta) {	
	setTxtProgressBar(StatBar, i, label = i)
	model_name_to_calcul <- names(Avail_in_Year)[i]
# delta means that two years' ranges are in the play
	Output_TabAndPlot(CMIP5_df = CMIP5ModelsInfo_df,
	Calcul_df = delta[[i]], 
	identif_text = paste(model_name_to_calcul, "_delta", sep = ""), 
	RD_name = rd_name, Years_Range_vct1 = YearsRange_check_0, 
	Years_Range_vct2 = YearsRange_check)
}
delta_z <- lapply(`[[`, X = delta, "z")
delta_x <- lapply(`[[`, X = delta, "x")
delta_y <- lapply(`[[`, X= delta, "y")
MM_AnAvr_x <- Reduce(`+`, delta_x)/length(delta_x)
MM_AnAvr_y <- Reduce(`+`, delta_y)/length(delta_y)
MM_AnAvr_z <- Reduce(`+`, delta_z)/length(delta_z)
MM_AnAvr_delta <- list(x = MM_AnAvr_x, y = MM_AnAvr_y,
	z = MM_AnAvr_z)
str(MM_AnAvr_delta)
Output_TabAndPlot(CMIP5_df = CMIP5ModelsInfo_df,
Calcul_df = MM_AnAvr_delta, identif_text = "_MM_Ens_delta",
RD_name = rd_name, Years_Range_vct1 = YearsRange_check_0, 
Years_Range_vct2 = YearsRange_check)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # delta is a relative value
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
delta_rel <- lapply(function(i) list(x = AnnAv_YearsRange_0[[i]]$x,
	y = AnnAv_YearsRange_0[[i]]$y,
	z = ((AnnAv_YearsRange[[i]]$z - AnnAv_YearsRange_0[[i]]$z)/AnnAv_YearsRange_0[[i]]$z)), 
	X = i_seq_delta)
str(delta_rel)
StatBar <- txtProgressBar(min = 1, max = length(delta_rel), style = 3)
cat("Delta_rel output", "  \n\r", sep = "")
for (i in i_seq_delta) {	
	setTxtProgressBar(StatBar, i, label = i)
	model_name_to_calcul <- names(Avail_in_Year)[i]
# ParamLim_vct should be added manually if some pecularities in data exist (as for pr, e.g.)	
	Output_TabAndPlot(CMIP5_df = CMIP5ModelsInfo_df,
	Calcul_df = delta_rel[[i]], 
	identif_text = paste(model_name_to_calcul, "_delta_rel", sep = ""), 
	RD_name = rd_name, Years_Range_vct1 = YearsRange_check_0, 
	Years_Range_vct2 = YearsRange_check, 
	ParamLim_vct = c(-0.3, 0.3))
}
delta_rel_z <- lapply(`[[`, X = delta_rel, "z")
delta_rel_x <- lapply(`[[`, X = delta_rel, "x")
delta_rel_y <- lapply(`[[`, X= delta_rel, "y")
MM_AnAvr_rel_x <- Reduce(`+`, delta_rel_x)/length(delta_rel_x)
MM_AnAvr_rel_y <- Reduce(`+`, delta_rel_y)/length(delta_rel_y)
MM_AnAvr_rel_z <- Reduce(`+`, delta_rel_z)/length(delta_rel_z)
MM_AnAvr_delta_rel <- list(x = MM_AnAvr_rel_x, y = MM_AnAvr_rel_y,
	z = MM_AnAvr_rel_z)
str(MM_AnAvr_delta_rel)
# ParamLim_vct should be added manually if some pecularities in data exist (as for pr, e.g.)
Output_TabAndPlot(CMIP5_df = CMIP5ModelsInfo_df,
Calcul_df = MM_AnAvr_delta_rel, identif_text = "_MM_Ens_rel_delta",
RD_name = rd_name, Years_Range_vct1 = YearsRange_check_0, 
Years_Range_vct2 = YearsRange_check, 
ParamLim_vct = c(-0.3, 0.3))