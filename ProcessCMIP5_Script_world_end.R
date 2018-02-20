# !!! ProcessCMIP5_Script will clean the workspace and set neccesary paths
rm(list = ls())
#  wd_name should be repeated by starting with scratch
# TODO: parametrisation
wd_name <- "name_the_folder_with_R_code"
setwd(wd_name) # for the first run due to rm in the beginning

source("0_ProcessCMIP5_Config.R")
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

# TODO SeasonsSet should be defined as a parameter
ProcessSeasonByYears <- function(Avail_List, ModelName, x_Range, 
	y_Range, YearsToCalcul, n_cells){
 	StitchedData <- StichModelledFiles(CMIP5_Dir_Name = CMIP5_dir_name, 
 		Models_To_Calcul_List = Avail_List, ModelName = ModelName, 
 		X_To_Proc_vct = x_Range, Y_To_Proc_vct = y_Range)
 	x_grid_model <- StitchedData$Grid_Lon
 	y_grid_model <- StitchedData$Grid_Lat
 	# results for a single year
 	res_list <- lapply(function(Z) ApproxForSeason2(Dates_vct = StitchedData$RealCalendarTime, 
 		SeasonPeriods = SeasonsSet, YearVal = Z, Param_3D = StitchedData$T_3D), 
 		X = YearsToCalcul)
 	res_T_3D <- (Reduce(`+`, res_list)/length(res_list)) # returns gridded seasonal mean
 	# x&y corresponds to col&strings resp => x is y, y is x in the interp()
 	res_regrid <- RegridModel(param_matrix = res_T_3D, x_bnd = y_Range, y_bnd = x_Range,
		x_grid = y_grid_model, y_grid = x_grid_model, n_Regrid_Cells = n_cells)
 	return(res_regrid)
 }
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 #					testing function
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ExtractSeason_test <- function(Avail_List, ModelName, x_Range, 
	y_Range, YearsToCalcul) {
		StitchedData <- StichModelledFiles(CMIP5_Dir_Name = CMIP5_dir_name, 
	 		Models_To_Calcul_List = Avail_List, ModelName = ModelName, 
	 		X_To_Proc_vct = x_Range, Y_To_Proc_vct = y_Range)
	ExtractSeason_end(Dates_vct = StitchedData$RealCalendarTime, 
		SeasonPeriods = c(9L:11L))
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# inspect model files in the dir
# adjustment work is needed by first call of the working dir
# (delete incomplete time periods, check modelled parameters etc.)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# extract models info from current .nc dir; may take quite long
CMIP5ModelsInfo_df <- ExtractModels(CMIP5_Dir_Name = CMIP5_dir_name)
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