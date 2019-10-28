current_param <- "tas"
it_is_temterature <- (current_param %in% "tas")

mod_set_id <- "good_for_all_names"

#-----------
# future
#-----------
YearsRange_check_0 <- 2007:2016
YearsRange_check <- 2045:2054

exp_code <- 45
exp_id <- paste0("rcp", exp_code)

#*********************************
# process all years: calculates seasonal mean 
# for each model and the set years
# & redridn the result on the "standard" mesh
SeasonsSet <- c(1L:12L)
#*********************************
x_RU_big <- 180
x_RU_small <- -20

y_RU_big <- 80
y_RU_small <- 40
#------------------------------------------------------------------------------

#***********************************

#------------------------------------------------------------------------------
#		model set definition
#------------------------------------------------------------------------------
mod_set_names <- list(
	test = c("MPI-ESM-LR", "CMCC-CMS"),
	ens_max = "ALL",
	recom_Carvalho8 = c("MPI-ESM-LR", "MPI-ESM-MR",
						"HadGEM2-CC","HadGEM2-ES",
						"CMCC-CMS", "CNRM-CM5",
						"GFDL-CM3", "ACCESS1-0"),
)

mod_config_names <- names(mod_set_names)

if (!(mod_set_id %in% mod_config_names)) {
	stop(
		paste0("The name of the model set is outside of the available range", 
		"\n", "The following sets are available: ",
		paste(mod_config_names, collapse = ", "))
	)
}

selected_names_to_proc <- mod_set_names[[mod_set_id]]