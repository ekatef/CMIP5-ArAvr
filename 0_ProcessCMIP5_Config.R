x_RU_big <- 180
x_RU_small <- -20
y_RU_big <- 75
y_RU_small <- 40

current_param <- "sfcWind"
it_is_temterature <- (current_param %in% "tas")

exp_code <- 45

YearsRange_check_0 <- 2007:2016
YearsRange_check <- 2085:2094

# months to be taken for seasonal calculations
SeasonsSet <- c(1L:12L)

mod_set_id <- "ens_max"

mod_set_names <- list(
	test = c("MPI-ESM-LR", "CMCC-CMS"),
	ens_max = "ALL")
mod_config_names <- names(mod_set_names)

if (!(mod_set_id %in% mod_config_names)) {
	stop(
		paste0("The name of the model set is outside of the available range", 
		"\n", "The following sets are available: ",
		paste(mod_config_names, collapse = ", "))
	)
}
selected_names_to_proc <- mod_set_names[[mod_set_id]]

exp_id <- paste0("rcp", exp_code)