library("fields")
library("maps")
# library("maptools")

# calculation helpers ----------------------------------------------------------
RangeToName <- function(peri){
	return(paste0(range(peri)[1], "to", range(peri)[2]))
}

#	Estimation of a sample variance, correction using Student distribution is applied
EstimVariance<-function(vect, conf_probability = 0.95) {

	vect_mean <- mean(vect)
	n_obs <- length(vect)
	sigma_sum <- (vect_mean - vect[1])^2
	for (i_run in (2:n_obs)) {
		sigma_sum <- sigma_sum + (vect_mean - vect[i_run])^2
	}	
	sigma2 <- sigma_sum/(n_obs-1)

	delta_estim<-(sigma2^0.5)*(qt(p = conf_probability, df=(n_obs-1))/(n_obs^0.5))
	sigma2_left<-sigma2*((n_obs-1)/qchisq(p = 0.95, df = n_obs-1))
	sigma2_right<-sigma2*((n_obs-1)/qchisq(p = 0.05, df = n_obs-1))
	return(c(Delta = delta_estim, SigmaMin = sigma2_left,
		Sigma = sigma2, Sigma_max = sigma2_right))
	# return(S2_av)
	#return(qt(p=conf_probability,df=(n_obs-1)))
	#return(qt(p=conf_probability,df=(n_obs-1))/(n_obs^0.5))
	#return(c(delta_estim,S2_left,S2_av,S2_right))
}
# #	verification: an example from [Bendat,pp.97-98]
# #	NB a mistake in the example: the probability 0.95 was assumed instead an indicated 0.9
# test_vct<-c(60,61,47,56,61,63,
# 			65,69,54,59,43,61,
# 			55,61,56,48,67,65,
# 			60,58,57,62,57,58,
# 			53,59,58,61,67,62,
# 		54)
# mean(test_vct)
# estim_variance(test_vct,conf_probability=0.95)
# # values to compare
# 58.61			# mean			
# (60.37-56.85)/2	# delta
# 22.91			# sigma2 left
# 33.43			# sigma2	
# 54.22			# sigma2 right
# #

BuildVect<-function(List, i_mx, j_mx) {
	vect <- List[[1]][i_mx, j_mx]
	for (k in seq(along.with = List)[-1]) {
		vect <- c(vect, List[[k]][i_mx, j_mx])
	}
	return(vect)
}

EstimListVariance <- function(List, i_l, j_l) {
	VectFromList <- BuildVect(List = List, i_mx = i_l, j_mx = j_l)
	ij_Variance <- EstimVariance(vect = VectFromList, conf_probability = 0.95)
	return(ij_Variance["Delta"])
}

# confidence assesment
max_mean <- function(x, conf_lev = 0.9){
	if (sum(!is.na(x)) >= 3) {
		zero_t_res <- t.test(x, rep(0, times = length(x)),
			conf.level = conf_lev)
		return(zero_t_res$estimate[1] + zero_t_res$conf.int[2])
	} else {
		return(NA)
	}	
}
min_mean <- function(x, conf_lev = 0.9){
	if (sum(!is.na(x)) >= 3) {
		zero_t_res <- t.test(x, rep(0, times = length(x)),
			conf.level = conf_lev)
		return(zero_t_res$estimate[1] + zero_t_res$conf.int[1])
	} else {
		return(NA)
	}	
}
# return p of a Student test against zero
# (the smaller the better -> the delta is relably different from zero)
p_t_test <- function(x){
	if (sum(!is.na(x)) >= 3) {
		return(
			t.test(x, rep(0, times = length(x)))$p.value
		)
	} else {
		return(NA)
	}	
}

# plot functions ---------------------------------------------------------------

PlotField<-function(MatrixToPlot, xToPlot, yToPlot,
	ZLimParam, PlotInfo = "",PlotTitule = "",TCol = FALSE,
	value_ofThickCount = NULL, NSign = 3, NCLevels = 7) {
	if (TCol) {
		mypalette <- rgb(
			red=(0:99)/100, 
			green = (c(seq(from = 0,to = 100, length = 25),
				rep(100, times = 45), seq(from = 100,to = 0, length = 30)))/100, 
			blue = (c(rep(100, times = 25),seq(from = 100,to = 0, length = 50),
				seq(from = 0, to = 0, length = 25)))/100
			)
	}
	else {
		mypalette<-rgb(
			red = (100:0)/100,
			green = (c(seq(from=0,to=90,length=25),
				rep(90,times = 50), seq(from = 90, to = 0,length = 25)))/100,
			blue = (0:100)/100)
	}

	pr_min <- min(MatrixToPlot, na.rm = TRUE)
	pr_max <- max(MatrixToPlot, na.rm = TRUE)
	pr_step <- (pr_max-pr_min)/NCLevels
	if (is.null(value_ofThickCount)) {
		value_ofThickCount <- mean(MatrixToPlot,na.rm = TRUE)
	}

	countours_value<-seq(from = pr_min, to = pr_max, by = pr_step)
	#countours_value<-c(seq(from=pr_min,to=pr_max,by=pr_step))
	countours_value <- countours_value[order(countours_value)]
	countours_value <- countours_value[-round(NCLevels/2)]
	coarse_countours_value <- c(min(countours_value),
		value_ofThickCount, max(countours_value))

	# seems that the parameters' order in image is a little bit odd (rows are y)
	# but it's assumed for calculation that rows correspond to x
	x_plot = xToPlot
	y_plot = yToPlot

	image(y = x_plot, x = y_plot,
		z = MatrixToPlot, col = mypalette, zlim = ZLimParam,
		useRaster = FALSE, main = PlotTitule,
		sub = PlotInfo, xlab = "", ylab = "")

	# TODO resolve hard-coding (not only Russian area is of interest)
	map(database = "world", regions = "russia",
		col = "gray25",lwd = 5,add = TRUE)

	contour(y = x_plot,x = y_plot,
		z = MatrixToPlot, add = TRUE, labcex = 1.5,
		levels = coarse_countours_value, col = "gray9",lwd = 2,
		labels = signif(coarse_countours_value, d = NSign), cex = 1.5)
	contour(y = x_plot, x = y_plot,
		z = MatrixToPlot, add = TRUE, labcex = 1.5,
		levels = countours_value, col="gray9", lwd=1,
		labels = signif(countours_value, d = NSign),cex = 1.5)
	image.plot(legend.only = TRUE, zlim = ZLimParam, col = mypalette,
		horizontal = TRUE, legend.mar = 2,legend.width = 0.5,
		panel.first = grid(col="black"))
}

PlotGlobal <- function(MatrixToPlot, xToPlot, yToPlot,
	ZLimParam, PlotInfo = "",PlotTitule = "",TCol = FALSE,
	value_ofThickCount = NULL,NSign = 3,NCLevels = 7) {
	if (TCol) {
		mypalette <- rgb(
			red = (0:99)/100,
			green = (c(seq(from = 0,to = 100,length = 25),
				rep(100,times = 45),seq(from = 100,to = 0,length = 30)))/100, 
			blue = (c(rep(100,times = 25),seq(from = 100,to = 0,length = 50),
				seq(from = 0,to = 0,length = 25)))/100)
	}
	else {
		mypalette <- rgb(
			red = (100:0)/100,
			green = (c(seq(from = 0,to = 90,length = 25),
				rep(90,times = 50),
				seq(from = 90,to = 0,length = 25)))/100,
			blue = (0:100)/100)
	}
	pr_min <- min(MatrixToPlot, na.rm = TRUE)
	pr_max <- max(MatrixToPlot, na.rm = TRUE)
	pr_step <- (pr_max-pr_min)/NCLevels
	if (is.null(value_ofThickCount)) {
		value_ofThickCount <- mean(MatrixToPlot,na.rm = TRUE)
	}
	countours_value <- seq(from = pr_min,to = pr_max,by = pr_step)
	countours_value <- countours_value[order(countours_value)]
	countours_value <- countours_value[-round(NCLevels/2)]
	coarse_countours_value <- c(min(countours_value),value_ofThickCount,max(countours_value))
	# seems that the parameters' order in image is a little bit odd (rows are y)
	# but it's assumed for calculation that rows correspond to x
	# fixed -> the order should be normal now
	x_plot = xToPlot
	y_plot = yToPlot
	image(x = x_plot,y = y_plot,
		z = MatrixToPlot,col = mypalette, zlim = ZLimParam,useRaster = F,main = PlotTitule,
		sub = PlotInfo,xlab = "",ylab = "")
	map(database = "world", col = "gray25",lwd = 5,add = T)
	contour(y = x_plot,x = y_plot,
		z = MatrixToPlot,add = TRUE,labcex = 1.5,
		levels = coarse_countours_value,col = "gray9",lwd = 2,
		labels = signif(coarse_countours_value,d = NSign),cex = 1.5)
	contour(y = x_plot,x = y_plot,
		z = MatrixToPlot,add = T,labcex = 1.5,
		levels = countours_value,col = "gray9",lwd = 1,
		labels = signif(countours_value,d = NSign),cex = 1.5)
	image.plot(legend.only = TRUE, zlim= ZLimParam,col = mypalette,
		horizontal = TRUE,legend.mar = 2,legend.width = 0.5,
		panel.first = grid(col="black"))
}
PlotField_UPD<-function(MatrixToPlot, xToPlot, yToPlot,
	ZLimParam, PlotInfo = "", PlotTitule = "", TCol=FALSE,
	value_ofThickCount = NULL, NSign = 3, NCLevels = 7) {
	if (TCol) {
		mypalette<-rgb(red=(0:99)/100,green=(c(seq(from=0,to=100,length=25),
			rep(100,times=45),seq(from=100,to=0,length=30)))/100, 
			blue = (c(rep(100,times=25),seq(from=100,to=0,length=50),
			seq(from=0,to=0,length=25)))/100)
	}
	else {
		mypalette<-rgb(red=(100:0)/100,green=(c(seq(from=0,to=90,length=25),
			rep(90,times=50),seq(from=90,to=0,length=25)))/100, blue = (0:100)/100)
	}
	pr_min<-min(MatrixToPlot, na.rm=TRUE)
	pr_max<-max(MatrixToPlot, na.rm=TRUE)
	pr_step<-(pr_max-pr_min)/NCLevels
	if (is.null(value_ofThickCount)) {
		value_ofThickCount<-mean(MatrixToPlot,na.rm=TRUE)
	}
	countours_value<-seq(from=pr_min,to=pr_max,by=pr_step)
	#countours_value<-c(seq(from=pr_min,to=pr_max,by=pr_step))
	countours_value<-countours_value[order(countours_value)]
	countours_value<-countours_value[-round(NCLevels/2)]
	coarse_countours_value<-c(min(countours_value),value_ofThickCount,max(countours_value))
	# seems that the parameters' order in image is a little bit odd (rows are y)
	# but it's assumed for calculation that rows correspond to x
	# x_plot = xToPlot
	# y_plot = yToPlot
	if (missing(ZLimParam)) ZLimParam <- c(pr_min, 0.3*pr_max)
	# filled.contour(x = xToPlot, y = yToPlot,
	# 	z = t(MatrixToPlot),col = mypalette, 
	# 	nlevels = 100,
	# 	zlim=ZLimParam, main=PlotTitule,
	# 	sub=PlotInfo,xlab="",ylab="")
	image(x = xToPlot, y = yToPlot,
		z = t(MatrixToPlot), col = mypalette, zlim = ZLimParam,useRaster = F,
		main = PlotTitule, sub = PlotInfo,xlab = "",ylab = "")
	map(database = "world", col = "gray25", lwd = 2, add = T, 
		wrap = c(0,360)) # regions = "russia", 
	# contour(x = xToPlot, y = yToPlot,
	# 	z = t(MatrixToPlot), add = TRUE, labcex = 1.5,
	# 	levels = coarse_countours_value, col = "gray9", lwd = 2,
	# 	labels = signif(coarse_countours_value, d = NSign), cex = 1.5)
	# contour(x = xToPlot, y = yToPlot,
	# 	z = t(MatrixToPlot), add = T, labcex=1.5,
	# 	levels = countours_value, col="gray9", lwd = 1,
	# 	labels = signif(countours_value, d=NSign), cex = 1.5)
	image.plot(legend.only = TRUE, zlim= ZLimParam, col = mypalette,
		horizontal = TRUE, legend.mar = 2, legend.width = 0.5,
		panel.first = grid(col = "black"))
}
PlotFieldGlob <- function(MatrixToPlot, xToPlot, yToPlot,
	ZLimParam, PlotInfo = "", PlotTit_Text = "",TCol = FALSE,
	value_ofThickCount = NULL, NSign = 3, NCLevels = 7,
	PlotCont = TRUE) {
	if (TCol) {
		mypalette<-rgb(red=(0:99)/100,green=(c(seq(from=0,to=100,length=25),
			rep(100,times=45),seq(from=100,to=0,length=30)))/100, 
			blue = (c(rep(100,times=25),seq(from=100,to=0,length=50),
			seq(from=0,to=0,length=25)))/100)
	}
	else {
		mypalette<-rgb(red=(100:0)/100,green=(c(seq(from=0,to=90,length=25),
			rep(90,times=50),seq(from=90,to=0,length=25)))/100, blue = (0:100)/100)
	}
	pr_min <- min(MatrixToPlot, na.rm=TRUE)
	pr_max <- max(MatrixToPlot, na.rm=TRUE)
	pr_step <- (pr_max-pr_min)/NCLevels
	if (is.null(value_ofThickCount)) {
		value_ofThickCount <- mean(MatrixToPlot,na.rm=TRUE)
	}
	countours_value<-seq(from=pr_min,to=pr_max,by=pr_step)
	# countours_value<-c(seq(from=pr_min,to=pr_max,by=pr_step))
	countours_value<-countours_value[order(countours_value)]
	countours_value<-countours_value[-round(NCLevels/2)]
	coarse_countours_value<-c(min(countours_value),value_ofThickCount,max(countours_value))
	# seems that the parameters' order in image is a little bit odd (rows are y)
	# but it's assumed for calculation that rows correspond to x
	x_plot = xToPlot
	y_plot = yToPlot
	if (missing(ZLimParam)) ZLimParam <- c(pr_min, pr_max)
	# filled.contour(x = xToPlot, y = yToPlot,
	# 	z = t(MatrixToPlot),col = mypalette, 
	# 	nlevels = 100,
	# 	zlim=ZLimParam, main=PlotTit_Text,
	# 	sub=PlotInfo,xlab="",ylab="")
	image(x = xToPlot, y = yToPlot,
		z = t(MatrixToPlot), col=mypalette, zlim=ZLimParam, useRaster=F,
		main = PlotTit_Text,
		sub=PlotInfo,xlab="",ylab="")
	maps::map(database = "world", col="gray25", lwd = 2, add = T, 
		wrap =c(0,360)) # regions = "russia", 
	if (PlotCont) {	
			contour(x = xToPlot, y = yToPlot,
			z = t(MatrixToPlot), add = TRUE, labcex = 1.5,
			levels = coarse_countours_value, col = "gray9", lwd = 2,
			labels = signif(coarse_countours_value, d = NSign), cex = 1.5)
		contour(x = xToPlot, y = yToPlot,
			z = t(MatrixToPlot), add = T, labcex=1.5,
			levels = countours_value, col="gray9", lwd = 1,
			labels = signif(countours_value, d=NSign), cex = 1.5)
	}
	image.plot(legend.only = TRUE, zlim= ZLimParam, col = mypalette,
		horizontal = TRUE, legend.mar = 2, legend.width = 0.5,
		panel.first = grid(col = "black"))
}
PlotFieldReg <- function(MatrixToPlot, xToPlot, yToPlot,
	ZLimParam, PlotInfo  ="", PlotTit_Text = "",TCol=FALSE,
	value_ofThickCount = NULL,NSign = 3,NCLevels = 7, RegToPlot = "russia") {
	if (TCol) {
		mypalette<-rgb(red=(0:99)/100,green=(c(seq(from=0,to=100,length=25),
			rep(100,times=45),seq(from=100,to=0,length=30)))/100, 
			blue = (c(rep(100,times=25),seq(from=100,to=0,length=50),
			seq(from=0,to=0,length=25)))/100)
	}
	else {
		mypalette<-rgb(red=(100:0)/100,green=(c(seq(from=0,to=90,length=25),
			rep(90,times=50),seq(from=90,to=0,length=25)))/100, blue = (0:100)/100)
	}
	pr_min <- min(MatrixToPlot, na.rm=TRUE)
	pr_max <- max(MatrixToPlot, na.rm=TRUE)
	pr_step <- (pr_max-pr_min)/NCLevels
	if (is.null(value_ofThickCount)) {
		value_ofThickCount <- mean(MatrixToPlot,na.rm=TRUE)
	}
	# countours_value<-seq(from=pr_min,to=pr_max,by=pr_step)
	#countours_value<-c(seq(from=pr_min,to=pr_max,by=pr_step))
	# countours_value<-countours_value[order(countours_value)]
	# countours_value<-countours_value[-round(NCLevels/2)]
	# coarse_countours_value<-c(min(countours_value),value_ofThickCount,max(countours_value))
	# seems that the parameters' order in image is a little bit odd (rows are y)
	# but it's assumed for calculation that rows correspond to x
	# x_plot = xToPlot
	# y_plot = yToPlot
	# if (missing(ZLimParam)) ZLimParam <- c(pr_min, pr_max)
	# filled.contour(x = xToPlot, y = yToPlot,
	# 	z = t(MatrixToPlot),col = mypalette, 
	# 	nlevels = 100,
	# 	zlim=ZLimParam, main=PlotTitule,
	# 	sub=PlotInfo,xlab="",ylab="")
	image(x = xToPlot, y = yToPlot,
		z = t(MatrixToPlot), col=mypalette, zlim = ZLimParam, useRaster=F,
		main = PlotTit_Text,
		sub=PlotInfo,xlab="",ylab="")
	map(database = "world", col="gray25", lwd = 2, add = T, 
		wrap =c(0,360), regions = RegToPlot)
	# contour(x = xToPlot, y = yToPlot,
	# 	z = t(MatrixToPlot), add = TRUE, labcex = 1.5,
	# 	levels = coarse_countours_value, col = "gray9", lwd = 2,
	# 	labels = signif(coarse_countours_value, d = NSign), cex = 1.5)
	# contour(x = xToPlot, y = yToPlot,
	# 	z = t(MatrixToPlot), add = T, labcex=1.5,
	# 	levels = countours_value, col="gray9", lwd = 1,
	# 	labels = signif(countours_value, d=NSign), cex = 1.5)
	image.plot(legend.only = TRUE, zlim= ZLimParam, col = mypalette,
		horizontal = TRUE, legend.mar = 2, legend.width = 0.5,
		panel.first = grid(col = "black"))
}
PlotFieldTest <- function(MatrixToPlot, xToPlot, yToPlot,
	ZLimParam, PlotInfo  ="", PlotTit_Text = "",TCol=FALSE,
	value_ofThickCount = NULL,NSign = 3,NCLevels = 7, RegToPlot = "russia") {
	if (TCol) {
		mypalette<-rgb(red=(0:99)/100,green=(c(seq(from=0,to=100,length=25),
			rep(100,times=45),seq(from=100,to=0,length=30)))/100, 
			blue = (c(rep(100,times=25),seq(from=100,to=0,length=50),
			seq(from=0,to=0,length=25)))/100)
	}
	else {
		mypalette<-rgb(red=(100:0)/100,green=(c(seq(from=0,to=90,length=25),
			rep(90,times=50),seq(from=90,to=0,length=25)))/100, blue = (0:100)/100)
	}
	pr_min <- min(MatrixToPlot, na.rm=TRUE)
	pr_max <- max(MatrixToPlot, na.rm=TRUE)
	pr_step <- (pr_max-pr_min)/NCLevels
	# if (is.null(value_ofThickCount)) {
	# 	value_ofThickCount <- mean(MatrixToPlot,na.rm=TRUE)
	# }
	image(x = xToPlot, y = yToPlot,
		z = t(MatrixToPlot), col = mypalette, zlim = ZLimParam, useRaster=F,
		main = PlotTit_Text,
		sub=PlotInfo,xlab="",ylab="")
	map(database = "world", col="gray25", lwd = 2, add = T, 
		wrap =c(0,360), regions = RegToPlot)
	image.plot(legend.only = TRUE, zlim= ZLimParam, col = mypalette,
		horizontal = TRUE, legend.mar = 2, legend.width = 0.5,
		panel.first = grid(col = "black"))
}

# ggplot version of a custom levelplot
# !!! x is lon, y is lat

# TODO
# 1) cut margins (an empty map is not terribly interesting)
# 2) change map background to white
# 3) smooth borders
# 4) draw countour-lines
Draw_Ens_Plot_Optim <- function(Delta_DF = delta_appr, Sc_name_To_Proceed,
	Map_Data = ru, PDF_name = pdf_output, Levels_Val = values_for_the_levels,
	neg_col = "black", neg_alpha = 0.5, neg_size = 0.3, neg_shape = 124) {

	col_to_proceed <- Delta_DF[, Sc_name_To_Proceed]
# it seems, base subsetting fits better for use within functions
	df_neg_to_plot <- Delta_DF[col_to_proceed < 0, ]
	df_NA_to_plot <- Delta_DF[is.na(col_to_proceed), ]

	values_for_the_labels <- paste0(100 * Levels_Val[-length(Levels_Val)], 
		"..", 100 * Levels_Val[-1], "%")
	labels_for_the_levels <- c(paste0("< ", range(Levels_Val)[1] * 100, "%"),
		values_for_the_labels,
		paste0("> ", range(Levels_Val)[2] * 100, "%"))
	# include.lowest = TRUE means that the min value should be included
	test_cut <- cut(col_to_proceed, 
		breaks = c(min(col_to_proceed, na.rm = TRUE), 
		Levels_Val, 
		max(col_to_proceed, na.rm = TRUE)),
		include.lowest = TRUE)
	Delta_DF$val_level <- test_cut

	# return(Delta_DF)
	# return(labels_for_the_levels)
	# return(df_neg_to_plot)

	p_test_plot <- ggplot(data = Delta_DF,
				aes(x = y, y = x, z = value, fill = val_level)) +
				geom_tile() +
				# # just makes a border a little dizzy
				# geom_raster(interpolate = TRUE) +
				# scale_fill_manual() +
				# scale_fill_manual(values = c("gray0", "gray20", "gray45", "gray99",
				# 	"gray80", "gray60", "gray50"), labels = labels_for_the_levels) #+		
				# # dashing of negative changes		
				scale_fill_manual(values = c("red", "orange", "gold", 
					"green3", "forestgreen"), labels = labels_for_the_levels) +		
				# dashing of negative changes		
				geom_point(data = df_neg_to_plot, aes(x = y, 
					y = x),
					inherit.aes = FALSE, size = neg_size, color = neg_col,
					alpha = neg_alpha, shape = neg_shape) +
				geom_polygon(data = Map_Data, aes(Map_Data$long, Map_Data$lat, 
					group = Map_Data$group) , 
					color = "gray30", fill = "transparent", size = 0.25,
					inherit.aes = FALSE) +
				coord_quickmap(xlim = c(0, 183), ylim = c(40, 80)) +
				labs(x = "long", y = "lat", 
					fill = "",
					title = Sc_name_To_Proceed, 
					subtitle = paste0(""),
					inherit.aes = FALSE) +
				theme(panel.border = element_rect(colour = "gray30", 
					fill = NA, size = 0.1),
					legend.text = element_text(size = 14),
					axis.text = element_text(size = 14))

	return(p_test_plot)					

	# pdf(PDF_name, width = (range(Delta_DF$x_val)[2] - range(Delta_DF$x_val)[1])/15,
	# 			height = (range(Delta_DF$y_val)[2] - range(Delta_DF$y_val)[1])/8)
	# plot(p_test_plot)
	# dev.off()
}

PlotLattField <- function(spartial_inp,
	print_contour = FALSE) {
	library(lattice)
	library(RColorBrewer)
	library(latticeExtra)
	library(tidyverse)
	# library(ggplot2)
	
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	wrld <- maps::map('world', xlim = c(0, 180),
		ylim = c(40, 70))
	wrld <- data.frame(lon=wrld$x, lat=wrld$y)
	
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	if (is.null(spartial_inp[["x"]])) {
		data_df <- spartial_inp
	} else{
		data_df <- data.frame(x = spartial_inp[["x"]], y = spartial_inp[["y"]],
			spartial_inp[["z"]])
	}
	
	data_df_tmp <- data_df
	colnames(data_df_tmp)[-(1:2)] <- paste0("y_", data_df_tmp$y)
	# print(head(data_df_tmp[, 1:10]))
	
	data_df_long <- data_df_tmp %>% select(-y) %>%
		gather(key, value, -x) %>%
		mutate(y = as.numeric(str_replace(key, "y_", ""))) %>%
		select(x, y, value) %>%
		filter(!is.na(value))
	# print(head(data_df_long))
	# print(range(data_df_long$y))
	
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	#-----------------------------------
	if (min(data_df_long$value, na.rm = TRUE) >= 0){ 
		# good for absolute wind values
		cols <- brewer.pal(9, "YlGnBu")
	} else {
		# good for decrease
		cols <- brewer.pal(10, "RdYlBu")[-(1:2)]
	}
	#-----------------------------------
	
	lp <- levelplot(data = data_df_long, 
		value ~ y + x, 	
		pretty = TRUE, 
		at = pretty(data_df_long$value, 3),
		aspect = 0.4, 
		col.regions = cols, 
		contour = TRUE,
		xlab = "", ylab = "")
	
	cp <- contourplot(data = data_df_long, value ~ y + x, 
		cuts = 5, 
		at = pretty(data_df_long$value, 10),
		pretty = TRUE,
		aspect = 0.5, 
		contour = TRUE)
	
	wp <- xyplot(
		data = wrld, lat ~ lon, 
		lty = 1, lwd = 0.5, col = 'black',
		# type = c("l", "g"), 
		type = c("l"),
		panel = function(...) {
			panel.xyplot(...)
			panel.grid(h = -1, v = -1, col.line = "gray10", lwd = 0.1)
		}
		)
	
	if (!print_contour) {
		res <- lp + as.layer(wp)
	} else {
		res <- lp + cp + as.layer(wp)
	}

	return(res)
}

# overlay points to map
PlotLattFieldPoints <- function(spartial_inp,
	points_df = NULL,
	print_contour = FALSE) {
	library(lattice)
	library(RColorBrewer)
	library(latticeExtra)
	library(tidyverse)
	# library(ggplot2)
	
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	wrld <- maps::map('world', xlim = c(0, 180),
		ylim = c(40, 70))
	wrld <- data.frame(lon=wrld$x, lat=wrld$y)
	
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	if (is.null(spartial_inp[["x"]])) {
		data_df <- spartial_inp
	} else{
		data_df <- data.frame(x = spartial_inp[["x"]], y = spartial_inp[["y"]],
			spartial_inp[["z"]])
	}
	
	data_df_tmp <- data_df
	colnames(data_df_tmp)[-(1:2)] <- paste0("y_", data_df_tmp$y)
	# print(head(data_df_tmp[, 1:10]))
	
	data_df_long <- data_df_tmp %>% select(-y) %>%
		gather(key, value, -x) %>%
		mutate(y = as.numeric(str_replace(key, "y_", ""))) %>%
		select(x, y, value) %>%
		filter(!is.na(value))
	# print(head(data_df_long))
	# print(range(data_df_long$y))
	
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	#-----------------------------------
	if (min(data_df_long$value, na.rm = TRUE) >= 0){ 
		# good for absolute wind values
		cols <- brewer.pal(9, "YlGnBu")
	} else {
		# good for decrease
		cols <- brewer.pal(10, "RdYlBu")[-(1:2)]
	}
	#-----------------------------------
	
	lp <- levelplot(data = data_df_long, 
		value ~ y + x, 	
		pretty = TRUE, 
		at = pretty(data_df_long$value, 3),
		aspect = 0.4, 
		col.regions = cols, 
		contour = TRUE,
		xlab = "", ylab = "")
	
	cp <- contourplot(data = data_df_long, value ~ y + x, 
		cuts = 5, 
		at = pretty(data_df_long$value, 10),
		pretty = TRUE,
		aspect = 0.5, 
		contour = TRUE)
	
	wp <- xyplot(
		data = wrld, lat ~ lon, 
		lty = 1, lwd = 0.5, col = 'black',
		# type = c("l", "g"), 
		type = c("l"),
		panel = function(...) {
			panel.xyplot(...)
			panel.grid(h = -1, v = -1, col.line = "gray10", lwd = 0.1)
		}
		)
	
	if (!print_contour) {
		res <- lp + as.layer(wp)
	} else {
		res <- lp + cp + as.layer(wp)
	}

	if (!(is.null(points_df ))) {
		xyp <- xyplot(data = points_df, lat ~ long)
		res <- wp + as.layer(xyp)
	}

	return(res)
}

# @spartial_inp is either a list with x, y and z components, z is a matrix
# or a matrix
# adjust color scales
PlotLattField_play <- function(spartial_inp,
	points_df. = NULL,
	print_contour. = FALSE, 
	is_temper. = FALSE, is_long. = FALSE, fill_ocean. = FALSE, 
	n_cols., custom_cuts. = NULL, 
	n_pal_adj.,
	jpeg_output. = FALSE,
	show_legend. = TRUE) {

	# res <- PlotLattField_play(spartial_inp = spt_var, 
	# 	points_df. = points_df,
	# 	print_contour. = print_contour,
	# 	is_temper. = is_temper, is_long. = is_long, fill_ocean. = fill_ocean,
	# 	n_cols = n_cols, custom_cuts = custom_cuts, 
	# 	n_pal_adj = n_pal_adj, 
	# 	jpeg_output = jpeg_output,
	# 	show_legend = show_legend)

	library(lattice)
	library(RColorBrewer)
	library(latticeExtra)
	library(tidyverse)
	# library(ggplot2)

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	wrld <- maps::map('world', xlim = c(0, 180),
		ylim = c(40, 70))
	wrld <- data.frame(lon = wrld$x, lat = wrld$y)
	
	# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	if (is.null(spartial_inp[["x"]])) {
		data_df <- spartial_inp
	# @spartial_inp is a list with the components x, y, z
	} else {
		data_df <- data.frame(x = spartial_inp[["x"]], y = spartial_inp[["y"]],
			z = spartial_inp[["z"]])
	}

	data_df_tmp <- data_df

	if (is_long.) {
		data_df_long <- data_df
	} else {
		colnames(data_df_tmp)[-(1:2)] <- paste0("y_", data_df_tmp$y)
		# print(head(data_df_tmp[, 1:10]))		
		data_df_long <- data_df_tmp %>% select(-y) %>%
			gather(key, z, -x) %>%
			mutate(y = as.numeric(str_replace(key, "y_", ""))) %>%
			select(x, y, z) %>%
			filter(!is.na(z))
	}

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	# NB if the scale is a diverging one, the @n_cols. and @n_pal_adj.
	# arguments are silently ignored
	if (is_temper.) {
		cols <- brewer.pal(9, "YlOrRd")
	} else {
		#-----------------------------------
		if (min(data_df_long$z, na.rm = TRUE) >= 0){ 
			# good for absolute wind values
			cols <- brewer.pal(9, "YlGnBu")
		} else {
			# good for decrease
			# cols <- brewer.pal(11, "RdYlBu")
			# cols <- brewer.pal(11, "RdYlBu")[-(10:11)]
			cols <- brewer.pal(n_cols., "RdYlBu")[n_pal_adj.]
		}
		#-----------------------------------
	}

	if (is.null(custom_cuts.)){
		# scale_breaks <- pretty(data_df_long$z, 3)
		scale_breaks <- pretty(data_df_long$z, length(cols) - 2)
		# scale_breaks <- do.breaks(range(data_df_long$z, na.rm = TRUE), 
		# 	length(cols) - 2)
	} else {
		scale_breaks = custom_cuts.
	}

	if (fill_ocean.) {
		library(maptools)
		data(wrld_simpl)

		# note that x and y are traditionally reversed
		pts <- SpatialPoints(data_df_long[, c("y", "x")], 
			proj4string=CRS(proj4string(wrld_simpl)))
		
		# find which points fall over land
		ii <- !is.na(over(pts, wrld_simpl)$FIPS)

		data_df_long <- data_df_long[ii, ]

	}

	
	# jpeg output is required sometimes, but is very rather tricky to plot
	if (!jpeg_output.) {	
		if (show_legend.) {
			lp <- levelplot(data = data_df_long, 
				z ~ y + x, 	
				pretty = TRUE, 
				at = scale_breaks,
				aspect = 0.4, 
				col.regions = cols, 
				contour = TRUE,
				# margin = TRUE,
				xlab = "", ylab = "")
		} else {
			lp <- levelplot(data = data_df_long, 
				z ~ y + x, 	
				pretty = TRUE, 
				at = scale_breaks,
				aspect = 0.4, 
				col.regions = cols, 
				contour = TRUE,
				# margin = TRUE,
				xlab = "", ylab = "", colorkey=FALSE)
		}
		
		cp <- contourplot(data = data_df_long, z ~ y + x, 
			cuts = 5, 
			at = pretty(data_df_long$z, 10),
			pretty = TRUE,
			aspect = 0.5, 
			# margin = TRUE,
			contour = TRUE)
		
		wp <- xyplot(
			data = wrld, lat ~ lon, 
			lty = 1, lwd = 0.5, fill = 'black', col = 'black', pch = 21,
			type = c("l", "g"), 
			# type = c("l"), 
			cex = 0.5,
			# margin = TRUE,
			panel = function(...) {
				panel.xyplot(...)
				panel.grid(h = -1, v = -1, col.line = "gray10", lwd = 0.5)
			}
			)	
	# jpeg otput	
	} else {

		# https://stackoverflow.com/q/32657426/8465924
		lattice.options(
		  layout.heights = list(bottom.padding = list(x = 0.5), 
		  	top.padding = list(x = 0.5), axis.ylab.padding = list(x = 0.5)),
		  layout.widths = list(left.padding = list(x = 0.5),
		  	right.padding = list(x = 0.5), axis.xlab.padding = list(x = 0.5))
		)

		lp <- levelplot(data = data_df_long, 
			z ~ y + x, 	
			# pretty = TRUE, 

			## throws an error
			at = custom_cuts.,
			colorkey = list(col = cols, 
				at = custom_cuts.),

			# cuts = length(cols) - 2,
			aspect = 0.4, 
			col.regions = cols, 
			contour = TRUE,
			margin = TRUE,
			xlab = "", ylab = "",
			scales = list(tck = 0),
			# another option
			# scales = list(tck = c(1, 0), x = list(cex = 1.2), 
			# 	y = list(cex = 1.5)),
			par.settings = list(fontsize = list(text = 33, points = 10)))
		
		cp <- contourplot(data = data_df_long, z ~ y + x, 
			cuts = 5, 
			at = pretty(data_df_long$z, 10),
			pretty = TRUE,
			aspect = 0.5, 
			margin = TRUE,
			contour = TRUE)
		
		wp <- xyplot(
			data = wrld, lat ~ lon, 
			lty = 1, lwd = 0.5, fill = 'black', col = 'black', pch = 21,
			type = c("l", "g"), 
			# type = c("l"), 
			cex = 3,
			scales = list(tck = c(1, 0), x = list(cex = 1.2), 
				y = list(cex = 1.25)),
			margin = TRUE,
			panel = function(...) {
				panel.xyplot(...)
				panel.grid(h = -1, v = -1, col.line = "gray10", lwd = 0.5)
			}
		)
	}
	
	if (!print_contour.) {
		res <- lp + as.layer(wp)
	} else {
		res <- lp + cp + as.layer(wp)
	}

	if (!(is.null(points_df.))) {
		xyp <- xyplot(data = points_df., lat ~ long,
			col = "gray10", pch = 20, cex = 2)
		res <- res + as.layer(xyp)
	}	

	return(res)
}

OutputLattPlot <- function(spt_var,
	points_df = NULL,

	is_temper = FALSE, is_long = FALSE, fill_ocean = FALSE,
	
	print_contour = FALSE,
	default_output = FALSE, 
	yrs_range1 = YearsRange_check_0, 
	yrs_range2 = YearsRange_check,
	months_range,
	res_dir = ".", identif_text = "",
	
	n_cols = 11, custom_cuts = NULL, 
	n_pal_adj = -c(1:2),
	jpeg_output = FALSE, 
	show_legend = TRUE) {
 
	res <- PlotLattField_play(spartial_inp = spt_var, 
		points_df. = points_df,
		print_contour. = print_contour,
		is_temper. = is_temper, is_long. = is_long, fill_ocean. = fill_ocean,
		n_cols. = n_cols, custom_cuts. = custom_cuts, 
		n_pal_adj. = n_pal_adj, 
		jpeg_output. = jpeg_output,
		show_legend. = show_legend)

	if (default_output) {

		fl_name <- paste0(substitute(spt_var), "_",
			identif_text, "_",
			"months_", RangeToName(months_range), "_",
			RangeToName(yrs_range1), "_vs_",
			RangeToName(yrs_range2), "_",
			".pdf")		

		# pdf(file.path(res_dir, fl_name))
		cairo_pdf(file.path(res_dir, fl_name))
		plot(res)
		dev.off()
	}

	# return(file.path(res_dir, fl_name))

	return(res)

}
