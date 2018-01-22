library("fields")
library("maps")
#	Estimation of a sample variance, correction using Student distribution is applied
EstimVariance<-function(vect,conf_probability=0.95) {
	vect_mean<-mean(vect)
	n_obs<-length(vect)
	sigma_sum<-(vect_mean-vect[1])^2
	for (i_run in (2:n_obs)) {
		sigma_sum<-sigma_sum+(vect_mean-vect[i_run])^2
	}	
	sigma2<-sigma_sum/(n_obs-1)
	#return(sigma_sum)
	delta_estim<-(sigma2^0.5)*(qt(p=conf_probability,df=(n_obs-1))/(n_obs^0.5))
	sigma2_left<-sigma2*((n_obs-1)/qchisq(p=0.95, df=n_obs-1))
	sigma2_right<-sigma2*((n_obs-1)/qchisq(p=0.05, df=n_obs-1))
	return(c(Delta=delta_estim,SigmaMin=sigma2_left,
		Sigma=sigma2,Sigma_max=sigma2_right))
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
#
BuildVect<-function(List,i_mx,j_mx) {
	vect<-List[[1]][i_mx,j_mx]
	for (k in seq(along.with=List)[-1]) {
		vect<-c(vect,List[[k]][i_mx,j_mx])
	}
	return(vect)
}
EstimListVariance<-function(List,i_l,j_l) {
	VectFromList<-BuildVect(List=List,i_mx=i_l,j_mx=j_l)
	ij_Variance<-EstimVariance(vect=VectFromList,conf_probability=0.95)
	return(ij_Variance["Delta"])
}
PlotField<-function(MatrixToPlot,xToPlot,yToPlot,
	ZLimParam,PlotInfo="",PlotTitule="",TCol=FALSE,
	value_ofThickCount=NULL,NSign=3,NCLevels=7) {
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
	pr_min<-min(MatrixToPlot,na.rm=TRUE)
	pr_max<-max(MatrixToPlot,na.rm=TRUE)
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
	x_plot=xToPlot
	y_plot=yToPlot
	image(y=x_plot,x=y_plot,
		z=MatrixToPlot,col=mypalette,zlim=ZLimParam,useRaster=F,main=PlotTitule,
		sub=PlotInfo,xlab="",ylab="")
	map(database = "world", regions = "russia",col="gray25",lwd=5,add=T)
	contour(y=x_plot,x=y_plot,
		z=MatrixToPlot,add=TRUE,labcex=1.5,
		levels=coarse_countours_value,col="gray9",lwd=2,
		labels=signif(coarse_countours_value,d=NSign),cex=1.5)
	contour(y=x_plot,x=y_plot,
		z=MatrixToPlot,add=T,labcex=1.5,
		levels=countours_value,col="gray9",lwd=1,
		labels=signif(countours_value,d=NSign),cex=1.5)
	image.plot(legend.only=TRUE, zlim= ZLimParam,col=mypalette,
		horizontal=TRUE,legend.mar=2,legend.width=0.5,
		panel.first=grid(col="black"))
}
