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
	map(database = "world", regions = "russia",col="brown4",lwd=2,add=T)
	contour(y=x_plot,x=y_plot,
		MatrixToPlot,add=TRUE,labcex=1,
		levels=coarse_countours_value,col="gray9",lwd=2,
		labels=signif(coarse_countours_value,d=NSign))
	contour(y=x_plot,x=y_plot,z=MatrixToPlot,add=T,
		levels=countours_value,col="gray9",lwd=1,
		labels=signif(countours_value,d=NSign),cex=0.75)
	image.plot(legend.only=TRUE, zlim= ZLimParam,col=mypalette,
		horizontal=TRUE,legend.mar=2,legend.width=0.5,
		panel.first=grid(col="black"))
}