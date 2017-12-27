delta_file_pdf<-paste(rd_name,MMEnsDelta_Info,"_2.pdf",sep="")
plotInfo_delta<-MMEnsDelta_Info
# most beautiful plot range depends on the nature of the parameter
if (paramForProc_name=="tas") {
	LimParam_delta<-c(0,1.5)
} else {
	LimParam_delta<-c(-0.05,0.15)
}
pdf(delta_file_pdf,width=(200/1)/15, height=((40*3)/1)/15)	
	PlotField(MatrixToPlot=MMEns_Delta,
		xToPlot=y_ReGrid,yToPlot=x_ReGrid,
		ZLimParam=LimParam_delta_glob,PlotTitule=plotInfo_delta,TCol=TCol_flag,
		value_ofThickCount=0,NSign=2,NCLevels=20)	
dev.off()
#