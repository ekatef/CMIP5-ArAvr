# Significance estimation should be done for the multimodel estimation
# for the parameter delta; absolute parameter delta should be considered
MMEns_Delta_abs<-(MMEnsemble2_Avr-MMEnsemble1_Avr)
MM_Delta_Spread<-mapply('-',MMEnsemble2,MMEnsemble1,SIMPLIFY=FALSE)
# check of mapply
print("Should be TRUE")
print(MM_Delta_Spread[[2]][2,3]==(MMEnsemble2[[2]]-MMEnsemble1[[2]])[2,3])
print("Should be FALSE")
print(MM_Delta_Spread[[2]][2,3]==(MMEnsemble2[[1]]-MMEnsemble1[[3]])[2,3])
#
SpreadOfMMEnsDelta<-matrix(NA,ncol=length(MM_Delta_Spread[[1]][,1]),
	nrow=length(MM_Delta_Spread[[1]][1,]))
#
pb <- txtProgressBar(style=3,min=1,max=length(MM_Delta_Spread[[1]][1,]))
print("Progress of Significance Estimation")
for (j in seq(along.with=MM_Delta_Spread[[1]][1,])) {
	setTxtProgressBar(pb, j)
	SpreadOfMMEnsDelta[,j]<-sapply(FUN=function(i) EstimListVariance(List=MM_Delta_Spread,i_l=i,j_l=j),
		X=seq(along.with=MM_Delta_Spread[[1]][,1]))
}
#
sign_test<-(SpreadOfMMEnsDelta<MMEns_Delta_abs)
signQuant_perc<-(SpreadOfMMEnsDelta/MMEns_Delta_abs)
#
SpreadOfMMEnsDelta_file_name_txt<-paste("SpreadOfMMEnsDelta_",paramForProc_name,"_",
	scenarioForProc_name,"_",
	"_",YearsRange2[1],"to",YearsRange2[length(YearsRange2)],
	".txt",sep="")
SpreadOfMMEnsDelta_txt_output_name<-paste(rd_name,SpreadOfMMEnsDelta_file_name_txt,sep="")
#
SpreadOfMMEnsDelta_file_pdf<-paste("MMEns_SpreadOfMMEnsDelta_",paramForProc_name,"_",
	scenarioForProc_name,"__",
	YearsRange1[1],"to",YearsRange1[length(YearsRange1)],
	"vs",YearsRange2[1],"to",YearsRange2[length(YearsRange2)],".pdf",sep="")
SpreadOfMMEnsDelta_output_name<-paste(rd_name,SpreadOfMMEnsDelta_file_pdf,sep="")
plotInfo_SpreadOfMMEnsDelta<-paste(YearsRange1[1],"-",YearsRange1[length(YearsRange1)],
	"vs",YearsRange2[1],"-",YearsRange2[length(YearsRange2)]," ",
	"Estimation of multimodel spread of calculated MMEns_delta_",paramForProc_name,
	"_",scenarioForProc_name,sep="")
QuantSign_output_name<-paste(rd_name,"SignEstim_",paramForProc_name,"_",
	scenarioForProc_name,"__",
	YearsRange1[1],"to",YearsRange1[length(YearsRange1)],
	"vs",YearsRange2[1],"to",YearsRange2[length(YearsRange2)],".pdf",sep="")
plotInfo_QuantSign<-paste(YearsRange1[1],"-",YearsRange1[length(YearsRange1)],
	"vs",YearsRange2[1],"-",YearsRange2[length(YearsRange2)]," ",
	"Multimodel spread of calculated MMEns_delta divided to MMEns_delta_",paramForProc_name,
	"_",scenarioForProc_name,sep="")
#
pdf(SpreadOfMMEnsDelta_output_name,width=(200/1)/15, height=((40*3)/1)/15)	
	PlotField(MatrixToPlot=SpreadOfMMEnsDelta,
		xToPlot=y_ReGrid,yToPlot=x_ReGrid,
		ZLimParam=c(min(SpreadOfMMEnsDelta,na.rm=T),max(SpreadOfMMEnsDelta,na.rm=T)),
		PlotTitule=plotInfo_SpreadOfMMEnsDelta,TCol=TRUE,
		value_ofThickCount=0,NSign=2,NCLevels=20)	
dev.off()
pdf(QuantSign_output_name,width=(200/1)/15, height=((40*3)/1)/15)	
	PlotField(MatrixToPlot=signQuant_perc,
		xToPlot=y_ReGrid,yToPlot=x_ReGrid,
		ZLimParam=c(0,2),PlotTitule=plotInfo_QuantSign,TCol=TRUE,
		value_ofThickCount=1,NSign=2,NCLevels=20)	
dev.off()
#
write.table(SpreadOfMMEnsDelta,file=SpreadOfMMEnsDelta_txt_output_name,row.names = TRUE,
	col.names = TRUE, quote = FALSE)