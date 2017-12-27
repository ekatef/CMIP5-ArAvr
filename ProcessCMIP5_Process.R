# Works quite long due to plenty of internim outputs
# set coordinates of the area under consideration and the standard grid
x_RU_big<-200
x_RU_small<-20
y_RU_big<-80
y_RU_small<-40
# x and y dimensions have equal number of points
# according to the reqirements of the interp.surface.grid function
# from 'fields' package
x_ReGrid<-seq(from=x_RU_small,to=x_RU_big,length.out=200)
y_ReGrid<-seq(from=y_RU_small,to=y_RU_big,length.out=200)
I_ModelProc<-3
I_file<-10
# YearsRange1<-c(1961:1990)
# YearsRange2<-c(1981:2004)
# the same phase of the runoff according to Georgiadi
# YearsRange1<-c(1894:1914)
# YearsRange2<-c(1984:2004)
# similar NAO values (for pr analysis)
YearsRange1<-c(1897:1902)
YearsRange2<-c(1976:1981)
# temporary block to find suitable models automatically
# should be replaced with a more elegant modificationL
LowYearsBound<-min(YearsRange1,YearsRange2)
HighYearsBound<-max(YearsRange1,YearsRange2)
CMIP5ModelsInfo_df<-ExtractModels(CMIP5_DirName=CMIP5_dir_name,WDName=wd_name)
# print("CMIP5ModelsInfo")
# print(CMIP5ModelsInfo_df)
filesInDir_names<-ListCMIPFiles(CMIP5_DirName=CMIP5_dir_name,WDName=wd_name)
# TO DO compare with Scenario calculations
filesForProc_names<-CMIP5ModelsInfo_df$FileName[CMIP5ModelsInfo_df$year_0<=LowYearsBound &
 CMIP5ModelsInfo_df$year_N>=HighYearsBound]
filesForProc_numbers<-sapply(FUN=function(i) which(filesInDir_names==filesForProc_names[i]),
	X=seq(along.with=filesForProc_names))
MMEnsemble1<-list()
MMEnsemble2<-list()
mm_counter<-0
# TO DO find automatically the first and the last days of the interpolation interval 
FirstCalendDay_string<-"-01-01"
FinalCalendDay_string<-"-12-31"
#N_runs_vct<-filesForProc_numbers[25:27]
#N_runs_vct<-filesForProc_numbers
N_runs_vct<-filesForProc_numbers[-11]
	#StatBar <- txtProgressBar(min = 1, max = length(N_runs_vct), style = 3)
#for (I_file in filesForProc_numbers[-c(1:7)]) {
#for (I_file in filesForProc_numbers) {
for (I_file in N_runs_vct) {
	mm_counter<-mm_counter+1	
		#setTxtProgressBar(StatBar, mm_counter,label=mm_counter)
	setwd(CMIP5_dir_name)
	file_nc_obj_procSsn<-nc_open(filesInDir_names[I_file],verbose=FALSE)
	paramForProc_name<-file_nc_obj_procSsn$var[[length(file_nc_obj_procSsn$var)]]$name
	fileForProc_attributes<-ncatt_get(file_nc_obj_procSsn,varid=0)
	modelForProc_name<-fileForProc_attributes$model_id
	scenarioForProc_name<-fileForProc_attributes$experiment_id
	#
	timeInFile_df<-ExtractTime(file_nc_obj_procSsn)
	yearsInFile<-unique(timeInFile_df$year_byDates)
	yearsToProc1_i<-which(yearsInFile %in% YearsRange1)
	yearsToProc2_i<-which(yearsInFile %in% YearsRange2)
	#
	proc_grid<-ExtractGrid(ProcFile=file_nc_obj_procSsn,x_bnd=c(x_RU_small,x_RU_big),
		y_bnd=c(y_RU_small,y_RU_big))
	proc_YearsRange1<-lapply(FUN=function(i) ApproxForSeason(ProcFile=file_nc_obj_procSsn,
		x_bnd=c(x_RU_small,x_RU_big),y_bnd=c(y_RU_small,y_RU_big),
		SeasonPeriods=all_months_names,
		FirstSeasonDay_date=FirstCalendDay_string,LastSeasonDay_date=FinalCalendDay_string,i_OfYear=i),
		X=yearsToProc1_i)
	proc_YearsRange1_Avr<-Reduce("+", proc_YearsRange1) / length(proc_YearsRange1)
	proc_YearsRange1_regridded<-RegridModel(param_matrix=proc_YearsRange1_Avr,x_bnd=c(x_RU_small,x_RU_big),
		y_bnd=c(y_RU_small,y_RU_big),x_grid=proc_grid$GridLon,y_grid=proc_grid$GridLat,
		n_Regrid_Cells=200)
	MMEnsemble1[[mm_counter]]<-proc_YearsRange1_regridded$z
	proc_YearsRange2<-lapply(FUN=function(i) ApproxForSeason(ProcFile=file_nc_obj_procSsn,
		x_bnd=c(x_RU_small,x_RU_big),y_bnd=c(y_RU_small,y_RU_big),
		SeasonPeriods=all_months_names,
		FirstSeasonDay_date=FirstCalendDay_string,LastSeasonDay_date=FinalCalendDay_string,i_OfYear=i),
		X=yearsToProc2_i)
	proc_YearsRange2_Avr<-Reduce("+", proc_YearsRange2) / length(proc_YearsRange2)
	proc_YearsRange2_regridded<-RegridModel(param_matrix=proc_YearsRange2_Avr,x_bnd=c(x_RU_small,x_RU_big),
		y_bnd=c(y_RU_small,y_RU_big),x_grid=proc_grid$GridLon,y_grid=proc_grid$GridLat,
		n_Regrid_Cells=200)
	MMEnsemble2[[mm_counter]]<-proc_YearsRange2_regridded$z
	ParamDelta<-MMEnsemble2[[mm_counter]]-MMEnsemble1[[mm_counter]]
	nc_close(file_nc_obj_procSsn)
	setwd(wd_name)
	# standard string for output identification
	YearsRange1_Info<-paste(YearsRange1[1],"to",YearsRange1[length(YearsRange1)],sep="")
	YearsRange2_Info<-paste(YearsRange2[1],"to",YearsRange2[length(YearsRange2)],sep="")
	CalculInfo1<-paste(modelForProc_name,"_",paramForProc_name,"_",
			scenarioForProc_name,"__",YearsRange1_Info, sep="")
	CalculInfo2<-paste(modelForProc_name,"_",paramForProc_name,"_",
			scenarioForProc_name,"__",YearsRange2_Info, sep="")
	DeltaInfo<-paste(modelForProc_name,"_",paramForProc_name,"_",
		scenarioForProc_name,"_delta__",YearsRange2_Info,"vs",
		YearsRange1_Info,sep="")
	file_name1_pdf<-paste(CalculInfo1,".pdf",sep="")
	output1_name<-paste(rd_name,file_name1_pdf,sep="")
	plotInfo1<-CalculInfo1
	file_name2_pdf<-paste(CalculInfo2,".pdf",sep="")
	output2_name<-paste(rd_name,file_name2_pdf,sep="")
	plotInfo2<-CalculInfo2
	file_delta_name_pdf<-paste(DeltaInfo,".pdf",sep="")
	output_delta_name<-paste(rd_name,file_delta_name_pdf,sep="")
	plotInfo_delta<-DeltaInfo
	# different parametars should have different plot ranges to make field plots beautiful
	if (paramForProc_name=="tas") {
		LimParam<-c(250,290)
		LimParam_delta<-c(0,1.5)
		TCol_flag<-TRUE
	} 	else if (paramForProc_name=="pr") {
		LimParam<-c(0.5e-5,6e-5)
		LimParam_delta<-c(-5e-6,6e-6)
		TCol_flag<-FALSE
	}	else if ((paramForProc_name=="mrro") | (paramForProc_name=="mrros")) {
		LimParam<-c(1e-6,15e-6)
		LimParam_delta<-c(-5e-7,5e-7)
		TCol_flag<-FALSE
	}	else {
		LimParam<-c(min(proc_YearsRange1_Avr,proc_YearsRange2_Avr,na.rm = TRUE),
			max(proc_YearsRange1_Avr,proc_YearsRange2_Avr,na.rm = TRUE))
		TCol_flag<-FALSE
		}
	# different parametars should have different plot ranges to make field plots beautiful
	pdf(output1_name,width=(200/1)/15, height=((40*3)/1)/15)
		PlotField(MatrixToPlot=proc_YearsRange1_Avr,
			xToPlot=proc_grid$GridLat,yToPlot=proc_grid$GridLon,
			ZLimParam=LimParam,PlotTitule=plotInfo1,TCol=TCol_flag)
		PlotField(MatrixToPlot=proc_YearsRange1_regridded$z,
			xToPlot=y_ReGrid,yToPlot=x_ReGrid,
			ZLimParam=LimParam,PlotTitule=plotInfo1,TCol=TCol_flag)	
	dev.off()
	pdf(output2_name,width=(200/1)/15, height=((40*3)/1)/15)	
		PlotField(MatrixToPlot=proc_YearsRange2_Avr,
			xToPlot=proc_grid$GridLat,yToPlot=proc_grid$GridLon,
			ZLimParam=LimParam,PlotTitule=plotInfo2,TCol=TCol_flag)
		PlotField(MatrixToPlot=proc_YearsRange2_regridded$z,
			xToPlot=y_ReGrid,yToPlot=x_ReGrid,
			ZLimParam=LimParam,PlotTitule=plotInfo2,TCol=TCol_flag)	
	dev.off()
	pdf(output_delta_name,width=(200/1)/15, height=((40*3)/1)/15)
	PlotField(MatrixToPlot=ParamDelta,
		xToPlot=y_ReGrid,yToPlot=x_ReGrid,
		ZLimParam=LimParam_delta,PlotTitule=plotInfo_delta,
		value_ofThickCount=0,TCol=TCol_flag)	
	dev.off()
}	
MMEnsemble1_Avr<-Reduce("+", MMEnsemble1) / length(MMEnsemble1)
MMEnsemble2_Avr<-Reduce("+", MMEnsemble2) / length(MMEnsemble2)
# TO DO: it's incorrect to use paramForProc_name as the global var outside the sycle 
MMEns1_Info<-paste("MMEns_",paramForProc_name,"_",
	scenarioForProc_name,"__",YearsRange1_Info, sep="")
MMEns2_Info<-paste("MMEns_",paramForProc_name,"_",
	scenarioForProc_name,"__",YearsRange2_Info, sep="")
# absolut changes are valuable for the temperature only
					# if (paramForProc_name=="tas") {
					# 	Delta<-(MMEnsemble2_Avr-MMEnsemble1_Avr)
					# 	Delta_Info<-paste("MMEns_delta(abs)_",paramForProc_name,"_",
					# 			scenarioForProc_name,"__",YearsRange1_Info,"vs",
					# 			YearsRange2_Info, sep="")
					# } else {
					# 	Delta<-(MMEnsemble2_Avr-MMEnsemble1_Avr)/MMEnsemble1_Avr
					# 	Delta_Info<-paste("MMEns_delta(relative)_",paramForProc_name,"_",
					# 		scenarioForProc_name,"__",YearsRange1_Info,"vs",
					# 		YearsRange2_Info, sep="")
					# }
					# #
if (paramForProc_name=="tas") {
	MMEns_Delta<-(MMEnsemble2_Avr-MMEnsemble1_Avr)
	MMEnsDelta_Info<-paste("MMEns_delta(abs)_",paramForProc_name,"_",
			scenarioForProc_name,"__",YearsRange2_Info,"vs",
			YearsRange1_Info, sep="")
} else {
	MMEns_Delta<-(MMEnsemble2_Avr-MMEnsemble1_Avr)/MMEnsemble1_Avr
	MMEnsDelta_Info<-paste("MMEns_delta(relative)_",paramForProc_name,"_",
		scenarioForProc_name,"__",YearsRange2_Info,"vs",
		YearsRange1_Info, sep="")
}
ens_file_name1_pdf<-paste(MMEns1_Info,".pdf",sep="")
ens_file_name1_txt<-paste(MMEns1_Info,".txt",sep="")
ens_output1_name<-paste(rd_name,ens_file_name1_pdf,sep="")
ens_txt1_name<-paste(rd_name,ens_file_name1_txt,sep="")
plotInfo1<-MMEns1_Info
ens_file_name2_pdf<-paste(MMEns1_Info,".pdf",sep="")
ens_file_name2_txt<-paste(MMEns1_Info,".txt",sep="")
ens_output2_name<-paste(rd_name,ens_file_name2_pdf,sep="")
ens_txt2_name<-paste(rd_name,ens_file_name2_txt,sep="")
plotInfo2<-MMEns2_Info
write.table(MMEnsemble1_Avr,file=ens_txt1_name,row.names = TRUE,
	col.names = TRUE, quote = FALSE)
pdf(ens_output1_name,width=(200/1)/15, height=((40*3)/1)/15)
	PlotField(MatrixToPlot=MMEnsemble1_Avr,
		xToPlot=y_ReGrid,yToPlot=x_ReGrid,
		ZLimParam=LimParam,PlotTitule=plotInfo1,TCol=TCol_flag)	
dev.off()
write.table(MMEnsemble2_Avr,file=ens_txt2_name,row.names = TRUE,
	col.names = TRUE, quote = FALSE)
pdf(ens_output2_name,width=(200/1)/15, height=((40*3)/1)/15)	
	PlotField(MatrixToPlot=MMEnsemble2_Avr,
		xToPlot=y_ReGrid,yToPlot=x_ReGrid,
		ZLimParam=LimParam,PlotTitule=plotInfo2,TCol=TCol_flag)	
dev.off()
delta_file_pdf<-paste(rd_name,MMEnsDelta_Info,".pdf",sep="")
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
		ZLimParam=LimParam_delta,PlotTitule=plotInfo_delta,TCol=TCol_flag,
		value_ofThickCount=0,NSign=2,NCLevels=20)	
dev.off()
#