rm(list=ls())
#
fileOnly_names<-c("mrro_Lmon_bcc-csm1-1-m_rcp45_r1i1p1_200601-210012.nc","mrro_Lmon_bcc-csm1-1_rcp45_r1i1p1_200601-209912.nc",
	"mrro_Lmon_BNU-ESM_rcp45_r1i1p1_200601-210012.nc","mrro_Lmon_CanESM2_rcp45_r1i1p1_200601-210012.nc",
	"mrro_Lmon_CCSM4_rcp45_r1i1p1_200601-210012.nc","mrro_Lmon_CESM1-BGC_rcp45_r1i1p1_200601-210012.nc",
	"mrro_Lmon_GISS-E2-H_rcp45_r1i1p1_200601-205012.nc","mrro_Lmon_IPSL-CM5A-MR_rcp45_r1i1p1_200601-210012.nc",
	"mrro_Lmon_inmcm4_rcp45_r1i1p1_200601-210012.nc","mrro_Lmon_MPI-ESM-LR_rcp45_r1i1p1_200601-210012.nc",
	"mrro_Lmon_MRI-CGCM3_rcp45_r1i1p1_200601-210012.nc","mrro_Lmon_NorESM1-M_rcp45_r1i1p1_200601-210012.nc",
	"mrro_Lmon_NorESM1-ME_rcp45_r1i1p1_200601-210212.nc","mrro_Lmon_MIROC-ESM_rcp45_r1i1p1_200601-210012.nc",
	"mrro_Lmon_MIROC5_rcp45_r1i1p1_200601-210012.nc")
ith_file<-2
file_name<-paste("D://_CFD Data_//CERA_Data//ETHr4 experiment//mrro_4-5//",fileOnly_names[ith_file],sep="")
CMIP5_dir_name<-"D://_CFD Data_//CERA_Data//ETHr4 experiment//mrro_4-5//"
#CMIP5_dir_name<-"D://_CFD Data_//CERA_Data//ETHr4 experiment//tas_4-5"
# CMIP5_dir_name<-"D://_CFD Data_//CERA_Data//ETHr4 experiment//pr_4-5//"
rd_name<-"D://_CFD Data_//Data Processing//_Runoff_//results"
wd_name<-"D://_CFD Data_//Data Processing//_Runoff_"
setwd(wd_name)