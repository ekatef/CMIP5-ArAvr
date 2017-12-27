# following parameters sholud be checked before a run
# 	1) file_name names of the processed files (including the path if nessecary)
# 	2) CMIP5_dir_name is the directory with CMIP5 calculation files
# 	3) wd_name is the name of the working directory were results will be placed
#
rm(list=ls())
# Past
#CMIP5_dir_name<-"D://_CFD Data_//CERA_Data//ETH historical//tas//"
CMIP5_dir_name<-"D://_CFD Data_//CERA_Data//ETH historical//pr//"
#CMIP5_dir_name<-"D://_CFD Data_//CERA_Data//ETH historical//mrro//"
#CMIP5_dir_name<-"D://_CFD Data_//CERA_Data//ETH historical//mrros//"
# Future: version rcp 4.5
#CMIP5_dir_name<-"D://_CFD Data_//CERA_Data//ETHr4 experiment//tas_4-5//"
#CMIP5_dir_name<-"D://_CFD Data_//CERA_Data//ETHr4 experiment//mrros_4_5"
#CMIP5_dir_name<-"D://_CFD Data_//CERA_Data//ETHr4 experiment//pr_4-5//"
#CMIP5_dir_name<-"D://_CFD Data_//CERA_Data//ETHr4 experiment//mrro_4-5//"
rd_name<-"D://_CFD Data_//Data Processing//Results//Gridded//"
wd_name<-"D://_CFD Data_//Data Processing//_Runoff_//"
setwd(wd_name)