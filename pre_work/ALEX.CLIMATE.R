install.packages("readxl")
require(readxl)
require(zoo)

prec<-read_excel("D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX/R_package/TREX - Construction/Environmental data.xlsx",sheet="preci")
prec_raw<-data.frame(prec)
prec_raw[,2]<-round(as.numeric(prec_raw[,2]),6)
prec.in<-is.trex(prec_raw,time.format="%Y-%m-%d",tz="UTC")
index(prec.in)
preci<-prec.in
save(preci,file="D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX/R_package/TREX - Construction/preci.rda")
prec.in[,1]

vpd<-read_excel("D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX/R_package/TREX - Construction/Environmental data.xlsx",sheet="vpd")
vpd<-data.frame(vpd)
vpd_raw   <-is.trex(vpd,tz="GMT",time.format="(%m/%d/%y %H:%M:%S)",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
vpd.input <-time_step(input=vpd_raw,time.int=15,max.gap=60,decimals=6,df=F)
vpd <-vpd.input
save(vpd,file="D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX/R_package/TREX - Construction/vpd.rda")

sr<-read_excel("D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX/R_package/TREX - Construction/Environmental data.xlsx",sheet="sr")
sr<-data.frame(sr)
sr_raw   <-is.trex(sr,tz="GMT",time.format="(%m/%d/%y %H:%M:%S)",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
sr.input <-time_step(input=sr_raw,time.int=15,max.gap=60,decimals=6,df=F)
sr <-sr.input
save(sr,file="D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX/R_package/TREX - Construction/sr.rda")

