# preci <-
#     read.table(
#         "./data-raw/Precipitation.txt",
#         header = TRUE,
#         sep = "\t"
#     )
# colnames(preci) <- c("timestamp", "value")
#
#
# usethis::use_data(preci)
#


prec<-readxl::read_excel("./data-raw/Environmental data.xlsx",sheet="preci")
prec_raw<-data.frame(prec)
prec_raw[,2]<-round(as.numeric(prec_raw[,2]),6)
prec.in<-TREX::is.trex(prec_raw,time.format="%Y-%m-%d",tz="UTC")
zoo::index(prec.in)
preci<-prec.in
usethis::use_data(preci, overwrite = TRUE)
