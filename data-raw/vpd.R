# ## code to prepare `vpd` dataset goes here
#
#
# vpd <-
#     read.table(
#         "./data-raw/Vapour_pressure_deficit.txt",
#         header = TRUE,
#         sep = "\t"
#     )
# vpd <- vpd[, c("Date", "N13")]
# colnames(vpd) <- c("timestamp", "value")
#
#
#
# usethis::use_data("vpd")


vpd<-readxl::read_excel("./data-raw/Environmental data.xlsx",sheet="vpd")
vpd<-data.frame(vpd)
vpd_raw   <-TREX::is.trex(vpd,tz="GMT",time.format="(%m/%d/%y %H:%M:%S)",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
vpd.input <-TREX::dt.steps(input=vpd_raw,time.int=15,max.gap=60,decimals=6,df=F)
vpd <-vpd.input
usethis::use_data(vpd, internal = FALSE, overwrite = TRUE)
