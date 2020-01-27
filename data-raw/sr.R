## code to prepare `sr` dataset goes here
# sr <-
#     read.table(
#         "./data-raw/Solar_radiance.txt",
#         header = TRUE,
#         sep = "\t"
#     )
# sr <- sr[, c("Timestamp", "N13")]
# colnames(sr) <- c("timestamp", "value")
#
# usethis::use_data("sr")

# Note, that this requires function is.trex to be built/working prior prepping data
sr<-readxl::read_excel("./data-raw/Environmental data.xlsx",sheet="sr")
sr<-data.frame(sr)
sr_raw   <- TREX::is.trex(sr,tz="GMT",time.format="(%m/%d/%y %H:%M:%S)",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
sr.input <- TREX::dt.steps(input=sr_raw,time.int=15,max.gap=60,decimals=6,df=F)
sr <-sr.input
usethis::use_data(sr, overwrite = TRUE)

