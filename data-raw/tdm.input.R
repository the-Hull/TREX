

# prepares tdm input data as internal
tdm.input.txt<-read.table("./data-raw/tdm.input.txt",header=TRUE,sep="\t")
usethis::use_data(tdm.input.txt,internal = TRUE)

