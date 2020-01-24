

# prepares tdm input data as internal
# tdm.input.txt<-read.table("./data-raw/tdm.input.txt",header=TRUE,sep="\t")
tdm.data <- read.table("./data-raw/tdm.input.txt",header=TRUE,sep="\t")
usethis::use_data(tdm.data, internal = FALSE)

