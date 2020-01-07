preci <-
    read.table(
        "./data-raw/Precipitation.txt",
        header = TRUE,
        sep = "\t"
    )
colnames(preci) <- c("timestamp", "value")


usethis::use_data(preci)
