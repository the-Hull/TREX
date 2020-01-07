## code to prepare `sr` dataset goes here
sr <-
    read.table(
        "./data-raw/Solar_radiance.txt",
        header = TRUE,
        sep = "\t"
    )
sr <- sr[, c("Timestamp", "N13")]
colnames(sr) <- c("timestamp", "value")

usethis::use_data("vpd")
