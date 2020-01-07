## code to prepare `vpd` dataset goes here


vpd <-
    read.table(
        "./data-raw/Vapour_pressure_deficit.txt",
        header = TRUE,
        sep = "\t"
    )
vpd <- vpd[, c("Date", "N13")]
colnames(vpd) <- c("timestamp", "value")



usethis::use_data("vpd")
