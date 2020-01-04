cal.data <- read.table("./data-raw/cal.data.txt",
                       sep = "\t",
                       header = TRUE,
                       stringsAsFactors = FALSE)
usethis::use_data(cal.data)
