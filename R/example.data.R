#example.data
#' Generate example TDM input data
#'
#' This function returns a \code{data.frame} containing standard TDM (thermal dissipation method)
#'  measurements provided in two different formats. The data is obtained from \code{tdm.data}
#'  where \eqn{\Delta V} measurements are given for Norway spruce (\emph{Picea abies} Karts.)
#'  growing in a valley in the Swiss Alps. See \code{\link{tdm.data}} for additional details.
#'
#' @usage example.data(type = "timestamp")
#'
#' @param type Character string, indicating whether the example data should be
#' displayed with a timestamp (default = \code{“timestamp”})
#' or separate year, day of day (\code{“doy”}).
#'
#'
#' @details This dataset can be applied for testing the functions provided in \code{TREX}.
#'
#' @return A \code{data.frame} containing TDM measurements according to a specific type.
#' @export
#'
#' @examples
#'
#' # get example data
#' input_data <- example.data(type = "timestamp")
#' input_data <- example.data(type = "doy")
#' head(input_data)
#'
example.data <- function(type = "timestamp") {
  #t= test
  #type="timestamp"


  #d= default conditions
  if (missing(type)) {
    type = "timestamp"
  }


  #e= errors
  if (type != "timestamp" &
      type != "doy")
    stop(paste0("Unused argument, please use: timestamp|doy."))
  # if (species != "PCAB" &
  #     species != "PCMA")
  #   stop(paste0("Unused argument, please use: PCAB|PCMA."))


  species <- "PCAB"

  #p= processing
  #p1= data is loaded
  # proc.1 <- TREX:::tdm.input.txt
  # read.table("tdm.input.txt", header = TRUE, sep = "\t") #p1 data is loaded {CHANGE}
  # proc.1 <- TREX::tdm.data
  proc.1 <- getExportedValue('TREX', 'tdm.data')

  #p2= isolating of data according to criteria
  if (as.character(type) == "timestamp") {
    proc.2 <-
      proc.1[which(as.character(proc.1$species) == as.character(species)), c(1, 5)]
    proc.2[, 1] <- as.character(proc.2[, 1])
    proc.2[, 2] <- as.numeric(proc.2[, 2])
  } else{
    proc.2 <-
      proc.1[which(as.character(proc.1$species) == as.character(species)), c(2, 3, 4, 5)]
    proc.2[, 1] <- as.integer(proc.2[, 1])
    proc.2[, 2] <- as.integer(proc.2[, 2])
    proc.2[, 3] <- as.character(proc.2[, 3])
    proc.2[, 4] <- as.numeric(proc.2[, 4])
  }

  #o= output
  return(proc.2)
}


