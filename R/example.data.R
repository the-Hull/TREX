#example.data
#' Generate example TDM input data
#'
#' This returns a data.frame containing standard TDM (thermal dissipation method)
#'  measurements provided in two different formats. The data is obtain from \code{tdm.data}
#'  where \eqn{\Delta V} measurements are provide for i) Norway spruce (\emph{Picea abies} Karts.)
#'  growing in a valley in the Swiss Alps or ii) black spruce (\emph{Picea mariana} Mill. BSP)/eastern
#'  larch (\emph{Larix laricina} Du Roi K. Koch) growing at the southern limit of the boreal
#'  ecozone in central Canada.
#'
#' @param type Character string, indicating whether the example data should be
#' displayed with a timestamp (default = “timestamp”)
#' or whether a separate day of day of year and hour column
#' are provided (“doy”).
#'
#' @param species Character string, indicating whether the example data
#' should be displayed for \emph{P. abies} (default= “PCAB”) or \emph{P. mariana}
#' (“PCMA”)/L. laricina (“LALA”).
#'
#' @details This dataset can be applied for testing the functions provided in the package.
#'
#' @return A data.frame containing TDM measurements according to a specific type.
#' @export
#'
example.data <- function(type = "timestamp", species = "PCAB") {
  #t= test
  #type="timestamp"


  #d= default conditions
  if (missing(type)) {
    type = "timestamp"
  }
  if (missing(species)) {
    species = "PCAB"
  }

  #e= errors
  if (type != "timestamp" &
      type != "doy")
    stop(paste0("Unused argument, please use: timestamp|doy."))
  if (species != "PCAB" &
      species != "PCMA")
    stop(paste0("Unused argument, please use: PCAB|PCMA."))

  #p= processing
  #p1= data is loaded
  # proc.1 <- TREX:::tdm.input.txt
  proc.1 <- tdm.input.txt
    # read.table("tdm.input.txt", header = TRUE, sep = "\t") #p1 data is loaded {CHANGE}

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

# #loading example data
# setwd("D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX/R_package/TREX - Construction")
# input<-example.data(type="timestamp")
# head(input)
# str(input)







# #REMOVE: generate example data
# require("chron")
# require("zoo")
#
# left = function(string, char){
#   substr(string, 1,char)}
# right = function (string, char){
#   substr(string,nchar(string)-(char-1),nchar(string))
# }

# #generate input data
# tdm.data<-read.table("tdm.data.txt",header=TRUE,sep="\t")
# tdm.zoo<-zoo(tdm.data, order.by= as.chron(as.POSIXct(as.character(tdm.data[,1]),format="%d-%m-%Y %H:%M",tz="GMT")))
# plot(tdm.zoo)
# tdm.data<-data.frame(timestamp=as.character(tdm.data[,1]),year=left(right(as.character(tdm.data[,1]),10),4),
#                      doy=as.integer(strftime(as.Date(index(tdm.zoo)),format="%j")),
#                      hour=substr(as.character(tdm.data$Timestamp),nchar(as.character(tdm.data$Timestamp))-(5-1),nchar(as.character(tdm.data$Timestamp)))
#                      ,value=as.numeric(tdm.data[,2]))
# tdm.data$species<-"PCAB"
# write.table(tdm.data,"tdm.input.txt",col.names=TRUE,row.names=FALSE,sep="\t")
