

#' Calibration Measurements
#'
#'
#'
#' @description Returns raw calibration experiment data obtained from literature,
#'  with K values combined with gravimetrically determined sap flux density.
#'  The data.frame contains 22 studies with 37 different species. The data is used
#'  within the \code{cal.sfd()} function to calculate sap flux density. The data
#'  originates from the manuscript by Flo \emph{et al.} 2019 and provides a
#'  description on the genus, species, calibration material, wood porosity and diameter
#'  of the stem. The presented data is open for public use.
#'
#'
#'
#' @usage cal.data
#'
#' @details Currently included studies are given below; individual labels (quoted) can be applied in \code{\link{cal.sfd}} (argument "study"):
#'
#' \itemize{
#' item{Braun and Schmid 1999}
#; item{Cabibel et al. 1991}
#; item{Cain 2009}
#; item{Chan 2015}
#; item{Fuchs et al. 2017}
#; item{Granier 1985}
#; item{Gutierrez & Santiago 2005}
#; item{Herbst et al. 2007}
#; item{Liu et al. 2008}
#; item{Lu 2002}
#; item{Lu and Chacko 1998}
#; item{Oliveira et al. 2006}
#; item{Paudel et al. 2013}
#; item{Rubilar et al. 2016}
#; item{Schmidt-walter et al. 2014}
#; item{Sperling et al. 2012}
#; item{Sugiura et al. 2009}
#; item{Sun et al. 2012}
#; item{Vellame et al. 2009}
#; item{Hubbard et al. 2010}
#; item{Peters et al. 2017}
#; item{Steppe et al. 2010}
#'
#' }
#'
#'
#' @references Flo V, Martinez-Vilalta J, Steppe K, Schuldt B, Poyatos, R. 2019.
#' A synthesis of bias and uncertainty in sap flow methods.
#' Agricultural and Forest Meteorology 271:362-374 \url{doi: 10.1016/j.agrformet.2019.03.012}
#'
#' Granier A. 1985. Une nouvelle methode pour la measure du flux de seve brute dans le tronc des arbres.
#' Annales des Sciences Forestieres 42:193–200 \url{doi: 10.1051/forest:19850204}
#'
#' @name cal.data
"cal.data"




#' Vapor pressure deficit measurements (raw)
#'
#'
#'
#' @description Returns raw vapor pressure deficit measurements in UNITS, collected at ORIGIN
#'
#'
#' @usage vpd
#'
#'
#' @name vpd
"vpd"


#' Solar radiation measurements (raw)
#'
#'
#'
#' @description Returns raw solar radiation measurements in UNITS, collected at ORIGIN
#'
#'
#' @usage sr
#'
#'
#' @name sr
"sr"

