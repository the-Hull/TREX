#' Sap flow measurements
#'
#'
#'
#' @description Returns an example thermal dissipation probe (TDM)
#'  datasets of either two types measurement formats (either “timestamp” or “doy” format).
#'   TDM \eqn{\Delta V} measurements are provide at a 15-minute resolution from 2012-2015 from
#'    a Norway spruce (\emph{Picea abies} (L.) Karts.) growing at 1300 m a.s.l.
#'    in the Swiss Alps (Loetschental, Switzerland; see Peters et al. 2019).
#'    The presented data is open for public use.
#'
#'
#' @usage tdm.data
#'
#'
#' @format Provides a data.frame with 11,6466  rows and two  columns.
#' \describe{
#'   \item{timestamp }{Date and time of the measurements (\code{character})}
#'   \item{year}{Year of measurements (\code{integer})}
#'   \item{doy}{Day of year (\code{integer})}
#'   \item{hour}{Hour of the measurements (\code{character})}
#'   \item{value}{\eqn{\Delta V}(or \eqn{\Delta T}) values obtained from TDM measurements  (\code{numeric})}
#'   \item{species}{Monitored species (\code{character})}
#' }
#'
#'
#' @name tdm.data
"tdm.data"




#' Calibration Measurements
#'
#'
#'
#' @description Returns raw calibration experiment data obtained from literature,
#'  with K values combined with gravimetrically determined sap flux density.
#'  The data.frame contains 22 studies with 37 different species. The data is used
#'  within the \code{\link{tdm_cal.sfd}} function to calculate sap flux density. The data
#'  originates from the manuscript by Flo \emph{et al.} 2019 and provides a
#'  description on the genus, species, calibration material, wood porosity and diameter
#'  of the stem. The presented data is open for public use.
#'
#'
#'
#' @usage cal.data
#'
#' @details Currently included studies are given below; individual labels (quoted) can be applied in \code{\link{tdm_cal.sfd}} (argument "study"):
#'
#' \itemize{
#'     \item{Braun and Schmid 1999}
#'     \item{Cabibel et al. 1991}
#'     \item{Cain 2009}
#'     \item{Chan 2015}
#'     \item{Fuchs et al. 2017}
#'     \item{Granier 1985}
#'     \item{Gutierrez & Santiago 2005}
#'     \item{Herbst et al. 2007}
#'     \item{Liu et al. 2008}
#'     \item{Lu 2002}
#'     \item{Lu and Chacko 1998}
#'     \item{Oliveira et al. 2006}
#'     \item{Paudel et al. 2013}
#'     \item{Rubilar et al. 2016}
#'     \item{Schmidt-walter et al. 2014}
#'     \item{Sperling et al. 2012}
#'     \item{Sugiura et al. 2009}
#'     \item{Sun et al. 2012}
#'     \item{Vellame et al. 2009}
#'     \item{Hubbard et al. 2010}
#'     \item{Peters et al. 2017}
#'     \item{Steppe et al. 2010}
#'
#' }
#'
#' @format Provides a data.frame with 4024 rows and 10 columns.
#' \describe{
#'   \item{Study}{Study from which the data originates (see Flo et al. 2019) (\code{character})}
#'   \item{Genus}{Monitored genus (\code{character})}
#'   \item{Species}{Monitored species (\code{character})}
#'   \item{Calibration.material}{Description on the calibration method that was used,
#'         including stem segment, whole plant and whole plant without roots (\code{character})}
#'   \item{Wood.porosity}{Wood structure type of the examined species, including coniferous, diffuse-porous, ring-porous and monocots (\code{character})}
#'   \item{Diameter}{Diameter at breast height of the calibration subject (in cm) (\code{numeric})}
#'   \item{k}{Proportional difference between \eqn{\Delta T} and \eqn{\Delta T_{max}}{\Delta Tmax} measured
#'          by the thermal dissipation probes (unitless; -) (\code{numeric})}
#'   \item{SFD}{Sap flux density measured gravimetrically (in \eqn{cm^3}{cm3} cm-2 h-1) (\code{numeric})}
#'   \item{Granier}{Sap flux density calculated according to Granier et al. 1985 using k
#'        (in \eqn{cm^{3}}{cm3} cm-2 h-1; using \eqn{43.84 k^{1.231}{k^1.231}}) (\code{numeric})}
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
#' @description Returns an example datasets of vapour pressure deficits monitoring from
#'  2012-2015 at 1300 m a.s.l. in the Swiss Alps (Loetschental, Switzerland; Peters et al. 2019).
#'  Sensors were installed at the site on a central tower (~2.5 m above the ground)
#'  within the canopy to measure air temperature and relative humidity (Onset, USA, U23-002Pro) with a 15‐min resolution.
#'   Vpd (\eqn{kPa} was calculated from the air temperature and relative humidity measurements according to WMO (2008).
#'
#' @format Provides an \code{\link{is.trex}}-compliant object with 135840 rows and 1 column.
#'
#' \describe{
#'   \item{index}{Date of the measurements in solar time (“yyyy-mm-dd”) (\code{character})}
#'   \item{value}{kPa values obtained from the site-specific monitoring (\code{numeric})}
#' }
#' @usage vpd
#'
#' @references
#' Peters RL, Speich M, Pappas C, Kahmen A, von Arx G, Graf Pannatier E, Steppe K, Treydte K, Stritih A, Fonti P. 2018.
#' Contrasting stomatal sensitivity to temperature and soil drought in mature alpine conifers.
#' Plant, Cell & Environment 42:1674-1689 <doi: 10.1111/pce.13500>
#'
#' WMO. 2008.Guide to meteorological instruments and methods of observation, appendix 4B, WMO‐No. 8 (CIMO Guide).
#'  Geneva, Switzerland: World Meteorological Organization.
#'
#' @name vpd
"vpd"


#' Solar radiation measurements (raw)
#'
#'
#'
#' @description Returns an example datasets of solar irradiance monitoring from 2012-2015 at 1300 m a.s.l. in the Swiss Alps
#'  (Loetschental, Switzerland; Peters et al. 2019). Solar irradiance in W m-2 was measured with
#'  15‐min resolution using a microstation (Onset,USA, H21-002 Micro Station)
#'  and pyranometer (Onset, USA, S‐LIB‐M003) positioned in an open field.
#'
#' @format Provides an \code{\link{is.trex}}-compliant object with 135840 rows and 1 column.
#'
#' \describe{
#'   \item{index}{Date of the measurements in solar time (“yyyy-mm-dd”) (\code{character})}
#'   \item{value}{W m-2 values obtained from the site-specific monitoring  (\code{numeric})}
#' }
#'
#'
#' @usage sr
#'
#' @references
#' Peters RL, Speich M, Pappas C, Kahmen A, von Arx G, Graf Pannatier E, Steppe K, Treydte K, Stritih A, Fonti P. 2018.
#' Contrasting stomatal sensitivity to temperature and soil drought in mature alpine conifers.
#' Plant, Cell & Environment 42:1674-1689 <doi: 10.1111/pce.13500>
#'
#'
#' @name sr
"sr"



#' Daily precipitation (raw)
#'
#'
#'
#' @description Returns an example dataset of daily precipitation data in mm d-1
#' from 2012-2015 originating from weather stations surrounding the Loetschental in the Swiss Alps.
#' The data was obtained from the nine nearest weather stations
#' (6‐to 43‐km distance to the site, including Adelboden, Blatten, Grächen, Montana, Jungfraujoch,
#' Sion, Ulrichen, Visp, and Zermatt; Federal Office of Meteorology and Climatology MeteoSwiss).
#'
#'
#' @usage preci
#'
#' @format Provides an \code{\link{is.trex}}-compliant object with 1415 rows and 1 column.
#'
#' \describe{
#'   \item{index}{Date of the measurements in solar time (“yyyy-mm-dd”) (\code{character})}
#'   \item{value}{Daily precipitation (mm d-1) from local weather stations (\code{numeric})}
#' }
#'
#' @references
#' Peters RL, Speich M, Pappas C, Kahmen A, von Arx G, Graf Pannatier E, Steppe K, Treydte K, Stritih A, Fonti P. 2018.
#' Contrasting stomatal sensitivity to temperature and soil drought in mature alpine conifers.
#' Plant, Cell & Environment 42:1674-1689 <doi: 10.1111/pce.13500>

#'
#' @name preci
"preci"
