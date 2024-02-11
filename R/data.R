#' Sap flow measurements
#'
#'
#'
#' @description Returns an example thermal dissipation probe (TDM)
#'  dataset with time stamp (n = 1) and doy-columns (n = 3), a value and a species column.
#'   TDM \eqn{\Delta V} measurements are provided at a 15-minute resolution from 2012-2015 from
#'    a Norway spruce (\emph{Picea abies} (L.) Karts.) growing at 1300 m a.s.l.
#'    in the Swiss Alps (Loetschental, Switzerland; see Peters \emph{et al.} 2019).
#'    The presented data is open for public use.
#'
#'
#' @usage tdm.data
#'
#'
#' @format Provides a data.frame with 11,6466  rows and 6 columns.
#' \describe{
#'   \item{timestamp }{Date and time of the measurements (\code{character})}
#'   \item{year}{Year of measurements (\code{integer})}
#'   \item{doy}{Day of year (\code{integer})}
#'   \item{hour}{Hour of the measurements (\code{character})}
#'   \item{value}{\eqn{\Delta V} values obtained from TDM measurements (\code{numeric})}
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
#'  with \emph{K} values combined with gravimetrically determined sap flux density,
#'  as detailed in Flo \emph{et al.} (2019).
#'  The \code{data.frame} contains 22 studies with 37 different species. The data is used
#'  within the \code{\link{tdm_cal.sfd}} function to calculate sap flux density.
#'  Description on the genus, species, calibration material, wood porosity and diameter
#'  of the stem is provided in Flo \emph{et al.} (2019).
#'  The presented data is open for public use.
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
#'   \item{Method}{Heat-based sap flow measurement method (TD = Thermal Dissipation) (\code{character})}
#'   \item{Genus}{Monitored genus (\code{character})}
#'   \item{Species}{Monitored species (\code{character})}
#'   \item{Calibration.material}{Description on the calibration method that was used,
#'         including stem segment, whole plant and whole plant without roots (\code{character})}
#'   \item{Wood.porosity}{Wood structure type of the examined species, including coniferous, diffuse-porous, ring-porous and monocots (\code{character})}
#'   \item{Diameter}{Diameter at breast height of the calibration subject (in cm) (\code{numeric})}
#'   \item{k}{Proportional difference between \eqn{\Delta T} and \eqn{\Delta T_{max}}{\Delta Tmax} measured
#'          by the thermal dissipation probes (unitless; \code{numeric})}
#'   \item{SFD}{Sap flux density measured gravimetrically (in \eqn{cm^3 cm^{-2} h^{-1}}{cm3 cm-2 h-1}; \code{numeric})}
#'   \item{Granier}{Sap flux density calculated according to Granier et al. 1985 using k
#'        (in \eqn{cm^3 cm^{-2} h^{-1}}{cm3 cm-2 h-1}; using \eqn{43.84 k^{1.231}}{k^1.231}) (\code{numeric})}
#' }
#'
#'
#' @references Flo V, Martinez-Vilalta J, Steppe K, Schuldt B, Poyatos, R. 2019.
#' A synthesis of bias and uncertainty in sap flow methods.
#' Agricultural and Forest Meteorology 271:362-374. \doi{10.1016/j.agrformet.2019.03.012}
#'
#' Granier A. 1985. Une nouvelle methode pour la measure du flux de seve brute dans le tronc des arbres.
#' Annales des Sciences Forestieres 42:193–200. \doi{10.1051/forest:19850204}
#'
#' @name cal.data
"cal.data"




#' Vapor pressure deficit measurements (raw)
#'
#'
#'
#' @description Returns an example dataset of vapour pressure deficit (\eqn{VPD}) monitoring from
#'  2012-2015 at 1300 m a.s.l. in the Swiss Alps (Loetschental, Switzerland; Peters \emph{et al.} 2019).
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
#' Plant, Cell & Environment 42:1674-1689 \doi{10.1111/pce.13500}
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
#' @description Returns an example dataset of solar irradiance monitoring from 2012-2015 at 1300 m a.s.l. in the Swiss Alps
#'  (Loetschental, Switzerland; Peters et al. 2019). Solar irradiance (\eqn{W m^{-2}}{W m-2}) was measured with
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
#' Plant, Cell & Environment 42:1674-1689 \doi{10.1111/pce.13500}
#'
#'
#' @name sr
"sr"



#' Daily precipitation (raw)
#'
#'
#'
#' @description Returns an example dataset of daily precipitation data in \eqn{mm~d^{-1}}{mm d-1}
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
#'   \item{value}{Daily precipitation (\eqn{mm~d^{-1}}{mm d-1}) from local weather stations (\code{numeric})}
#' }
#'
#' @references
#' Peters RL, Speich M, Pappas C, Kahmen A, von Arx G, Graf Pannatier E, Steppe K, Treydte K, Stritih A, Fonti P. 2018.
#' Contrasting stomatal sensitivity to temperature and soil drought in mature alpine conifers.
#' Plant, Cell & Environment 42:1674-1689 \doi{10.1111/pce.13500}

#'
#' @name preci
"preci"








#' Sapwood area and depth, and bark allometric measurements
#'
#'
#'
#' @description Returns raw allometric data obtained for 14 spepcies from several North-American sites.
#'  These data are used to develop allometric equations for the parameters sapwood area and depth, as well as bark thickness.
#'  A detailed description on collection method and sites is provided in Pappas \emph{et al.} (2021; in prep.).
#'  The parameters were measured along an increment core extracted at breast height (DBH, 1.3 \eqn{m}) with a Haglof borer.
#'  Bark thickness (\code{Bark_cm}) was measured directly, while sapwood depth (\code{Sap_cm}) was estimated through discoloration and translucence.
#'  Sapwood area (\code{Saparea_cm2} was then calculated using:
#'  \deqn{Saparea_cm = \pi (DBH/2 - Bark_cm)^2  - \pi (DBH/2 - Bark_cm - Sap_cm)}
#'
#' @usage allometry.data
#'
#' @details Currently included species are:
#'
#' \itemize{
#'      \item {Abies balsamea}
#'      \item {Acer rubrum}
#'      \item {Acer saccharum}
#'      \item {Betula alleghaniensis}
#'      \item {Betula papyrifera}
#'      \item {Fagus grandifolia}
#'      \item {Fraxinus nigra}
#'      \item {Picea glauca}
#'      \item {Picea mariana}
#'      \item {Picea rubens}
#'      \item {Pinus banksiana}
#'      \item {Populus tremuloides}
#'      \item {Quercus rubra}
#'      \item {Thuja occidentalis}
#'
#' }
#'
#' @format Provides a data.frame with 651  rows and 8 columns.
#' \describe{
#'   \item{Plot}{Measurement plot (\code{character})}
#'   \item{treeID}{Unique tree ID (\code{character})}
#'   \item{Species}{Assessed species (\code{character})}
#'   \item{DBH_cm}{Diameter at breast height (1.3 \eqn{m}) in \eqn{cm} (\code{numeric})}
#'   \item{Bark_cm}{Bark thickness in \eqn{cm} (\code{numeric})}
#'   \item{Sap_cm}{Depth of sapwood in \eqn{cm} (\code{numeric})}
#'   \item{Saparea_cm2}{Sapwood area in \eqn{cm^2} (\code{numeric})}
#'   \item{Site}{Measurement site \code{character})}
#' }
#'
#'
#' @name allometry.data
"allometry.data"










#' Sapwood area and depth, and bark allometric models
#'
#'
#'
#' @description Returns allometric models for 14 species from several North-American sites.
#'  The models relate independent (i.e. outcomes) parameters sapwood area and depth, as well as bark thickness
#'  to diameter at breast height (DBH, 1.3 \eqn{m}).
#'  A detailed description on model development and assessment is provided in Pappas \emph{et al.} (2021; in prep.).
#'  The data used for these models is found in \code{\link{allometry.data}}.
#'
#' @usage allometry.models
#'
#' @details OLS models (y ~ x) are used for relationships between DBH (dependent) and sapwood / bark thickness;
#' A non-linear LS model (y ~ a * x ^ b ) is used for the relationship between DBH (dependent) and sapwood area.
#'
#' \itemize{
#'      \item {Abies balsamea}
#'      \item {Acer rubrum}
#'      \item {Acer saccharum}
#'      \item {Betula alleghaniensis}
#'      \item {Betula papyrifera}
#'      \item {Fagus grandifolia}
#'      \item {Fraxinus nigra}
#'      \item {Picea glauca}
#'      \item {Picea mariana}
#'      \item {Picea rubens}
#'      \item {Pinus banksiana}
#'      \item {Populus tremuloides}
#'      \item {Quercus rubra}
#'      \item {Thuja occidentalis}
#'
#' }
#'
#' @format Provides a list (one item for each allometric parameter)
#' with model data.frames/tibbles nested by species.
#' Relevant parameters can be extracted either by unnesting (\code{tidyr::unnest()}), or by
#' selecting a single nested item, e.g., \code{allometric.models$Saparea_cm2$fit[[1]]}.
#'
#' Each data.frame/tibble has four columns
#'
#' \describe{
#'   \item{Species}{Assessed species (\code{character})}
#'   \item{data}{data used to fit model;
#'   single-species subset of \code{\link{allometry.data}} (\code{list column})}
#'   \item{fit}{nls or lm model fit (\code{list column})}
#'   \item{model_parameters}{Convenient summary of models as produced by \code{broom::tidy()} (\code{list column})}
#' }
#'
#'
#' @name allometry.models
"allometry.models"
