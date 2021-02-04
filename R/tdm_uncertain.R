#' Uncertainty and sensitivity analysis
#'
#'
#' @description Quantifies the induced uncertainty on \eqn{SFD} and \eqn{K} time series due to the variability
#' in input parameters applied during TDM data processing. Moreover, it applies a global sensitivity
#'  analysis to quantify the impact of each individual parameter on three relevant outputs derived from \eqn{SFD} and \eqn{K}, namely:
#'   i) the mean daily sum of water use,
#'   ii) the variability of maximum daily \eqn{SFD} or \eqn{K} values,
#'   iii) and the duration of daily sap flow.
#'  This function provides both the uncertainty and sensitivity indices, as time-series of \eqn{SFD} and \eqn{K} with the mean,
#'  standard deviation (\eqn{sd}) and confidence interval (CI) due to parameter uncertainty.
#'  \strong{Users should ensure that no gaps are present within the input data and environmental time series}.
#'
#' @param input An \code{\link{is.trex}}-compliant object (\code{zoo} object
#'
#' @param vpd.input An \code{\link{is.trex}}-compliant object (\code{zoo} object,
#'   \code{data.frame}) containing a timestamp and a vapour pressure deficit
#'   (\eqn{VPD}; in \eqn{kPa}) column with the same temporal extent and time steps as the \code{input} object.
#'   This input is required when using the environmental dependent (\code{"ed"}) method.
#'
#' @param sr.input An \code{\link{is.trex}}-compliant object (\code{zoo} object,
#'   \code{data.frame}) a timestamp and a solar radiation data (sr; e.g., global radiation or PAR)
#'   column with the same temporal extent and time steps as the \code{input} object.
#'   This input is required when using the environmental dependent (\code{"ed"}) method.
#'
#' @param method Character, specifies the \eqn{\Delta T_{max}}{\Delta Tmax} method on which the
#'   sensitivity and uncertainty analysis are to be performed on (see \code{\link{tdm_dt.max}}).
#'   Only one method can be selected, including the pre-dawn (\code{"pd"}), moving window (\code{"mw"}),
#'   double regression (\code{"dr"}) or the environmental dependent (\code{"ed"}) method (default = \code{"pd"}).
#'
#' @param n Numeric, specifies the number of times the bootstrap resampling procedure is repeated (default = 2000).
#'   Keep in mind that high values increase processing time.
#'
#' @param zero.end Numeric, defines the end of the predawn period.
#'  Values should be in minutes (e.g., predawn conditions until 8:00 = 8*60; default = 8*60).
#'
#' @param range.end Numeric, defines the number of time steps for \code{zero.end} (the minimum time step of the input)
#'   for which an integer sampling range will be defined (default = 16, assuming a 15-min resolution or a 2 hour range around \code{zero.end}).
#'
#' @param zero.start  Numeric, defines the start of the predawn period.
#'  Values should be in minutes (e.g., predawn conditions from 1:00 = 1*60; default = 1*60).
#'
#' @param range.start Numeric, defines the number of time steps for \code{zero.start} (the minimum time step of the input)
#'   for which an integer sampling range will be defined (default = 16, assuming a 15-min resolution or a 2 hour range around \code{zero.start}).
#'
#' @param probe.length Numeric, the length of the TDM probes in mm (see \code{\link{tdm_hw.cor}}; default = 20 mm).
#'
#' @param sw.cor Numeric, the sapwood thickness in mm. Default conditions assume the sapwood thickness is equal to a standard probe length (default = 20).
#'
#' @param sw.sd Numeric, the standard deviation for sampling sapwood thickness sampling from a normal distribution (default = 16 mm;
#'  defined with a European database on sapwood thickness measurements).
#'
#' @param log.a_mu Numeric, value providing the natural logarithm of the calibration parameter \eqn{a} (see \code{\link{tdm_cal.sfd}}; \eqn{SFD = aK^b}).
#'   This value can be obtained from \code{\link{tdm_cal.sfd}} (see \code{out.param}).
#'   Default conditions are determined by using all calibration data as described in \code{\link{cal.data}} (default = 4.085).
#'
#' @param log.a_sd Numeric, the standard deviation of the \eqn{a} parameter (see \code{log.a_mu}) used within the calibration curve
#'   for calculating \eqn{SFD} (default = 0.628).
#'
#' @param b_mu Numeric, the value of the calibration parameter \eqn{b} (see \code{\link{tdm_cal.sfd}}; \eqn{SFD = aK^b}).
#'   This value can be obtained from \code{\link{tdm_cal.sfd}} (see \code{out.param}).
#'   Default conditions are determined by using all calibration data as described in \code{\link{cal.data}} (default = 1.275).
#'
#' @param b_sd Numeric, the standard deviation of the \eqn{b} parameter (see \code{log.a_mu}) used within the calibration curve
#'   for calculating \eqn{SFD} (default = 0.262).
#'
#' @param max.days_min Numeric, the minimum value for an integer sampling range of \code{max.days}
#'  (see \code{\link{tdm_dt.max}} for the \code{"mw"} and \code{"dr"} \eqn{\Delta T_{max}}{\Delta Tmax} method).
#'   As the \code{"mw"} and \code{"dr"} method apply a rolling maximum or mean, the provided value should be an
#'   uneven number (see \code{\link{tdm_dt.max}}; default = 15; required for the \code{"mw"} and \code{"dr"} \eqn{\Delta T_{max}}{\Delta Tmax} method).
#'
#' @param max.days_max Numeric, the maximum value for an integer sampling range of \code{max.days}
#'  (see \code{\link{tdm_dt.max}} for the \code{"mw"} and \code{"dr"} \eqn{\Delta T_{max}}{\Delta Tmax} method).
#'   As the \code{"mw"} and \code{"dr"} method apply a rolling maximum or mean, the provided value should be an
#'   uneven number (see \code{\link{tdm_dt.max}}; default = 5; required for the \code{"mw"} and \code{"dr"} \eqn{\Delta T_{max}}{\Delta Tmax} method).
#'
#' @param ed.window_min Numeric, the minimum number of time steps for the \code{ed.window parameter} (see \code{\link{tdm_dt.max}}; the minimum time step of the input)
#'  for which an integer sampling range will be defined (default = 8, assuming a 15-min resolution or a 2 hour range; required for the \code{"ed"} \eqn{\Delta T_{max}}{\Delta Tmax} method).
#'
#' @param ed.window_max Numeric, the maximum number of time steps for the \code{ed.window} sampling range
#'  (default = 16, assuming a 15-min resolution or a 4 hour range; required for the \code{"ed"} \eqn{\Delta T_{max}}{\Delta Tmax} method).
#'
#' @param criteria.vpd_min Numeric, value in \eqn{kPa} defining the minimum for the fixed sampling range to define the vapour pressure deficit (VPD)
#'  threshold to establish zero-flow conditions (default = 0.05 \eqn{kPa}; see \code{\link{tdm_dt.max}}; required for the \code{"ed"} \eqn{\Delta T_{max}}{\Delta Tmax} method).
#'
#' @param criteria.vpd_max Numeric, value in \eqn{kPa} defining the maximum for the fixed sampling range to define the VPD threshold to establish
#'  zero-flow conditions (default = 0.5 \eqn{kPa}; required for the \code{"ed"}  \eqn{\Delta T_{max}}{\Delta Tmax} method).
#'
#' @param criteria.sr_mean Numeric value defining the mean \code{sr.input} value around which the fixed sampling
#'  range for the solar irradiance threshold should be established for defining zero-flow conditions
#'  (see \code{\link{tdm_dt.max}}; default = 30 W m-2; required for the \code{"ed"} \eqn{\Delta T_{max}}{\Delta Tmax} method).
#'
#' @param criteria.sr_range Numeric, the range (in \%) around \code{criteria.sr_mean} for establishing the solar irradiance threshold
#'  (see \code{\link{tdm_dt.max}}; default = 30\%; required for the \code{"ed"} \eqn{\Delta T_{max}}{\Delta Tmax} method).
#'
#' @param criteria.cv_min Numeric, value (in \%) defining the minimum value for the fixed sampling range to
#'   determine the coefficient of variation (CV) threshold for establishing zero-flow conditions
#'   (default = 0.5\%; see \code{\link{tdm_dt.max}}; required for the \code{"ed"} \eqn{\Delta T_{max}}{\Delta Tmax} method).
#'
#' @param criteria.cv_max Numeric, value (in \%) defining the maximum value for the fixed sampling range
#'  to determine the coefficient of variation (CV) threshold for establishing zero-flow conditions
#'  (default = 1\%; see \code{\link{tdm_dt.max}}; required for the \code{"ed"} \eqn{\Delta T_{max}}{\Delta Tmax} method).
#'
#' @param min.sfd Numeric, defines at which \eqn{SFD} (\eqn{cm^3 cm^{-2} h^{-1}}{cm3 cm-2 h-1}) zero-flow conditions are expected.
#'  This parameter is used to define the duration of daily sap flow based on \eqn{SFD} (default = \eqn{0.5 cm^3 cm^{-2} h^{-1}}{0.5 cm3 cm-2 h-1}).
#'
#' @param min.k Numeric value defining at which \eqn{K} (dimensionless, -) zero-flow are expected.
#'  This parameter is used to define the duration of daily sap flow based on \eqn{K} (default = 0).
#'
#' @param make.plot Logical; If \code{TRUE}, a plot is generated presenting the sensitivity and uncertainty analyses output (default = \code{TRUE}).
#' @param df Logical; If \code{TRUE}, output is provided in a \code{data.frame} format with a timestamp and a value column.
#' If \code{FALSE}, output is provided as a zoo vector object (default = \code{FALSE}).
#'
#'
#' @details Uncertainty and sensitivity analysis can be performed on TDM \eqn{\Delta T} (or \eqn{\Delta V}) measurements.
#'  The function applies a Monte Carlo simulation approach (repetition defined by \code{n})
#'  to determine the variability in relevant output variables (defined as uncertainty)
#'  and quantifies the contribution of each parameter to this uncertainty (defined as sensitivity).
#'  To generate variability in the selected input parameters a Latin Hypercube Sampling is performed with a default
#'  or user defined range of parameter values per \eqn{\Delta T_{max}}{\Delta Tmax} method (see \code{\link{tdm_dt.max}()}).
#'  The sampling algorithm generates multiple sampling distributions, including an integer sampling range (for \code{zero.start},
#'  \code{zero.end}, \code{max.days}, and \code{ed.window}), a continuous sampling range (criteria for \code{sr}, \code{vpd} and \code{cv}),
#'  and a normal distribution (for \code{sw.cor} and calibration parameters \code{a} and \code{b}).
#'  Within this algorithm no within-day interpolations are made between the \eqn{\Delta T_{max}}{\Delta Tmax} points
#'  (see \code{\link{tdm_dt.max}}, \code{interpolate = FALSE}). This approach ensures near-random sampling across different
#'  types of sampling distributions, while avoiding the need for increasing the number of replicates
#'  (which increases computation time). For the application of this approach one needs to;
#'  i) select the output of interest,
#'  ii) identify the relevant input parameters, and
#'  iii) determine the parameter range and distribution.
#'  For a given time-series three output variables are considered, calculated as the mean over the entire time-series,
#'  to be relevant, namely;
#'  i) mean daily sum of water use (or Sum, expressed in \eqn{cm^3 cm^{-2} d^{-1}}{cm3 cm-2 d-1} for \eqn{SFD} and unitless for \eqn{K}),
#'  ii) the variability of maximum \eqn{SFD} or \eqn{K} values (or CV, expressed as the coefficient of variation in \%
#'  as this alters climate response correlations), and
#'  iii) the duration of daily sap flow based on \eqn{SFD} or \eqn{K} (or Duration, expressed in hours per day dependent on a threshold,
#'  see \code{min.sfd} and \code{min.k}).
#'  A minimum threshold to define zero-flow \eqn{SFD} or \eqn{K} is required for the duration calculation
#'  as small variations in night-time \eqn{SFD} or \eqn{K} are present. All data-processing steps
#'  (starting with \code{"tdm_"}) are incorporated within the function, excluding \code{\link{tdm_damp}()}
#'  due to the need for detailed visual inspection and significantly longer computation time.
#'
#'  For the sensitivity analysis the total overall sensitivity indices are determined according strategy originally proposed by
#'  Sobol' (1993), considering the improvements applied within the {sensitivity} R package.
#'  The method proposed by Sobol' (1993) is a variance-based sensitivity analysis,
#'  where sensitivity indices (dimensionless from 0 to 1) indicate the partial variance contribution
#'  by a given parameter over the total output variance (e.g., Pappas \emph{et al.} 2013).
#'  This global sensitivity analysis facilitates the identification of key parameters for data-processing
#'  improvement and highlights methodological limitations. Users should keep in mind that parameter ranges represent
#'  a very critical component of any sensitivity analysis and should be critically assessed and clearly reported
#'  for each case and analytical purpose. Moreover, it is advised to run this function on one growing season of input data to reduce processing time.
#'
#'
#' @return A named \code{list} of \code{zoo} or \code{data.frame} objects in the appropriate format for other functionalities.
#'  Items include:
#'
#'  \describe{
#'
#'    \item{output.data}{data.frame containing uncertainty and sensitivity indices for \eqn{SFD} and K and the included parameters.
#'         This includes the mean uncertainty/sensitivity [,"mean"], standard deviation [,"sd"], upper [,"ci.min"] and lower [,"ci.max"]
#'         95\% confidence interval.}
#'
#'    \item{output.sfd}{zoo object or data.frame with the \eqn{SFD} time series obtained from the bootstrap resampling.
#'               This includes the mean uncertainty/sensitivity [,"mean"], standard deviation [,"sd"], upper [,"CIup"] and lower [,"CIlo"]
#'                95\% confidence interval.}
#'
#'    \item{output.k}{zoo object or data.frame with the K time series obtained from the bootstrap resampling.
#'    This includes the mean uncertainty/sensitivity [,"mean"], standard deviation [,"sd"], upper [,"ci.max"]
#'    and lower [,"ci.min"] 95\% confidence interval. }
#'
#'
#'    \item{param}{a data.frame with an overview of selected parameters used within \code{\link{tdm_uncertain}()} function. }
#'
#'
#'  }
#'
#' @references
#'
#'  Sobol' I. 1993. Sensitivity analysis for nonlinear mathematical models.
#'  Math. Model Comput. Exp. 1:407-414
#'
#'  Pappas C, Fatichi S, Leuzinger S, Wolf A, Burlando P. 2013.
#'  Sensitivity analysis of a process-based ecosystem model: Pinpointing parameterization and structural issues.
#'   Journal of Geophysical Research 118:505-528 \doi{10.1002/jgrg.20035}
#'
#' @examples \dontrun{
#' #perform an uncertainty and sensitivity analysis on "dr" data processing
#' raw   <- example.data(type="doy")
#' input <- is.trex(raw, tz="GMT", time.format="%H:%M",
#'            solar.time=TRUE, long.deg=7.7459, ref.add=FALSE, df=FALSE)
#' input<-dt.steps(input,time.int=15,start="2013-04-01 00:00",
#'              end="2013-11-01 00:00",max.gap=180,decimals=15)
#' output<- tdm_uncertain(input, probe.length=20, method="pd",
#'                n=2000,sw.cor=32.28,sw.sd=16,log.a_mu=3.792436,
#'                log.a_sd=0.4448937,b_mu=1.177099,b_sd=0.3083603,
#'                make.plot=TRUE)
#' }
#'
#' @import foreach
#' @import stats
#'
#' @export
tdm_uncertain<-function(input, vpd.input, sr.input, method = "pd",
                        n = 2000, zero.end = 8*60, range.end = 16, zero.start = 1*60,
                        range.start = 16, probe.length = 20, sw.cor = 32.28, sw.sd = 16,
                        log.a_mu = 4.085, log.a_sd = 0.628, b_mu = 1.275, b_sd = 0.262,
                        max.days_min = 1, max.days_max = 7, ed.window_min = 8, ed.window_max = 16,
                        criteria.vpd_min = 0.05, criteria.vpd_max = 0.5, criteria.sr_mean = 30,
                        criteria.sr_range = 30, criteria.cv_min = 0.5, criteria.cv_max = 1,  min.sfd = 0.5 ,
                        min.k = 0 , make.plot = TRUE, df = FALSE){
  #t= test
  #  blabla<-0
  #if(blabla==1){
  #  raw   <- example.data(type="doy")
  #input <- is.trex(raw,tz="GMT",time.format="%H:%M",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE,df=FALSE)
  #cleaning
  #window(input,start=as.POSIXct(as.character("(01/01/12 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"),
  #     end=as.POSIXct(as.character("(01/01/13 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"))[which(
  #       window(input,start=as.POSIXct(as.character("(01/01/12 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"),
  #              end=as.POSIXct(as.character("(01/01/13 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"))<0.5)]<-NA
  #window(input,start=as.POSIXct(as.character("(01/01/13 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"),
  #     end=as.POSIXct(as.character("(01/01/16 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"))[which(
  #       window(input,start=as.POSIXct(as.character("(01/01/13 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"),
  #              end=as.POSIXct(as.character("(01/01/16 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"))<0.64|
  #         window(input,start=as.POSIXct(as.character("(01/01/13 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"),
  #                end=as.POSIXct(as.character("(01/01/16 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"))>0.81)]<-NA
  #input<-dt.steps(input,time.int=15,start="2013-04-01 00:00",end="2013-11-01 00:00",max.gap=180,decimals=15)
  #zero.end<-8*60
  #zero.start<-1*60
  #range.end<- 16# number of timesteps
  #range.start<- 16 # number of timesteps
  #probe.length<-20
  #sw.cor<-32.28
  #sw.sd<-16
  #log.a_mu<-3.792436
  #log.a_sd<-0.4448937
  #b_mu<-1.177099
  #b_sd<-0.3083603
  #method<-c("pd")
  #min.sfd<-0.05
  #min.k<-0
  #n=100
  #make.plot=TRUE

  #d= default
  if(missing(method)){method="pd"}
  if(missing(make.plot)){make.plot=F}
  if(missing(df)){df=F}
  if(missing(n)){n=2000}

  #p= process
  if(attributes(input)$class=="data.frame"){
    #e
    if(is.numeric(input$value)==F)stop("Invalid input data, values within the data.frame are not numeric.")
    if(is.character(input$timestamp)==F)stop("Invalid input data, timestamp within the data.frame are not numeric.")

    #p
    input<-zoo::zoo(input$value,order.by=base::as.POSIXct(input$timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC"))

    #e
    if(as.character(zoo::index(input)[1])=="(NA NA)"|is.na(zoo::index(input)[1])==T)stop("No timestamp present, time.format is likely incorrect.")
  }

  #e= error
  if(zoo::is.zoo(input)==F)stop("Invalid input data, use a zoo file from is.trex or a zoo vector containing numeric values (tz= UTC).")
  if(is.numeric(input)==F)stop("Invalid input data, values within the vector are not numeric.")
  if(make.plot!=T&make.plot!=F)stop("Unused argument, make.plot needs to be TRUE|FALSE.")
  if(method%in%c("pd","mw","dr","ed")==F)stop("Unused argument, method has to be a character object of either pd, mw, dr or ed.")
  if(length(method)!=1)stop("Unused argument, method can only contain one character object.")
  if(is.numeric(n)==F)stop("Unused argument, n is not numeric.")

  #w= warnings
  if(difftime(zoo::index(input[length(input)]),zoo::index(input[1]),units=c("days"))<30){
    warning("Selected input has a temporal extend of <30 days.")
  }
  if(n>2000)warning("Selected n > 2000 which can significantly reduce processing speed.")

  #f= small functions
  left = function(string, char){substr(string, 1,char)}
  right = function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}

  #convert input to a tibble
  minutes<-as.numeric(left(right(as.character(zoo::index(input)),8),2))*60+as.numeric(left(right(as.character(zoo::index(input)),5),2))
  days<-as.numeric(floor(difftime(zoo::index(input),as.Date(zoo::index(input)[1]),units="days"))+1)

  #w= warnings
  if(max(days)>365)warning("Input length > 365 days which can significantly reduce processing speed.")

  raw.input <- tibble::tibble(
    days = days,
    days.agg =days,
    minutes = minutes,
    value = as.numeric(as.character(input)),
    dt.max = as.numeric(as.character(input)),
    count = 1,
    gap = 1
  )

  #e
  if(nrow(raw.input)==0)stop("Invalid input object, no values are provided within the object.")

  length.input <- stats::na.omit(tibble::tibble(days = days,value = as.numeric(as.character(input))))
  raw.input$count<-dplyr::full_join(raw.input,stats::aggregate(length.input, list(length.input$days), FUN=length),by=c("days"="Group.1"))$value.y

  #d
  if(missing(zero.start)){zero.start<-1*60}
  if(missing(zero.end)){zero.end<-8*60}
  if(missing(range.start)){range.start<-round((60*4)/stats::median(diff(raw.input$minutes)),0)}
  if(missing(range.end)){range.end<-round((60*4)/stats::median(diff(raw.input$minutes)),0)}
  if(missing(sw.cor)){sw.cor<-20}
  if(missing(sw.sd)){sw.cor<-0}
  if(missing(log.a_mu)){log.a_mu<-4.085335}
  if(missing(log.a_sd)){log.a_sd<-0.627469}
  if(missing(b_mu)){b_mu<-1.274558}
  if(missing(b_sd)){b_sd<-0.26184}
  if(missing(min.sfd)){min.sfd<-0.05}
  if(missing(min.k)){min.k<-0}

  #e
  if(is.numeric(zero.start)==F)stop("Unused argument, zero.start is not numeric.")
  if(is.numeric(zero.end)==F)stop("Unused argument, zero.end is not numeric.")
  if(is.numeric(range.start)==F)stop("Unused argument, range.start is not numeric.")
  if(is.numeric(range.end)==F)stop("Unused argument, range.end is not numeric.")
  if(is.numeric(sw.cor)==F)stop("Unused argument, sw.cor is not numeric.")
  if(is.numeric(sw.sd)==F)stop("Unused argument, sw.sd is not numeric.")
  if(is.numeric(log.a_mu)==F)stop("Unused argument, log.a_mu is not numeric.")
  if(is.numeric(log.a_sd)==F)stop("Unused argument, log.a_sd is not numeric.")
  if(is.numeric(b_mu)==F)stop("Unused argument, b_mu is not numeric.")
  if(is.numeric(b_sd)==F)stop("Unused argument, b_sd is not numeric.")
  if(is.numeric(min.sfd)==F)stop("Unused argument, min.sfd is not numeric.")
  if(is.numeric(min.k)==F)stop("Unused argument, min.k is not numeric.")
  if(min.sfd<0)stop("Unused argument, min.sfd < 0.")
  if(min.k<0)stop("Unused argument, min.k < 0.")

  #pd----
  if(method=="pd"){
    #p= processing
    A <- lhs::randomLHS(n,5)
    B <- matrix(nrow = nrow(A), ncol = ncol(A))
    B[,1] <- ceiling(stats::qunif(A[,1], min =-(range.end-1)/2, max = range.end/2))
    B[,2] <- ceiling(stats::qunif(A[,2], min =-(range.start-1)/2, max = range.start/2))
    B[,3] <- msm::qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
    B[,4] <- stats::qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
    B[,5] <- stats::qnorm(A[,5], mean = b_mu, sd= b_sd)
    colnames(B)<-c("zero.end","zero.start","sw.cor","a","b")
    X1<-data.frame(B)

    A <- lhs::randomLHS(n,5)
    B <- matrix(nrow = nrow(A), ncol = ncol(A))
    B[,1] <- ceiling(stats::qunif(A[,1], min =-(range.end-1)/2, max = range.end/2))
    B[,2] <- ceiling(stats::qunif(A[,2], min =-(range.start-1)/2, max = range.start/2))
    B[,3] <- msm::qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
    B[,4] <- stats::qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
    B[,5] <- stats::qnorm(A[,5], mean = b_mu, sd= b_sd)
    colnames(B)<-c("zero.end","zero.start","sw.cor","a","b")
    X2<-data.frame(B)

    x <- sensitivity::soboljansen(model = NULL , X1, X2, nboot = n)
    B<-x$X
    n<-nrow(B)
    #p= process
    doParallel::registerDoParallel(cores = parallel::detectCores() - 2)
    cl<-parallel::makeCluster(parallel::detectCores() - 2)
    doSNOW::registerDoSNOW(cl)
    pb <- utils::txtProgressBar(max = nrow(B), style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    output<-foreach(i=c(1:n),.combine="rbind",.packages=c("tibble","dplyr","zoo"),.options.snow = opts)%dopar%{
      utils::setTxtProgressBar(pb, i)
      ze<-zero.end+(B[i,1]*stats::median(diff(minutes)))
      if(ze<0){ze<-24*60-ze}
      if(ze>24*60){ze<-ze-24*60}
      zs<-zero.start+(B[i,2]*stats::median(diff(minutes)))
      if(zs<0){zs<-24*60-zs}
      if(zs>24*60){zs<-zs-24*60}

      #dt max
      proc.1<-raw.input
      if(ze>zs){proc.1[which(proc.1$minutes>ze|minutes<zs),"dt.max"]<-NA}
      if(ze<zs){proc.1[which(proc.1$minutes>ze&minutes<zs),"dt.max"]<-NA
      offset<-(60*24/stats::median(diff(proc.1$minutes)))-ceiling(zs/stats::median(diff(proc.1$minutes)))+1
      proc.1$days.agg<-c(proc.1$days[(offset:length(proc.1$days))],rep(max(proc.1$days),offset-1))}
      #View(proc.1)
      if(ze==zs)stop(paste("Unused argument, zero.start and zero.end are too close together.",sep=""))
      add<-tibble::tibble(
        days.add = stats::aggregate(stats::na.omit(proc.1)$dt.max,by=list(stats::na.omit(proc.1)$days.agg),max,na.rm=TRUE)[,1],
        ddt.max=stats::aggregate(stats::na.omit(proc.1)$dt.max,by=list(stats::na.omit(proc.1)$days.agg),max,na.rm=TRUE)[,2]
      )
      proc.2<-dplyr::full_join(proc.1,add,by=c("days.agg"="days.add"))
      proc.2[which(proc.2$ddt.max!=proc.2$dt.max|is.na(proc.2$dt.max)==TRUE),"ddt.max"]<-NA
      proc.2[which(is.na(proc.2$ddt.max)==FALSE),"gap"]<-NA
      proc.2$gap<-stats::ave(proc.2$gap, rev(cumsum(rev(is.na(proc.2$gap)))), FUN=cumsum)*stats::median(diff(proc.1$minutes))
      proc.2$ddt.max<-zoo::na.locf(zoo::na.locf(proc.2$ddt.max,na.rm=F),fromLast=TRUE)
      proc.2[which(is.na(proc.2$value)==TRUE|proc.2$gap>(60*(24+12))|proc.2$count!=stats::median(proc.2$count,na.rm=TRUE)),"ddt.max"]<-NA

      #HW correction
      #i<-4
      swt<-B[i,3]
      if(probe.length<=swt){
        proc.2$k.pd                 <-((proc.2$ddt.max-proc.2$value)/proc.2$value)
        proc.2$k.pd[which(proc.2$k.pd<0)]<-0
      }
      if(probe.length>swt){
        proc.2$k.pd<-((proc.2$ddt.max-((proc.2$value-(1-(swt/probe.length))*proc.2$ddt.max)/(swt/probe.length)))/((proc.2$value-(1-(swt/probe.length))*proc.2$ddt.max)/(swt/probe.length)))
        proc.2$k.pd[which(proc.2$k.pd<0)]<-0
      }

      #still need to consider whether we include dampening
      a<-B[i,4] #48.25036#
      b<- B[i,5] #1.177099#
      proc.2$sfd<-a*proc.2$k.pd^b

      #think about relevant output proxies
      d.sum.k<-suppressWarnings(stats::aggregate(proc.2$k.pd,by=list(proc.2$days),max,na.rm=TRUE))
      d.sum.k[which(d.sum.k[,2]=="-Inf"),2]<-NA
      durat.k<-proc.2
      durat.k[which(proc.2$k.pd<=min.k),"k.pd"]<-NA
      durat.k<-stats::aggregate(durat.k$k.pd, by=list(durat.k$days), FUN=function(x) {length(stats::na.omit(x))})
      values.k<-stats::aggregate(proc.2$k.pd,by=list(proc.2$days),mean,na.rm=TRUE)*24
      values.k[values.k[,2]=="NaN",2]<-NA

      durat<-proc.2
      durat[which(proc.2$sfd<=min.sfd),"sfd"]<-NA
      durat<-stats::aggregate(durat$sfd, by=list(durat$days), FUN=function(x) {length(stats::na.omit(x))})
      values<-stats::aggregate(proc.2$sfd,by=list(proc.2$days),mean,na.rm=TRUE)*24
      values[values[,2]=="NaN",2]<-NA
      d.sum<-suppressWarnings(stats::aggregate(proc.2$sfd,by=list(proc.2$days),max,na.rm=TRUE))
      d.sum[which(d.sum[,2]=="-Inf"),2]<-NA
      return(
        c(c(i,mean(values[,2],na.rm=TRUE),(stats::sd(d.sum[,2],na.rm=TRUE)/mean(d.sum[,2],na.rm=TRUE))*100,(mean(durat[,2]*stats::median(diff(proc.2$minutes)))/60),
            mean(values.k[,2],na.rm=TRUE),(stats::sd(d.sum.k[,2],na.rm=TRUE)/mean(d.sum.k[,2],na.rm=TRUE))*100,(mean(durat.k[,2]*stats::median(diff(proc.2$minutes)))/60)),
          c(length(proc.2$sfd),proc.2$sfd,proc.2$k.pd))
      )
    }
    doParallel::stopImplicitCluster()

    #time series output
    number<-stats::median(output[,8])
    output.sfd<-output[,c(9:(8+number))]
    output.k<-output[,c((9+number):(8+number+number))]
    n<-nrow(output.k)
    output.sfd<-data.frame(mu=apply(output.sfd,2,mean,na.rm=T),sd=apply(output.sfd,2,sd,na.rm=T),ci.max=apply(output.sfd,2,stats::quantile,probs=c(0.975),na.rm=T),ci.min=apply(output.sfd,2,stats::quantile,probs=c(0.025),na.rm=T))
    output.k<-data.frame(mu=apply(output.k,2,mean,na.rm=T),sd=apply(output.k,2,sd,na.rm=T),ci.max=apply(output.k,2,stats::quantile,probs=c(0.975),na.rm=T),ci.min=apply(output.k,2,stats::quantile,probs=c(0.025),na.rm=T))
    output.sfd<-zoo::zoo(output.sfd,order.by=zoo::index(input))
    output.k<-zoo::zoo(output.k,order.by=zoo::index(input))

    if(df==T){
      output.sfd<-zoo::fortify.zoo(output.sfd)
      output.k<-zoo::fortify.zoo(output.k)
      colnames(output.sfd)<-c("timestamp","mu","sd","ci.max","ci.min")
      colnames(output.k)<-c("timestamp","mu","sd","ci.max","ci.min")
    }

    #o= output
    output<-output[,c(1:7)]
    output_sum<-sensitivity::tell(x,as.numeric(output[,2]))
    output_cv<-sensitivity::tell(x,output[,3])
    output_length<-sensitivity::tell(x,output[,4])

    output_sum.k<-sensitivity::tell(x,output[,5])
    output_cv.k<-sensitivity::tell(x,output[,6])
    output_length.k<-sensitivity::tell(x,output[,7])

    output.tot<-data.frame(item=c(row.names(output_sum$T),"daily.sum",row.names(output_cv$T),"max.cv",row.names(output_length$T),"length.dur"),
                           class=c(rep("param.sum",length(row.names(output_sum$T))),"stat.sum",rep("param.cv",length(row.names(output_cv$T))),"stat.cv",rep("param.length",length(row.names(output_length$T))),"stat.length"),
                           factor="SFD",
                           mean=c(output_sum$T[,1],mean(output[,2]),output_cv$T[,1],mean(output[,3]),output_length$T[,1],mean(output[,4])),
                           sd=c(output_sum$T[,3],stats::sd(output[,2]),output_cv$T[,3],stats::sd(output[,3]),output_length$T[,3],stats::sd(output[,4])),
                           ci.min=c(output_sum$T[,4],stats::quantile(output[,2],probs=0.025,na.rm=T),output_cv$T[,4],stats::quantile(output[,3],probs=0.025,na.rm=T),output_length$T[,4],stats::quantile(output[,4],probs=0.025,na.rm=T)),
                           ci.max=c(output_sum$T[,5],stats::quantile(output[,2],probs=0.975,na.rm=T),output_cv$T[,5],stats::quantile(output[,3],probs=0.975,na.rm=T),output_length$T[,5],stats::quantile(output[,4],probs=0.975,na.rm=T))
    )
    output.tot.k<-data.frame(item=c(row.names(output_sum.k$T),"daily.sum",row.names(output_cv.k$T),"max.cv",row.names(output_length.k$T),"length.dur"),
                             class=c(rep("param.sum",length(row.names(output_sum.k$T))),"stat.sum",rep("param.cv",length(row.names(output_cv.k$T))),"stat.cv",rep("param.length",length(row.names(output_length.k$T))),"stat.length"),
                             factor="K",
                             mean=c(output_sum.k$T[,1],mean(output[,5]),output_cv.k$T[,1],mean(output[,6]),output_length.k$T[,1],mean(output[,7])),
                             sd=c(output_sum.k$T[,3],stats::sd(output[,5]),output_cv.k$T[,3],stats::sd(output[,6]),output_length.k$T[,3],stats::sd(output[,7])),
                             ci.min=c(output_sum.k$T[,4],stats::quantile(output[,5],probs=0.025,na.rm=T),output_cv.k$T[,4],stats::quantile(output[,6],probs=0.025,na.rm=T),output_length.k$T[,4],stats::quantile(output[,7],probs=0.025,na.rm=T)),
                             ci.max=c(output_sum.k$T[,5],stats::quantile(output[,5],probs=0.975,na.rm=T),output_cv.k$T[,5],stats::quantile(output[,6],probs=0.975,na.rm=T),output_length.k$T[,5],stats::quantile(output[,7],probs=0.975,na.rm=T))
    )

    #figure output
    #setwd("D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX")
    #pdf("Figure_SPD_sensitivity_2000_pd.pdf",height=6,width=8)
    if(make.plot==T){
      graphics::layout(
        matrix(
          c(5,5,1,1,1,2,
            5,5,1,1,1,3,
            5,5,1,1,1,4),
          ncol=6, byrow = TRUE))
      #make output for the package
      graphics::par(oma=c(6,3,3,3))
      graphics::par(mar=c(0,2,0,0))
      graphics::plot(1,1,xlim=c(0.7,ncol(B)+0.3),ylim=c(0,1.2),xaxt="n",xlab="",ylab="",yaxt="n",col="white",cex.lab=1.2)
      graphics::abline(h=seq(0,1,0.2),col="grey")
      graphics::abline(v=seq(0,ncol(B),1),col="grey")
      graphics::axis(side=2,las=2,labels=F)
      lab<-colnames(B)
      graphics::axis(side=1,at=c(1:ncol(B)),labels=lab,las=2)
      graphics::legend("topright","SFD",bty="n",text.font=2,cex=2)
      graphics::mtext(side=3,"Pre-dawn",outer=TRUE,padj=-0.5)
      pal<-data.frame(class=c("param.sum","param.cv","param.length"),col=c("darkorange","cyan","darkblue"))
      loc<-data.frame(item=lab,loc=c(1:length(lab)))
      off<-data.frame(class=c("param.sum","param.cv","param.length"),off=c(-0.2,0,0.2))
      for(i in c(1:nrow(output.tot))){
        sel<-output.tot[i,]
        if(left(sel$class,4)=="stat"){next}
        graphics::lines(rep(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],2),
                        c(sel$ci.min,sel$ci.max))
        graphics::points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
                         sel$mean,
                         pch=16,col=as.character(pal[which(as.character(pal$class)==as.character(sel$class)),"col"]),cex=1.5)
        graphics::points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
                         sel$mean,
                         pch=1,cex=1.5)
      }
      graphics::par(mar=c(0,0,0,3))
      graphics::plot(output.tot[which(output.tot$class=="stat.sum"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.sum"),"ci.max"]))
      graphics::axis(side=4,las=2)
      graphics::lines(c(1,1),c(output.tot[which(output.tot$class=="stat.sum"),"ci.min"],output.tot[which(output.tot$class=="stat.sum"),"ci.max"] ))
      graphics::points(1,output.tot[which(output.tot$class=="stat.sum"),"mean"],pch=16,cex=1.5,col="darkorange")
      graphics::points(1,output.tot[which(output.tot$class=="stat.sum"),"mean"],pch=1,cex=1.5)
      graphics::mtext(side=4,expression("Sum ("*cm^3*" "*cm^-2*" "*d^-1*")"),padj=2.5,cex=0.8)
      graphics::par(mar=c(0.2,0,0.2,3))
      graphics::plot(output.tot[which(output.tot$class=="stat.cv"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.cv"),"ci.max"]))
      graphics::axis(side=4,las=2)
      graphics::lines(c(1,1),c(output.tot[which(output.tot$class=="stat.cv"),"ci.min"],output.tot[which(output.tot$class=="stat.cv"),"ci.max"] ))
      graphics::points(1,output.tot[which(output.tot$class=="stat.cv"),"mean"],pch=16,cex=1.5,col="cyan")
      graphics::points(1,output.tot[which(output.tot$class=="stat.cv"),"mean"],pch=1,cex=1.5)
      graphics::mtext(side=4,expression("CV (%)"),padj=3,cex=0.8)
      graphics::par(mar=c(0,0,0,3))
      graphics::plot(output.tot[which(output.tot$class=="stat.length"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.length"),"ci.max"]))
      graphics::axis(side=4,las=2)
      graphics::lines(c(1,1),c(output.tot[which(output.tot$class=="stat.length"),"ci.min"],output.tot[which(output.tot$class=="stat.length"),"ci.max"] ))
      graphics::points(1,output.tot[which(output.tot$class=="stat.length"),"mean"],pch=16,cex=1.5,col="darkblue")
      graphics::points(1,output.tot[which(output.tot$class=="stat.length"),"mean"],pch=1,cex=1.5)
      graphics::mtext(side=4,expression("Duration (h)"),padj=3,cex=0.8)

      graphics::par(mar=c(0,4,0,0))
      lab<-colnames(B)
      lab<-lab[-which(lab%in%c("a","b"))]
      graphics::plot(1,1,xlim=c(0.7,length(lab)+0.3),ylim=c(0,1.2),xaxt="n",ylab="Total sensitivity index (-)",xlab="",yaxt="n",col="white",cex.lab=1.2)
      graphics::abline(h=seq(0,1,0.2),col="grey")
      graphics::abline(v=seq(0,ncol(B),1),col="grey")
      graphics::axis(side=2,las=2)
      graphics::axis(side=1,at=c(1:length(lab)),labels=lab,las=2)
      graphics::legend("topleft",horiz=F,c("Sum","CV","Duration"),pch=16,col=c("darkorange","cyan","darkblue"),bty="n",cex=1.2)
      graphics::legend("topright","K",bty="n",text.font=2,cex=2)
      pal<-data.frame(class=c("param.sum","param.cv","param.length"),col=c("darkorange","cyan","darkblue"))
      loc<-data.frame(item=lab,loc=c(1:length(lab)))
      off<-data.frame(class=c("param.sum","param.cv","param.length"),off=c(-0.2,0,0.2))

      output.tot.k.gr<-output.tot.k[-which(output.tot.k$item%in%c("a","b")),]
      for(i in c(1:nrow(output.tot.k.gr))){
        #i<-1
        sel<-output.tot.k.gr[i,]
        if(left(sel$class,4)=="stat"){next}
        graphics::lines(rep(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],2),
                        c(sel$ci.min,sel$ci.max))
        graphics::points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
                         sel$mean,
                         pch=16,col=as.character(pal[which(as.character(pal$class)==as.character(sel$class)),"col"]),cex=1.5)
        graphics::points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
                         sel$mean,
                         pch=1,cex=1.5)
      }
    }

    output.tot.k.gr<-output.tot.k.gr[-which(left(output.tot.k.gr$class,4)=="stat"),]
    output.tot.gr<-output.tot[-which(left(output.tot$class,4)=="stat"),]
    output.tot.k.gr$analysis<-rep("sensitivity",nrow(output.tot.k.gr))
    output.tot.gr$analysis<-rep("sensitivity",nrow(output.tot.gr))
    output.sen<-output.tot[which(left(output.tot$class,4)=="stat"),]
    output.sen$analysis<-rep("uncertainty",nrow(output.sen))
    output.data<-rbind(output.tot.k.gr,output.tot.gr,output.sen)

    #write.table(output.data,"D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX/output.sen.pd.txt",col.names=T,row.names=F,sep="\t")

    #uncertainty parameters
    param<-data.frame(method=method,start.input=as.character(zoo::index(input)[1]),end.input=as.character(zoo::index(input)[length(input)]),
                      zero.start=zero.start,zero.end=zero.end,range.start=range.start,range.end=range.end,
                      sw.cor=sw.cor,sw.sd=sw.sd,log.a_mu=log.a_mu,log.a_sd=log.a_sd,b_mu=b_mu,b_sd=b_sd,min.sfd=min.sfd,min.k=min.k,n=n)
    if(is.numeric(sw.cor)==F)stop("Unused argument, sw.cor is not numeric.")

    output.all<-list(
      output.data,
      output.sfd,
      output.k,
      param
    )
    names(output.all)<-c("output.data","output.sfd","output.k","param")
    return(output.all)
  }
  #----
  #mw----
  if(method=="mw"){

    #d
    if(missing(max.days_min)){max.days_min=5}
    if(missing(max.days_min)){max.days_max=15}

    #e
    if((max.days_min %% 2)== 0)stop("Unused argument, max.days_min should be uneven.")
    if((max.days_max %% 2)== 0)stop("Unused argument, max.days_max should be uneven.")
    max.days_min<-(max.days_min-1)/2
    max.days_min<-(max.days_max-1)/2

    #e
    if(is.numeric(max.days_min)==F)stop("Unused argument, max.days_min is not numeric.")
    if(is.numeric(max.days_max)==F)stop("Unused argument, max.days_max is not numeric.")
    if(max.days_min<0)stop("Unused argument, max.days_min < 0.")
    if(max.days_max<0)stop("Unused argument, max.days_max < 0.")

    A <- lhs::randomLHS(n, 6)
    B <- matrix(nrow = nrow(A), ncol = ncol(A))
    B[,1] <- ceiling(stats::qunif(A[,1], min =-(range.end-1)/2, max = range.end/2))
    B[,2] <- ceiling(stats::qunif(A[,2], min =-(range.start-1)/2, max = range.start/2))
    B[,3] <- msm::qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
    B[,4] <- stats::qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
    B[,5] <- stats::qnorm(A[,5], mean = b_mu, sd= b_sd)
    B[,6] <- ceiling(stats::qunif(A[,6], min =max.days_min-1, max = max.days_max))

    colnames(B)<-c("zero.end","zero.start","sw.cor","a","b","max.days")
    X1<-data.frame(B)

    A <- lhs::randomLHS(n, 6)
    B <- matrix(nrow = nrow(A), ncol = ncol(A))
    B[,1] <- ceiling(stats::qunif(A[,1], min =-(range.end-1)/2, max = range.end/2))
    B[,2] <- ceiling(stats::qunif(A[,2], min =-(range.start-1)/2, max = range.start/2))
    B[,3] <- msm::qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
    B[,4] <- stats::qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
    B[,5] <- stats::qnorm(A[,5], mean = b_mu, sd= b_sd)
    B[,6] <- ceiling(stats::qunif(A[,6], min =max.days_min-1, max = max.days_max))
    colnames(B)<-c("zero.end","zero.start","sw.cor","a","b","max.days")
    X2<-data.frame(B)

    x <- sensitivity::soboljansen(model = NULL , X1, X2, nboot = n)
    B<-x$X
    n<-nrow(B)
    #p= process
    doParallel::registerDoParallel(cores = parallel::detectCores() - 2)
    cl<-parallel::makeCluster(parallel::detectCores() - 2)
    doSNOW::registerDoSNOW(cl)
    pb <- utils::txtProgressBar(max = nrow(B), style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    output<-foreach(i=c(1:n),.combine="rbind",.packages=c("tibble","dplyr","zoo"),.options.snow = opts)%dopar%{
      utils::setTxtProgressBar(pb, i)
      ze<-zero.end+(B[i,1]*stats::median(diff(minutes)))
      if(ze<0){ze<-24*60-ze}
      if(ze>24*60){ze<-ze-24*60}
      zs<-zero.start+(B[i,2]*stats::median(diff(minutes)))
      if(zs<0){zs<-24*60-zs}
      if(zs>24*60){zs<-zs-24*60}

      #dt max
      proc.1<-raw.input
      if(ze>zs){proc.1[which(proc.1$minutes>ze|minutes<zs),"dt.max"]<-NA}
      if(ze<zs){proc.1[which(proc.1$minutes>ze&minutes<zs),"dt.max"]<-NA
      offset<-(60*24/stats::median(diff(proc.1$minutes)))-ceiling(zs/stats::median(diff(proc.1$minutes)))+1
      proc.1$days.agg<-c(proc.1$days[(offset:length(proc.1$days))],rep(max(proc.1$days),offset-1))}
      #View(proc.1)
      if(ze==zs)stop(paste("Unused argument, zero.start and zero.end are too close together.",sep=""))
      add<-tibble::tibble(
        days.add = stats::aggregate(stats::na.omit(proc.1)$dt.max,by=list(stats::na.omit(proc.1)$days.agg),max,na.rm=TRUE)[,1],
        ddt.max=stats::aggregate(stats::na.omit(proc.1)$dt.max,by=list(stats::na.omit(proc.1)$days.agg),max,na.rm=TRUE)[,2]
      )
      m.day<-(B[i,"max.days"]*2)+1
      proc.1_2<-dplyr::full_join(add,data.frame(days=c(1:max(add[,1])),rmax=NA),by=c("days.add"="days"))
      proc.1_2<-proc.1_2[order(proc.1_2$days.add),]
      proc.1_2$rmax<-zoo::rollmax(proc.1_2$ddt.max,m.day,align = c("center"),na.rm=TRUE,fill=NA)
      gaps<-proc.1_2[which(is.na(proc.1_2$ddt.max)==TRUE),]
      if(nrow(gaps)!=0){

        gaps$NA_value<-c(1,diff(gaps$days.add))
        split<-c(1,gaps[which(gaps$NA_value>1),]$days.add,nrow(proc.1_2))

        for(z in c(1:(length(split)-1))){
          proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1])-1)),]$rmax<-zoo::na.fill(proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1])-1)),]$rmax,c("extend",NA))
          if(z==length(split)-1){
            proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1]))),]$rmax<-zoo::na.fill(proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1]))),]$rmax,c("extend",NA))
          }
        }
      }
      add<-proc.1_2[,c(1,2,3)]
      proc.2<-dplyr::full_join(proc.1,add,by=c("days.agg"="days.add"))
      proc.2[which(proc.2$ddt.max!=proc.2$dt.max|is.na(proc.2$dt.max)==TRUE),"ddt.max"]<-NA
      proc.2[which(is.na(proc.2$ddt.max)==FALSE),"gap"]<-NA
      proc.2$gap<-stats::ave(proc.2$gap, rev(cumsum(rev(is.na(proc.2$gap)))), FUN=cumsum)*stats::median(diff(proc.1$minutes))
      proc.2[which(is.na(proc.2$ddt.max)==FALSE),"ddt.max"]<-proc.2[which(is.na(proc.2$ddt.max)==FALSE),"rmax"]
      proc.2$ddt.max<-zoo::na.locf(zoo::na.locf(proc.2$ddt.max,na.rm=F),fromLast=TRUE)
      proc.2[which(is.na(proc.2$value)==TRUE|proc.2$gap>(60*(24+12))|proc.2$count!=stats::median(proc.2$count,na.rm=TRUE)),"ddt.max"]<-NA

      #HW correction
      #i<-4
      swt<-B[i,3]
      if(probe.length<=swt){
        proc.2$k.pd                 <-((proc.2$ddt.max-proc.2$value)/proc.2$value)
        proc.2$k.pd[which(proc.2$k.pd<0)]<-0
      }
      if(probe.length>swt){
        proc.2$k.pd<-((proc.2$ddt.max-((proc.2$value-(1-(swt/probe.length))*proc.2$ddt.max)/(swt/probe.length)))/((proc.2$value-(1-(swt/probe.length))*proc.2$ddt.max)/(swt/probe.length)))
        proc.2$k.pd[which(proc.2$k.pd<0)]<-0
      }

      #still need to consider whether we include dampening
      a<-B[i,4]
      b<-B[i,5]
      proc.2$sfd<-a*proc.2$k.pd^b

      #think about relevant output proxies
      d.sum.k<-suppressWarnings(stats::aggregate(proc.2$k.pd,by=list(proc.2$days),max,na.rm=TRUE))
      d.sum.k[which(d.sum.k[,2]=="-Inf"),2]<-NA
      durat.k<-proc.2
      durat.k[which(proc.2$k.pd<=min.k),"k.pd"]<-NA
      durat.k<-stats::aggregate(durat.k$k.pd, by=list(durat.k$days), FUN=function(x) {length(stats::na.omit(x))})
      values.k<-stats::aggregate(proc.2$k.pd,by=list(proc.2$days),mean,na.rm=TRUE)*24
      values.k[values.k[,2]=="NaN",2]<-NA

      durat<-proc.2
      durat[which(proc.2$sfd<=min.sfd),"sfd"]<-NA
      durat<-stats::aggregate(durat$sfd, by=list(durat$days), FUN=function(x) {length(stats::na.omit(x))})
      values<-stats::aggregate(proc.2$sfd,by=list(proc.2$days),mean,na.rm=TRUE)*24
      values[values[,2]=="NaN",2]<-NA
      d.sum<-suppressWarnings(stats::aggregate(proc.2$sfd,by=list(proc.2$days),max,na.rm=TRUE))
      d.sum[which(d.sum[,2]=="-Inf"),2]<-NA

      return(
        c(c(i,mean(values[,2],na.rm=TRUE),(stats::sd(d.sum[,2],na.rm=TRUE)/mean(d.sum[,2],na.rm=TRUE))*100,(mean(durat[,2]*stats::median(diff(proc.2$minutes)))/60),
            mean(values.k[,2],na.rm=TRUE),(stats::sd(d.sum.k[,2],na.rm=TRUE)/mean(d.sum.k[,2],na.rm=TRUE))*100,(mean(durat.k[,2]*stats::median(diff(proc.2$minutes)))/60)),
          c(length(proc.2$sfd),proc.2$sfd,proc.2$k.pd))
      )
    }
    doParallel::stopImplicitCluster()

    #time series output
    number<-stats::median(output[,8])
    output.sfd<-output[,c(9:(8+number))]
    output.k<-output[,c((9+number):(8+number+number))]
    n<-nrow(output.k)
    output.sfd<-data.frame(mu=apply(output.sfd,2,mean,na.rm=T),sd=apply(output.sfd,2,sd,na.rm=T),ci.max=apply(output.sfd,2,stats::quantile,probs=c(0.975),na.rm=T),ci.min=apply(output.sfd,2,stats::quantile,probs=c(0.025),na.rm=T))
    output.k<-data.frame(mu=apply(output.k,2,mean,na.rm=T),sd=apply(output.k,2,sd,na.rm=T),ci.max=apply(output.k,2,stats::quantile,probs=c(0.975),na.rm=T),ci.min=apply(output.k,2,stats::quantile,probs=c(0.025),na.rm=T))
    output.sfd<-zoo::zoo(output.sfd,order.by=zoo::index(input))
    output.k<-zoo::zoo(output.k,order.by=zoo::index(input))

    if(df==T){
      output.sfd<-zoo::fortify.zoo(output.sfd)
      output.k<-zoo::fortify.zoo(output.k)
      colnames(output.sfd)<-c("timestamp","mu","sd","ci.max","ci.min")
      colnames(output.k)<-c("timestamp","mu","sd","ci.max","ci.min")
    }

    #o= output
    output<-output[,c(1:7)]
    output_sum<-sensitivity::tell(x,as.numeric(output[,2]))
    output_cv<-sensitivity::tell(x,output[,3])
    output_length<-sensitivity::tell(x,output[,4])

    output_sum.k<-sensitivity::tell(x,output[,5])
    output_cv.k<-sensitivity::tell(x,output[,6])
    output_length.k<-sensitivity::tell(x,output[,7])

    output.tot<-data.frame(item=c(row.names(output_sum$T),"daily.sum",row.names(output_cv$T),"max.cv",row.names(output_length$T),"length.dur"),
                           class=c(rep("param.sum",length(row.names(output_sum$T))),"stat.sum",rep("param.cv",length(row.names(output_cv$T))),"stat.cv",rep("param.length",length(row.names(output_length$T))),"stat.length"),
                           factor="SFD",
                           mean=c(output_sum$T[,1],mean(output[,2]),output_cv$T[,1],mean(output[,3]),output_length$T[,1],mean(output[,4])),
                           sd=c(output_sum$T[,3],stats::sd(output[,2]),output_cv$T[,3],stats::sd(output[,3]),output_length$T[,3],stats::sd(output[,4])),
                           ci.min=c(output_sum$T[,4],stats::quantile(output[,2],probs=0.025,na.rm=T),output_cv$T[,4],stats::quantile(output[,3],probs=0.025,na.rm=T),output_length$T[,4],stats::quantile(output[,4],probs=0.025,na.rm=T)),
                           ci.max=c(output_sum$T[,5],stats::quantile(output[,2],probs=0.975,na.rm=T),output_cv$T[,5],stats::quantile(output[,3],probs=0.975,na.rm=T),output_length$T[,5],stats::quantile(output[,4],probs=0.975,na.rm=T))
    )
    output.tot.k<-data.frame(item=c(row.names(output_sum.k$T),"daily.sum",row.names(output_cv.k$T),"max.cv",row.names(output_length.k$T),"length.dur"),
                             class=c(rep("param.sum",length(row.names(output_sum.k$T))),"stat.sum",rep("param.cv",length(row.names(output_cv.k$T))),"stat.cv",rep("param.length",length(row.names(output_length.k$T))),"stat.length"),
                             factor="K",
                             mean=c(output_sum.k$T[,1],mean(output[,5]),output_cv.k$T[,1],mean(output[,6]),output_length.k$T[,1],mean(output[,7])),
                             sd=c(output_sum.k$T[,3],stats::sd(output[,5]),output_cv.k$T[,3],stats::sd(output[,6]),output_length.k$T[,3],stats::sd(output[,7])),
                             ci.min=c(output_sum.k$T[,4],stats::quantile(output[,5],probs=0.025,na.rm=T),output_cv.k$T[,4],stats::quantile(output[,6],probs=0.025,na.rm=T),output_length.k$T[,4],stats::quantile(output[,7],probs=0.025,na.rm=T)),
                             ci.max=c(output_sum.k$T[,5],stats::quantile(output[,5],probs=0.975,na.rm=T),output_cv.k$T[,5],stats::quantile(output[,6],probs=0.975,na.rm=T),output_length.k$T[,5],stats::quantile(output[,7],probs=0.975,na.rm=T))
    )

    #figure output
    if(make.plot==T){
      graphics::layout(
        matrix(
          c(5,5,1,1,1,2,
            5,5,1,1,1,3,
            5,5,1,1,1,4),
          ncol=6, byrow = TRUE))
      #make output for the package
      graphics::par(oma=c(6,3,3,3))
      graphics::par(mar=c(0,2,0,0))
      graphics::plot(1,1,xlim=c(0.7,ncol(B)+0.3),ylim=c(0,1.2),xaxt="n",xlab="",ylab="",yaxt="n",col="white",cex.lab=1.2)
      graphics::abline(h=seq(0,1,0.2),col="grey")
      graphics::abline(v=seq(0,ncol(B),1),col="grey")
      graphics::axis(side=2,las=2,labels=F)
      lab<-colnames(B)
      graphics::axis(side=1,at=c(1:ncol(B)),labels=lab,las=2)
      #graphics::legend("top",horiz=TRUE,c("Sum","CV","Duration"),pch=16,col=c("darkorange","cyan","darkblue"),bty="n")
      graphics::legend("topright","SFD",bty="n",text.font=2,cex=2)
      graphics::mtext(side=3,"Moving-window",outer=TRUE,padj=-0.5)
      pal<-data.frame(class=c("param.sum","param.cv","param.length"),col=c("darkorange","cyan","darkblue"))
      loc<-data.frame(item=lab,loc=c(1:length(lab)))
      off<-data.frame(class=c("param.sum","param.cv","param.length"),off=c(-0.2,0,0.2))
      for(i in c(1:nrow(output.tot))){
        sel<-output.tot[i,]
        if(left(sel$class,4)=="stat"){next}
        graphics::lines(rep(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],2),
                        c(sel$ci.min,sel$ci.max))
        graphics::points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
                         sel$mean,
                         pch=16,col=as.character(pal[which(as.character(pal$class)==as.character(sel$class)),"col"]),cex=1.5)
        graphics::points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
                         sel$mean,
                         pch=1,cex=1.5)
      }
      graphics::par(mar=c(0,0,0,3))
      graphics::plot(output.tot[which(output.tot$class=="stat.sum"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.sum"),"ci.max"]))
      graphics::axis(side=4,las=2)
      graphics::lines(c(1,1),c(output.tot[which(output.tot$class=="stat.sum"),"ci.min"],output.tot[which(output.tot$class=="stat.sum"),"ci.max"] ))
      graphics::points(1,output.tot[which(output.tot$class=="stat.sum"),"mean"],pch=16,cex=1.5,col="darkorange")
      graphics::points(1,output.tot[which(output.tot$class=="stat.sum"),"mean"],pch=1,cex=1.5)
      graphics::mtext(side=4,expression("Sum ("*cm^3*" "*cm^-2*" "*d^-1*")"),padj=2.5,cex=0.8)
      graphics::par(mar=c(0.2,0,0.2,3))
      graphics::plot(output.tot[which(output.tot$class=="stat.cv"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.cv"),"ci.max"]))
      graphics::axis(side=4,las=2)
      graphics::lines(c(1,1),c(output.tot[which(output.tot$class=="stat.cv"),"ci.min"],output.tot[which(output.tot$class=="stat.cv"),"ci.max"] ))
      graphics::points(1,output.tot[which(output.tot$class=="stat.cv"),"mean"],pch=16,cex=1.5,col="cyan")
      graphics::points(1,output.tot[which(output.tot$class=="stat.cv"),"mean"],pch=1,cex=1.5)
      graphics::mtext(side=4,expression("CV (%)"),padj=3,cex=0.8)
      graphics::par(mar=c(0,0,0,3))
      graphics::plot(output.tot[which(output.tot$class=="stat.length"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.length"),"ci.max"]))
      graphics::axis(side=4,las=2)
      graphics::lines(c(1,1),c(output.tot[which(output.tot$class=="stat.length"),"ci.min"],output.tot[which(output.tot$class=="stat.length"),"ci.max"] ))
      graphics::points(1,output.tot[which(output.tot$class=="stat.length"),"mean"],pch=16,cex=1.5,col="darkblue")
      graphics::points(1,output.tot[which(output.tot$class=="stat.length"),"mean"],pch=1,cex=1.5)
      graphics::mtext(side=4,expression("Duration (h)"),padj=3,cex=0.8)

      graphics::par(mar=c(0,4,0,0))
      lab<-colnames(B)
      lab<-lab[-which(lab%in%c("a","b"))]
      graphics::plot(1,1,xlim=c(0.7,length(lab)+0.3),ylim=c(0,1.2),xaxt="n",ylab="Total sensitivity index (-)",xlab="",yaxt="n",col="white",cex.lab=1.2)
      graphics::abline(h=seq(0,1,0.2),col="grey")
      graphics::abline(v=seq(0,ncol(B),1),col="grey")
      graphics::axis(side=2,las=2)
      graphics::axis(side=1,at=c(1:length(lab)),labels=lab,las=2)
      graphics::legend("topleft",horiz=F,c("Sum","CV","Duration"),pch=16,col=c("darkorange","cyan","darkblue"),bty="n",cex=1.2)
      graphics::legend("topright","K",bty="n",text.font=2,cex=2)
      pal<-data.frame(class=c("param.sum","param.cv","param.length"),col=c("darkorange","cyan","darkblue"))
      loc<-data.frame(item=lab,loc=c(1:length(lab)))
      off<-data.frame(class=c("param.sum","param.cv","param.length"),off=c(-0.2,0,0.2))

      output.tot.k.gr<-output.tot.k[-which(output.tot.k$item%in%c("a","b")),]
      for(i in c(1:nrow(output.tot.k.gr))){
        #i<-1
        sel<-output.tot.k.gr[i,]
        if(left(sel$class,4)=="stat"){next}
        graphics::lines(rep(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],2),
                        c(sel$ci.min,sel$ci.max))
        graphics::points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
                         sel$mean,
                         pch=16,col=as.character(pal[which(as.character(pal$class)==as.character(sel$class)),"col"]),cex=1.5)
        graphics::points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
                         sel$mean,
                         pch=1,cex=1.5)
      }
    }

    output.tot.k.gr<-output.tot.k.gr[-which(left(output.tot.k.gr$class,4)=="stat"),]
    output.tot.gr<-output.tot[-which(left(output.tot$class,4)=="stat"),]
    output.tot.k.gr$analysis<-rep("sensitivity",nrow(output.tot.k.gr))
    output.tot.gr$analysis<-rep("sensitivity",nrow(output.tot.gr))
    output.sen<-output.tot[which(left(output.tot$class,4)=="stat"),]
    output.sen$analysis<-rep("uncertainty",nrow(output.sen))
    output.data<-rbind(output.tot.k.gr,output.tot.gr,output.sen)

    #uncertainty parameters
    param<-data.frame(method=method,start.input=as.character(zoo::index(input)[1]),end.input=as.character(zoo::index(input)[length(input)]),
                      zero.start=zero.start,zero.end=zero.end,range.start=range.start,range.end=range.end,
                      sw.cor=sw.cor,sw.sd=sw.sd,log.a_mu=log.a_mu,log.a_sd=log.a_sd,b_mu=b_mu,b_sd=b_sd,max.days_min=max.days_min,
                      max.days_max=max.days_max,
                      min.sfd=min.sfd,min.k=min.k,n=n)
    if(is.numeric(sw.cor)==F)stop("Unused argument, sw.cor is not numeric.")

    output.all<-list(
      output.data,
      output.sfd,
      output.k,
      param
    )
    names(output.all)<-c("output.data","output.sfd","output.k","param")
    return(output.all)
  }
  #----
  #dr----
  if(method=="dr"){
    #d
    if(missing(max.days_min)){max.days_min=5}
    if(missing(max.days_min)){max.days_max=15}

    #e
    if((max.days_min %% 2) == 0)stop("Unused argument, max.days_min should be uneven.")
    if((max.days_max %% 2) == 0)stop("Unused argument, max.days_max should be uneven.")
    max.days_min<-(max.days_min-1)/2
    max.days_min<-(max.days_max-1)/2

    #e
    if(is.numeric(max.days_min)==F)stop("Unused argument, max.days_min is not numeric.")
    if(is.numeric(max.days_max)==F)stop("Unused argument, max.days_max is not numeric.")
    if(max.days_min<0)stop("Unused argument, max.days_min < 0.")
    if(max.days_max<0)stop("Unused argument, max.days_max < 0.")

    A <- lhs::randomLHS(n, 6)
    B <- matrix(nrow = nrow(A), ncol = ncol(A))
    B[,1] <- ceiling(stats::qunif(A[,1], min =-(range.end-1)/2, max = range.end/2))
    B[,2] <- ceiling(stats::qunif(A[,2], min =-(range.start-1)/2, max = range.start/2))
    B[,3] <- msm::qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
    B[,4] <- stats::qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
    B[,5] <- stats::qnorm(A[,5], mean = b_mu, sd= b_sd)
    B[,6] <- ceiling(stats::qunif(A[,6], min =max.days_min-1, max = max.days_max))
    colnames(B)<-c("zero.end","zero.start","sw.cor","a","b","max.days")
    X1<-data.frame(B)

    A <- lhs::randomLHS(n, 6)
    B <- matrix(nrow = nrow(A), ncol = ncol(A))
    B[,1] <- ceiling(stats::qunif(A[,1], min =-(range.end-1)/2, max = range.end/2))
    B[,2] <- ceiling(stats::qunif(A[,2], min =-(range.start-1)/2, max = range.start/2))
    B[,3] <- msm::qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
    B[,4] <- stats::qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
    B[,5] <- stats::qnorm(A[,5], mean = b_mu, sd= b_sd)
    B[,6] <- ceiling(stats::qunif(A[,6], min =max.days_min-1, max = max.days_max))
    colnames(B)<-c("zero.end","zero.start","sw.cor","a","b","max.days")
    X2<-data.frame(B)

    x <- sensitivity::soboljansen(model = NULL , X1, X2, nboot = n)
    B<-x$X
    n<-nrow(B)
    #p= process
    doParallel::registerDoParallel(cores = parallel::detectCores() - 2)
    cl<-parallel::makeCluster(parallel::detectCores() - 2)
    doSNOW::registerDoSNOW(cl)
    pb <- utils::txtProgressBar(max = nrow(B), style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    output<-foreach(i=c(1:n),.combine="rbind",.packages=c("tibble","dplyr","zoo"),.options.snow = opts)%dopar%{
      #i<-1
      utils::setTxtProgressBar(pb, i)
      ze<-zero.end+(B[i,1]*stats::median(diff(minutes)))
      if(ze<0){ze<-24*60-ze}
      if(ze>24*60){ze<-ze-24*60}
      zs<-zero.start+(B[i,2]*stats::median(diff(minutes)))
      if(zs<0){zs<-24*60-zs}
      if(zs>24*60){zs<-zs-24*60}

      #dt max
      proc.1<-raw.input
      if(ze>zs){proc.1[which(proc.1$minutes>ze|minutes<zs),"dt.max"]<-NA}
      if(ze<zs){proc.1[which(proc.1$minutes>ze&minutes<zs),"dt.max"]<-NA
      offset<-(60*24/stats::median(diff(proc.1$minutes)))-ceiling(zs/stats::median(diff(proc.1$minutes)))+1
      proc.1$days.agg<-c(proc.1$days[(offset:length(proc.1$days))],rep(max(proc.1$days),offset-1))}

      if(ze==zs)stop(paste("Unused argument, zero.start and zero.end are too close together.",sep=""))
      add<-tibble::tibble(
        days.add = stats::aggregate(stats::na.omit(proc.1)$dt.max,by=list(stats::na.omit(proc.1)$days.agg),max,na.rm=TRUE)[,1],
        ddt.max=stats::aggregate(stats::na.omit(proc.1)$dt.max,by=list(stats::na.omit(proc.1)$days.agg),max,na.rm=TRUE)[,2]
      )
      m.day<-(B[i,"max.days"]*2)+1
      proc.1_2<-dplyr::full_join(add,data.frame(days=c(1:max(add[,1])),rmax=NA),by=c("days.add"="days"))
      proc.1_2<-proc.1_2[order(proc.1_2$days.add),]
      proc.1_2$rmax<-zoo::rollmean(proc.1_2$ddt.max,m.day,align = c("center"),na.pad=TRUE,na.rm=TRUE)

      gaps<-proc.1_2[which(is.na(proc.1_2$ddt.max)==TRUE),]
      if(nrow(gaps)!=0){
        gaps$NA_value<-c(1,diff(gaps$days.add))
        split<-c(1,gaps[which(gaps$NA_value>1),]$days.add,nrow(proc.1_2))

        for(z in c(1:(length(split)-1))){
          proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1])-1)),]$rmax<-zoo::na.fill(proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1])-1)),]$rmax,c("extend",NA))
          if(z==length(split)-1){
            proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1]))),]$rmax<-zoo::na.fill(proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1]))),]$rmax,c("extend",NA))
          }
        }
      }
      proc.1_2$ddt.max.r<-proc.1_2[,"ddt.max"]
      proc.1_2[which(proc.1_2$ddt.max<proc.1_2$rmax),"ddt.max.r"]<-NA
      proc.1_2$rmax.r<-zoo::rollmean(proc.1_2$ddt.max.r,m.day,align = c("center"),na.pad=TRUE,na.rm=TRUE)

      gaps<-proc.1_2[which(is.na(proc.1_2$ddt.max)==TRUE),]
      if(nrow(gaps)!=0){
        gaps$NA_value<-c(1,diff(gaps$days.add))
        split<-c(1,gaps[which(gaps$NA_value>1),]$days.add,nrow(proc.1_2))

        for(z in c(1:(length(split)-1))){
          proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1])-1)),]$rmax.r<-zoo::na.fill(proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1])-1)),]$rmax.r,c("extend",NA))
          if(z==length(split)-1){
            proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1]))),]$rmax.r<-zoo::na.fill(proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1]))),]$rmax.r,c("extend",NA))
          }
        }
      }
      proc.1_2$rmax<-as.numeric(proc.1_2$rmax.r)
      add<-proc.1_2[,c(1,2,3)]
      proc.2<-dplyr::full_join(proc.1,add,by=c("days.agg"="days.add"))
      proc.2[which(proc.2$ddt.max!=proc.2$dt.max|is.na(proc.2$dt.max)==TRUE),"ddt.max"]<-NA
      proc.2[which(is.na(proc.2$ddt.max)==FALSE),"gap"]<-NA
      proc.2$gap<-stats::ave(proc.2$gap, rev(cumsum(rev(is.na(proc.2$gap)))), FUN=cumsum)*stats::median(diff(proc.1$minutes))
      proc.2[which(is.na(proc.2$ddt.max)==FALSE),"ddt.max"]<-proc.2[which(is.na(proc.2$ddt.max)==FALSE),"rmax"]
      proc.2$ddt.max<-zoo::na.locf(zoo::na.locf(proc.2$ddt.max,na.rm=F),fromLast=TRUE)
      proc.2[which(is.na(proc.2$value)==TRUE|proc.2$gap>(60*(24+12))|proc.2$count!=stats::median(proc.2$count,na.rm=TRUE)),"ddt.max"]<-NA

      #HW correction
      #i<-4
      swt<-B[i,3]
      if(probe.length<=swt){
        proc.2$k.pd                 <-((proc.2$ddt.max-proc.2$value)/proc.2$value)
        proc.2$k.pd[which(proc.2$k.pd<0)]<-0
      }
      if(probe.length>swt){
        proc.2$k.pd<-((proc.2$ddt.max-((proc.2$value-(1-(swt/probe.length))*proc.2$ddt.max)/(swt/probe.length)))/((proc.2$value-(1-(swt/probe.length))*proc.2$ddt.max)/(swt/probe.length)))
        proc.2$k.pd[which(proc.2$k.pd<0)]<-0
      }

      #still need to consider whether we include dampening
      a<-B[i,4]
      b<-B[i,5]
      proc.2$sfd<-a*proc.2$k.pd^b

      #think about relevant output proxies
      d.sum.k<-suppressWarnings(stats::aggregate(proc.2$k.pd,by=list(proc.2$days),max,na.rm=TRUE))
      d.sum.k[which(d.sum.k[,2]=="-Inf"),2]<-NA
      durat.k<-proc.2
      durat.k[which(proc.2$k.pd<=min.k),"k.pd"]<-NA
      durat.k<-stats::aggregate(durat.k$k.pd, by=list(durat.k$days), FUN=function(x) {length(stats::na.omit(x))})
      values.k<-stats::aggregate(proc.2$k.pd,by=list(proc.2$days),mean,na.rm=TRUE)*24
      values.k[values.k[,2]=="NaN",2]<-NA

      durat<-proc.2
      durat[which(proc.2$sfd<=min.sfd),"sfd"]<-NA
      durat<-stats::aggregate(durat$sfd, by=list(durat$days), FUN=function(x) {length(stats::na.omit(x))})
      values<-stats::aggregate(proc.2$sfd,by=list(proc.2$days),mean,na.rm=TRUE)*24
      values[values[,2]=="NaN",2]<-NA
      d.sum<-suppressWarnings(stats::aggregate(proc.2$sfd,by=list(proc.2$days),max,na.rm=TRUE))
      d.sum[which(d.sum[,2]=="-Inf"),2]<-NA
      return(
        c(c(i,mean(values[,2],na.rm=TRUE),(stats::sd(d.sum[,2],na.rm=TRUE)/mean(d.sum[,2],na.rm=TRUE))*100,(mean(durat[,2]*stats::median(diff(proc.2$minutes)))/60),
            mean(values.k[,2],na.rm=TRUE),(stats::sd(d.sum.k[,2],na.rm=TRUE)/mean(d.sum.k[,2],na.rm=TRUE))*100,(mean(durat.k[,2]*stats::median(diff(proc.2$minutes)))/60)),
          c(length(proc.2$sfd),proc.2$sfd,proc.2$k.pd))
      )
    }
    doParallel::stopImplicitCluster()

    #time series output
    number<-stats::median(output[,8])
    output.sfd<-output[,c(9:(8+number))]
    output.k<-output[,c((9+number):(8+number+number))]
    n<-nrow(output.k)
    output.sfd<-data.frame(mu=apply(output.sfd,2,mean,na.rm=T),sd=apply(output.sfd,2,sd,na.rm=T),ci.max=apply(output.sfd,2,stats::quantile,probs=c(0.975),na.rm=T),ci.min=apply(output.sfd,2,stats::quantile,probs=c(0.025),na.rm=T))
    output.k<-data.frame(mu=apply(output.k,2,mean,na.rm=T),sd=apply(output.k,2,sd,na.rm=T),ci.max=apply(output.k,2,stats::quantile,probs=c(0.975),na.rm=T),ci.min=apply(output.k,2,stats::quantile,probs=c(0.025),na.rm=T))
    output.sfd<-zoo::zoo(output.sfd,order.by=zoo::index(input))
    output.k<-zoo::zoo(output.k,order.by=zoo::index(input))

    if(df==T){
      output.sfd<-zoo::fortify.zoo(output.sfd)
      output.k<-zoo::fortify.zoo(output.k)
      colnames(output.sfd)<-c("timestamp","mu","sd","ci.max","ci.min")
      colnames(output.k)<-c("timestamp","mu","sd","ci.max","ci.min")
    }

    #o= output
    output<-output[,c(1:7)]
    output_sum<-sensitivity::tell(x,as.numeric(output[,2]))
    output_cv<-sensitivity::tell(x,output[,3])
    output_length<-sensitivity::tell(x,output[,4])

    output_sum.k<-sensitivity::tell(x,output[,5])
    output_cv.k<-sensitivity::tell(x,output[,6])
    output_length.k<-sensitivity::tell(x,output[,7])

    output.tot<-data.frame(item=c(row.names(output_sum$T),"daily.sum",row.names(output_cv$T),"max.cv",row.names(output_length$T),"length.dur"),
                           class=c(rep("param.sum",length(row.names(output_sum$T))),"stat.sum",rep("param.cv",length(row.names(output_cv$T))),"stat.cv",rep("param.length",length(row.names(output_length$T))),"stat.length"),
                           factor="SFD",
                           mean=c(output_sum$T[,1],mean(output[,2]),output_cv$T[,1],mean(output[,3]),output_length$T[,1],mean(output[,4])),
                           sd=c(output_sum$T[,3],stats::sd(output[,2]),output_cv$T[,3],stats::sd(output[,3]),output_length$T[,3],stats::sd(output[,4])),
                           ci.min=c(output_sum$T[,4],stats::quantile(output[,2],probs=0.025,na.rm=T),output_cv$T[,4],stats::quantile(output[,3],probs=0.025,na.rm=T),output_length$T[,4],stats::quantile(output[,4],probs=0.025,na.rm=T)),
                           ci.max=c(output_sum$T[,5],stats::quantile(output[,2],probs=0.975,na.rm=T),output_cv$T[,5],stats::quantile(output[,3],probs=0.975,na.rm=T),output_length$T[,5],stats::quantile(output[,4],probs=0.975,na.rm=T))
    )
    output.tot.k<-data.frame(item=c(row.names(output_sum.k$T),"daily.sum",row.names(output_cv.k$T),"max.cv",row.names(output_length.k$T),"length.dur"),
                             class=c(rep("param.sum",length(row.names(output_sum.k$T))),"stat.sum",rep("param.cv",length(row.names(output_cv.k$T))),"stat.cv",rep("param.length",length(row.names(output_length.k$T))),"stat.length"),
                             factor="K",
                             mean=c(output_sum.k$T[,1],mean(output[,5]),output_cv.k$T[,1],mean(output[,6]),output_length.k$T[,1],mean(output[,7])),
                             sd=c(output_sum.k$T[,3],stats::sd(output[,5]),output_cv.k$T[,3],stats::sd(output[,6]),output_length.k$T[,3],stats::sd(output[,7])),
                             ci.min=c(output_sum.k$T[,4],stats::quantile(output[,5],probs=0.025,na.rm=T),output_cv.k$T[,4],stats::quantile(output[,6],probs=0.025,na.rm=T),output_length.k$T[,4],stats::quantile(output[,7],probs=0.025,na.rm=T)),
                             ci.max=c(output_sum.k$T[,5],stats::quantile(output[,5],probs=0.975,na.rm=T),output_cv.k$T[,5],stats::quantile(output[,6],probs=0.975,na.rm=T),output_length.k$T[,5],stats::quantile(output[,7],probs=0.975,na.rm=T))
    )

    #figure output
    if(make.plot==T){
      graphics::layout(
        matrix(
          c(5,5,1,1,1,2,
            5,5,1,1,1,3,
            5,5,1,1,1,4),
          ncol=6, byrow = TRUE))
      #make output for the package
      graphics::par(oma=c(6,3,3,3))
      graphics::par(mar=c(0,2,0,0))
      graphics::plot(1,1,xlim=c(0.7,ncol(B)+0.3),ylim=c(0,1.2),xaxt="n",xlab="",ylab="",yaxt="n",col="white",cex.lab=1.2)
      graphics::abline(h=seq(0,1,0.2),col="grey")
      graphics::abline(v=seq(0,ncol(B),1),col="grey")
      graphics::axis(side=2,las=2,labels=F)
      lab<-colnames(B)
      graphics::axis(side=1,at=c(1:ncol(B)),labels=lab,las=2)
      #graphics::legend("top",horiz=TRUE,c("Sum","CV","Duration"),pch=16,col=c("darkorange","cyan","darkblue"),bty="n")
      graphics::legend("topright","SFD",bty="n",text.font=2,cex=2)
      graphics::mtext(side=3,"Double-regression",outer=TRUE,padj=-0.5)
      pal<-data.frame(class=c("param.sum","param.cv","param.length"),col=c("darkorange","cyan","darkblue"))
      loc<-data.frame(item=lab,loc=c(1:length(lab)))
      off<-data.frame(class=c("param.sum","param.cv","param.length"),off=c(-0.2,0,0.2))
      for(i in c(1:nrow(output.tot))){
        sel<-output.tot[i,]
        if(left(sel$class,4)=="stat"){next}
        graphics::lines(rep(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],2),
                        c(sel$ci.min,sel$ci.max))
        graphics::points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
                         sel$mean,
                         pch=16,col=as.character(pal[which(as.character(pal$class)==as.character(sel$class)),"col"]),cex=1.5)
        graphics::points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
                         sel$mean,
                         pch=1,cex=1.5)
      }
      graphics::par(mar=c(0,0,0,3))
      graphics::plot(output.tot[which(output.tot$class=="stat.sum"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.sum"),"ci.max"]))
      graphics::axis(side=4,las=2)
      graphics::lines(c(1,1),c(output.tot[which(output.tot$class=="stat.sum"),"ci.min"],output.tot[which(output.tot$class=="stat.sum"),"ci.max"] ))
      graphics::points(1,output.tot[which(output.tot$class=="stat.sum"),"mean"],pch=16,cex=1.5,col="darkorange")
      graphics::points(1,output.tot[which(output.tot$class=="stat.sum"),"mean"],pch=1,cex=1.5)
      graphics::mtext(side=4,expression("Sum ("*cm^3*" "*cm^-2*" "*d^-1*")"),padj=2.5,cex=0.8)
      graphics::par(mar=c(0.2,0,0.2,3))
      graphics::plot(output.tot[which(output.tot$class=="stat.cv"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.cv"),"ci.max"]))
      graphics::axis(side=4,las=2)
      graphics::lines(c(1,1),c(output.tot[which(output.tot$class=="stat.cv"),"ci.min"],output.tot[which(output.tot$class=="stat.cv"),"ci.max"] ))
      graphics::points(1,output.tot[which(output.tot$class=="stat.cv"),"mean"],pch=16,cex=1.5,col="cyan")
      graphics::points(1,output.tot[which(output.tot$class=="stat.cv"),"mean"],pch=1,cex=1.5)
      graphics::mtext(side=4,expression("CV (%)"),padj=3,cex=0.8)
      graphics::par(mar=c(0,0,0,3))
      graphics::plot(output.tot[which(output.tot$class=="stat.length"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.length"),"ci.max"]))
      graphics::axis(side=4,las=2)
      graphics::lines(c(1,1),c(output.tot[which(output.tot$class=="stat.length"),"ci.min"],output.tot[which(output.tot$class=="stat.length"),"ci.max"] ))
      graphics::points(1,output.tot[which(output.tot$class=="stat.length"),"mean"],pch=16,cex=1.5,col="darkblue")
      graphics::points(1,output.tot[which(output.tot$class=="stat.length"),"mean"],pch=1,cex=1.5)
      graphics::mtext(side=4,expression("Duration (h)"),padj=3,cex=0.8)

      graphics::par(mar=c(0,4,0,0))
      lab<-colnames(B)
      lab<-lab[-which(lab%in%c("a","b"))]
      graphics::plot(1,1,xlim=c(0.7,length(lab)+0.3),ylim=c(0,1.2),xaxt="n",ylab="Total sensitivity index (-)",xlab="",yaxt="n",col="white",cex.lab=1.2)
      graphics::abline(h=seq(0,1,0.2),col="grey")
      graphics::abline(v=seq(0,ncol(B),1),col="grey")
      graphics::axis(side=2,las=2)
      graphics::axis(side=1,at=c(1:length(lab)),labels=lab,las=2)
      graphics::legend("topleft",horiz=F,c("Sum","CV","Duration"),pch=16,col=c("darkorange","cyan","darkblue"),bty="n",cex=1.2)
      graphics::legend("topright","K",bty="n",text.font=2,cex=2)
      pal<-data.frame(class=c("param.sum","param.cv","param.length"),col=c("darkorange","cyan","darkblue"))
      loc<-data.frame(item=lab,loc=c(1:length(lab)))
      off<-data.frame(class=c("param.sum","param.cv","param.length"),off=c(-0.2,0,0.2))

      output.tot.k.gr<-output.tot.k[-which(output.tot.k$item%in%c("a","b")),]
      for(i in c(1:nrow(output.tot.k.gr))){
        #i<-1
        sel<-output.tot.k.gr[i,]
        if(left(sel$class,4)=="stat"){next}
        graphics::lines(rep(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],2),
                        c(sel$ci.min,sel$ci.max))
        graphics::points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
                         sel$mean,
                         pch=16,col=as.character(pal[which(as.character(pal$class)==as.character(sel$class)),"col"]),cex=1.5)
        graphics::points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
                         sel$mean,
                         pch=1,cex=1.5)
      }
    }

    output.tot.k.gr<-output.tot.k.gr[-which(left(output.tot.k.gr$class,4)=="stat"),]
    output.tot.gr<-output.tot[-which(left(output.tot$class,4)=="stat"),]
    output.tot.k.gr$analysis<-rep("sensitivity",nrow(output.tot.k.gr))
    output.tot.gr$analysis<-rep("sensitivity",nrow(output.tot.gr))
    output.sen<-output.tot[which(left(output.tot$class,4)=="stat"),]
    output.sen$analysis<-rep("uncertainty",nrow(output.sen))
    output.data<-rbind(output.tot.k.gr,output.tot.gr,output.sen)

    #uncertainty parameters
    param<-data.frame(method=method,start.input=as.character(zoo::index(input)[1]),end.input=as.character(zoo::index(input)[length(input)]),
                      zero.start=zero.start,zero.end=zero.end,range.start=range.start,range.end=range.end,
                      sw.cor=sw.cor,sw.sd=sw.sd,log.a_mu=log.a_mu,log.a_sd=log.a_sd,b_mu=b_mu,b_sd=b_sd,max.days_min=max.days_min,
                      max.days_max=max.days_max,
                      min.sfd=min.sfd,min.k=min.k,n=n)
    if(is.numeric(sw.cor)==F)stop("Unused argument, sw.cor is not numeric.")

    output.all<-list(
      output.data,
      output.sfd,
      output.k,
      param
    )
    names(output.all)<-c("output.data","output.sfd","output.k","param")
    return(output.all)
  }

  #environmental dependent
  #ed----
  if(method=="ed"){
    #test



    #e
    if(missing(vpd.input)){
      warning(paste0("No vpd.input data included."))
      vpd.input<-zoo::zoo(0,order.by=zoo::index(input))}
    if(missing(sr.input)){
      warning(paste0("No sr.input data included."))
      sr.input<-zoo::zoo(0,order.by=zoo::index(input))}
    if(missing(vpd.input)&missing(sr.input))stop("Invalid input data, no sr.input nor vpd.input provided.")
    if(attributes(vpd.input)$class=="data.frame"){
      #e
      if(is.numeric(vpd.input$value)==F)stop("Invalid vpd.input data, values within the data.frame are not numeric.")
      if(is.character(vpd.input$timestamp)==F)stop("Invalid vpd.input data, timestamp within the data.frame are not numeric.")

      #p
      vpd.input<-zoo::zoo(vpd.input$value,order.by=base::as.POSIXct(vpd.input$timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC"))

      #e
      if(as.character(zoo::index(vpd.input)[1])=="(NA NA)"|is.na(zoo::index(vpd.input)[1])==T)stop("No timestamp present, time.format is likely incorrect for vpd.input.")
    }
    if(zoo::is.zoo(vpd.input)==FALSE)stop("Invalid input data, vpd.input must be a zoo file (use is.trex).")

    if(attributes(sr.input)$class=="data.frame"){
      #e
      if(is.numeric(sr.input$value)==F)stop("Invalid sr.input data, values within the data.frame are not numeric.")
      if(is.character(sr.input$timestamp)==F)stop("Invalid sr.input data, timestamp within the data.frame are not numeric.")

      #p
      sr.input<-zoo::zoo(sr.input$value,order.by=base::as.POSIXct(sr.input$timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC"))

      #e
      if(as.character(zoo::index(sr.input)[1])=="(NA NA)"|is.na(zoo::index(sr.input)[1])==T)stop("No timestamp present, time.format is likely incorrect for sr.input.")
    }
    if(zoo::is.zoo(sr.input)==FALSE)stop("Invalid input data, sr.input must be a zoo file (use is.trex).")

    #p
    step.min<-as.numeric(min(difftime(zoo::index(input)[-1],zoo::index(input)[-length(input)],units=c("mins")),na.rm=TRUE))
    step.sr<-as.numeric(min(difftime(zoo::index(sr.input)[-1],zoo::index(sr.input)[-length(sr.input)],units=c("mins")),na.rm=TRUE))
    step.vpd<-as.numeric(min(difftime(zoo::index(vpd.input)[-1],zoo::index(vpd.input)[-length(vpd.input)],units=c("mins")),na.rm=TRUE))

    #e
    if(step.min!=step.sr|step.min!=step.vpd)stop("time steps between input and vpd.input/sr.input differ, results will not be correctly stats::aggregated.")
    if(zoo::index(sr.input)[1]-zoo::index(input)[1]!=0)stop("Invalid sr.input, timestamp start does not match with input.")
    if(zoo::index(sr.input)[length(sr.input)]-zoo::index(input)[length(input)]!=0)stop("Invalid sr.input, timestamp end does not match with input.")
    if(zoo::index(vpd.input)[1]-zoo::index(input)[1]!=0)stop("Invalid vpd.input, timestamp start does not match with input.")
    if(zoo::index(vpd.input)[length(vpd.input)]-zoo::index(input)[length(input)]!=0)stop("Invalid vpd.input, timestamp end does not match with input.")

    #d
    if(missing(ed.window_min)){ed.window_min<-60*2/15}
    if(missing(ed.window_max)){ed.window_max<-60*4/15}
    if(missing(criteria.vpd_min)){criteria.vpd_min<-0.05}
    if(missing(criteria.vpd_max)){criteria.vpd_max<-0.5}
    if(missing(criteria.sr_mean)){criteria.sr_mean <-30}
    if(missing(criteria.sr_range)){criteria.sr_range<-30} #in percentage
    if(missing(criteria.cv_min)){criteria.cv_min<-0.5}
    if(missing(criteria.cv_max)){criteria.cv_max<-1}

    #e
    if(is.numeric(ed.window_min)==F)stop("Unused argument, ed.window_min is not numeric.")
    if(is.numeric(ed.window_max)==F)stop("Unused argument, ed.window_max is not numeric.")
    if(is.numeric(criteria.vpd_min)==F)stop("Unused argument, criteria.vpd_min is not numeric.")
    if(is.numeric(criteria.vpd_max)==F)stop("Unused argument, criteria.vpd_max is not numeric.")
    if(is.numeric(criteria.sr_mean)==F)stop("Unused argument, criteria.sr_mean is not numeric.")
    if(is.numeric(criteria.sr_range)==F)stop("Unused argument, criteria.sr_range is not numeric.")
    if(is.numeric(criteria.cv_min)==F)stop("Unused argument, criteria.cv_min is not numeric.")
    if(is.numeric(criteria.cv_max)==F)stop("Unused argument, criteria.cv_max is not numeric.")
    if(is.integer(ed.window_min)==F)stop("Unused argument, ed.window_min is not integer.")
    if(is.integer(ed.window_max)==F)stop("Unused argument, ed.window_max is not integer.")
    if(ed.window_min<0)stop("Unused argument, ed.window_min < 0.")
    if(ed.window_max<0)stop("Unused argument, ed.window_max < 0.")
    if(criteria.vpd_min<0)stop("Unused argument, criteria.vpd_min < 0.")
    if(criteria.vpd_max<0)stop("Unused argument, criteria.vpd_max < 0.")
    if(criteria.sr_mean<0)stop("Unused argument, criteria.sr_mean < 0.")
    if(criteria.sr_range<0)stop("Unused argument, criteria.sr_range < 0.")
    if(criteria.cv_min<0)stop("Unused argument, criteria.cv_min < 0.")
    if(criteria.cv_max<0)stop("Unused argument, criteria.cv_max < 0.")
    if(ed.window_min<0)stop("Unused argument, ed.window_min < 0.")
    if(ed.window_max<0)stop("Unused argument, ed.window_max < 0.")


    #adding environmental data
    env<-cbind(sr.input,vpd.input,input)

    #convert input to a tibble
    minutes<-as.numeric(left(right(as.character(zoo::index(env)),8),2))*60+as.numeric(left(right(as.character(zoo::index(env)),5),2))
    days<-as.numeric(floor(difftime(zoo::index(env),as.Date(zoo::index(env)[1]),units="days"))+1)

    #w= warnings
    if(max(days)>365)warning("Input length > 365 days which can significantly recude processing speed.")

    #
    raw.env <- tibble::tibble(
      days = days,
      days.agg =days,
      minutes = minutes,
      value = as.numeric(as.character(env$input)),
      sr= as.numeric(as.character(env$sr)),
      vpd= as.numeric(as.character(env$vpd))
    )

    if(nrow(raw.env)==0)stop("Invalid environmental input data, input.sr or input.vpd do not cover the temporal range of input.")

    #p= processing
    criteria.sr_min<- criteria.sr_mean-(criteria.sr_mean*(criteria.sr_range/100))
    criteria.sr_max<- criteria.sr_mean+(criteria.sr_mean*(criteria.sr_range/100))

    A <- lhs::randomLHS(n,9)
    B <- matrix(nrow = nrow(A), ncol = ncol(A))
    B[,1] <- ceiling(stats::qunif(A[,1], min =-(range.end-1)/2, max = range.end/2))
    B[,2] <- ceiling(stats::qunif(A[,2], min =-(range.start-1)/2, max = range.start/2))
    B[,3] <- msm::qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
    B[,4] <- stats::qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
    B[,5] <- stats::qnorm(A[,5], mean = b_mu, sd= b_sd)
    B[,6] <- ceiling(stats::qunif(A[,6], min =ed.window_min-1, max = ed.window_max))
    B[,7] <- stats::qunif(A[,7], min =criteria.vpd_min, max = criteria.vpd_max)
    B[,8] <- stats::qunif(A[,8], min =criteria.sr_min, max = criteria.sr_max)
    B[,9] <- stats::qunif(A[,9], min =criteria.cv_min, max = criteria.cv_max)
    colnames(B)<-c("zero.end","zero.start","sw.cor","a","b","ed.window","criteria.vpd","criteria.sr","criteria.cv")
    X1<-data.frame(B)

    A <- lhs::randomLHS(n,9)
    B <- matrix(nrow = nrow(A), ncol = ncol(A))
    B[,1] <- ceiling(stats::qunif(A[,1], min =-(range.end-1)/2, max = range.end/2))
    B[,2] <- ceiling(stats::qunif(A[,2], min =-(range.start-1)/2, max = range.start/2))
    B[,3] <- msm::qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
    B[,4] <- stats::qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
    B[,5] <- stats::qnorm(A[,5], mean = b_mu, sd= b_sd)
    B[,6] <- ceiling(stats::qunif(A[,6], min =ed.window_min-1, max = ed.window_max))
    B[,7] <- stats::qunif(A[,7], min =criteria.vpd_min, max = criteria.vpd_max)
    B[,8] <- stats::qunif(A[,8], min =criteria.sr_min, max = criteria.sr_max)
    B[,9] <- stats::qunif(A[,9], min =criteria.cv_min, max = criteria.cv_max)
    colnames(B)<-c("zero.end","zero.start","sw.cor","a","b","ed.window","criteria.vpd","criteria.sr","criteria.cv")
    X2<-data.frame(B)

    x <- sensitivity::soboljansen(model = NULL , X1, X2, nboot = n)
    B<-x$X
    n<-nrow(B)
    #p= process
    doParallel::registerDoParallel(cores = parallel::detectCores() - 2)
    cl<-parallel::makeCluster(parallel::detectCores() - 2)
    doSNOW::registerDoSNOW(cl)
    pb <- utils::txtProgressBar(max = nrow(B), style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    output<-foreach(i=c(1:n),.combine="rbind",.packages=c("tibble","dplyr","zoo"),.options.snow = opts)%dopar%{
      utils::setTxtProgressBar(pb, i)
      ze<-zero.end+(B[i,1]*stats::median(diff(minutes)))
      if(ze<0){ze<-24*60-ze}
      if(ze>24*60){ze<-ze-24*60}
      zs<-zero.start+(B[i,2]*stats::median(diff(minutes)))
      if(zs<0){zs<-24*60-zs}
      if(zs>24*60){zs<-zs-24*60}

      #dt max
      proc.1<-raw.input
      if(ze>zs){proc.1[which(proc.1$minutes>ze|minutes<zs),"dt.max"]<-NA}
      if(ze<zs){proc.1[which(proc.1$minutes>ze&minutes<zs),"dt.max"]<-NA
      offset<-(60*24/stats::median(diff(proc.1$minutes)))-ceiling(zs/stats::median(diff(proc.1$minutes)))+1
      proc.1$days.agg<-c(proc.1$days[(offset:length(proc.1$days))],rep(max(proc.1$days),offset-1))}
      #View(proc.1)
      if(ze==zs)stop(paste("Unused argument, zero.start and zero.end are too close together.",sep=""))
      add<-tibble::tibble(
        days.add = stats::aggregate(stats::na.omit(proc.1)$dt.max,by=list(stats::na.omit(proc.1)$days.agg),max,na.rm=TRUE)[,1],
        ddt.max=stats::aggregate(stats::na.omit(proc.1)$dt.max,by=list(stats::na.omit(proc.1)$days.agg),max,na.rm=TRUE)[,2]
      )

      #adding environmental data
      raw.env$sr.roll   <-zoo::rollmean(raw.env$sr,B[i,"ed.window"],align=c("right"),na.rm=TRUE,fill=NA)
      raw.env$sr.roll   <-zoo::na.locf(raw.env$sr.roll,fromLast=T)
      raw.env$vpd.roll  <-zoo::rollmean(raw.env$vpd,B[i,"ed.window"],align=c("right"),na.rm=TRUE,fill=NA)
      raw.env$vpd.roll  <-zoo::na.locf(raw.env$vpd.roll,fromLast=T)
      raw.env$value_sd  <-zoo::rollapply(raw.env$value, width = B[i,"ed.window"], FUN = stats::sd, align = "right",na.rm=TRUE,fill=NA)
      raw.env$value_sd  <-zoo::na.locf(raw.env$value_sd,fromLast=T)
      raw.env$value_mean<-zoo::rollapply(raw.env$value, width = B[i,"ed.window"], FUN = base::mean, align = "right",na.rm=TRUE,fill=NA)
      raw.env$value_mean<-zoo::na.locf(raw.env$value_mean,fromLast=T)
      raw.env$cv.roll   <-raw.env$value_sd/raw.env$value_mean*100

      proc.2<-dplyr::full_join(proc.1,add,by=c("days.agg"="days.add"))
      proc.2[which(proc.2$ddt.max!=proc.2$dt.max|is.na(proc.2$dt.max)==TRUE),"ddt.max"]<-NA
      proc.2[which(is.na(proc.2$ddt.max)==FALSE),"gap"]<-NA

      #e
      if(max(proc.2$days)!=max(raw.env$days))stop("Invalid environmental input data, input.sr or input.vpd do not fully cover the temporal range of input.")
      if(proc.2$minutes[length(proc.2$minutes)]!=raw.env$minutes[length(raw.env$minutes)])stop("Invalid environmental input data, input.sr or input.vpd do not fully cover the temporal range of input.")

      #select the dt.max values from the
      proc.2[,"ddt.max.raw"]<- proc.2[,"ddt.max"]
      proc.2[which(raw.env$sr.roll>B[i,"criteria.sr"]),"ddt.max"]<-NA  #remove values above the solar irradiance
      proc.2[which(raw.env$vpd.roll>B[i,"criteria.vpd"]),"ddt.max"]<-NA  #remove values above the solar irradiance
      proc.2[which(raw.env$cv.roll>B[i,"criteria.cv"]),"ddt.max"]<-NA  #remove values above the solar irradiance

      #adding daily values
      ddtmax<-suppressWarnings(stats::aggregate((proc.2)$ddt.max,by=list((proc.2)$days.agg),max,na.rm=TRUE))[,2]
      ddtmax[which(ddtmax=="-Inf")]<-NA
      length(ddtmax)
      ddtmax<-zoo::na.locf(zoo::na.locf(zoo::na.approx(ddtmax,na.rm=F),na.rm=F),fromLast=T)
      add2<-tibble::tibble(
        days.add = suppressWarnings(stats::aggregate((proc.2)$ddt.max,by=list((proc.2)$days.agg),max,na.rm=TRUE))[,1],
        ddt.max.add= ddtmax
      )
      proc.add<-dplyr::full_join(proc.2,add2,by=c("days.agg"="days.add"))
      proc.add[which(is.na(proc.add$ddt.max.raw)==T),"ddt.max.add"]<-NA
      nrow(proc.add)
      nrow(proc.2)
      proc.2$ddt.max<-proc.add$ddt.max.add
      proc.2$gap<-stats::ave(proc.2$gap, rev(cumsum(rev(is.na(proc.2$gap)))), FUN=cumsum)*stats::median(diff(proc.1$minutes))
      proc.2$ddt.max<-zoo::na.locf(zoo::na.locf(proc.2$ddt.max,na.rm=F),fromLast=T)
      proc.2[which(is.na(proc.2$value)==TRUE|proc.2$gap>(60*(24+12))|proc.2$count!=stats::median(proc.2$count,na.rm=TRUE)),"ddt.max"]<-NA

      #HW correction
      #i<-4
      swt<-B[i,3]
      if(probe.length<=swt){
        proc.2$k.pd                 <-((proc.2$ddt.max-proc.2$value)/proc.2$value)
        proc.2$k.pd[which(proc.2$k.pd<0)]<-0
      }
      if(probe.length>swt){
        proc.2$k.pd<-((proc.2$ddt.max-((proc.2$value-(1-(swt/probe.length))*proc.2$ddt.max)/(swt/probe.length)))/((proc.2$value-(1-(swt/probe.length))*proc.2$ddt.max)/(swt/probe.length)))
        proc.2$k.pd[which(proc.2$k.pd<0)]<-0
      }

      #still need to consider whether we include dampening
      a<-B[i,4] #48.25036#
      b<- B[i,5] #1.177099#
      proc.2$sfd<-a*proc.2$k.pd^b

      #think about relevant output proxies
      d.sum.k<-suppressWarnings(stats::aggregate(proc.2$k.pd,by=list(proc.2$days),max,na.rm=TRUE))
      d.sum.k[which(d.sum.k[,2]=="-Inf"),2]<-NA
      durat.k<-proc.2
      durat.k[which(proc.2$k.pd<=min.k),"k.pd"]<-NA
      durat.k<-stats::aggregate(durat.k$k.pd, by=list(durat.k$days), FUN=function(x) {length(stats::na.omit(x))})
      values.k<-stats::aggregate(proc.2$k.pd,by=list(proc.2$days),mean,na.rm=TRUE)*24
      values.k[values.k[,2]=="NaN",2]<-NA

      durat<-proc.2
      durat[which(proc.2$sfd<=min.sfd),"sfd"]<-NA
      durat<-stats::aggregate(durat$sfd, by=list(durat$days), FUN=function(x) {length(stats::na.omit(x))})
      values<-stats::aggregate(proc.2$sfd,by=list(proc.2$days),mean,na.rm=TRUE)*24
      values[values[,2]=="NaN",2]<-NA
      d.sum<-suppressWarnings(stats::aggregate(proc.2$sfd,by=list(proc.2$days),max,na.rm=TRUE))
      d.sum[which(d.sum[,2]=="-Inf"),2]<-NA
      return(
        c(c(i,mean(values[,2],na.rm=TRUE),(stats::sd(d.sum[,2],na.rm=TRUE)/mean(d.sum[,2],na.rm=TRUE))*100,(mean(durat[,2]*stats::median(diff(proc.2$minutes)))/60),
            mean(values.k[,2],na.rm=TRUE),(stats::sd(d.sum.k[,2],na.rm=TRUE)/mean(d.sum.k[,2],na.rm=TRUE))*100,(mean(durat.k[,2]*stats::median(diff(proc.2$minutes)))/60)),
          c(length(proc.2$sfd),proc.2$sfd,proc.2$k.pd))
      )
    }
    doParallel::stopImplicitCluster()

    #time series output
    number<-stats::median(output[,8])
    output.sfd<-output[,c(9:(8+number))]
    output.k<-output[,c((9+number):(8+number+number))]
    n<-nrow(output.k)
    output.sfd<-data.frame(mu=apply(output.sfd,2,mean,na.rm=T),sd=apply(output.sfd,2,sd,na.rm=T),ci.max=apply(output.sfd,2,stats::quantile,probs=c(0.975),na.rm=T),ci.min=apply(output.sfd,2,stats::quantile,probs=c(0.025),na.rm=T))
    output.k<-data.frame(mu=apply(output.k,2,mean,na.rm=T),sd=apply(output.k,2,sd,na.rm=T),ci.max=apply(output.k,2,quantile,probs=c(0.975),na.rm=T),ci.min=apply(output.k,2,stats::quantile,probs=c(0.025),na.rm=T))
    output.sfd<-zoo::zoo(output.sfd,order.by=zoo::index(input))
    output.k<-zoo::zoo(output.k,order.by=zoo::index(input))

    if(df==T){
      output.sfd<-zoo::fortify.zoo(output.sfd)
      output.k<-zoo::fortify.zoo(output.k)
      colnames(output.sfd)<-c("timestamp","mu","sd","ci.max","ci.min")
      colnames(output.k)<-c("timestamp","mu","sd","ci.max","ci.min")
    }

    #o= output
    output<-output[,c(1:7)]
    output_sum<-sensitivity::tell(x,as.numeric(output[,2]))
    output_cv<-sensitivity::tell(x,output[,3])
    output_length<-sensitivity::tell(x,output[,4])

    output_sum.k<-sensitivity::tell(x,output[,5])
    output_cv.k<-sensitivity::tell(x,output[,6])
    output_length.k<-sensitivity::tell(x,output[,7])

    output.tot<-data.frame(item=c(row.names(output_sum$T),"daily.sum",row.names(output_cv$T),"max.cv",row.names(output_length$T),"length.dur"),
                           class=c(rep("param.sum",length(row.names(output_sum$T))),"stat.sum",rep("param.cv",length(row.names(output_cv$T))),"stat.cv",rep("param.length",length(row.names(output_length$T))),"stat.length"),
                           factor="SFD",
                           mean=c(output_sum$T[,1],mean(output[,2]),output_cv$T[,1],mean(output[,3]),output_length$T[,1],mean(output[,4])),
                           sd=c(output_sum$T[,3],stats::sd(output[,2]),output_cv$T[,3],stats::sd(output[,3]),output_length$T[,3],stats::sd(output[,4])),
                           ci.min=c(output_sum$T[,4],stats::quantile(output[,2],probs=0.025,na.rm=T),output_cv$T[,4],stats::quantile(output[,3],probs=0.025,na.rm=T),output_length$T[,4],stats::quantile(output[,4],probs=0.025,na.rm=T)),
                           ci.max=c(output_sum$T[,5],stats::quantile(output[,2],probs=0.975,na.rm=T),output_cv$T[,5],stats::quantile(output[,3],probs=0.975,na.rm=T),output_length$T[,5],stats::quantile(output[,4],probs=0.975,na.rm=T))
    )
    output.tot.k<-data.frame(item=c(row.names(output_sum.k$T),"daily.sum",row.names(output_cv.k$T),"max.cv",row.names(output_length.k$T),"length.dur"),
                             class=c(rep("param.sum",length(row.names(output_sum.k$T))),"stat.sum",rep("param.cv",length(row.names(output_cv.k$T))),"stat.cv",rep("param.length",length(row.names(output_length.k$T))),"stat.length"),
                             factor="K",
                             mean=c(output_sum.k$T[,1],mean(output[,5]),output_cv.k$T[,1],mean(output[,6]),output_length.k$T[,1],mean(output[,7])),
                             sd=c(output_sum.k$T[,3],stats::sd(output[,5]),output_cv.k$T[,3],stats::sd(output[,6]),output_length.k$T[,3],stats::sd(output[,7])),
                             ci.min=c(output_sum.k$T[,4],stats::quantile(output[,5],probs=0.025,na.rm=T),output_cv.k$T[,4],stats::quantile(output[,6],probs=0.025,na.rm=T),output_length.k$T[,4],stats::quantile(output[,7],probs=0.025,na.rm=T)),
                             ci.max=c(output_sum.k$T[,5],stats::quantile(output[,5],probs=0.975,na.rm=T),output_cv.k$T[,5],stats::quantile(output[,6],probs=0.975,na.rm=T),output_length.k$T[,5],stats::quantile(output[,7],probs=0.975,na.rm=T))
    )


    if(make.plot==T){
      graphics::layout(
        matrix(
          c(5,5,1,1,1,2,
            5,5,1,1,1,3,
            5,5,1,1,1,4),
          ncol=6, byrow = TRUE))
      #make output for the package
      graphics::par(oma=c(6,3,3,3))
      graphics::par(mar=c(0,2,0,0))
      graphics::plot(1,1,xlim=c(0.7,ncol(B)+0.3),ylim=c(0,1.2),xaxt="n",xlab="",ylab="",yaxt="n",col="white",cex.lab=1.2)
      graphics::abline(h=seq(0,1,0.2),col="grey")
      graphics::abline(v=seq(0,ncol(B),1),col="grey")
      graphics::axis(side=2,las=2,labels=F)
      lab<-colnames(B)
      graphics::axis(side=1,at=c(1:ncol(B)),labels=lab,las=2)
      graphics::legend("topright","SFD",bty="n",text.font=2,cex=2)
      graphics::mtext(side=3,"Environmental dependent",outer=TRUE,padj=-0.5)
      pal<-data.frame(class=c("param.sum","param.cv","param.length"),col=c("darkorange","cyan","darkblue"))
      loc<-data.frame(item=lab,loc=c(1:length(lab)))
      off<-data.frame(class=c("param.sum","param.cv","param.length"),off=c(-0.2,0,0.2))
      for(i in c(1:nrow(output.tot))){
        sel<-output.tot[i,]
        if(left(sel$class,4)=="stat"){next}
        graphics::lines(rep(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],2),
                        c(sel$ci.min,sel$ci.max))
        graphics::points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
                         sel$mean,
                         pch=16,col=as.character(pal[which(as.character(pal$class)==as.character(sel$class)),"col"]),cex=1.5)
        graphics::points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
                         sel$mean,
                         pch=1,cex=1.5)
      }
      graphics::par(mar=c(0,0,0,3))
      graphics::plot(output.tot[which(output.tot$class=="stat.sum"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.sum"),"ci.max"]))
      graphics::axis(side=4,las=2)
      graphics::lines(c(1,1),c(output.tot[which(output.tot$class=="stat.sum"),"ci.min"],output.tot[which(output.tot$class=="stat.sum"),"ci.max"] ))
      graphics::points(1,output.tot[which(output.tot$class=="stat.sum"),"mean"],pch=16,cex=1.5,col="darkorange")
      graphics::points(1,output.tot[which(output.tot$class=="stat.sum"),"mean"],pch=1,cex=1.5)
      graphics::mtext(side=4,expression("Sum ("*cm^3*" "*cm^-2*" "*d^-1*")"),padj=2.5,cex=0.8)
      graphics::par(mar=c(0.2,0,0.2,3))
      graphics::plot(output.tot[which(output.tot$class=="stat.cv"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.cv"),"ci.max"]))
      graphics::axis(side=4,las=2)
      graphics::lines(c(1,1),c(output.tot[which(output.tot$class=="stat.cv"),"ci.min"],output.tot[which(output.tot$class=="stat.cv"),"ci.max"] ))
      graphics::points(1,output.tot[which(output.tot$class=="stat.cv"),"mean"],pch=16,cex=1.5,col="cyan")
      graphics::points(1,output.tot[which(output.tot$class=="stat.cv"),"mean"],pch=1,cex=1.5)
      graphics::mtext(side=4,expression("CV (%)"),padj=3,cex=0.8)
      graphics::par(mar=c(0,0,0,3))
      graphics::plot(output.tot[which(output.tot$class=="stat.length"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.length"),"ci.max"]))
      graphics::axis(side=4,las=2)
      graphics::lines(c(1,1),c(output.tot[which(output.tot$class=="stat.length"),"ci.min"],output.tot[which(output.tot$class=="stat.length"),"ci.max"] ))
      graphics::points(1,output.tot[which(output.tot$class=="stat.length"),"mean"],pch=16,cex=1.5,col="darkblue")
      graphics::points(1,output.tot[which(output.tot$class=="stat.length"),"mean"],pch=1,cex=1.5)
      graphics::mtext(side=4,expression("Duration (h)"),padj=3,cex=0.8)

      graphics::par(mar=c(0,4,0,0))
      lab<-colnames(B)
      lab<-lab[-which(lab%in%c("a","b"))]
      graphics::plot(1,1,xlim=c(0.7,length(lab)+0.3),ylim=c(0,1.2),xaxt="n",ylab="Total sensitivity index (-)",xlab="",yaxt="n",col="white",cex.lab=1.2)
      graphics::abline(h=seq(0,1,0.2),col="grey")
      graphics::abline(v=seq(0,ncol(B),1),col="grey")
      graphics::axis(side=2,las=2)
      graphics::axis(side=1,at=c(1:length(lab)),labels=lab,las=2)
      graphics::legend("topleft",horiz=F,c("Sum","CV","Duration"),pch=16,col=c("darkorange","cyan","darkblue"),bty="n",cex=1.2)
      graphics::legend("topright","K",bty="n",text.font=2,cex=2)
      pal<-data.frame(class=c("param.sum","param.cv","param.length"),col=c("darkorange","cyan","darkblue"))
      loc<-data.frame(item=lab,loc=c(1:length(lab)))
      off<-data.frame(class=c("param.sum","param.cv","param.length"),off=c(-0.2,0,0.2))

      output.tot.k.gr<-output.tot.k[-which(output.tot.k$item%in%c("a","b")),]
      for(i in c(1:nrow(output.tot.k.gr))){
        #i<-1
        sel<-output.tot.k.gr[i,]
        if(left(sel$class,4)=="stat"){next}
        graphics::lines(rep(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],2),
                        c(sel$ci.min,sel$ci.max))
        graphics::points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
                         sel$mean,
                         pch=16,col=as.character(pal[which(as.character(pal$class)==as.character(sel$class)),"col"]),cex=1.5)
        graphics::points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
                         sel$mean,
                         pch=1,cex=1.5)
      }
    }

    output.tot.k.gr<-output.tot.k.gr[-which(left(output.tot.k.gr$class,4)=="stat"),]
    output.tot.gr<-output.tot[-which(left(output.tot$class,4)=="stat"),]
    output.tot.k.gr$analysis<-rep("sensitivity",nrow(output.tot.k.gr))
    output.tot.gr$analysis<-rep("sensitivity",nrow(output.tot.gr))
    output.sen<-output.tot[which(left(output.tot$class,4)=="stat"),]
    output.sen$analysis<-rep("uncertainty",nrow(output.sen))
    output.data<-rbind(output.tot.k.gr,output.tot.gr,output.sen)

    #write.table(output.data,"D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX/output.sen.pd.txt",col.names=T,row.names=F,sep="\t")

    #uncertainty parameters
    param<-data.frame(method=method,start.input=as.character(zoo::index(input)[1]),end.input=as.character(zoo::index(input)[length(input)]),
                      zero.start=zero.start,zero.end=zero.end,range.start=range.start,range.end=range.end,
                      sw.cor=sw.cor,sw.sd=sw.sd,log.a_mu=log.a_mu,log.a_sd=log.a_sd,b_mu=b_mu,b_sd=b_sd,
                      ed.window_min=ed.window_min,ed.window_max=ed.window_max,criteria.vpd_min=criteria.vpd_min,
                      criteria.vpd_max=criteria.vpd_max,criteria.sr_mean=criteria.sr_mean,
                      criteria.sr_range=criteria.sr_range,criteria.cv_min=criteria.cv_min,
                      criteria.cv_max=,criteria.cv_max,
                      min.sfd=min.sfd,min.k=min.k,n=n)
    if(is.numeric(sw.cor)==F)stop("Unused argument, sw.cor is not numeric.")

    output.all<-list(
      output.data,
      output.sfd,
      output.k,
      param
    )
    names(output.all)<-c("output.data","output.sfd","output.k","param")
    return(output.all)
  }
}
