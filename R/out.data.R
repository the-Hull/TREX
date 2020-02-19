#' Generation of TDM output
#'
#' @description Generates relevant output from the sap flux density (\eqn{SFD}) values.
#' This function provides both \eqn{F_{d}}{Fd} (\eqn{SFD} expressed in \eqn{mmol~m^{-2}s^{-1}}{mmol m-2 s-1}) and crown conductance values
#' (\eqn{G_{C}}{Gc}; Meinzer \emph{et al.} 2013, Pappas \emph{et al.} 2018, Peters \emph{et al.} 2018); an analogue to stomatal conductance) in an easily exportable format.
#' Additionally, the function can perform environmental filtering on \eqn{F_{d}}{Fd} and \eqn{G_{C}}{Gc} and model \eqn{G_{C}}{Gc} sensitivity to vapour pressure deficit (\eqn{VPD}).
#' The user can choose between in- (\code{method = “env.filt”}) or excluding (\code{method = “stat”}) environmental filtering
#' on the \eqn{G_{C}}{Gc} and adjust the filter threshold manually.
#'
#' @param input An \code{\link{is.trex}}-compliant time series from \code{\link{tdm_cal.sfd}} outputs
#' (e.g., \code{X$sfd.mw$sfd})
#' @param vpd.input An \code{\link{is.trex}}-compliant object an individual series of \eqn{VPD} in \eqn{kPa} (see \code{\link{vpd}}).
#' The extent and temporal resolution should be equal to input.
#' Use \code{\link{dt.steps}} to correct if needed.
#' @param sr.input  An \code{\link{is.trex}}-compliant object of an individual series of solar irradiance
#'  (e.g. either PAR or global radiation; see \code{\link{sr}}).
#'   The extent and temporal resolution should be equal to input. Use \code{\link{dt.steps}} to correct if needed.
#'   This data is only needed when applying the \code{“env.filt”} method.
#' @param prec.input An \code{\link{is.trex}}-compliant object of daily precipitation in \eqn{mm~d^{-1}}{mm d-1} (see \code{\link{preci}}).
#'  The extent should be equal to input with a daily temporal resolution.
#'   Use \code{\link{dt.steps}} to correct if needed.
#'   This data is only needed when applying the \code{“env.filt”} method.
#' @param peak.hours Numeric vector with hours which should be considered as peak-of-the-day hours
#'  (default = \code{c(10:14)}).
#'  This variable is only needed when the \code{“stat”} method is selected.
#' @param low.sr Numeric threshold value in the unit of the \code{sr.input} time-series (e.g., \eqn{W~m^{-2}}{W m-2})
#'  to exclude cloudy days which impact \eqn{G_{C}}{Gc} (default = 150  \eqn{W~m^{-2}}{W m-2}).
#'   This variable is only needed when the \code{“env.filt”} method is selected.
#' @param peak.sr Numeric threshold value in the unit of the sr.input time-series (e.g.,  \eqn{W~m^{-2}}{W m-2})
#'  to exclude hours which are not considered as peak-of-the-day hours (default = 300  \eqn{W~m^{-2}}{W m-2}).
#'  This variable is only needed when the “\code{env.filt”} method is selected.
#' @param vpd.cutoff Numeric threshold value in \eqn{kPa} for peak-of-the-day mean \eqn{VPD} to eliminate unrealistic
#'  and extremely high values of \eqn{G_{C}}{Gc} due to low \eqn{VPD} values or high values of \eqn{G_{C}}{Gc} (default = 0.5 \eqn{kPa}).
#' @param prec.lim Numeric threshold value in \eqn{mm~d^{-1}}{mm d-1}  for daily precipitation to remove rainy days (default = 1 \eqn{mm~d^{-1}}{mm d-1}).
#'  This variable is only needed when \code{“env.filt”} method is selected.
#' @param method Character string indicating whether precipitation and solar irradiance data should be used
#'  to determined peak-of-the-day \eqn{G_{C}}{Gc} values and filter the daily \eqn{G_{C}}{Gc} values (“env.filt”)
#'   or not (“stat”; default). When \code{“env.filt”} is selected, \code{input}, \code{vpd.input}, \code{sr.input}, \code{prec.input},
#'    \code{peak.sr}, \code{low.sr}, \code{vpd.cutoff} and \code{prec.lim} have to be provided.
#'    When \code{“stat”} is selected only \code{input}, \code{vpd.input} and \code{peak.hours}.
#' @param max.quant Numeric, defining the quantile of the \eqn{G_{C}}{Gc} data which should be considered as GC.max (default = 1).
#' @param make.plot Logical; if \code{TRUE}, a plot is generated presenting the response of \eqn{G_{C}}{Gc} to \eqn{VPD}.
#'
#' @details Various relevant outputs can be derived from the \eqn{SFD} data.
#' This function provides the option to recalculate \eqn{SFD} to \eqn{F_{d}}{Fd} (expressed in \eqn{mmol~m^{-2}s^{-1}}{mmol m-2 s-1})
#' and crown conductance (according to Pappas \emph{et al.} 2018).
#' \eqn{G_{C}}{Gc} is estimated per unit sapwood area, where \eqn{G_{C} = F_{d} / VPD}{GC = Fd / VPD} (in kPa), assuming that
#' i) the stem hydraulic capacitance between the height of sensor and the leaves is negligible, and
#' ii) that the canopy is well coupled to the atmosphere. In order to reduce the effect of stem hydraulic capacitance,
#' peak-of-the-day \eqn{G_{C}}{Gc} are solely considered for calculating daily average \eqn{G_{C}}{Gc}.
#' Peak-of-the-day conditions are defined by \code{peak.hours} or \code{peak.sr}. Moreover, to analyse the relationship between \eqn{G_{C}}{Gc}
#'  and environmental measurements (e.g., \eqn{VPD}), the daily mean peak-of-the-day \eqn{G_{C}}{Gc} values can be restricted to:
#'  i) non-cloudy days (see \code{low.sr}), to reduce the impact of low irradiance on \eqn{G_{C}}{Gc},
#'  ii) non-rainy days (see \code{prec.lim}), as wet leaves are not well coupled to the atmosphere, and
#'  iii) daily mean peak-of-the-day \eqn{G_{C}}{Gc} great then a threshold (see \code{vpd.cutoff}),
#'  to eliminate unrealistically high \eqn{G_{C}}{Gc} values due to low \eqn{F_{d}}{Fd} or \eqn{VPD} values (when method = \code{“env.filt”}).
#'  Moreover, the sensitivity of the daily mean peak-of-the-day \eqn{G_{C}}{Gc} to \eqn{VPD} is modelled by fitting the following model:
#'
#'   \deqn{G_{C} = \alpha + \beta VPD^{-0.5}}{GC = \alpha + \beta VPD^{-0.5}}
#'
#'   Besides using the raw daily mean peak-of-the-day \eqn{G_{C}}{Gc} values, the function also applies
#'   a normalization where daily mean peak-of-the-day \eqn{G_{C}}{Gc} is standardized to the maximum conductance (GC.max; see \code{max.quant}).
#'
#'
#'
#' @return A named list of \code{data.frame} objects,
#'  containing the following items:
#'
#' \describe{
#'    \item{raw}{A \code{data.frame} containing the input data and filtered values. Columns include the timestamp [,“timestamp”]
#' (e.g., “2012-01-01 00:00:00”), year of the data [,“year”], day of year [,“doy”], input solar radiance data [,“sr”],
#'  daily average radiance data [,“sr”], input vapour pressure deficit data [,“vpd”], isolated peak-of-the-day vapour pressure
#'  deficit values [,“vpd.filt”], input daily precipitation [,“prec.day”], sap flux density expressed in \eqn{mmol~m^{-2}s^{-1}}{mmol m-2 s-1} [,“fd”],
#'  crown conductance expressed in \eqn{mmol~m^{-2}s^{-1}~kPa^{-1}}{mmol m-2 s-1 kPa-1} [,“gc”], and the filtered crown conductance [,“gc.filt”]}
#'
#'  \item{peak.mean}{A \code{data.frame} containing the daily mean crown conductance values.
#'  Columns include the timestamp [,“timestamp”] (e.g., “2012-01-01”), peak-of-the-day vapour pressure deficit [,“vpd.filt”],
#'   the filtered crown conductance \eqn{mmol~m^{-2}s^{-1}~kPa^{-1}}{mmol m-2 s-1 kPa-1} [,“gc.filt”],
#'    and the normalized crown conductance according
#'    to the maximum crown conductance [,“gc.norm”].}
#'
#'   \item{sum.mod}{A model summary object (see \code{\link{summary}()})
#'    of the model between \eqn{VPD} and \eqn{G_{C}}{Gc}.}
#'
#'
#'    \item{sum.mod.norm}{A model summary object (see \code{\link{summary}()})
#'    of the model between \eqn{VPD} and \eqn{G_{C}}{Gc}/\eqn{GC.max}.}
#'
#'
#'
#' }
#'
#' @references
#' Meinzer, F. C., D. R. Woodruff, D. M. Eissenstat,
#'  H. S. Lin, T. S. Adams, and K. A. McCulloh. 2013. Above-and belowground controls on water use by
#'  trees of different wood types in an eastern US deciduous forest.
#'  Tree Physiology, 33(4):345–356. \url{doi:10.1093/treephys/tpt012}.
#'
#'  Pappas, C. et al. 2018. Boreal tree hydrodynamics: asynchronous, diverging, yet complementary.
#'  Tree Physiololgy. 38(7):953–964. \url{doi:10.1093/treephys/tpy043}.
#'
#'  Peters, R. L., M. Speich, C. Pappas, A. Kahmen, G. Arx, E. Graf Pannatier,
#'  K. Steppe, K. Treydte, A. Stritih, and P. Fonti. 2018.
#'  Contrasting stomatal sensitivity to temperature and soil drought in mature
#'  alpine conifers, Plant, Cell & Environment. 42(5):1674-1689. \url{doi:10.1111/pce.13500}.

#'
#' @export
#'
#' @examples
#' \dontrun{
#' #Gc response function
#' #Gc response function
#' raw   <- is.trex(example.data(type="doy"), tz="GMT",
#'                  time.format="%H:%M", solar.time=TRUE,
#'                  long.deg=7.7459, ref.add=FALSE)
#'
#' input <- dt.steps(input=raw, start="2013-05-01 00:00", end="2013-11-01 00:00",
#'                    time.int=15, max.gap=60, decimals=10, df=FALSE)
#'
#' input[which(input<0.2)]<- NA
#' input <- tdm_dt.max(input, methods=c("dr"), det.pd=TRUE, interpolate=FALSE,
#'                  max.days=10, df=FALSE)
#'
#' output.data<- tdm_cal.sfd(input,make.plot=TRUE,df=FALSE,wood="Coniferous")
#'
#' input<- output.data$sfd.dr$sfd
#'
#' output<- out.data(input=input, vpd.input=vpd, sr.input=sr, prec.input=preci,
#'                   low.sr = 150, peak.sr=300, vpd.cutoff= 0.5, prec.lim=1,
#'                   method="env.filt", max.quant=0.99, make.plot=TRUE)
#' head(output)
#'
#' }
#'
out.data<-function(input,vpd.input,sr.input,prec.input,peak.hours=c(10:14),low.sr = 150,
                   peak.sr=300,vpd.cutoff= 0.5,prec.lim=1,method="env.filt",max.quant=1,make.plot=TRUE){


  #f= small functions
  left = function(string, char){
    substr(string, 1,char)}
  right = function (string, char){
    substr(string,nchar(string)-(char-1),nchar(string))
  }

  #d= default conditions
  if(missing(method)){method="stat"}
  if(missing(peak.hours)){peak.hours=c(10:14)}
  if(missing(low.sr)){low.sr=150}
  if(missing(peak.sr)){peak.sr=300}
  if(missing(vpd.cutoff)){vpd.cutoff=0.5}
  if(missing(prec.lim)){prec.lim=1}
  if(missing(make.plot)){make.plot=F}
  if(missing(max.quant)){max.quant=1}

  #e
  if(method!="stat"&method!="env.filt")stop("Unused argument, method needs to be a character of stat|env.filt.")
  if(is.numeric(low.sr)==F)stop("Unused argument, low.sr needs to be numeric.")
  if(is.numeric(max.quant)==F)stop("Unused argument, max.quant needs to be numeric.")
  if(max.quant<0)stop("Unused argument, max.quant is smaller than 0.")
  if(max.quant>1)stop("Unused argument, max.quant is larger than 1.")
  if(is.numeric(peak.sr)==F)stop("Unused argument, peak.sr needs to be numeric.")
  if(is.numeric(vpd.cutoff)==F)stop("Unused argument, vpd.cutoff needs to be numeric.")
  if(is.numeric(prec.lim)==F)stop("Unused argument, prec.lim needs to be numeric.")
  if(make.plot!=T&make.plot!=F)stop("Unused argument, make.plot needs to be TRUE|FALSE.")
  if(missing(vpd.input))stop("Invalid vpd.input data, missing numeric values.")

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

  #e= errors
  if(zoo::is.zoo(input)==F)stop("Invalid input data, use a zoo file from is.trex or a zoo vector containing numeric values (tz= UTC).")
  if(is.numeric(input)==F)stop("Invalid input data, values within the vector are not numeric.")

  #w= warnings
  if(difftime(zoo::index(input[length(input)]),zoo::index(input[1]),units=c("days"))<30){
    warning("Selected input has a temporal extend of <30 days.")
  }

  if(missing(vpd.input))stop("No vpd.input data included.")
  if(missing(sr.input)){
    warning(paste0("No sr.input data included."))
    sr.input<-zoo::zoo(0,order.by=zoo::index(input))}

  #e
  if(attributes(vpd.input)$class=="data.frame"){
    #e
    if(is.numeric(vpd.input$value)==F)stop("Invalid vpd.input data, values within the data.frame are not numeric.")
    if(is.character(vpd.input$timestamp)==F)stop("Invalid vpd.input data, timestamp within the data.frame are not numeric.")

    #p
    vpd.input<-zoo::zoo(vpd.input$value,order.by=base::as.POSIXct(vpd.input$timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC"))

    #e
    if(as.character(zoo::index(vpd.input)[1])=="(NA NA)"|is.na(zoo::index(vpd.input)[1])==T)stop("No timestamp present, time.format is likely incorrect for vpd.input.")
  }

  if(attributes(vpd.input)$class!="zoo")stop("Invalid input data, vpd.input must be a zoo file (use is.trex).")

  if(attributes(sr.input)$class=="data.frame"){
    #e
    if(is.numeric(sr.input$value)==F)stop("Invalid sr.input data, values within the data.frame are not numeric.")
    if(is.character(sr.input$timestamp)==F)stop("Invalid sr.input data, timestamp within the data.frame are not numeric.")

    #p
    sr.input<-zoo::zoo(sr.input$value,order.by=base::as.POSIXct(sr.input$timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC"))

    #e
    if(as.character(zoo::index(sr.input)[1])=="(NA NA)"|is.na(zoo::index(sr.input)[1])==T)stop("No timestamp present, time.format is likely incorrect for sr.input.")
  }
  if(attributes(sr.input)$class!="zoo")stop("Invalid input data, sr.input must be a zoo file (use is.trex).")

  if(method=="stat"){
    #d
    sr.input=input[]
    sr.input[]<-NA

    prec.input<-aggregate(input[],mean,na.rm=T,by=as.Date(zoo::index(input)))
    prec.input[]<-0
    prec.lim <-100
  }


  if(attributes(prec.input)$class=="data.frame"){
    #e
    if(is.numeric(prec.input$value)==F)stop("Invalid prec.input data, values within the data.frame are not numeric.")
    if(is.character(prec.input$timestamp)==F)stop("Invalid prec.input data, timestamp within the data.frame are not numeric.")

    #p
    prec.input<-zoo::zoo(prec.input$value,order.by=base::as.POSIXct(prec.input$timestamp,format="%Y-%m-%d",tz="UTC"))

    #e
    if(as.character(zoo::index(prec.input)[1])=="(NA NA)"|is.na(zoo::index(prec.input)[1])==T)stop("No timestamp present, time.format is likely incorrect for vpd.input.")
  }
  if(attributes(prec.input)$class!="zoo")stop("Invalid input data, prec.input must be a zoo file (use is.trex).")

  #e
  if(method=="env.filt"){
    if(mean(sr.input,na.rm=TRUE)=="NaN")stop("Invalid sr.input data, missing numeric values.")
    if((mean(prec.input,na.rm=TRUE)==0)==T)stop("Invalid prec.input data, missing numeric values.")
  }

  #p
  step.min<-as.numeric(min(difftime(zoo::index(input)[-1],zoo::index(input)[-length(input)],units=c("mins")),na.rm=TRUE))
  step.sr<-as.numeric(min(difftime(zoo::index(sr.input)[-1],zoo::index(sr.input)[-length(sr.input)],units=c("mins")),na.rm=TRUE))
  step.vpd<-as.numeric(min(difftime(zoo::index(vpd.input)[-1],zoo::index(vpd.input)[-length(vpd.input)],units=c("mins")),na.rm=TRUE))

  #w
  if(step.min!=step.sr|step.min!=step.vpd){
    warning(paste0("time steps between input and vpd.input/sr.input differ, results might not be correctly aggregated."))
  }

  #p
  sfd<-((input*10000/3600)/18.01528)*1000 #cm3 cm-2 h-1 to cm3 m-2 s-1 to mmol m-2 s-1
  doy<-zoo::zoo(lubridate::yday(zoo::index(input)), order.by=zoo::index(input))
  h  <-zoo::zoo(lubridate::hour(zoo::index(input)), order.by=zoo::index(input))
  y  <-zoo::zoo(lubridate::year(zoo::index(input)), order.by=zoo::index(input))
  doy_y<-zoo::zoo(as.numeric(paste(doy,y,sep="")), order.by=zoo::index(input))

  if(method=="stat"){
    #d
    sr.input=input[]
    sr.input[]<-NA

    prec.input<-stats::aggregate(input[],mean,na.rm=T,by=as.Date(zoo::index(input)))
    prec.input[]<-0
    prec.lim <-100

    #p
    sfd_df<-cbind(sfd,vpd.input,sr.input,doy,h,y,doy_y)
    sfd_df$gc<-sfd_df$sfd/sfd_df$vpd.input

    ##non-rainy days (P_daily < 1 mm)
    prec_df = as.data.frame(prec.input)
    prec_df$doy = lubridate::yday(zoo::index(prec.input))
    prec_df$y = lubridate::year(zoo::index(prec.input))
    prec_df$doy_y= as.numeric(paste(prec_df$doy,prec_df$y,sep=""))
    add<-data.frame(timestamp=as.character(zoo::index(sfd_df)),sfd=as.numeric(sfd_df$sfd),doy_y=as.numeric(sfd_df$doy_y))
    add2<-data.frame(prec=as.numeric(prec_df$prec.input),doy_y=as.numeric(prec_df$doy_y))
    adding<-merge(add,add2,by="doy_y")
    prec_day<-zoo::zoo(adding$prec,order.by=base::as.POSIXct(adding$timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC"))

    #add dummy
    sfd_df$sr_day<-sfd_df$sr.input
    sfd_df$sr_day<-NA
    sfd_df$prec_day<-prec_day
    sfd_df$prec_day<-NA
    sfd_df$gc_filt<-sfd_df$gc
    sfd_df$vpd_filt<-sfd_df$vpd.input
    sfd_df[which((sfd_df$h %in% peak.hours)==F),"gc_filt"]<-NA
    sfd_df[which((sfd_df$h %in% peak.hours)==F),"vpd_filt"]<-NA

    #aggregate to daily values
    sfd_df<-stats::window(sfd_df,start=zoo::index(sfd_df[which(is.na(sfd_df$sfd)==F),])[1],end=zoo::index(sfd_df[which(is.na(sfd_df$sfd)==F),])[nrow(sfd_df[which(is.na(sfd_df$sfd)==F),])])
    sfd_df_peak_daily = stats::aggregate(sfd_df[,],
                                         mean,
                                         na.rm=T,
                                         by=list(as.Date(zoo::index(sfd_df))))
    sfd_df_peak_daily[which(sfd_df_peak_daily$sr.input=="NaN"),"sr.input"]<-NA
    sfd_df_peak_daily[which(sfd_df_peak_daily$sr_day=="NaN"),"sr_day"]<-NA
    sfd_df_peak_daily[which(sfd_df_peak_daily$prec_day=="NaN"),"prec_day"]<-NA
  }

  if(method=='env.filt'){
    ## 0. calculate conductance (SFD/VPD) from all data
    sfd_df<-cbind(sfd,vpd.input,sr.input,doy,h,y,doy_y)
    sfd_df$gc<-sfd_df$sfd/sfd_df$vpd.input

    ## 1. non-rainy days (P_daily < 1 mm)
    prec_df = as.data.frame(prec.input)
    prec_df$doy = lubridate::yday(zoo::index(prec.input))
    prec_df$y = lubridate::year(zoo::index(prec.input))
    prec_df$doy_y= as.numeric(paste(prec_df$doy,prec_df$y,sep=""))
    add<-data.frame(timestamp=as.character(zoo::index(sfd_df)),sfd=as.numeric(sfd_df$sfd),doy_y=as.numeric(sfd_df$doy_y))
    add2<-data.frame(prec=as.numeric(prec_df$prec.input),doy_y=as.numeric(prec_df$doy_y))
    adding<-merge(add,add2,by="doy_y")
    prec_day<-zoo::zoo(adding$prec,order.by=base::as.POSIXct(adding$timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC"))

    ## 2. non-cloudy days (SW > q25) #(PAR_daily > 300 umol/m2/s)
    sr_df = as.data.frame(sr.input)
    sr_df$doy = lubridate::yday(zoo::index(sr.input))
    sr_df$y = lubridate::year(zoo::index(sr.input))
    sr_df$doy_y= as.numeric(paste(sr_df$doy,sr_df$y,sep=""))
    #sr_df = zoo::zoo(sr_df,order.by=zoo::index(sr.input))
    sr_df_daily = stats::aggregate(sr_df$sr.input,
                                   mean,
                                   na.rm=T,
                                   by=list(sr_df$doy_y))
    add2<-data.frame(sr=as.numeric(sr_df_daily$x),doy_y=as.numeric(sr_df_daily$Group.1))
    adding<-merge(add,add2,by="doy_y")
    sr_day<-zoo::zoo(adding$sr,order.by=base::as.POSIXct(adding$timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC"))

    ##3. merge all data together and select the criteria
    sfd_df$sr_day<-sr_day
    sfd_df$prec_day<-prec_day
    sfd_df$gc_filt<-sfd_df$gc
    sfd_df$vpd_filt<-sfd_df$vpd.input
    sfd_df[which(sfd_df$prec_day>as.numeric(prec.lim)),"gc_filt"]<-NA
    sfd_df[which(sfd_df$sr_day<as.numeric(low.sr)),"gc_filt"]<-NA
    sfd_df[which(sfd_df$prec_day>as.numeric(prec.lim)),"vpd_filt"]<-NA
    sfd_df[which(sfd_df$sr_day<as.numeric(low.sr)),"vpd_filt"]<-NA
    sfd_df[which(sfd_df$sr.input<peak.sr),"gc_filt"]<-NA
    sfd_df[which(sfd_df$sr.input<peak.sr),"vpd_filt"]<-NA

    ##4. aggregate to daily values
    sfd_df<-stats::window(sfd_df,start=zoo::index(sfd_df[which(is.na(sfd_df$sfd)==F),])[1],end=zoo::index(sfd_df[which(is.na(sfd_df$sfd)==F),])[nrow(sfd_df[which(is.na(sfd_df$sfd)==F),])])
    sfd_df_peak_daily = stats::aggregate(sfd_df[,],
                                         mean,
                                         na.rm=T,
                                         by=list(as.Date(zoo::index(sfd_df))))
  }
  sfd_df_peak_daily<-zoo::fortify.zoo(sfd_df_peak_daily)
  sfd_df_peak_daily[which(sfd_df_peak_daily$gc_fil=="NaN"),"gc_filt"]<-NA
  sfd_df_peak_daily[which(sfd_df_peak_daily$vpd_fil=="NaN"),"vpd_filt"]<-NA

  #e
  if(length(stats::na.omit(sfd_df_peak_daily$gc_filt))<10)stop("Invalid output data, less than 10 days selected.")

  ## 5. Filter low VPD conditions (i.e., VPD_peakdaymean <= 0.5 kPa)
  sfd_df_4gcfit<-sfd_df_peak_daily
  sfd_df_4gcfit[which(sfd_df_4gcfit$vpd_filt<vpd.cutoff),"gc_filt"]<-NA
  sfd_df_4gcfit[which(sfd_df_4gcfit$vpd_filt<vpd.cutoff),"vpd_filt"]<-NA

  ## 6. normalize Gc in [0,1]
  sfd_df_4gcfit$gc_norm<- as.numeric(sfd_df_4gcfit$gc_filt)/as.numeric(stats::quantile(sfd_df_4gcfit$gc_filt, na.rm=T,probs=c(max.quant)))

  ## 7. Model fitting preparation
  df <- data.frame(x= sfd_df_4gcfit$vpd_filt, y= sfd_df_4gcfit$gc_filt,doy=sfd_df_4gcfit$doy)
  df <- stats::na.omit(df)
  df_n <- data.frame(x= sfd_df_4gcfit$vpd_filt, y= sfd_df_4gcfit$gc_norm,doy=sfd_df_4gcfit$doy)
  df_n <- stats::na.omit(df_n)

  #model fitting
  mod<-stats::nls(y ~ a + d * x^(-0.5), start = list(a = 1, d = 1), data=df)
  mod_n<-stats::nls(y ~ a + d * x^(-0.5), start = list(a = 1, d = 1), data= df_n)
  Alpha<-round(summary(mod)$coef[1,1],3)
  Beta<-round(summary(mod)$coef[2,1],3)

  #plotting output
  if(make.plot==T){
    graphics::par(oma=c(5,8,5,8))
    graphics::par(mar=c(0,0,0,0))
    graphics::layout(
      matrix(c(1,2),ncol=1, byrow = TRUE))
    test<-sfd_df_4gcfit[which(is.na(sfd_df_4gcfit$sfd)==F),"doy"]
    min.doy<-min(test,na.rm=TRUE)
    max.doy<-max(test,na.rm=TRUE)
    min.fd<-min(sfd_df_4gcfit$sfd,na.rm=TRUE)
    max.fd<-max(sfd_df_4gcfit$sfd,na.rm=TRUE)

    graphics::plot(1,1,type="l",yaxt="n",xaxt="n",ylim=c(0-(max.fd*0.05),max.fd),xlim=c(min.doy,max.doy+max.doy*0.1))
    graphics::axis(side=3)
    graphics::axis(side=2,las=2)
    graphics::mtext(side=3,"Day of year",padj=-3.5,outer=T)
    graphics::mtext(side=2,expression(italic(F)[d]*" ("*"mmol "*m^-2*" "*s^-1*")"),padj=-3.5)

    years<-unique(right(sfd_df_4gcfit$doy_y,4))
    colfunc <- grDevices::colorRampPalette(c("black","white"))
    colfunc2 <- grDevices::colorRampPalette(c("black","lightgrey"))

    doy_length<-c(min(sfd_df_4gcfit$doy):max(sfd_df_4gcfit$doy))

    for(year in c(1:length(years))){
      yrs.sfd<-sfd_df_4gcfit[which(right(sfd_df_4gcfit$doy_y,4)==years[year]),]
      pl.sfd<-data.frame(doy=yrs.sfd$doy,value=yrs.sfd$sfd,sfd=yrs.sfd$sfd,gc=yrs.sfd$gc_filt)
      pl.sfd<-merge(data.frame(doy=doy_length),pl.sfd,by="doy",all=T)
      pl.sfd[which(is.na(pl.sfd$gc)==T),"value"]<-NA
      graphics::lines(pl.sfd$doy,pl.sfd$sfd,type="l",yaxt="n",xaxt="n",col=colfunc2(length(years))[year])
      graphics::points(pl.sfd$doy,pl.sfd$value,col=colfunc(length(doy_length)),pch=16,cex=1.3)
      graphics::points(pl.sfd$doy,pl.sfd$value,pch=1,cex=1.3)
    }
    graphics::legend("topright",as.character(years),col=colfunc2(length(years)),bty="n",lty=1)

    graphics::plot(sfd_df_4gcfit$vpd_filt,sfd_df_4gcfit$y,pch=16,col="white",cex=2.5,yaxt="n",ylab="",xlab="",ylim=c(0,max(df$y)))
    graphics::axis(side=2,las=2)

    for(year in c(1:length(years))){
      yrs.sfd<-sfd_df_4gcfit[which(right(sfd_df_4gcfit$doy_y,4)==years[year]),]
      pl.sfd<-data.frame(doy=yrs.sfd$doy,value=yrs.sfd$sfd,sfd=yrs.sfd$sfd,gc=yrs.sfd$gc_filt,vpd=yrs.sfd$vpd_filt)
      pl.sfd<-merge(data.frame(doy=doy_length),pl.sfd,by="doy",all=T)
      graphics::points(pl.sfd$vpd,pl.sfd$gc,col=colfunc(length(doy_length)),pch=16,cex=1.3)
      graphics::points(pl.sfd$vpd,pl.sfd$gc,col="black",pch=1,cex=1.3)
    }

    newdat<-data.frame(x=seq(round((min(df$x)),2),round(max(df$x),2),0.1))
    graphics::lines(seq(round((min(df$x)),2),round(max(df$x),2),0.1),stats::predict(mod,newdata=newdat),col="black",lwd=7)
    graphics::lines(seq(round((min(df$x)),2),round(max(df$x),2),0.1),stats::predict(mod,newdata=newdat),col="orange",lwd=5)
    graphics::legend("topright",c(expression(italic("G")[C]*" = "*italic(alpha)*" + "*italic(beta)*" VPD"^-0.5),
                                  substitute(paste(italic(alpha)," = ", Alpha, " mmol "," ",m^-2," ",s^-1," ",kPa^-1*"       "),list(Alpha=(Alpha))),
                                  substitute(paste(italic(beta)," = ", Beta, " ",kPa^0.5),list(Beta=(Beta)))),bty="n")
    graphics::mtext(side=2,expression(italic(G)[C]*" ("*"mmol "*m^-2*" "*s^-1*" "*kPa^-1*")"),padj=-3.5)
    graphics::mtext(side=1,expression(VPD*" (kPa)"),padj=3,outer=T)
  }

  #o= output
  raw<-zoo::fortify.zoo(sfd_df)
  colnames(raw)<-c("timestamp","fd","vpd","sr","doy","hour","year","doy.y","gc","sr.day","prec.day","gc.filt","vpd.filt")
  raw<-raw[,c(1,7,5,6,4,10,3,13,11,2,9,12)]
  peak.mean<-sfd_df_4gcfit
  colnames(peak.mean)<-c("timestamp","fd","vpd","sr","doy","hour","year","doy.y","gc","sr.day","prec.day","gc.filt","vpd.filt","gc.norm")
  peak.mean[,1]<-as.character(peak.mean[,1])
  row.names(peak.mean)<-c(1:nrow(peak.mean))
  peak.mean<-peak.mean[,c(1,13,12,14)]
  sum.mod<-summary(mod)
  sum.mod.norm<-summary(mod_n)
  output.data<-list(raw,peak.mean,sum.mod,sum.mod.norm)
  names(output.data)<-c("raw","peak.mean","sum.mod",'sum.mod.norm')
  return(output.data)
}
