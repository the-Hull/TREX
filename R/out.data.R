#' Generating TDM output
#'
#' @description Generating relevant outputs from the sap flux density (SFD) values.
#' This function provides both \eqn{F_{d}}{Fd} (\eqn{SFD} expressed in mmol m-2 s-1) and crown conductance
#' (GC; an analogue to stomatal conductance) values in an easily exportable format.
#' Additionally, the function can perform environmental filtering on Fd and GC and model GC sensitivity to vapour pressure deficit (VPD).
#' The user can choose between in- (method = “env.filt”) or excluding (method = “stat”) environmental filtering
#' on the GC and adjust the filter threshold manually.
#'
#' @param input An \code{\link{is.trex}}-compliant time series from \code{tdm_cal.sfd} outputs
#' (e.g., \code{X$sfd.mw$sfd})
#' @param vpd.input An \code{\link{is.trex}}-compliant object an individual series of VPD in kPa (see vpd).
#' The extent and temporal resolution should be equal to input.
#' Use \code{\link{dt.steps}} to correct if needed.
#' @param sr.input  An \code{\link{is.trex}}-compliant object of an individual series of solar irradiance
#'  (e.g. either PAR or global radiation; see \code{\link{sr}}).
#'   The extent and temporal resolution should be equal to input. Use \code{\link{dt.steps}} to correct if needed.
#'   This data is only needed when applying the “env.filt” method.
#' @param prec.input An \code{\link{is.trex}}-compliant object of daily precipitation in mm d-1 (see \code{\link{{preci}}).
#'  The extent should be equal to input with a daily temporal resolution.
#'   Use \code{\{link{dt.steps}} to correct if needed.
#'   This data is only needed when applying the “env.filt” method.
#' @param peak.hours Numeric vector with hours which should be considered as peak-of-the-day hours
#'  (default = c(10:14)).
#'  This variable is only needed when the “stat” method is selected.
#' @param low.sr Numeric threshold value in the unit of the sr.input time-series (e.g., W m-2)
#'  to exclude cloudy days which impact GC (default 150 W m-2).
#'   This variable is only needed when the “env.filt” method is selected.
#' @param peak.sr Numeric threshold value in the unit of the sr.input time-series (e.g., W m-2)
#'  to exclude hours which are not considered as peak-of-the-day hours (default 300 W m-2).
#'  This variable is only needed when the “env.filt” method is selected.
#' @param vpd.cutoff Numeric threshold value in kPa for peak-of-the-day mean VPD to eliminate unrealistic
#'  and extremely high values of GC due to low VPD values or high values of GC (default = 0.5 kPa).
#' @param prec.lim nNmeric threshold value in mm d-1 for daily precipitation to remove rainy days (default = 1 mm d-1).
#'  This variable is only needed when “env.filt” method is selected.
#' @param method character string indicating whether precipitation and solar irradiance data should be used
#'  to determined peak-of-the-day GC values and filter the daily GC values (“env.filt”)
#'   or not (“stat”; default). When “env.filt” is selected, input, vpd.input, sr.input, prec.input,
#'    peak.sr, low.sr, vpd.cutoff and prec.lim have to be provided.
#'    When “stat” is selected only input, vpd.input and \code{peak.hours}.
#' @param max.quant a numeric value defining the quantile of the GC data which should be considered as GC.max (default = 1).
#' @param make.plot Logical; if \code{TRUE}, a plot is generated presenting the FILL.
#'
#' @details Various relevant outputs can be derived from the SFD data.
#' This function provides the option to recalculate SFD to Fd (expressed in mmol m-2 s-1)
#' and crown conductance (according to Pappas et al. 2018).
#' GC is estimated per unit sapwood area, where \eqn{GC = Fd / VPD} (in kPa), assuming that
#' i) the stem hydraulic capacitance between the height of sensor and the leaves is negligible, and
#' ii) that the canopy is well coupled to the atmosphere. In order to reduce the effect of stem hydraulic capacitance,
#' peak-of-the-day GC are solely considered for calculating daily average GC.
#' Peak-of-the-day conditions are defined by \code{peak.hours} or \code{peak.sr}. Moreover, to analyse the relationship between GC
#'  and environmental measurements (e.g., VPD), the daily mean peak-of-the-day GC values can be restricted to:
#'  i) non-cloudy days (see \code{low.sr}), to reduce the impact of low irradiance on GC,
#'  ii) non-rainy days (see \code{prec.lim}), as wet leaves are not well coupled to the atmosphere, and
#'  iii) daily mean peak-of-the-day GC great then a threshold (see \code{vpd.cutoff}),
#'  to eliminate unrealistically high GC values due to low Fd or VPD values (when method = “env.filt”).
#'  Moreover, the sensitivity of the daily mean peak-of-the-day GC to VPD is modelled by fitting the following model:
#'   \eqn{GC = \alpha + \beta VPD^{-0.5}}{GC = \alpha + \beta VPD^(-0.5)}.
#'   Besides using the raw daily mean peak-of-the-day GC values, the function also applies
#'   a normalization where daily mean peak-of-the-day GC is standardized to the maximum conductance (GC.max; see \code{max.quant}).
#'
#'
#'
#' @return A list of data.frame objects in the appropriate format for other functionalities.
#' [[“raw”]] raw = a data.frame containing the input data and filtered values. Columns include the timestamp [,“timestamp”]
#' (e.g., “2012-01-01 00:00:00”), year of the data [,“year”], day of year [,“doy”], input solar radiance data [,“sr”],
#'  daily average radiance data [,“sr”], input vapour pressure deficit data [,“vpd”], isolated peak-of-the-day vapour pressure
#'  deficit values [,“vpd.filt”], input daily precipitation [,“prec.day”], sap flux density expressed in mmol m-2 s-1 [,“fd”],
#'  crown conductance expressed in mmol m-2 s-1 kPa-1 [,“gc”], and the filtered crown conductance [,“gc.filt”].
#'  [[“peak.mean”]] peak.mean = a data.frame containing the daily mean crown conductance values.
#'  Columns include the timestamp [,“timestamp”] (e.g., “2012-01-01”), peak-of-the-day vapour pressure deficit [,“vpd.filt”],
#'   the filtered crown conductance mmol m-2 s-1 kPa-1 [,“gc.filt”], and the normalized crown conductance according
#'    to the maximum crown conductance [,“gc.norm”]. [[“sum.mod”]] sum.mod = model summary object (see summary())
#'    of the model between VPD and GC. [[“sum.mod.norm”]] sum.mod.norm = model summary object (see summary())
#'    of the model between VPD and GC/GC.max.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #Gc response function
#' #Gc response function
#' raw   <- is.trex(example.data(type="doy"), tz="GMT", time.format="%H:%M", solar.time=TRUE, long.deg=7.7459, ref.add=FALSE)
#' input <- time.step(input=raw, start="2013-05-01 00:00", end="2013-11-01 00:00",
#'                    time.int=15, max.gap=60, decimals=10, df=FALSE)
#' input[which(input<0.2)]<- NA
#' input <- dt.max(input, methods=c("dr"), det.pd=TRUE, interpolate=FALSE, max.days=10, df=FALSE)
#' output.data<- cal.sfd(input,make.plot=TRUE,df=FALSE,wood="Coniferous")
#' input<- output.data$sfd.dr$sfd
#' output<- out.data(input=input, vpd.input=vpd, sr.input=sr, prec.input=preci,
#'                   low.sr = 150, peak.sr=300, vpd.cutoff= 0.5, prec.lim=1,
#'                   method="env.filt", max.quant=0.99, make.plot=TRUE)
#' head(output)
#'
#' }
#'
out.data<-function(input,vpd.input=vpd,sr.input=sr,prec.input=prec,peak.hours=c(10:14),low.sr = 150,
                   peak.sr=300,vpd.cutoff= 0.5,prec.lim=1,method="env.filt",max.quant=1,make.plot=TRUE){

  #t= test
  #setwd("D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX/R_package/TREX - Construction")
  #calib<-read.table("D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX/R_package/TREX - Construction/cal.data.txt",header=TRUE,sep="\t")
  #raw   <-is.trex(example.data(type="doy"),tz="GMT",time.format="%H:%M",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
  #input <-time.step(input=raw,start="2013-05-01 00:00",end="2014-11-01 00:00",
  #                time.int=15,max.gap=60,decimals=10,df=FALSE)
  #input[which(input<0.2)]<-NA
  #input <-dt.max(input, methods=c("dr"),det.pd=TRUE,interpolate=FALSE,max.days=10,df=FALSE)
  #output.data<-cal.sfd(input,make.plot=TRUE,df=FALSE,wood="Coniferous")
  #vpd<-read.table("D:/Documents/WSL/06_basic_data/1_database/Environmental_data/All_output_Tier3/Vapour_pressure_deficit.txt",header=TRUE,sep="\t")
  #sr<-read.table("D:/Documents/WSL/06_basic_data/1_database/Environmental_data/All_output_Tier3/Solar_radiance.txt",header=TRUE,sep="\t")
  #vpd<-vpd[,c("Date","N13")]
  #colnames(vpd)<-c("timestamp","value")
  #sr<-sr[,c("Timestamp","N13")]
  #colnames(sr)<-c("timestamp","value")
  #vpd_raw   <-is.trex(vpd,tz="GMT",time.format="(%m/%d/%y %H:%M:%S)",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
  #vpd.input <-time.step(input=vpd_raw,start="2012-01-01 00:00",end="2015-11-15 00:00",
  #                      time.int=15,max.gap=60,decimals=10,df=FALSE)
  #sr_raw   <-is.trex(sr,tz="GMT",time.format="(%m/%d/%y %H:%M:%S)",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
  #sr.input <-time.step(input=sr_raw,start="2012-01-01 00:00",end="2015-11-15 00:00",
  #                     time.int=15,max.gap=60,decimals=10,df=FALSE)

  #input<-(output.data$sfd.dr$sfd)
  #prec_raw<-read.table("D:/Documents/WSL/06_basic_data/1_database/Environmental_data/All_output_Tier3/Precipitation.txt",header=TRUE,sep="\t")
  #colnames(prec_raw)<-c("timestamp","value")
  #prec.in<-is.trex(prec_raw,time.format="%d/%m/%y",tz="UTC")
  #prec.input<-window(prec.in,
  #  start=as.POSIXct(as.character(index(vpd.input)[1]),format="%Y-%m-%d",tz="UTC"),
  #  end=as.POSIXct(as.character(index(vpd.input)[length(vpd.input)]),format="%Y-%m-%d",tz="UTC"))
  #vpd.input<-vpd
  #method = 'stat'
  #peak.hours = c(10:14) # hours that are considered as 'peak of the day'
  #sr.input # kPa time-1
  #vpd.input # kPa time-1
  #prec.input # mm d-1
  #low.sr = 150
  #peak.sr= 300
  #vpd.cutoff= 0.5 # kPa
  #prec.lim= 1
  #max.quant= 0.9
  #make.plot=T
  #prec.input<-preci
  #sr.input<-sr
  #vpd.input<-vpd


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
    if(as.character(index(input)[1])=="(NA NA)"|is.na(index(input)[1])==T)stop("No timestamp present, time.format is likely incorrect.")
  }

  #e= errors
  if(zoo::is.zoo(input)==F)stop("Invalid input data, use a zoo file from is.trex or a zoo vector containing numeric values (tz= UTC).")
  if(is.numeric(input)==F)stop("Invalid input data, values within the vector are not numeric.")

  #w= warnings
  if(difftime(index(input[length(input)]),index(input[1]),units=c("days"))<30){
    warning("Selected input has a temporal extend of <30 days.")
  }

  if(missing(vpd.input))stop("No vpd.input data included.")
  if(missing(sr.input)){
    warning(paste0("No sr.input data included."))
    sr.input<-zoo::zoo(0,order.by=index(input))}

  #e
  if(attributes(vpd.input)$class=="data.frame"){
    #e
    if(is.numeric(vpd.input$value)==F)stop("Invalid vpd.input data, values within the data.frame are not numeric.")
    if(is.character(vpd.input$timestamp)==F)stop("Invalid vpd.input data, timestamp within the data.frame are not numeric.")

    #p
    vpd.input<-zoo::zoo(vpd.input$value,order.by=base::as.POSIXct(vpd.input$timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC"))

    #e
    if(as.character(index(vpd.input)[1])=="(NA NA)"|is.na(index(vpd.input)[1])==T)stop("No timestamp present, time.format is likely incorrect for vpd.input.")
  }
  if(is.zoo(vpd.input)==FALSE)stop("Invalid input data, vpd.input must be a zoo file (use is.trex).")

  if(attributes(sr.input)$class=="data.frame"){
    #e
    if(is.numeric(sr.input$value)==F)stop("Invalid sr.input data, values within the data.frame are not numeric.")
    if(is.character(sr.input$timestamp)==F)stop("Invalid sr.input data, timestamp within the data.frame are not numeric.")

    #p
    sr.input<-zoo::zoo(sr.input$value,order.by=base::as.POSIXct(sr.input$timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC"))

    #e
    if(as.character(index(sr.input)[1])=="(NA NA)"|is.na(index(sr.input)[1])==T)stop("No timestamp present, time.format is likely incorrect for sr.input.")
  }
  if(is.zoo(sr.input)==FALSE)stop("Invalid input data, sr.input must be a zoo file (use is.trex).")

  if(attributes(prec.input)$class=="data.frame"){
    #e
    if(is.numeric(prec.input$value)==F)stop("Invalid prec.input data, values within the data.frame are not numeric.")
    if(is.character(prec.input$timestamp)==F)stop("Invalid prec.input data, timestamp within the data.frame are not numeric.")

    #p
    prec.input<-zoo::zoo(prec.input$value,order.by=base::as.POSIXct(prec.input$timestamp,format="%Y-%m-%d",tz="UTC"))

    #e
    if(as.character(index(prec.input)[1])=="(NA NA)"|is.na(index(prec.input)[1])==T)stop("No timestamp present, time.format is likely incorrect for vpd.input.")
  }
  if(is.zoo(prec.input)==FALSE)stop("Invalid input data, vpd.input must be a zoo file (use is.trex).")

  #e
  if(method=="env.filt"){
    if(mean(sr.input,na.rm=TRUE)=="NaN")stop("Invalid sr.input data, missing numeric values.")
    if((mean(prec.input,na.rm=TRUE)==0)==T)stop("Invalid prec.input data, missing numeric values.")
  }

  #p
  step.min<-as.numeric(min(difftime(index(input)[-1],index(input)[-length(input)],units=c("mins")),na.rm=TRUE))
  step.sr<-as.numeric(min(difftime(index(sr.input)[-1],index(sr.input)[-length(sr.input)],units=c("mins")),na.rm=TRUE))
  step.vpd<-as.numeric(min(difftime(index(vpd.input)[-1],index(vpd.input)[-length(vpd.input)],units=c("mins")),na.rm=TRUE))

  #w
  if(step.min!=step.sr|step.min!=step.vpd){
    warning(paste0("time steps between input and vpd.input/sr.input differ, results might not be correctly aggregated."))
  }

  #p
  sfd<-((input*10000/3600)/18.01528)*1000 #cm3 cm-2 h-1 to cm3 m-2 s-1 to mmol m-2 s-1
  doy<-zoo::zoo(lubridate::yday(index(input)), order.by=index(input))
  h  <-zoo::zoo(lubridate::hour(index(input)), order.by=index(input))
  y  <-zoo::zoo(lubridate::year(index(input)), order.by=index(input))
  doy_y<-zoo::zoo(as.numeric(paste(doy,y,sep="")), order.by=index(input))

  if(method=="stat"){
    #d
    sr.input=input[]
    sr.input[]<-NA

    prec.input<-aggregate(input[],mean,na.rm=T,by=as.Date(index(input)))
    prec.input[]<-0
    prec.lim <-100

    #p
    sfd_df<-cbind(sfd,vpd.input,sr.input,doy,h,y,doy_y)
    sfd_df$gc<-sfd_df$sfd/sfd_df$vpd.input

    ##non-rainy days (P_daily < 1 mm)
    prec_df = as.data.frame(prec.input)
    prec_df$doy = lubridate::yday(index(prec.input))
    prec_df$y = lubridate::year(index(prec.input))
    prec_df$doy_y= as.numeric(paste(prec_df$doy,prec_df$y,sep=""))
    add<-data.frame(timestamp=as.character(index(sfd_df)),sfd=as.numeric(sfd_df$sfd),doy_y=as.numeric(sfd_df$doy_y))
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
    sfd_df<-window(sfd_df,start=index(sfd_df[which(is.na(sfd_df$sfd)==F),])[1],end=index(sfd_df[which(is.na(sfd_df$sfd)==F),])[nrow(sfd_df[which(is.na(sfd_df$sfd)==F),])])
    sfd_df_peak_daily = aggregate(sfd_df[,],
                                  mean,
                                  na.rm=T,
                                  by=list(as.Date(index(sfd_df))))
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
    prec_df$doy = lubridate::yday(index(prec.input))
    prec_df$y = lubridate::year(index(prec.input))
    prec_df$doy_y= as.numeric(paste(prec_df$doy,prec_df$y,sep=""))
    add<-data.frame(timestamp=as.character(index(sfd_df)),sfd=as.numeric(sfd_df$sfd),doy_y=as.numeric(sfd_df$doy_y))
    add2<-data.frame(prec=as.numeric(prec_df$prec.input),doy_y=as.numeric(prec_df$doy_y))
    adding<-merge(add,add2,by="doy_y")
    prec_day<-zoo::zoo(adding$prec,order.by=base::as.POSIXct(adding$timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC"))

    ## 2. non-cloudy days (SW > q25) #(PAR_daily > 300 umol/m2/s)
    sr_df = as.data.frame(sr.input)
    sr_df$doy = lubridate::yday(index(sr.input))
    sr_df$y = lubridate::year(index(sr.input))
    sr_df$doy_y= as.numeric(paste(sr_df$doy,sr_df$y,sep=""))
    #sr_df = zoo::zoo(sr_df,order.by=index(sr.input))
    sr_df_daily = aggregate(sr_df$sr.input,
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
    sfd_df<-window(sfd_df,start=index(sfd_df[which(is.na(sfd_df$sfd)==F),])[1],end=index(sfd_df[which(is.na(sfd_df$sfd)==F),])[nrow(sfd_df[which(is.na(sfd_df$sfd)==F),])])
    sfd_df_peak_daily = aggregate(sfd_df[,],
                                  mean,
                                  na.rm=T,
                                  by=list(as.Date(index(sfd_df))))
  }
  sfd_df_peak_daily<-zoo::fortify.zoo(sfd_df_peak_daily)
  sfd_df_peak_daily[which(sfd_df_peak_daily$gc_fil=="NaN"),"gc_filt"]<-NA
  sfd_df_peak_daily[which(sfd_df_peak_daily$vpd_fil=="NaN"),"vpd_filt"]<-NA

  #e
  if(length(na.omit(sfd_df_peak_daily$gc_filt))<10)stop("Invalid output data, less than 10 days selected.")

  ## 5. Filter low VPD conditions (i.e., VPD_peakdaymean <= 0.5 kPa)
  sfd_df_4gcfit<-sfd_df_peak_daily
  sfd_df_4gcfit[which(sfd_df_4gcfit$vpd_filt<vpd.cutoff),"gc_filt"]<-NA
  sfd_df_4gcfit[which(sfd_df_4gcfit$vpd_filt<vpd.cutoff),"vpd_filt"]<-NA

  ## 6. normalize Gc in [0,1]
  sfd_df_4gcfit$gc_norm<- as.numeric(sfd_df_4gcfit$gc_filt)/as.numeric(quantile(sfd_df_4gcfit$gc_filt, na.rm=T,probs=c(max.quant)))

  ## 7. Model fitting preparation
  df <- data.frame(x= sfd_df_4gcfit$vpd_filt, y= sfd_df_4gcfit$gc_filt,doy=sfd_df_4gcfit$doy)
  df <- na.omit(df)
  df_n <- data.frame(x= sfd_df_4gcfit$vpd_filt, y= sfd_df_4gcfit$gc_norm,doy=sfd_df_4gcfit$doy)
  df_n <- na.omit(df_n)

  #model fitting
  mod<-nls(y ~ a + d * x^(-0.5), start = list(a = 1, d = 1), data=df)
  mod_n<-nls(y ~ a + d * x^(-0.5), start = list(a = 1, d = 1), data= df_n)
  Alpha<-round(summary(mod)$coef[1,1],3)
  Beta<-round(summary(mod)$coef[2,1],3)

  #plotting output
  if(make.plot==T){
    par(oma=c(5,8,5,8))
    par(mar=c(0,0,0,0))
    layout(
      matrix(c(1,2),nc=1, byrow = TRUE))
    test<-sfd_df_4gcfit[which(is.na(sfd_df_4gcfit$sfd)==F),"doy"]
    min.doy<-min(test,na.rm=TRUE)
    max.doy<-max(test,na.rm=TRUE)
    min.fd<-min(sfd_df_4gcfit$sfd,na.rm=TRUE)
    max.fd<-max(sfd_df_4gcfit$sfd,na.rm=TRUE)

    plot(1,1,type="l",yaxt="n",xaxt="n",ylim=c(0-(max.fd*0.05),max.fd),xlim=c(min.doy,max.doy+max.doy*0.1))
    axis(side=3)
    axis(side=2,las=2)
    mtext(side=3,"Day of year",padj=-3.5,outer=T)
    mtext(side=2,expression(italic(F)[d]*" ("*"mmol "*m^-2*" "*s^-1*")"),padj=-3.5)

    years<-unique(right(sfd_df_4gcfit$doy_y,4))
    colfunc <- colorRampPalette(c("black","white"))
    colfunc2 <- colorRampPalette(c("black","lightgrey"))

    doy_length<-c(min(sfd_df_4gcfit$doy):max(sfd_df_4gcfit$doy))

    for(year in c(1:length(years))){
      yrs.sfd<-sfd_df_4gcfit[which(right(sfd_df_4gcfit$doy_y,4)==years[year]),]
      pl.sfd<-data.frame(doy=yrs.sfd$doy,value=yrs.sfd$sfd,sfd=yrs.sfd$sfd,gc=yrs.sfd$gc_filt)
      pl.sfd<-merge(data.frame(doy=doy_length),pl.sfd,by="doy",all=T)
      pl.sfd[which(is.na(pl.sfd$gc)==T),"value"]<-NA
      lines(pl.sfd$doy,pl.sfd$sfd,type="l",yaxt="n",xaxt="n",col=colfunc2(length(years))[year])
      points(pl.sfd$doy,pl.sfd$value,col=colfunc(length(doy_length)),pch=16,cex=1.3)
      points(pl.sfd$doy,pl.sfd$value,pch=1,cex=1.3)
    }
    legend("topright",as.character(years),col=colfunc2(length(years)),bty="n",lty=1)

    plot(sfd_df_4gcfit$vpd_filt,sfd_df_4gcfit$y,pch=16,col="white",cex=2.5,yaxt="n",ylab="",xlab="",ylim=c(0,max(df$y)))
    axis(side=2,las=2)

    for(year in c(1:length(years))){
      yrs.sfd<-sfd_df_4gcfit[which(right(sfd_df_4gcfit$doy_y,4)==years[year]),]
      pl.sfd<-data.frame(doy=yrs.sfd$doy,value=yrs.sfd$sfd,sfd=yrs.sfd$sfd,gc=yrs.sfd$gc_filt,vpd=yrs.sfd$vpd_filt)
      pl.sfd<-merge(data.frame(doy=doy_length),pl.sfd,by="doy",all=T)
      points(pl.sfd$vpd,pl.sfd$gc,col=colfunc(length(doy_length)),pch=16,cex=1.3)
      points(pl.sfd$vpd,pl.sfd$gc,col="black",pch=1,cex=1.3)
    }

    newdat<-data.frame(x=seq(round((min(df$x)),2),round(max(df$x),2),0.1))
    lines(seq(round((min(df$x)),2),round(max(df$x),2),0.1),predict(mod,newdata=newdat),col="black",lwd=7)
    lines(seq(round((min(df$x)),2),round(max(df$x),2),0.1),predict(mod,newdata=newdat),col="orange",lwd=5)
    legend("topright",c(expression(italic("G")[C]*" = "*italic(alpha)*" + "*italic(beta)*" VPD"^-0.5),
                        substitute(paste(italic(alpha)," = ", Alpha, " mmol "," ",m^-2," ",s^-1," ",kPa^-1*"       "),list(Alpha=(Alpha))),
                        substitute(paste(italic(beta)," = ", Beta, " ",kPa^0.5),list(Beta=(Beta)))),bty="n")
    mtext(side=2,expression(italic(G)[C]*" ("*"mmol "*m^-2*" "*s^-1*" "*kPa^-1*")"),padj=-3.5)
    mtext(side=1,expression(VPD*" (kPa)"),padj=3,outer=T)
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







