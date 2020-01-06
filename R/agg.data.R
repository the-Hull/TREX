# require(zoo)

#agg.data
#' Aggregation of time-series data
#'
#' @description Aggregation of time-series data and start/end time selection.
#' This function provides the option to select the temporal step size
#' for aggregation of a single time series origination from an \code{is.trex}-compliant object.
#' Additionally, the user can define the start and end time of the series and select
#' the mean of aggregating the data.
#'
#' @usage (input,
#'  time.agg = 60*24,
#'  start = "2012-07-28 00:00",
#'  end = "2012-10-29 00:00",
#'  FUN = "sum",
#'  unit = 60,
#'  na.rm = TRUE,
#'  df = FALSE)
#'
#' @param input An \code[is.trex]-compliant time series from \code{cal.sfd} outputs
#' (e.g., \code(X$sfd.mw$sfd)).
#' @param time.agg Numeric, the aggregation time in minutes (default = 60).
#' @param start Character string, the start time for the series. Format has
#'  to be provided in "UTC" (e.g. "2012-05-28 00:00" or Year-Month-Day Hour:Minute).
#'  Starting time should not be earlier than the start of the series.
#'  If not provided the entire series is considered.
#' @param end character string, the end time for the series.
#'  Format has to be provided in "UTC" (e.g. "2012-06-28 00:50" or Year-Month-Day Hour:Minute).
#'  Starting time should be earlier than the end time and the end time should not exceed the
#'  length of the series.  If not provided the entire series is considered.
#' @param FUN Quoted function name to compute the summary statistics which can be
#' applied to all data subsets (see aggregate; including “sum", "mean",
#'  "median", "sd", "se", "min", "max").
#' @param unit Numeric, the minutes in which a velocity unity is provided
#'  (e.g., cm3 cm-2 h-1 = 60) for summation (\code{FUN = “sum”}; default = 60).
#' @param na.rm Logical; iff \code{TRUE} missing values are removed (default = \code{TRUE}).
#' @param df Logical; If \code{TRUE}, output is provided in a \code{data.frame} format
#'  with a timestamp and a value column. If \code{FALSE}, output
#'  is provided as a zoo vector object (default = \code{FALSE}).
#'
#' @return A zoo object or data.frame in the appropriate format for other functionalities.
#' @export
#'
agg.data<-function(input,time.agg=60*24,start="2012-07-28 00:00",end="2012-08-29 00:00",FUN="mean",unit=60,na.rm=TRUE,df=FALSE){

#t= test
#raw   <- example.data(type="doy", species="PCAB")
#input <- is.trex(raw,tz="GMT",time.format="%H:%M",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE,df=FALSE)
#input[which(input<0.4)]<-NA
#input<-time.step(input,time.int=15,max.gap=180,decimals=10)
#k.input<-dt.max(input,methods=c("mw"))
#output.data<-cal.sfd(k.input,make.plot=FALSE,df=FALSE,wood="Coniferous")$sfd.mw$sfd
#input<-output.data
#na.rm=TRUE
#time.agg=60*24 # in minutes
#start= "2012-07-28 00:00"  #format %Y-%m-%d %H:%M
#end=   "2012-08-29 00:00"  #format %Y-%m-%d %H:%M
#unit=60 # in minutes
#df=FALSE
#FUN= "sum"
#na.rm=TRUE

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

#d= default conditions
if(missing(start)){start=as.character(index(input[1]))}
if(missing(end)){end=as.character(index(input[length(input)]))}
if(missing(time.agg)){time.agg<-60}
if(missing(df)){df=F}
if(missing(unit)){unit=60}
if(df!=T&df!=F)stop("Unused argument, df needs to be TRUE|FALSE.")
if(missing(na.rm)){na.rm=T}

#e= errors
if(is.zoo(input)==F)stop("Invalid input data, use a zoo file from is.trex or a zoo vector containing numeric values (tz= UTC).")
if(is.numeric(input)==F)stop("Invalid input data, values within the vector are not numeric.")
if(is.character(start)==F)stop("Unused argument, start is not a character (format= %Y-%m-%d %H:%M:%S).")
if(is.character(end)==F)stop("Unused argument, end is not a character (format= %Y-%m-%d %H:%M:%S).")
if(is.numeric(time.agg)==F)stop("Unused argument, time.int is not numeric.")
if(is.numeric(unit)==F)stop("Unused argument, unit is not numeric.")
if(is.numeric(decimals)==F)stop("Unused argument, decimals is not numeric.")
if((FUN%in%c("sum","mean","median","sd","se","min","max"))==F)stop("Unused argument, FUN should be a character of either sum|mean|median|sd|se|min|max.")
if(na.rm!=T&na.rm!=F)stop("Unused argument, na.rm needs to be TRUE|FALSE.")

#p
ts.start<-as.POSIXct(as.character(base::paste0(start,":00")),format="%Y-%m-%d %H:%M:%S",tz="UTC")-1
ts.end<-as.POSIXct(as.character(base::paste0(end,":00")),format="%Y-%m-%d %H:%M:%S",tz="UTC")+1

#e
if(is.na(ts.start)==TRUE)stop("Unused argument, start is not in the correct format (%Y-%m-%d %H:%M:%S).")
if(is.na(ts.end)==TRUE)stop("Unused argument, end is not in the correct format (%Y-%m-%d %H:%M:%S).")
if(as.numeric(ts.start-index(input[1]))< -1)stop("Unused argument, start is earlier then start of the timestamp.")
if(as.numeric(index(input[length(input)])-ts.end)< -1)stop("Unused argument, end is later then start of the timestamp.")

#p= process
value<-na.omit(stats::window(input,start=ts.start,end=ts.end))
raw.gap<-as.numeric(difftime(index(value)[-1],index(value[-length(value)]),tz,units = c("mins")))

#e
if(median(gap,na.rm=TRUE)>time.agg)stop("Unused argument, time.agg is smaller the minimum timestep.")
if(time.agg/median(raw.gap)!=round(time.agg/median(raw.gap)))stop("Unused argument, time.agg does not match the minimum timestep of the series.")

#p
gap<-stats::window(zoo::zoo(raw.gap,order.by=index(value)),start=ts.start,end=ts.end)
dummy<-zoo::zoo(NA,order.by=seq(from=ts.start+1,to=ts.end-1,by=(60*median(raw.gap)))) #*time.int
agg.links<-zoo::zoo(NA,order.by=seq(from=ts.start+1,to=ts.end-1,by=(60*time.agg))) #*time.int
agg.links<-zoo::zoo(c(1:length(agg.links)),order.by=index(agg.links))
proc.1<-cbind.zoo(value,gap,dummy,agg.links)
proc.1$agg<-zoo::na.locf(proc.1$agg.links)
proc.2<-proc.1[which(is.na(proc.1$value)==F),]
proc.3<-aggregate(proc.2$value,by=list(proc.2$agg),FUN=FUN,na.rm=TRUE)

if(na.rm==T){
test<-aggregate(proc.2$value,by=list(proc.2$agg),FUN="length")
proc.3[which(test<max(test))]<-NA
}

proc.4<-zoo::zoo(proc.3,order.by=index(proc.1[which(as.character(proc.1$agg.links)%in%as.character(as.numeric(index(proc.3)))),]))
proc.5<-cbind(proc.1,proc.4)
proc.6<-proc.5[which(is.na(proc.5$agg.links)==F),"proc.4"]
if(FUN=="sum"){proc.6<-proc.6/(unit/median(gap,na.rm=TRUE))}
if(na.rm==T){proc.6<-na.omit(proc.6)}
if(df==T){
  output.data<-data.frame(timestamp=as.character(index(proc.6)),value=as.numeric(as.character(proc.6)))
  }else{
    output.data<-proc.6
  }
return(output.data)
}

#aggregate SFD values to mean hourly and daily sums
# raw   <- example.data(type="doy", species="PCAB")
# input <- is.trex(raw,tz="GMT",time.format="%H:%M",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE,df=FALSE)
# input[which(input<0.4)]<-NA
# k.input<-dt.max(time.step(input,time.int=15,max.gap=180,decimals=10),methods=c("mw"))
# sfd.input<-cal.sfd(k.input,make.plot=FALSE,df=FALSE,wood="Coniferous")$sfd.mw$sfd
#
# output.1hmean<-agg.data(sfd.input,time.agg=60,start="2012-07-28 00:00",end="2012-08-29 00:00",FUN="mean",na.rm=TRUE,df=FALSE)
# output.3hmean<-agg.data(sfd.input,time.agg=60*3,start="2012-07-28 00:00",end="2012-08-29 00:00",FUN="mean",na.rm=TRUE,df=FALSE)
# output.6hmean<-agg.data(sfd.input,time.agg=60*6,start="2012-07-28 00:00",end="2012-08-29 00:00",FUN="mean",na.rm=TRUE,df=FALSE)
#
# plot(output.1hmean,col="cyan")
# lines(output.3hmean,col="orange")
# lines(output.6hmean,col="black")
#
# output.dsum<-agg.data(sfd.input,time.agg=60*24,start="2012-07-28 00:00",end="2012-10-29 00:00",FUN="sum",unit=60,na.rm=TRUE,df=FALSE)
# plot(output.dsum)
# points(output.dsum,pch=16)
