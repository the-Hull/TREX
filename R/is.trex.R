require(chron)
require(zoo)
require(solaR)

#is.trex
is.trex<-function(data,tz="UTC",time.format="%m/%d/%y %H:%M:%S",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE,df=FALSE){
#t= test
  #data= example.data(type="timestamp", species="PCAB")
  #tz= "GMT"
  #time.format="%H:%M"   
  #solar.time=TRUE
  #ref.add=FALSE
  #long.deg=7.7459
  #df= FALSE
  
#d= default conditions
  if(missing(tz)){tz="GMT"
  warning("No timezone specified : Using default setting (= GMT/UTC)")}  
  if(missing(ref.add)){ref.add=F}
  if(missing(time.format)){time.format= "%d-%m-%Y %H:%M"
    warning("No time.format specified : Using default setting (= %d-%m-%Y %H:%M)")}  
  if(missing(solar.time)){solar.time=F}
  if(missing(df)){df=F}
  
#e= errors
  if(length(which(tz%in%base::OlsonNames()))==0)stop("Unused argument, please use a valid time zone.")
  if(solar.time!=T&solar.time!=F)stop("Unused argument, solar.time needs to be TRUE|FALSE.")
  if(ref.add!=T&ref.add!=F)stop("Unused argument, ref.add needs to be TRUE|FALSE.")
  if(df!=T&df!=F)stop("Unused argument, df needs to be TRUE|FALSE.")
  if(length(which(c("timestamp","doy")%in%colnames(data)))==0)stop("Incorrect data format, no doy|timestamp column present.")
    type=NA
  if(length(which("timestamp"%in%colnames(data)))==0){
    type="doy"
  if(length(which("year"%in%colnames(data)))==0)stop("Incorrect data format, no year column present.")
  if(length(which("hour"%in%colnames(data)))==0)stop("Incorrect data format, no hour column present.")
  if(length(which("value"%in%colnames(data)))==0)stop("Incorrect data format, no value column present.")
  }else{
    type="timestamp"
  if(length(which("value"%in%colnames(data)))==0)stop("Incorrect data format, no value column present.")
  if(length(which(nchar(as.character(data$timestamp))>21))!=0){
  print(which(nchar(as.character(data$timestamp))>21))
  stop("Incorrect data format, timestamp has too many characters in the above given lines.")
  }}
  if(ref.add==T){
  if(length(which("ref"%in%substr(colnames(data), 1,3)))==0)stop("Incorrect input data, no additional reference probes present.")}
  if(is.na(base::suppressWarnings(base::mean(c(data$value),na.rm=TRUE)))==T)stop("Incorrect data format, value is not numeric.")
  if(solar.time==T&missing(long.deg)==T)stop("Missing argument, solar.time needs long.deg in decimal degrees E.")
  if(solar.time==T){if(is.numeric(long.deg)==FALSE|long.deg>180|long.deg< -180)stop("Unused argument, long.deg needs to be numeric and between -180:180.")}

#c= conversions
  time.format.orig<-NA
  if(type=="timestamp"){data$timestamp<-as.character(data$timestamp)
  time.format.orig<-time.format
  }
  if(type=="doy"){
  #e
  if(is.integer(data$doy)==F)stop("Incorrect data format, doy is not integer.")
  if(is.integer(data$year)==F)stop("Incorrect data format, year is not integer.")
  data$hour<-as.character(data$hour)    
  if((min(nchar(data$hour))>3&max(nchar(data$hour))<6)==F){"Incorrect data format, invalid hour column."}
  if(min(nchar(data$hour))==4){data[which(nchar(data$hour)==4),"hour"]<-base::paste0("0",data[which(nchar(data$hour)==4),"hour"])}
  timestamp<-paste(base::as.Date(data$doy - 1, origin = paste0(data$year,"-01-01")),data$hour,sep=" ") 
  time.format.orig<-paste0("%Y-%m-%d ",time.format) 
  data$timestamp<-timestamp
  data<-data[,-which(colnames(data)%in%c("year","doy","hour"))]
  }    

#w= warnings
  if(length(which(base::duplicated(data$timestamp)==T))!=0){
  warning("Double timestamp present, daylight saving could be present within the timestamp.")} 

#p= process
  if(solar.time==T){
  timestamp<-suppressWarnings(solaR::local2Solar(base::as.POSIXct(as.character(data$timestamp),format=time.format.orig,tz=tz), lon = long.deg))  
  }else{
  timestamp<-chron::as.chron(base::as.POSIXct(as.character(data$timestamp),format=time.format.orig,tz=tz))
  }

#e
  if(as.character(timestamp[1])=="(NA NA)"|is.na(timestamp[1])==T)stop("No timestamp present, time.format is likely incorrect.")
  if(length(which(base::duplicated(timestamp)==T))!=0){
  print(data[which(base::duplicated(timestamp)==T),])
    stop("Double timestamp present, either due to errors in the timestamp or issues with daylight saving (change tz).")}  
    
#p
  if(ref.add==T){
  if(length(which(left(colnames(data),3)=="ref"))==1){
  value<-data$value-(data[,which(left(colnames(data),3)=="ref")])
  }
  if(length(which(left(colnames(data),3)=="ref"))>1){
  value<-data$value-(base::rowMeans(data[,which(left(colnames(data),3)=="ref")]))  
  }
  if(length(which(left(colnames(data),3)=="ref"))==0){
  stop("No reference probe measurements, missing ref1, ref2, refn columns.")}  
  data<-data.frame(timestamp=data$timestamp, value=value)
  }

#p
  if(length(unique(timestamp))==length(unique(format(timestamp, "%Y-%m-%d %H:%M")))){
  output.data<-zoo::zoo(data$value,order.by=base::as.POSIXct(format(timestamp, "%Y-%m-%d %H:%M")))
  }else{
  agg<-stats::aggregate(output.data,by=format(timestamp, "%Y-%m-%d %H:%M"),mean)
  output.data<-zoo::zoo(agg,order.by=base::as.POSIXct(index(agg)))
  }

#o= output
  if(df==T){
  output.data<-data.frame(timestamp=as.character(index(output.data)),value=as.numeric(as.character(output.data)))
  output.data$timestamp<-as.character(output.data$timestamp)
  output.data$value<-as.numeric(output.data$value)}
  return(output.data)
}

#validating and structuring example data
raw   <- example.data(type="doy", species="PCAB")
input <- is.trex(raw,tz="GMT",time.format="%H:%M",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE,df=FALSE)
head(raw)
str(input)
head(input)
plot(input)

#Figure 1
pdf("D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX/Figure 1.pdf",height=6,width=8)
layout(
  matrix(
    c(2,2,1,1,
      1,1,1,1), 
    nc=4, byrow = TRUE
  )
)
par(oma=c(2,5,2,2))
par(mar=c(4,5,4,4))
plot(input,ylab=expression(Delta*"V (mV)"),xlab="Time",yaxt="n",ylim=c(0,1.5),cex.axis=1.5,cex.lab=1.5)
axis(side=2,las=2,cex.axis=1.5)
mtext(side=3,"Picea abies",font=3,cex=1.5)

par(mar=c(6,6,6,6))
plot(window(input,start=(as.POSIXct(as.character("(06/01/13 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT")),
         end=(as.POSIXct(as.character("(07/01/13 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"))),yaxt="n",
     ylab="",xlab="",ylim=c(0.66,0.84),cex.axis=1.5)
axis(side=4,las=2,cex.axis=1.5)
legend("topleft","Year = 2013",bty="n",cex=1.5)
dev.off()





#TRASH    
#example
input<-tdm.data(type="timestamp")


left = function(string, char){
  substr(string, 1,char)}
right = function (string, char){
  substr(string,nchar(string)-(char-1),nchar(string))
}



#transform to zoo object
type  <-"timestamp"
format<-"(%m/%d/%y %H:%M:%S)"
tz    <-"GMT"

#timezone tests





zoo::zoo(x, order.by= chron::as.chron(as.POSIXct(as.character(x[,1]),format=format,tz=tz)))




nrow(input)





#examples
View(OlsonNames())
as.chron(base::as.POSIXct(as.character(data$timestamp)[1],format=time.format.orig,tz=tz))  

data<-read.table("D:/Documents/WSL/06_basic_data/1_database/Environmental_data/All_output_Tier3/Solar_radiance.txt",header=TRUE,sep="\t")  
data<-data[,c("Timestamp","N13")]
time.format="(%m/%d/%y %H:%M:%S)"   
tz<-"GMT"
sr<-zoo(data$N13,order.by=local2Solar(base::as.POSIXct(as.character(data$Timestamp),format=time.format,tz=tz), lon = long.deg))
start<-as.POSIXct(as.character("(11/02/15 00:00:00)"),format=time.format,tz=tz)
end<-as.POSIXct(as.character("(11/03/15 00:00:00)"),format=time.format,tz=tz)
plot(window(sr,start=start,end=end))

as.character(data$timestamp)[c(1:24)]
View(cbind(
  as.character(chron::as.chron(base::as.POSIXct(as.character(data$timestamp),format=time.format.orig,tz=tz))),
  as.character(chron::as.chron(base::as.POSIXct(as.character(data$timestamp),format=time.format.orig,tz="GMT"))),
  as.character(local2Solar(base::as.POSIXct(as.character(data$timestamp),format=time.format.orig,tz=tz), lon = long.deg)),
  as.character(data$timestamp)
))
