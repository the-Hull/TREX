require(zoo)

#gap.fill
gap.fill<-function(input,max.gap=60,decimals=10,df=F){
  #t= test
  raw   <- example.data(type="doy", species="PCAB")
  input <- is.trex(raw,tz="GMT",time.format="%H:%M",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE,df=FALSE)
  input[which(input<0.4|input>0.82)]<-NA
  max.gap= 120
  decimals= 10 
  df=FALSE
  
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
  if(missing(max.gap)){max.gap<-60}
  if(missing(decimals)){decimals<-10}
  if(missing(df)){df=F}
  if(df!=T&df!=F)stop("Unused argument, df needs to be TRUE|FALSE.")
  
  #e= errors
  if(is.zoo(input)==F)stop("Invalid input data, use a zoo file from is.trex or a zoo vector containing numeric values (tz= UTC).")
  if(is.numeric(input)==F)stop("Invalid input data, values within the vector are not numeric.")
  if(is.numeric(max.gap)==F)stop("Unused argument, max.gap is not numeric.")
  if(is.numeric(decimals)==F)stop("Unused argument, decimals is not numeric.")
  if(decimals<3|decimals>15)stop("Unused argument, decimals can only fall between 3-15.")
  
  #p
  ts.start<-as.POSIXct(as.character(index(input)[1]),format="%Y-%m-%d %H:%M:%S",tz="UTC")-1  
  ts.end<-as.POSIXct(as.character(index(input)[length(input)]),format="%Y-%m-%d %H:%M:%S",tz="UTC")+1  
  
  #p
  value<-na.omit(stats::window(input,start=ts.start,end=ts.end))
  raw.gap<-as.numeric(difftime(index(value)[-1],index(value[-length(value)]),tz,units = c("mins")))
  gap<-c(raw.gap,NA) #minimum gap in minutes
  
  #d
  if(missing(max.gap)){max.gap<-median(raw.gap)}
  
  #e
  if(min(gap,na.rm=TRUE)>max.gap)stop("Unused argument, min.gap is smaller the minimum timestep.")
  
  #p
  gap<-zoo::zoo(gap,order.by=index(value))
  dummy<-zoo::zoo(NA,order.by=seq(from=ts.start+1,to=ts.end-1,by=(60*median(raw.gap)))) #*time.int
  proc.1<-cbind.zoo(value,gap,dummy)
  proc.1$value[3]<-NA
  proc.1$gap[3]<-30
  proc.1[which(is.na(proc.1$value)==F),"dummy"]<-0
  proc.1$value<-zoo::na.approx(proc.1$value,na.rm=F)
  proc.1$gap<-zoo::na.locf(proc.1$gap,na.rm=F)
  proc.1[which(is.na(proc.1$value)==TRUE),"gap"]<-NA
  proc.1[which(proc.1$dummy==0),"gap"]<-0

  #p  
  proc.1$value<-zoo::na.locf(zoo::na.locf(proc.1$value,na.rm=F),fromLast=TRUE)
  proc.1[which(proc.1$gap>max.gap),"value"]<-NA
  proc.1$value<-round(proc.1$value,decimals)
  
  #o= output  
  output.data<-proc.1[
    which(as.character(index(proc.1))%in%as.character(seq(from=ts.start+1,to=ts.end-1,by=(60*time.int)))),"value"]

  if(df==T){
    output.data<-data.frame(timestamp=as.character(index(output.data)),value=as.numeric(as.character(output.data)))
    output.data$timestamp<-as.character(output.data$timestamp)
    output.data$value<-as.numeric(output.data$value)
  }
  return(output.data)
}

#fill two hour gaps 
raw   <- example.data(type="doy", species="PCAB")
input <- is.trex(raw,tz="GMT",time.format="%H:%M",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE,df=FALSE)
input[which(input<0.4|input>0.82)]<-NA
fill_120<-gap.fill(input=input,max.gap=120,decimals=10,df=F)
fill_15<-gap.fill(input=input,max.gap=15,decimals=10,df=F)
base::paste0("Number of data points filled: ",length(input[which(is.na(fill_15)==TRUE)])-length(filled[which(is.na(fill_120)==TRUE)]))