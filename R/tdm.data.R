#tdm.data
tdm.data<-function(type="timestamp"){
  if(missing(type)){type="timestamp"}
  if(type!="timestamp"&type!="doy")stop(paste("Unused argument, please use: timestamp|doy.",sep=""))
  if(type=="timestamp"){return(read.table("tdm.input.txt",header=TRUE,sep="\t")[,c(1,5)])
  }else{return(read.table("tdm.input.txt",header=TRUE,sep="\t")[,c(2,3,4,5)])}}

#example
#viewing TDM data 
tdm.data(type="timestamp")









#REMOVE: generate example data
require("chron")
require("zoo")

left = function(string, char){
  substr(string, 1,char)}
right = function (string, char){
  substr(string,nchar(string)-(char-1),nchar(string))
}

#generate input data
tdm.data<-read.table("tdm.data.txt",header=TRUE,sep="\t")
tdm.zoo<-zoo(tdm.data, order.by= as.chron(as.POSIXct(as.character(tdm.data[,1]),format="%d-%m-%Y %H:%M",tz="GMT")))
tdm.data<-data.frame(timestamp=as.character(tdm.data[,1]),year=left(right(as.character(tdm.data[,1]),10),4),
                     doy=as.integer(strftime(index(tdm.zoo), format = "%j")),
                     hour=substr(as.character(tdm.data$Timestamp),nchar(as.character(tdm.data$Timestamp))-(5-1),nchar(as.character(tdm.data$Timestamp)))
                     ,value=as.numeric(tdm.data[,2]))

write.table(tdm.data,"tdm.input.txt",col.names=TRUE,row.names=FALSE,sep="\t")
