#tdm_uncertain
#install.packages(c("lhs","doParallel","foreach","tibble","dplyr","zoo","sensitivity","randtoolbox","boot","doSNOW","msm"))
require(lhs)
require(zoo)
library(doParallel)
library(foreach)
require(tibble)
require(dplyr)
require(zoo)
library(sensitivity)
library(randtoolbox)
library(boot)
library(doSNOW)
#install.packages("msm")
require(msm)
#detach("package:doSNOW", unload=TRUE)
#detach("package:foreach",unload=TRUE)
require(zoo)

tdm_uncertain<-function(input, vpd.input = vpd, sr.input = sr, method = "pd"
                         , n = 2000, zero.end = 8*60, range.end = 16, zero.start = 1*60, 
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
  #input<-time.step(input,time.int=15,start="2013-04-01 00:00",end="2013-11-01 00:00",max.gap=180,decimals=15)
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
  if(as.character(index(input)[1])=="(NA NA)"|is.na(index(input)[1])==T)stop("No timestamp present, time.format is likely incorrect.")
} 

#e= error
if(zoo::is.zoo(input)==F)stop("Invalid input data, use a zoo file from is.trex or a zoo vector containing numeric values (tz= UTC).")
if(is.numeric(input)==F)stop("Invalid input data, values within the vector are not numeric.")
if(make.plot!=T&make.plot!=F)stop("Unused argument, make.plot needs to be TRUE|FALSE.")
if(method%in%c("pd","mw","dr","ed")==F)stop("Unused argument, method has to be a character object of either pd, mw, dr or ed.")
if(length(method)!=1)stop("Unused argument, method can only contain one character object.")
if(is.numeric(n)==F)stop("Unused argument, n is not numeric.")

#w= warnings
if(difftime(index(input[length(input)]),index(input[1]),units=c("days"))<30){
  warning("Selected input has a temporal extend of <30 days.")  
}  
if(n>2000)warning("Selected n > 2000 which can significantly reduce processing speed.")  

#f= small functions
left = function(string, char){substr(string, 1,char)}
right = function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}

#convert input to a tibble
minutes<-as.numeric(left(right(as.character(index(input)),8),2))*60+as.numeric(left(right(as.character(index(input)),5),2))
days<-as.numeric(floor(difftime(index(input),as.Date(index(input)[1]),units="days"))+1)

#w= warnings
if(max(days)>365)warning("Input length > 365 days which can significantly recude processing speed.")

raw.input <- tibble(
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

length.input <- na.omit(tibble(days = days,value = as.numeric(as.character(input))))
raw.input$count<-full_join(raw.input,aggregate(length.input, list(length.input$days), FUN=length),by=c("days"="Group.1"))$value.y

#d
if(missing(zero.start)){zero.start<-1*60}
if(missing(zero.end)){zero.end<-8*60}
if(missing(range.start)){range.start<-round((60*4)/median(diff(raw.input$minutes)),0)}
if(missing(range.end)){range.end<-round((60*4)/median(diff(raw.input$minutes)),0)}
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
A <- randomLHS(n,5) 
B <- matrix(nrow = nrow(A), ncol = ncol(A))
B[,1] <- ceiling(qunif(A[,1], min =-(range.end-1)/2, max = range.end/2))
B[,2] <- ceiling(qunif(A[,2], min =-(range.start-1)/2, max = range.start/2))
B[,3] <- qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
B[,4] <- qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
B[,5] <- qnorm(A[,5], mean = b_mu, sd= b_sd)
colnames(B)<-c("zero.end","zero.start","sw.cor","a","b")
X1<-data.frame(B)

A <- randomLHS(n,5) 
B <- matrix(nrow = nrow(A), ncol = ncol(A))
B[,1] <- ceiling(qunif(A[,1], min =-(range.end-1)/2, max = range.end/2))
B[,2] <- ceiling(qunif(A[,2], min =-(range.start-1)/2, max = range.start/2))
B[,3] <- qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
B[,4] <- qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
B[,5] <- qnorm(A[,5], mean = b_mu, sd= b_sd)
colnames(B)<-c("zero.end","zero.start","sw.cor","a","b")
X2<-data.frame(B)

x <- soboljansen(model = NULL , X1, X2, nboot = n)
B<-x$X
n<-nrow(B)
#p= process
registerDoParallel(cores = detectCores() - 2)
cl<-makeCluster(detectCores() - 2)
registerDoSNOW(cl)
pb <- txtProgressBar(max = nrow(B), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

output<-foreach(i=c(1:n),.combine="rbind",.packages=c("tibble","dplyr","zoo"),.options.snow = opts)%dopar%{
setTxtProgressBar(pb, i) 
ze<-zero.end+(B[i,1]*median(diff(minutes)))
if(ze<0){ze<-24*60-ze}
if(ze>24*60){ze<-ze-24*60}
zs<-zero.start+(B[i,2]*median(diff(minutes)))
if(zs<0){zs<-24*60-zs}
if(zs>24*60){zs<-zs-24*60}

#dt max
proc.1<-raw.input
if(ze>zs){proc.1[which(proc.1$minutes>ze|minutes<zs),"dt.max"]<-NA}
if(ze<zs){proc.1[which(proc.1$minutes>ze&minutes<zs),"dt.max"]<-NA
  offset<-(60*24/median(diff(proc.1$minutes)))-ceiling(zs/median(diff(proc.1$minutes)))+1
  proc.1$days.agg<-c(proc.1$days[(offset:length(proc.1$days))],rep(max(proc.1$days),offset-1))}
#View(proc.1)
if(ze==zs)stop(paste("Unused argument, zero.start and zero.end are too close together.",sep=""))
add<-tibble(
  days.add = aggregate(na.omit(proc.1)$dt.max,by=list(na.omit(proc.1)$days.agg),max,na.rm=TRUE)[,1],
  ddt.max=aggregate(na.omit(proc.1)$dt.max,by=list(na.omit(proc.1)$days.agg),max,na.rm=TRUE)[,2]
  )
proc.2<-full_join(proc.1,add,by=c("days.agg"="days.add"))
proc.2[which(proc.2$ddt.max!=proc.2$dt.max|is.na(proc.2$dt.max)==TRUE),"ddt.max"]<-NA
proc.2[which(is.na(proc.2$ddt.max)==FALSE),"gap"]<-NA
proc.2$gap<-ave(proc.2$gap, rev(cumsum(rev(is.na(proc.2$gap)))), FUN=cumsum)*median(diff(proc.1$minutes)) 
proc.2$ddt.max<-na.locf(zoo::na.locf(proc.2$ddt.max,na.rm=F),fromLast=TRUE)
proc.2[which(is.na(proc.2$value)==TRUE|proc.2$gap>(60*(24+12))|proc.2$count!=median(proc.2$count,na.rm=TRUE)),"ddt.max"]<-NA

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
d.sum.k<-suppressWarnings(aggregate(proc.2$k.pd,by=list(proc.2$days),max,na.rm=TRUE))
d.sum.k[which(d.sum.k[,2]=="-Inf"),2]<-NA
durat.k<-proc.2
durat.k[which(proc.2$k.pd<=min.k),"k.pd"]<-NA
durat.k<-aggregate(durat.k$k.pd, by=list(durat.k$days), FUN=function(x) {length(na.omit(x))})
values.k<-aggregate(proc.2$k.pd,by=list(proc.2$days),mean,na.rm=TRUE)*24
values.k[values.k[,2]=="NaN",2]<-NA

durat<-proc.2
durat[which(proc.2$sfd<=min.sfd),"sfd"]<-NA
durat<-aggregate(durat$sfd, by=list(durat$days), FUN=function(x) {length(na.omit(x))})
values<-aggregate(proc.2$sfd,by=list(proc.2$days),mean,na.rm=TRUE)*24
values[values[,2]=="NaN",2]<-NA
d.sum<-suppressWarnings(aggregate(proc.2$sfd,by=list(proc.2$days),max,na.rm=TRUE))
d.sum[which(d.sum[,2]=="-Inf"),2]<-NA
return(
  c(c(i,mean(values[,2],na.rm=TRUE),(sd(d.sum[,2],na.rm=TRUE)/mean(d.sum[,2],na.rm=TRUE))*100,(mean(durat[,2]*median(diff(proc.2$minutes)))/60),
    mean(values.k[,2],na.rm=TRUE),(sd(d.sum.k[,2],na.rm=TRUE)/mean(d.sum.k[,2],na.rm=TRUE))*100,(mean(durat.k[,2]*median(diff(proc.2$minutes)))/60)),
    c(length(proc.2$sfd),proc.2$sfd,proc.2$k.pd))
)
}
stopImplicitCluster()

#time series output
number<-median(output[,8])
output.sfd<-output[,c(9:(8+number))]
output.k<-output[,c((9+number):(8+number+number))]
n<-nrow(output.k)
output.sfd<-data.frame(mu=apply(output.sfd,2,mean,na.rm=T),sd=apply(output.sfd,2,sd,na.rm=T),ci.max=apply(output.sfd,2,quantile,probs=c(0.975),na.rm=T),ci.min=apply(output.sfd,2,quantile,probs=c(0.025),na.rm=T))
output.k<-data.frame(mu=apply(output.k,2,mean,na.rm=T),sd=apply(output.k,2,sd,na.rm=T),ci.max=apply(output.k,2,quantile,probs=c(0.975),na.rm=T),ci.min=apply(output.k,2,quantile,probs=c(0.025),na.rm=T))
output.sfd<-zoo(output.sfd,order.by=index(input))
output.k<-zoo(output.k,order.by=index(input))

if(df==T){
  output.sfd<-zoo::fortify.zoo(output.sfd)
  output.k<-zoo::fortify.zoo(output.k)
  colnames(output.sfd)<-c("timestamp","mu","sd","ci.max","ci.min")
  colnames(output.k)<-c("timestamp","mu","sd","ci.max","ci.min")
}

#o= output
output<-output[,c(1:7)]
output_sum<-tell(x,as.numeric(output[,2]))
output_cv<-tell(x,output[,3])
output_length<-tell(x,output[,4])

output_sum.k<-tell(x,output[,5])
output_cv.k<-tell(x,output[,6])
output_length.k<-tell(x,output[,7])

output.tot<-data.frame(item=c(row.names(output_sum$T),"daily.sum",row.names(output_cv$T),"max.cv",row.names(output_length$T),"length.dur"),
           class=c(rep("param.sum",length(row.names(output_sum$T))),"stat.sum",rep("param.cv",length(row.names(output_cv$T))),"stat.cv",rep("param.length",length(row.names(output_length$T))),"stat.length"),
           factor="SFD",
           mean=c(output_sum$T[,1],mean(output[,2]),output_cv$T[,1],mean(output[,3]),output_length$T[,1],mean(output[,4])),
           sd=c(output_sum$T[,3],sd(output[,2]),output_cv$T[,3],sd(output[,3]),output_length$T[,3],sd(output[,4])),
           ci.min=c(output_sum$T[,4],quantile(output[,2],probs=0.025),output_cv$T[,4],quantile(output[,3],probs=0.025),output_length$T[,4],quantile(output[,4],probs=0.025)),
           ci.max=c(output_sum$T[,5],quantile(output[,2],probs=0.975),output_cv$T[,5],quantile(output[,3],probs=0.975),output_length$T[,5],quantile(output[,4],probs=0.975))
           )
output.tot.k<-data.frame(item=c(row.names(output_sum.k$T),"daily.sum",row.names(output_cv.k$T),"max.cv",row.names(output_length.k$T),"length.dur"),
                       class=c(rep("param.sum",length(row.names(output_sum.k$T))),"stat.sum",rep("param.cv",length(row.names(output_cv.k$T))),"stat.cv",rep("param.length",length(row.names(output_length.k$T))),"stat.length"),
                       factor="K",
                       mean=c(output_sum.k$T[,1],mean(output[,5]),output_cv.k$T[,1],mean(output[,6]),output_length.k$T[,1],mean(output[,7])),
                       sd=c(output_sum.k$T[,3],sd(output[,5]),output_cv.k$T[,3],sd(output[,6]),output_length.k$T[,3],sd(output[,7])),
                       ci.min=c(output_sum.k$T[,4],quantile(output[,5],probs=0.025),output_cv.k$T[,4],quantile(output[,6],probs=0.025),output_length.k$T[,4],quantile(output[,7],probs=0.025)),
                       ci.max=c(output_sum.k$T[,5],quantile(output[,5],probs=0.975),output_cv.k$T[,5],quantile(output[,6],probs=0.975),output_length.k$T[,5],quantile(output[,7],probs=0.975))
)

#figure output
#setwd("D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX")
#pdf("Figure_SPD_sensitivity_2000_pd.pdf",height=6,width=8)
if(make.plot==T){
layout(
  matrix(
    c(5,5,1,1,1,2,
      5,5,1,1,1,3,
      5,5,1,1,1,4), 
    nc=6, byrow = TRUE))
#make output for the package
par(oma=c(6,3,3,3))
par(mar=c(0,2,0,0))
plot(1,1,xlim=c(0.7,ncol(B)+0.3),ylim=c(0,1.2),xaxt="n",xlab="",ylab="",yaxt="n",col="white",cex.lab=1.2)
abline(h=seq(0,1,0.2),col="grey")
abline(v=seq(0,ncol(B),1),col="grey")
axis(side=2,las=2,labels=F)
lab<-colnames(B)
axis(side=1,at=c(1:ncol(B)),labels=lab,las=2)
legend("topright","SFD",bty="n",text.font=2,cex=2)
mtext(side=3,"Pre-dawn",outer=TRUE,padj=-0.5)
pal<-data.frame(class=c("param.sum","param.cv","param.length"),col=c("darkorange","cyan","darkblue"))
loc<-data.frame(item=lab,loc=c(1:length(lab)))
off<-data.frame(class=c("param.sum","param.cv","param.length"),off=c(-0.2,0,0.2))
for(i in c(1:nrow(output.tot))){
sel<-output.tot[i,]
if(left(sel$class,4)=="stat"){next}
lines(rep(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],2),
      c(sel$ci.min,sel$ci.max))
points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
       sel$mean,
       pch=16,col=as.character(pal[which(as.character(pal$class)==as.character(sel$class)),"col"]),cex=1.5)
points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
       sel$mean,
       pch=1,cex=1.5)
}
par(mar=c(0,0,0,3))
plot(output.tot[which(output.tot$class=="stat.sum"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.sum"),"ci.max"]))
axis(side=4,las=2)
lines(c(1,1),c(output.tot[which(output.tot$class=="stat.sum"),"ci.min"],output.tot[which(output.tot$class=="stat.sum"),"ci.max"] ))
points(1,output.tot[which(output.tot$class=="stat.sum"),"mean"],pch=16,cex=1.5,col="darkorange")
points(1,output.tot[which(output.tot$class=="stat.sum"),"mean"],pch=1,cex=1.5)
mtext(side=4,expression("Sum ("*cm^3*" "*cm^-2*" "*d^-1*")"),padj=2.5,cex=0.8)
par(mar=c(0.2,0,0.2,3))
plot(output.tot[which(output.tot$class=="stat.cv"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.cv"),"ci.max"]))
axis(side=4,las=2)
lines(c(1,1),c(output.tot[which(output.tot$class=="stat.cv"),"ci.min"],output.tot[which(output.tot$class=="stat.cv"),"ci.max"] ))
points(1,output.tot[which(output.tot$class=="stat.cv"),"mean"],pch=16,cex=1.5,col="cyan")
points(1,output.tot[which(output.tot$class=="stat.cv"),"mean"],pch=1,cex=1.5)
mtext(side=4,expression("CV (%)"),padj=3,cex=0.8)
par(mar=c(0,0,0,3))
plot(output.tot[which(output.tot$class=="stat.length"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.length"),"ci.max"]))
axis(side=4,las=2)
lines(c(1,1),c(output.tot[which(output.tot$class=="stat.length"),"ci.min"],output.tot[which(output.tot$class=="stat.length"),"ci.max"] ))
points(1,output.tot[which(output.tot$class=="stat.length"),"mean"],pch=16,cex=1.5,col="darkblue")
points(1,output.tot[which(output.tot$class=="stat.length"),"mean"],pch=1,cex=1.5)
mtext(side=4,expression("Duration (h)"),padj=3,cex=0.8)

par(mar=c(0,4,0,0))
lab<-colnames(B)
lab<-lab[-which(lab%in%c("a","b"))]
plot(1,1,xlim=c(0.7,length(lab)+0.3),ylim=c(0,1.2),xaxt="n",ylab="Total sensitivity index (-)",xlab="",yaxt="n",col="white",cex.lab=1.2)
abline(h=seq(0,1,0.2),col="grey")
abline(v=seq(0,ncol(B),1),col="grey")
axis(side=2,las=2)
axis(side=1,at=c(1:length(lab)),labels=lab,las=2)
legend("topleft",horiz=F,c("Sum","CV","Duration"),pch=16,col=c("darkorange","cyan","darkblue"),bty="n",cex=1.2)
legend("topright","K",bty="n",text.font=2,cex=2)
pal<-data.frame(class=c("param.sum","param.cv","param.length"),col=c("darkorange","cyan","darkblue"))
loc<-data.frame(item=lab,loc=c(1:length(lab)))
off<-data.frame(class=c("param.sum","param.cv","param.length"),off=c(-0.2,0,0.2))

output.tot.k.gr<-output.tot.k[-which(output.tot.k$item%in%c("a","b")),]
for(i in c(1:nrow(output.tot.k.gr))){
  #i<-1
  sel<-output.tot.k.gr[i,]
  if(left(sel$class,4)=="stat"){next}
  lines(rep(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],2),
        c(sel$ci.min,sel$ci.max))
  points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
         sel$mean,
         pch=16,col=as.character(pal[which(as.character(pal$class)==as.character(sel$class)),"col"]),cex=1.5)
  points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
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
param<-data.frame(method=method,start.input=as.character(index(input)[1]),end.input=as.character(index(input)[length(input)]),
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
  
A <- randomLHS(n, 6) 
B <- matrix(nrow = nrow(A), ncol = ncol(A))
B[,1] <- ceiling(qunif(A[,1], min =-(range.end-1)/2, max = range.end/2))
B[,2] <- ceiling(qunif(A[,2], min =-(range.start-1)/2, max = range.start/2))
B[,3] <- qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
B[,4] <- qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
B[,5] <- qnorm(A[,5], mean = b_mu, sd= b_sd)
B[,6] <- ceiling(qunif(A[,6], min =max.days_min-1, max = max.days_max))

colnames(B)<-c("zero.end","zero.start","sw.cor","a","b","max.days")
X1<-data.frame(B)

A <- randomLHS(n, 6) 
B <- matrix(nrow = nrow(A), ncol = ncol(A))
B[,1] <- ceiling(qunif(A[,1], min =-(range.end-1)/2, max = range.end/2))
B[,2] <- ceiling(qunif(A[,2], min =-(range.start-1)/2, max = range.start/2))
B[,3] <- qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
B[,4] <- qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
B[,5] <- qnorm(A[,5], mean = b_mu, sd= b_sd)
B[,6] <- ceiling(qunif(A[,6], min =max.days_min-1, max = max.days_max))
colnames(B)<-c("zero.end","zero.start","sw.cor","a","b","max.days")
X2<-data.frame(B)

x <- soboljansen(model = NULL , X1, X2, nboot = n)
B<-x$X
n<-nrow(B)
#p= process
registerDoParallel(cores = detectCores() - 2)
cl<-makeCluster(detectCores() - 2)
registerDoSNOW(cl)
pb <- txtProgressBar(max = nrow(B), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
output<-foreach(i=c(1:n),.combine="rbind",.packages=c("tibble","dplyr","zoo"),.options.snow = opts)%dopar%{
  setTxtProgressBar(pb, i) 
  ze<-zero.end+(B[i,1]*median(diff(minutes)))
  if(ze<0){ze<-24*60-ze}
  if(ze>24*60){ze<-ze-24*60}
  zs<-zero.start+(B[i,2]*median(diff(minutes)))
  if(zs<0){zs<-24*60-zs}
  if(zs>24*60){zs<-zs-24*60}
  
  #dt max
  proc.1<-raw.input
  if(ze>zs){proc.1[which(proc.1$minutes>ze|minutes<zs),"dt.max"]<-NA}
  if(ze<zs){proc.1[which(proc.1$minutes>ze&minutes<zs),"dt.max"]<-NA
  offset<-(60*24/median(diff(proc.1$minutes)))-ceiling(zs/median(diff(proc.1$minutes)))+1
  proc.1$days.agg<-c(proc.1$days[(offset:length(proc.1$days))],rep(max(proc.1$days),offset-1))}
  #View(proc.1)
  if(ze==zs)stop(paste("Unused argument, zero.start and zero.end are too close together.",sep=""))
  add<-tibble(
    days.add = aggregate(na.omit(proc.1)$dt.max,by=list(na.omit(proc.1)$days.agg),max,na.rm=TRUE)[,1],
    ddt.max=aggregate(na.omit(proc.1)$dt.max,by=list(na.omit(proc.1)$days.agg),max,na.rm=TRUE)[,2]
  )
  m.day<-(B[i,"max.days"]*2)+1
  proc.1_2<-full_join(add,data.frame(days=c(1:max(add[,1])),rmax=NA),by=c("days.add"="days"))
  proc.1_2<-proc.1_2[order(proc.1_2$days.add),]
  proc.1_2$rmax<-rollmax(proc.1_2$ddt.max,m.day,align = c("center"),na.rm=TRUE,fill=NA)
  gaps<-proc.1_2[which(is.na(proc.1_2$ddt.max)==TRUE),]
  if(nrow(gaps)!=0){
    
  gaps$NA_value<-c(1,diff(gaps$days.add))
  split<-c(1,gaps[which(gaps$NA_value>1),]$days.add,nrow(proc.1_2))

  for(z in c(1:(length(split)-1))){
    proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1])-1)),]$rmax<-na.fill(proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1])-1)),]$rmax,c("extend",NA))
  if(z==length(split)-1){
    proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1]))),]$rmax<-na.fill(proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1]))),]$rmax,c("extend",NA))
  }
    }
  }
  add<-proc.1_2[,c(1,2,3)]
  proc.2<-full_join(proc.1,add,by=c("days.agg"="days.add"))
  proc.2[which(proc.2$ddt.max!=proc.2$dt.max|is.na(proc.2$dt.max)==TRUE),"ddt.max"]<-NA
  proc.2[which(is.na(proc.2$ddt.max)==FALSE),"gap"]<-NA
  proc.2$gap<-ave(proc.2$gap, rev(cumsum(rev(is.na(proc.2$gap)))), FUN=cumsum)*median(diff(proc.1$minutes)) 
  proc.2[which(is.na(proc.2$ddt.max)==FALSE),"ddt.max"]<-proc.2[which(is.na(proc.2$ddt.max)==FALSE),"rmax"]
  proc.2$ddt.max<-na.locf(zoo::na.locf(proc.2$ddt.max,na.rm=F),fromLast=TRUE)
  proc.2[which(is.na(proc.2$value)==TRUE|proc.2$gap>(60*(24+12))|proc.2$count!=median(proc.2$count,na.rm=TRUE)),"ddt.max"]<-NA
  
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
  d.sum.k<-suppressWarnings(aggregate(proc.2$k.pd,by=list(proc.2$days),max,na.rm=TRUE))
  d.sum.k[which(d.sum.k[,2]=="-Inf"),2]<-NA
  durat.k<-proc.2
  durat.k[which(proc.2$k.pd<=min.k),"k.pd"]<-NA
  durat.k<-aggregate(durat.k$k.pd, by=list(durat.k$days), FUN=function(x) {length(na.omit(x))})
  values.k<-aggregate(proc.2$k.pd,by=list(proc.2$days),mean,na.rm=TRUE)*24
  values.k[values.k[,2]=="NaN",2]<-NA
  
  durat<-proc.2
  durat[which(proc.2$sfd<=min.sfd),"sfd"]<-NA
  durat<-aggregate(durat$sfd, by=list(durat$days), FUN=function(x) {length(na.omit(x))})
  values<-aggregate(proc.2$sfd,by=list(proc.2$days),mean,na.rm=TRUE)*24
  values[values[,2]=="NaN",2]<-NA
  d.sum<-suppressWarnings(aggregate(proc.2$sfd,by=list(proc.2$days),max,na.rm=TRUE))
  d.sum[which(d.sum[,2]=="-Inf"),2]<-NA
  
  return(
    c(c(i,mean(values[,2],na.rm=TRUE),(sd(d.sum[,2],na.rm=TRUE)/mean(d.sum[,2],na.rm=TRUE))*100,(mean(durat[,2]*median(diff(proc.2$minutes)))/60),
        mean(values.k[,2],na.rm=TRUE),(sd(d.sum.k[,2],na.rm=TRUE)/mean(d.sum.k[,2],na.rm=TRUE))*100,(mean(durat.k[,2]*median(diff(proc.2$minutes)))/60)),
      c(length(proc.2$sfd),proc.2$sfd,proc.2$k.pd))
  )
}
stopImplicitCluster()

#time series output
number<-median(output[,8])
output.sfd<-output[,c(9:(8+number))]
output.k<-output[,c((9+number):(8+number+number))]
n<-nrow(output.k)
output.sfd<-data.frame(mu=apply(output.sfd,2,mean,na.rm=T),sd=apply(output.sfd,2,sd,na.rm=T),ci.max=apply(output.sfd,2,quantile,probs=c(0.975),na.rm=T),ci.min=apply(output.sfd,2,quantile,probs=c(0.025),na.rm=T))
output.k<-data.frame(mu=apply(output.k,2,mean,na.rm=T),sd=apply(output.k,2,sd,na.rm=T),ci.max=apply(output.k,2,quantile,probs=c(0.975),na.rm=T),ci.min=apply(output.k,2,quantile,probs=c(0.025),na.rm=T))
output.sfd<-zoo(output.sfd,order.by=index(input))
output.k<-zoo(output.k,order.by=index(input))

if(df==T){
  output.sfd<-zoo::fortify.zoo(output.sfd)
  output.k<-zoo::fortify.zoo(output.k)
  colnames(output.sfd)<-c("timestamp","mu","sd","ci.max","ci.min")
  colnames(output.k)<-c("timestamp","mu","sd","ci.max","ci.min")
}

#o= output
output<-output[,c(1:7)]
output_sum<-tell(x,as.numeric(output[,2]))
output_cv<-tell(x,output[,3])
output_length<-tell(x,output[,4])

output_sum.k<-tell(x,output[,5])
output_cv.k<-tell(x,output[,6])
output_length.k<-tell(x,output[,7])

output.tot<-data.frame(item=c(row.names(output_sum$T),"daily.sum",row.names(output_cv$T),"max.cv",row.names(output_length$T),"length.dur"),
                       class=c(rep("param.sum",length(row.names(output_sum$T))),"stat.sum",rep("param.cv",length(row.names(output_cv$T))),"stat.cv",rep("param.length",length(row.names(output_length$T))),"stat.length"),
                       factor="SFD",
                       mean=c(output_sum$T[,1],mean(output[,2]),output_cv$T[,1],mean(output[,3]),output_length$T[,1],mean(output[,4])),
                       sd=c(output_sum$T[,3],sd(output[,2]),output_cv$T[,3],sd(output[,3]),output_length$T[,3],sd(output[,4])),
                       ci.min=c(output_sum$T[,4],quantile(output[,2],probs=0.025),output_cv$T[,4],quantile(output[,3],probs=0.025),output_length$T[,4],quantile(output[,4],probs=0.025)),
                       ci.max=c(output_sum$T[,5],quantile(output[,2],probs=0.975),output_cv$T[,5],quantile(output[,3],probs=0.975),output_length$T[,5],quantile(output[,4],probs=0.975))
)
output.tot.k<-data.frame(item=c(row.names(output_sum.k$T),"daily.sum",row.names(output_cv.k$T),"max.cv",row.names(output_length.k$T),"length.dur"),
                         class=c(rep("param.sum",length(row.names(output_sum.k$T))),"stat.sum",rep("param.cv",length(row.names(output_cv.k$T))),"stat.cv",rep("param.length",length(row.names(output_length.k$T))),"stat.length"),
                         factor="K",
                         mean=c(output_sum.k$T[,1],mean(output[,5]),output_cv.k$T[,1],mean(output[,6]),output_length.k$T[,1],mean(output[,7])),
                         sd=c(output_sum.k$T[,3],sd(output[,5]),output_cv.k$T[,3],sd(output[,6]),output_length.k$T[,3],sd(output[,7])),
                         ci.min=c(output_sum.k$T[,4],quantile(output[,5],probs=0.025),output_cv.k$T[,4],quantile(output[,6],probs=0.025),output_length.k$T[,4],quantile(output[,7],probs=0.025)),
                         ci.max=c(output_sum.k$T[,5],quantile(output[,5],probs=0.975),output_cv.k$T[,5],quantile(output[,6],probs=0.975),output_length.k$T[,5],quantile(output[,7],probs=0.975))
)

#figure output
if(make.plot==T){
layout(
  matrix(
    c(5,5,1,1,1,2,
      5,5,1,1,1,3,
      5,5,1,1,1,4), 
    nc=6, byrow = TRUE))
#make output for the package
par(oma=c(6,3,3,3))
par(mar=c(0,2,0,0))
plot(1,1,xlim=c(0.7,ncol(B)+0.3),ylim=c(0,1.2),xaxt="n",xlab="",ylab="",yaxt="n",col="white",cex.lab=1.2)
abline(h=seq(0,1,0.2),col="grey")
abline(v=seq(0,ncol(B),1),col="grey")
axis(side=2,las=2,labels=F)
lab<-colnames(B)
axis(side=1,at=c(1:ncol(B)),labels=lab,las=2)
#legend("top",horiz=TRUE,c("Sum","CV","Duration"),pch=16,col=c("darkorange","cyan","darkblue"),bty="n")
legend("topright","SFD",bty="n",text.font=2,cex=2)
mtext(side=3,"Moving-window",outer=TRUE,padj=-0.5)
pal<-data.frame(class=c("param.sum","param.cv","param.length"),col=c("darkorange","cyan","darkblue"))
loc<-data.frame(item=lab,loc=c(1:length(lab)))
off<-data.frame(class=c("param.sum","param.cv","param.length"),off=c(-0.2,0,0.2))
for(i in c(1:nrow(output.tot))){
  sel<-output.tot[i,]
  if(left(sel$class,4)=="stat"){next}
  lines(rep(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],2),
        c(sel$ci.min,sel$ci.max))
  points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
         sel$mean,
         pch=16,col=as.character(pal[which(as.character(pal$class)==as.character(sel$class)),"col"]),cex=1.5)
  points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
         sel$mean,
         pch=1,cex=1.5)
}
par(mar=c(0,0,0,3))
plot(output.tot[which(output.tot$class=="stat.sum"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.sum"),"ci.max"]))
axis(side=4,las=2)
lines(c(1,1),c(output.tot[which(output.tot$class=="stat.sum"),"ci.min"],output.tot[which(output.tot$class=="stat.sum"),"ci.max"] ))
points(1,output.tot[which(output.tot$class=="stat.sum"),"mean"],pch=16,cex=1.5,col="darkorange")
points(1,output.tot[which(output.tot$class=="stat.sum"),"mean"],pch=1,cex=1.5)
mtext(side=4,expression("Sum ("*cm^3*" "*cm^-2*" "*d^-1*")"),padj=2.5,cex=0.8)
par(mar=c(0.2,0,0.2,3))
plot(output.tot[which(output.tot$class=="stat.cv"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.cv"),"ci.max"]))
axis(side=4,las=2)
lines(c(1,1),c(output.tot[which(output.tot$class=="stat.cv"),"ci.min"],output.tot[which(output.tot$class=="stat.cv"),"ci.max"] ))
points(1,output.tot[which(output.tot$class=="stat.cv"),"mean"],pch=16,cex=1.5,col="cyan")
points(1,output.tot[which(output.tot$class=="stat.cv"),"mean"],pch=1,cex=1.5)
mtext(side=4,expression("CV (%)"),padj=3,cex=0.8)
par(mar=c(0,0,0,3))
plot(output.tot[which(output.tot$class=="stat.length"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.length"),"ci.max"]))
axis(side=4,las=2)
lines(c(1,1),c(output.tot[which(output.tot$class=="stat.length"),"ci.min"],output.tot[which(output.tot$class=="stat.length"),"ci.max"] ))
points(1,output.tot[which(output.tot$class=="stat.length"),"mean"],pch=16,cex=1.5,col="darkblue")
points(1,output.tot[which(output.tot$class=="stat.length"),"mean"],pch=1,cex=1.5)
mtext(side=4,expression("Duration (h)"),padj=3,cex=0.8)

par(mar=c(0,4,0,0))
lab<-colnames(B)
lab<-lab[-which(lab%in%c("a","b"))]
plot(1,1,xlim=c(0.7,length(lab)+0.3),ylim=c(0,1.2),xaxt="n",ylab="Total sensitivity index (-)",xlab="",yaxt="n",col="white",cex.lab=1.2)
abline(h=seq(0,1,0.2),col="grey")
abline(v=seq(0,ncol(B),1),col="grey")
axis(side=2,las=2)
axis(side=1,at=c(1:length(lab)),labels=lab,las=2)
legend("topleft",horiz=F,c("Sum","CV","Duration"),pch=16,col=c("darkorange","cyan","darkblue"),bty="n",cex=1.2)
legend("topright","K",bty="n",text.font=2,cex=2)
pal<-data.frame(class=c("param.sum","param.cv","param.length"),col=c("darkorange","cyan","darkblue"))
loc<-data.frame(item=lab,loc=c(1:length(lab)))
off<-data.frame(class=c("param.sum","param.cv","param.length"),off=c(-0.2,0,0.2))

output.tot.k.gr<-output.tot.k[-which(output.tot.k$item%in%c("a","b")),]
for(i in c(1:nrow(output.tot.k.gr))){
  #i<-1
  sel<-output.tot.k.gr[i,]
  if(left(sel$class,4)=="stat"){next}
  lines(rep(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],2),
        c(sel$ci.min,sel$ci.max))
  points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
         sel$mean,
         pch=16,col=as.character(pal[which(as.character(pal$class)==as.character(sel$class)),"col"]),cex=1.5)
  points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
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
param<-data.frame(method=method,start.input=as.character(index(input)[1]),end.input=as.character(index(input)[length(input)]),
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
  
A <- randomLHS(n, 6) 
B <- matrix(nrow = nrow(A), ncol = ncol(A))
B[,1] <- ceiling(qunif(A[,1], min =-(range.end-1)/2, max = range.end/2))
B[,2] <- ceiling(qunif(A[,2], min =-(range.start-1)/2, max = range.start/2))
B[,3] <- qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
B[,4] <- qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
B[,5] <- qnorm(A[,5], mean = b_mu, sd= b_sd)
B[,6] <- ceiling(qunif(A[,6], min =max.days_min-1, max = max.days_max))
colnames(B)<-c("zero.end","zero.start","sw.cor","a","b","max.days")
X1<-data.frame(B)

A <- randomLHS(n, 6) 
B <- matrix(nrow = nrow(A), ncol = ncol(A))
B[,1] <- ceiling(qunif(A[,1], min =-(range.end-1)/2, max = range.end/2))
B[,2] <- ceiling(qunif(A[,2], min =-(range.start-1)/2, max = range.start/2))
B[,3] <- qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
B[,4] <- qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
B[,5] <- qnorm(A[,5], mean = b_mu, sd= b_sd)
B[,6] <- ceiling(qunif(A[,6], min =max.days_min-1, max = max.days_max))
colnames(B)<-c("zero.end","zero.start","sw.cor","a","b","max.days")
X2<-data.frame(B)

x <- soboljansen(model = NULL , X1, X2, nboot = n)
B<-x$X
n<-nrow(B)
#p= process
registerDoParallel(cores = detectCores() - 2)
cl<-makeCluster(detectCores() - 2)
registerDoSNOW(cl)
pb <- txtProgressBar(max = nrow(B), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
output<-foreach(i=c(1:n),.combine="rbind",.packages=c("tibble","dplyr","zoo"),.options.snow = opts)%dopar%{
  #i<-1
  setTxtProgressBar(pb, i) 
  ze<-zero.end+(B[i,1]*median(diff(minutes)))
  if(ze<0){ze<-24*60-ze}
  if(ze>24*60){ze<-ze-24*60}
  zs<-zero.start+(B[i,2]*median(diff(minutes)))
  if(zs<0){zs<-24*60-zs}
  if(zs>24*60){zs<-zs-24*60}
  
  #dt max
  proc.1<-raw.input
  if(ze>zs){proc.1[which(proc.1$minutes>ze|minutes<zs),"dt.max"]<-NA}
  if(ze<zs){proc.1[which(proc.1$minutes>ze&minutes<zs),"dt.max"]<-NA
  offset<-(60*24/median(diff(proc.1$minutes)))-ceiling(zs/median(diff(proc.1$minutes)))+1
  proc.1$days.agg<-c(proc.1$days[(offset:length(proc.1$days))],rep(max(proc.1$days),offset-1))}
  
  if(ze==zs)stop(paste("Unused argument, zero.start and zero.end are too close together.",sep=""))
  add<-tibble(
    days.add = aggregate(na.omit(proc.1)$dt.max,by=list(na.omit(proc.1)$days.agg),max,na.rm=TRUE)[,1],
    ddt.max=aggregate(na.omit(proc.1)$dt.max,by=list(na.omit(proc.1)$days.agg),max,na.rm=TRUE)[,2]
  )
  m.day<-(B[i,"max.days"]*2)+1
  proc.1_2<-full_join(add,data.frame(days=c(1:max(add[,1])),rmax=NA),by=c("days.add"="days"))
  proc.1_2<-proc.1_2[order(proc.1_2$days.add),]
  proc.1_2$rmax<-rollmean(proc.1_2$ddt.max,m.day,align = c("center"),na.pad=TRUE,na.rm=TRUE)
   
  gaps<-proc.1_2[which(is.na(proc.1_2$ddt.max)==TRUE),]
  if(nrow(gaps)!=0){
  gaps$NA_value<-c(1,diff(gaps$days.add))
  split<-c(1,gaps[which(gaps$NA_value>1),]$days.add,nrow(proc.1_2))
  
  for(z in c(1:(length(split)-1))){
  proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1])-1)),]$rmax<-na.fill(proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1])-1)),]$rmax,c("extend",NA))
    if(z==length(split)-1){
      proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1]))),]$rmax<-na.fill(proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1]))),]$rmax,c("extend",NA))
    }
  }
  }
  proc.1_2$ddt.max.r<-proc.1_2[,"ddt.max"]
  proc.1_2[which(proc.1_2$ddt.max<proc.1_2$rmax),"ddt.max.r"]<-NA
  proc.1_2$rmax.r<-rollmean(proc.1_2$ddt.max.r,m.day,align = c("center"),na.pad=TRUE,na.rm=TRUE)
  
  gaps<-proc.1_2[which(is.na(proc.1_2$ddt.max)==TRUE),]
  if(nrow(gaps)!=0){
  gaps$NA_value<-c(1,diff(gaps$days.add))
  split<-c(1,gaps[which(gaps$NA_value>1),]$days.add,nrow(proc.1_2))
  
  for(z in c(1:(length(split)-1))){
    proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1])-1)),]$rmax.r<-na.fill(proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1])-1)),]$rmax.r,c("extend",NA))
    if(z==length(split)-1){
      proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1]))),]$rmax.r<-na.fill(proc.1_2[c(which(proc.1_2$days.add==split[z]):(which(proc.1_2$days.add==split[z+1]))),]$rmax.r,c("extend",NA))
    }
  }
  }
  proc.1_2$rmax<-proc.1_2$rmax.r
  add<-proc.1_2[,c(1,2,3)]
  proc.2<-full_join(proc.1,add,by=c("days.agg"="days.add"))
  proc.2[which(proc.2$ddt.max!=proc.2$dt.max|is.na(proc.2$dt.max)==TRUE),"ddt.max"]<-NA
  proc.2[which(is.na(proc.2$ddt.max)==FALSE),"gap"]<-NA
  proc.2$gap<-ave(proc.2$gap, rev(cumsum(rev(is.na(proc.2$gap)))), FUN=cumsum)*median(diff(proc.1$minutes)) 
  proc.2[which(is.na(proc.2$ddt.max)==FALSE),"ddt.max"]<-proc.2[which(is.na(proc.2$ddt.max)==FALSE),"rmax"]
  proc.2$ddt.max<-na.locf(zoo::na.locf(proc.2$ddt.max,na.rm=F),fromLast=TRUE)
  proc.2[which(is.na(proc.2$value)==TRUE|proc.2$gap>(60*(24+12))|proc.2$count!=median(proc.2$count,na.rm=TRUE)),"ddt.max"]<-NA

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
  d.sum.k<-suppressWarnings(aggregate(proc.2$k.pd,by=list(proc.2$days),max,na.rm=TRUE))
  d.sum.k[which(d.sum.k[,2]=="-Inf"),2]<-NA
  durat.k<-proc.2
  durat.k[which(proc.2$k.pd<=min.k),"k.pd"]<-NA
  durat.k<-aggregate(durat.k$k.pd, by=list(durat.k$days), FUN=function(x) {length(na.omit(x))})
  values.k<-aggregate(proc.2$k.pd,by=list(proc.2$days),mean,na.rm=TRUE)*24
  values.k[values.k[,2]=="NaN",2]<-NA
  
  durat<-proc.2
  durat[which(proc.2$sfd<=min.sfd),"sfd"]<-NA
  durat<-aggregate(durat$sfd, by=list(durat$days), FUN=function(x) {length(na.omit(x))})
  values<-aggregate(proc.2$sfd,by=list(proc.2$days),mean,na.rm=TRUE)*24
  values[values[,2]=="NaN",2]<-NA
  d.sum<-suppressWarnings(aggregate(proc.2$sfd,by=list(proc.2$days),max,na.rm=TRUE))
  d.sum[which(d.sum[,2]=="-Inf"),2]<-NA
  return(
    c(c(i,mean(values[,2],na.rm=TRUE),(sd(d.sum[,2],na.rm=TRUE)/mean(d.sum[,2],na.rm=TRUE))*100,(mean(durat[,2]*median(diff(proc.2$minutes)))/60),
        mean(values.k[,2],na.rm=TRUE),(sd(d.sum.k[,2],na.rm=TRUE)/mean(d.sum.k[,2],na.rm=TRUE))*100,(mean(durat.k[,2]*median(diff(proc.2$minutes)))/60)),
      c(length(proc.2$sfd),proc.2$sfd,proc.2$k.pd))
  )
  }
stopImplicitCluster()

#time series output
number<-median(output[,8])
output.sfd<-output[,c(9:(8+number))]
output.k<-output[,c((9+number):(8+number+number))]
n<-nrow(output.k)
output.sfd<-data.frame(mu=apply(output.sfd,2,mean,na.rm=T),sd=apply(output.sfd,2,sd,na.rm=T),ci.max=apply(output.sfd,2,quantile,probs=c(0.975),na.rm=T),ci.min=apply(output.sfd,2,quantile,probs=c(0.025),na.rm=T))
output.k<-data.frame(mu=apply(output.k,2,mean,na.rm=T),sd=apply(output.k,2,sd,na.rm=T),ci.max=apply(output.k,2,quantile,probs=c(0.975),na.rm=T),ci.min=apply(output.k,2,quantile,probs=c(0.025),na.rm=T))
output.sfd<-zoo(output.sfd,order.by=index(input))
output.k<-zoo(output.k,order.by=index(input))

if(df==T){
  output.sfd<-zoo::fortify.zoo(output.sfd)
  output.k<-zoo::fortify.zoo(output.k)
  colnames(output.sfd)<-c("timestamp","mu","sd","ci.max","ci.min")
  colnames(output.k)<-c("timestamp","mu","sd","ci.max","ci.min")
}

#o= output
output<-output[,c(1:7)]
output_sum<-tell(x,as.numeric(output[,2]))
output_cv<-tell(x,output[,3])
output_length<-tell(x,output[,4])

output_sum.k<-tell(x,output[,5])
output_cv.k<-tell(x,output[,6])
output_length.k<-tell(x,output[,7])

output.tot<-data.frame(item=c(row.names(output_sum$T),"daily.sum",row.names(output_cv$T),"max.cv",row.names(output_length$T),"length.dur"),
                       class=c(rep("param.sum",length(row.names(output_sum$T))),"stat.sum",rep("param.cv",length(row.names(output_cv$T))),"stat.cv",rep("param.length",length(row.names(output_length$T))),"stat.length"),
                       factor="SFD",
                       mean=c(output_sum$T[,1],mean(output[,2]),output_cv$T[,1],mean(output[,3]),output_length$T[,1],mean(output[,4])),
                       sd=c(output_sum$T[,3],sd(output[,2]),output_cv$T[,3],sd(output[,3]),output_length$T[,3],sd(output[,4])),
                       ci.min=c(output_sum$T[,4],quantile(output[,2],probs=0.025),output_cv$T[,4],quantile(output[,3],probs=0.025),output_length$T[,4],quantile(output[,4],probs=0.025)),
                       ci.max=c(output_sum$T[,5],quantile(output[,2],probs=0.975),output_cv$T[,5],quantile(output[,3],probs=0.975),output_length$T[,5],quantile(output[,4],probs=0.975))
)
output.tot.k<-data.frame(item=c(row.names(output_sum.k$T),"daily.sum",row.names(output_cv.k$T),"max.cv",row.names(output_length.k$T),"length.dur"),
                         class=c(rep("param.sum",length(row.names(output_sum.k$T))),"stat.sum",rep("param.cv",length(row.names(output_cv.k$T))),"stat.cv",rep("param.length",length(row.names(output_length.k$T))),"stat.length"),
                         factor="K",
                         mean=c(output_sum.k$T[,1],mean(output[,5]),output_cv.k$T[,1],mean(output[,6]),output_length.k$T[,1],mean(output[,7])),
                         sd=c(output_sum.k$T[,3],sd(output[,5]),output_cv.k$T[,3],sd(output[,6]),output_length.k$T[,3],sd(output[,7])),
                         ci.min=c(output_sum.k$T[,4],quantile(output[,5],probs=0.025),output_cv.k$T[,4],quantile(output[,6],probs=0.025),output_length.k$T[,4],quantile(output[,7],probs=0.025)),
                         ci.max=c(output_sum.k$T[,5],quantile(output[,5],probs=0.975),output_cv.k$T[,5],quantile(output[,6],probs=0.975),output_length.k$T[,5],quantile(output[,7],probs=0.975))
)

#figure output
if(make.plot==T){
layout(
  matrix(
    c(5,5,1,1,1,2,
      5,5,1,1,1,3,
      5,5,1,1,1,4), 
    nc=6, byrow = TRUE))
#make output for the package
par(oma=c(6,3,3,3))
par(mar=c(0,2,0,0))
plot(1,1,xlim=c(0.7,ncol(B)+0.3),ylim=c(0,1.2),xaxt="n",xlab="",ylab="",yaxt="n",col="white",cex.lab=1.2)
abline(h=seq(0,1,0.2),col="grey")
abline(v=seq(0,ncol(B),1),col="grey")
axis(side=2,las=2,labels=F)
lab<-colnames(B)
axis(side=1,at=c(1:ncol(B)),labels=lab,las=2)
#legend("top",horiz=TRUE,c("Sum","CV","Duration"),pch=16,col=c("darkorange","cyan","darkblue"),bty="n")
legend("topright","SFD",bty="n",text.font=2,cex=2)
mtext(side=3,"Double-regression",outer=TRUE,padj=-0.5)
pal<-data.frame(class=c("param.sum","param.cv","param.length"),col=c("darkorange","cyan","darkblue"))
loc<-data.frame(item=lab,loc=c(1:length(lab)))
off<-data.frame(class=c("param.sum","param.cv","param.length"),off=c(-0.2,0,0.2))
for(i in c(1:nrow(output.tot))){
  sel<-output.tot[i,]
  if(left(sel$class,4)=="stat"){next}
  lines(rep(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],2),
        c(sel$ci.min,sel$ci.max))
  points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
         sel$mean,
         pch=16,col=as.character(pal[which(as.character(pal$class)==as.character(sel$class)),"col"]),cex=1.5)
  points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
         sel$mean,
         pch=1,cex=1.5)
}
par(mar=c(0,0,0,3))
plot(output.tot[which(output.tot$class=="stat.sum"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.sum"),"ci.max"]))
axis(side=4,las=2)
lines(c(1,1),c(output.tot[which(output.tot$class=="stat.sum"),"ci.min"],output.tot[which(output.tot$class=="stat.sum"),"ci.max"] ))
points(1,output.tot[which(output.tot$class=="stat.sum"),"mean"],pch=16,cex=1.5,col="darkorange")
points(1,output.tot[which(output.tot$class=="stat.sum"),"mean"],pch=1,cex=1.5)
mtext(side=4,expression("Sum ("*cm^3*" "*cm^-2*" "*d^-1*")"),padj=2.5,cex=0.8)
par(mar=c(0.2,0,0.2,3))
plot(output.tot[which(output.tot$class=="stat.cv"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.cv"),"ci.max"]))
axis(side=4,las=2)
lines(c(1,1),c(output.tot[which(output.tot$class=="stat.cv"),"ci.min"],output.tot[which(output.tot$class=="stat.cv"),"ci.max"] ))
points(1,output.tot[which(output.tot$class=="stat.cv"),"mean"],pch=16,cex=1.5,col="cyan")
points(1,output.tot[which(output.tot$class=="stat.cv"),"mean"],pch=1,cex=1.5)
mtext(side=4,expression("CV (%)"),padj=3,cex=0.8)
par(mar=c(0,0,0,3))
plot(output.tot[which(output.tot$class=="stat.length"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.length"),"ci.max"]))
axis(side=4,las=2)
lines(c(1,1),c(output.tot[which(output.tot$class=="stat.length"),"ci.min"],output.tot[which(output.tot$class=="stat.length"),"ci.max"] ))
points(1,output.tot[which(output.tot$class=="stat.length"),"mean"],pch=16,cex=1.5,col="darkblue")
points(1,output.tot[which(output.tot$class=="stat.length"),"mean"],pch=1,cex=1.5)
mtext(side=4,expression("Duration (h)"),padj=3,cex=0.8)

par(mar=c(0,4,0,0))
lab<-colnames(B)
lab<-lab[-which(lab%in%c("a","b"))]
plot(1,1,xlim=c(0.7,length(lab)+0.3),ylim=c(0,1.2),xaxt="n",ylab="Total sensitivity index (-)",xlab="",yaxt="n",col="white",cex.lab=1.2)
abline(h=seq(0,1,0.2),col="grey")
abline(v=seq(0,ncol(B),1),col="grey")
axis(side=2,las=2)
axis(side=1,at=c(1:length(lab)),labels=lab,las=2)
legend("topleft",horiz=F,c("Sum","CV","Duration"),pch=16,col=c("darkorange","cyan","darkblue"),bty="n",cex=1.2)
legend("topright","K",bty="n",text.font=2,cex=2)
pal<-data.frame(class=c("param.sum","param.cv","param.length"),col=c("darkorange","cyan","darkblue"))
loc<-data.frame(item=lab,loc=c(1:length(lab)))
off<-data.frame(class=c("param.sum","param.cv","param.length"),off=c(-0.2,0,0.2))

output.tot.k.gr<-output.tot.k[-which(output.tot.k$item%in%c("a","b")),]
for(i in c(1:nrow(output.tot.k.gr))){
  #i<-1
  sel<-output.tot.k.gr[i,]
  if(left(sel$class,4)=="stat"){next}
  lines(rep(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],2),
        c(sel$ci.min,sel$ci.max))
  points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
         sel$mean,
         pch=16,col=as.character(pal[which(as.character(pal$class)==as.character(sel$class)),"col"]),cex=1.5)
  points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
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
param<-data.frame(method=method,start.input=as.character(index(input)[1]),end.input=as.character(index(input)[length(input)]),
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
vpd<-read.table("D:/Documents/WSL/06_basic_data/1_database/Environmental_data/All_output_Tier3/Vapour_pressure_deficit.txt",header=TRUE,sep="\t")
sr<-read.table("D:/Documents/WSL/06_basic_data/1_database/Environmental_data/All_output_Tier3/Solar_radiance.txt",header=TRUE,sep="\t")
vpd<-vpd[,c("Date","N13")]
colnames(vpd)<-c("timestamp","value")
sr<-sr[,c("Timestamp","N13")]
colnames(sr)<-c("timestamp","value")
vpd_raw   <-is.trex(vpd,tz="GMT",time.format="(%m/%d/%y %H:%M:%S)",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
vpd.input <-time.step(input=vpd_raw,time.int=15,max.gap=180,decimals=15,df=F,start="2013-04-01 00:00",end="2013-11-01 00:00")
sr_raw   <-is.trex(sr,tz="GMT",time.format="(%m/%d/%y %H:%M:%S)",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
sr.input <-time.step(input=sr_raw,start="2013-04-01 00:00",end="2013-11-01 00:00",
                     time.int=15,max.gap=60,decimals=15,df=F)


#e
if(missing(vpd.input)){
  warning(paste0("No vpd.input data included."))
  vpd.input<-zoo::zoo(0,order.by=index(input))}
if(missing(sr.input)){
  warning(paste0("No sr.input data included."))
  sr.input<-zoo::zoo(0,order.by=index(input))}
if(missing(vpd.input)&missing(sr.input))stop("Invalid input data, no sr.input nor vpd.input provided.")
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

#p
step.min<-as.numeric(min(difftime(index(input)[-1],index(input)[-length(input)],units=c("mins")),na.rm=TRUE))
step.sr<-as.numeric(min(difftime(index(sr.input)[-1],index(sr.input)[-length(sr.input)],units=c("mins")),na.rm=TRUE))
step.vpd<-as.numeric(min(difftime(index(vpd.input)[-1],index(vpd.input)[-length(vpd.input)],units=c("mins")),na.rm=TRUE))

#e
if(step.min!=step.sr|step.min!=step.vpd)stop("time steps between input and vpd.input/sr.input differ, results will not be correctly aggregated.")
if(index(sr.input)[1]-index(input)[1]!=0)stop("Invalid sr.input, timestamp start does not match with input.")
if(index(sr.input)[length(sr.input)]-index(input)[length(input)]!=0)stop("Invalid sr.input, timestamp end does not match with input.")
if(index(vpd.input)[1]-index(input)[1]!=0)stop("Invalid vpd.input, timestamp start does not match with input.")
if(index(vpd.input)[length(vpd.input)]-index(input)[length(input)]!=0)stop("Invalid vpd.input, timestamp end does not match with input.")

#d
if(missing(ed.window_min)){ed.window_min<-60*2/15}
if(missing(ed.window_max)){ed.window_max<-60*4/15}
if(missing(criteria.vpd_min)){criteria.vpd_min<-0.05}
if(missing(criteria.vpd_max)){criteria.vpd_max<-0.5}
if(missing(criteria.sr_mean)){criteria.sr_mean <-30}
if(mssing(criteria.sr_range)){criteria.sr_range<-30} #in percentage
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
minutes<-as.numeric(left(right(as.character(index(env)),8),2))*60+as.numeric(left(right(as.character(index(env)),5),2))
days<-as.numeric(floor(difftime(index(env),as.Date(index(env)[1]),units="days"))+1)

#w= warnings
if(max(days)>365)warning("Input length > 365 days which can significantly recude processing speed.")

#
raw.env <- tibble(
  days = days,
  days.agg =days,
  minutes = minutes,
  value = as.numeric(as.character(env$input)),
  sr= as.numeric(as.character(env$sr)),
  vpd= as.numeric(as.character(env$vpd))
)

if(nrow(raw.env)==0)stop("Invalid environmental input data, input.sr or input.vpd do not cover the temporal range of input.")

#p= processing
criteria.sr_min<- criteria.sr_mean-(cirteria.sr_mean*(criteria.sr_range/100))
criteria.sr_max<- criteria.sr_mean+(cirteria.sr_mean*(criteria.sr_range/100))

A <- randomLHS(n,9) 
B <- matrix(nrow = nrow(A), ncol = ncol(A))
B[,1] <- ceiling(qunif(A[,1], min =-(range.end-1)/2, max = range.end/2))
B[,2] <- ceiling(qunif(A[,2], min =-(range.start-1)/2, max = range.start/2))
B[,3] <- qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
B[,4] <- qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
B[,5] <- qnorm(A[,5], mean = b_mu, sd= b_sd)
B[,6] <- ceiling(qunif(A[,6], min =ed.window_min-1, max = ed.window_max))
B[,7] <- qunif(A[,7], min =criteria.vpd_min, max = criteria.vpd_max)
B[,8] <- qunif(A[,8], min =criteria.sr_min, max = criteria.sr_max)
B[,9] <- qunif(A[,9], min =criteria.cv_min, max = criteria.cv_max)
colnames(B)<-c("zero.end","zero.start","sw.cor","a","b","ed.window","criteria.vpd","criteria.sr","criteria.cv")
X1<-data.frame(B)

A <- randomLHS(n,9) 
B <- matrix(nrow = nrow(A), ncol = ncol(A))
B[,1] <- ceiling(qunif(A[,1], min =-(range.end-1)/2, max = range.end/2))
B[,2] <- ceiling(qunif(A[,2], min =-(range.start-1)/2, max = range.start/2))
B[,3] <- qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
B[,4] <- qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
B[,5] <- qnorm(A[,5], mean = b_mu, sd= b_sd)
B[,6] <- ceiling(qunif(A[,6], min =ed.window_min-1, max = ed.window_max))
B[,7] <- qunif(A[,7], min =criteria.vpd_min, max = criteria.vpd_max)
B[,8] <- qunif(A[,8], min =criteria.sr_min, max = criteria.sr_max)
B[,9] <- qunif(A[,9], min =criteria.cv_min, max = criteria.cv_max)
colnames(B)<-c("zero.end","zero.start","sw.cor","a","b","ed.window","criteria.vpd","criteria.sr","criteria.cv")
X2<-data.frame(B)

x <- soboljansen(model = NULL , X1, X2, nboot = n)
B<-x$X
n<-nrow(B)
#p= process
registerDoParallel(cores = detectCores() - 2)
cl<-makeCluster(detectCores() - 2)
registerDoSNOW(cl)
pb <- txtProgressBar(max = nrow(B), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

output<-foreach(i=c(1:n),.combine="rbind",.packages=c("tibble","dplyr","zoo"),.options.snow = opts)%dopar%{
  setTxtProgressBar(pb, i) 
  ze<-zero.end+(B[i,1]*median(diff(minutes)))
  if(ze<0){ze<-24*60-ze}
  if(ze>24*60){ze<-ze-24*60}
  zs<-zero.start+(B[i,2]*median(diff(minutes)))
  if(zs<0){zs<-24*60-zs}
  if(zs>24*60){zs<-zs-24*60}
  
  #dt max
  proc.1<-raw.input
  if(ze>zs){proc.1[which(proc.1$minutes>ze|minutes<zs),"dt.max"]<-NA}
  if(ze<zs){proc.1[which(proc.1$minutes>ze&minutes<zs),"dt.max"]<-NA
  offset<-(60*24/median(diff(proc.1$minutes)))-ceiling(zs/median(diff(proc.1$minutes)))+1
  proc.1$days.agg<-c(proc.1$days[(offset:length(proc.1$days))],rep(max(proc.1$days),offset-1))}
  #View(proc.1)
  if(ze==zs)stop(paste("Unused argument, zero.start and zero.end are too close together.",sep=""))
  add<-tibble(
    days.add = aggregate(na.omit(proc.1)$dt.max,by=list(na.omit(proc.1)$days.agg),max,na.rm=TRUE)[,1],
    ddt.max=aggregate(na.omit(proc.1)$dt.max,by=list(na.omit(proc.1)$days.agg),max,na.rm=TRUE)[,2]
  )
  
  #adding environmental data
  raw.env$sr.roll   <-zoo::rollmean(raw.env$sr,B[i,"ed.window"],align=c("right"),na.rm=TRUE,fill=NA)
  raw.env$sr.roll   <-zoo::na.locf(raw.env$sr.roll,fromLast=T)
  raw.env$vpd.roll  <-zoo::rollmean(raw.env$vpd,B[i,"ed.window"],align=c("right"),na.rm=TRUE,fill=NA)
  raw.env$vpd.roll  <-zoo::na.locf(raw.env$vpd.roll,fromLast=T)
  raw.env$value_sd  <-zoo::rollapply(raw.env$value, width = B[i,"ed.window"], FUN = sd, align = "right",na.rm=TRUE,fill=NA)
  raw.env$value_sd  <-zoo::na.locf(raw.env$value_sd,fromLast=T)
  raw.env$value_mean<-zoo::rollapply(raw.env$value, width = B[i,"ed.window"], FUN = mean, align = "right",na.rm=TRUE,fill=NA)
  raw.env$value_mean<-zoo::na.locf(raw.env$value_mean,fromLast=T)
  raw.env$cv.roll   <-raw.env$value_sd/raw.env$value_mean*100
  
  proc.2<-full_join(proc.1,add,by=c("days.agg"="days.add"))
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
  ddtmax<-suppressWarnings(aggregate((proc.2)$ddt.max,by=list((proc.2)$days.agg),max,na.rm=TRUE))[,2]
  ddtmax[which(ddtmax=="-Inf")]<-NA
  length(ddtmax)
  ddtmax<-zoo::na.locf(zoo::na.locf(zoo::na.approx(ddtmax,na.rm=F),na.rm=F),fromLast=T)
  add2<-tibble(
    days.add = suppressWarnings(aggregate((proc.2)$ddt.max,by=list((proc.2)$days.agg),max,na.rm=TRUE))[,1],
    ddt.max.add= ddtmax 
  )
  proc.add<-full_join(proc.2,add2,by=c("days.agg"="days.add"))
  proc.add[which(is.na(proc.add$ddt.max.raw)==T),"ddt.max.add"]<-NA
  nrow(proc.add)
  nrow(proc.2)
  proc.2$ddt.max<-proc.add$ddt.max.add
  proc.2$gap<-ave(proc.2$gap, rev(cumsum(rev(is.na(proc.2$gap)))), FUN=cumsum)*median(diff(proc.1$minutes)) 
  proc.2$ddt.max<-na.locf(zoo::na.locf(proc.2$ddt.max,na.rm=F),fromLast=T)
  proc.2[which(is.na(proc.2$value)==TRUE|proc.2$gap>(60*(24+12))|proc.2$count!=median(proc.2$count,na.rm=TRUE)),"ddt.max"]<-NA

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
  d.sum.k<-suppressWarnings(aggregate(proc.2$k.pd,by=list(proc.2$days),max,na.rm=TRUE))
  d.sum.k[which(d.sum.k[,2]=="-Inf"),2]<-NA
  durat.k<-proc.2
  durat.k[which(proc.2$k.pd<=min.k),"k.pd"]<-NA
  durat.k<-aggregate(durat.k$k.pd, by=list(durat.k$days), FUN=function(x) {length(na.omit(x))})
  values.k<-aggregate(proc.2$k.pd,by=list(proc.2$days),mean,na.rm=TRUE)*24
  values.k[values.k[,2]=="NaN",2]<-NA
  
  durat<-proc.2
  durat[which(proc.2$sfd<=min.sfd),"sfd"]<-NA
  durat<-aggregate(durat$sfd, by=list(durat$days), FUN=function(x) {length(na.omit(x))})
  values<-aggregate(proc.2$sfd,by=list(proc.2$days),mean,na.rm=TRUE)*24
  values[values[,2]=="NaN",2]<-NA
  d.sum<-suppressWarnings(aggregate(proc.2$sfd,by=list(proc.2$days),max,na.rm=TRUE))
  d.sum[which(d.sum[,2]=="-Inf"),2]<-NA
  return(
    c(c(i,mean(values[,2],na.rm=TRUE),(sd(d.sum[,2],na.rm=TRUE)/mean(d.sum[,2],na.rm=TRUE))*100,(mean(durat[,2]*median(diff(proc.2$minutes)))/60),
        mean(values.k[,2],na.rm=TRUE),(sd(d.sum.k[,2],na.rm=TRUE)/mean(d.sum.k[,2],na.rm=TRUE))*100,(mean(durat.k[,2]*median(diff(proc.2$minutes)))/60)),
      c(length(proc.2$sfd),proc.2$sfd,proc.2$k.pd))
  )
}
stopImplicitCluster()

#time series output
number<-median(output[,8])
output.sfd<-output[,c(9:(8+number))]
output.k<-output[,c((9+number):(8+number+number))]
n<-nrow(output.k)
output.sfd<-data.frame(mu=apply(output.sfd,2,mean,na.rm=T),sd=apply(output.sfd,2,sd,na.rm=T),ci.max=apply(output.sfd,2,quantile,probs=c(0.975),na.rm=T),ci.min=apply(output.sfd,2,quantile,probs=c(0.025),na.rm=T))
output.k<-data.frame(mu=apply(output.k,2,mean,na.rm=T),sd=apply(output.k,2,sd,na.rm=T),ci.max=apply(output.k,2,quantile,probs=c(0.975),na.rm=T),ci.min=apply(output.k,2,quantile,probs=c(0.025),na.rm=T))
output.sfd<-zoo(output.sfd,order.by=index(input))
output.k<-zoo(output.k,order.by=index(input))

if(df==T){
  output.sfd<-zoo::fortify.zoo(output.sfd)
  output.k<-zoo::fortify.zoo(output.k)
  colnames(output.sfd)<-c("timestamp","mu","sd","ci.max","ci.min")
  colnames(output.k)<-c("timestamp","mu","sd","ci.max","ci.min")
}

#o= output
output<-output[,c(1:7)]
output_sum<-tell(x,as.numeric(output[,2]))
output_cv<-tell(x,output[,3])
output_length<-tell(x,output[,4])

output_sum.k<-tell(x,output[,5])
output_cv.k<-tell(x,output[,6])
output_length.k<-tell(x,output[,7])

output.tot<-data.frame(item=c(row.names(output_sum$T),"daily.sum",row.names(output_cv$T),"max.cv",row.names(output_length$T),"length.dur"),
                       class=c(rep("param.sum",length(row.names(output_sum$T))),"stat.sum",rep("param.cv",length(row.names(output_cv$T))),"stat.cv",rep("param.length",length(row.names(output_length$T))),"stat.length"),
                       factor="SFD",
                       mean=c(output_sum$T[,1],mean(output[,2]),output_cv$T[,1],mean(output[,3]),output_length$T[,1],mean(output[,4])),
                       sd=c(output_sum$T[,3],sd(output[,2]),output_cv$T[,3],sd(output[,3]),output_length$T[,3],sd(output[,4])),
                       ci.min=c(output_sum$T[,4],quantile(output[,2],probs=0.025),output_cv$T[,4],quantile(output[,3],probs=0.025),output_length$T[,4],quantile(output[,4],probs=0.025)),
                       ci.max=c(output_sum$T[,5],quantile(output[,2],probs=0.975),output_cv$T[,5],quantile(output[,3],probs=0.975),output_length$T[,5],quantile(output[,4],probs=0.975))
)
output.tot.k<-data.frame(item=c(row.names(output_sum.k$T),"daily.sum",row.names(output_cv.k$T),"max.cv",row.names(output_length.k$T),"length.dur"),
                         class=c(rep("param.sum",length(row.names(output_sum.k$T))),"stat.sum",rep("param.cv",length(row.names(output_cv.k$T))),"stat.cv",rep("param.length",length(row.names(output_length.k$T))),"stat.length"),
                         factor="K",
                         mean=c(output_sum.k$T[,1],mean(output[,5]),output_cv.k$T[,1],mean(output[,6]),output_length.k$T[,1],mean(output[,7])),
                         sd=c(output_sum.k$T[,3],sd(output[,5]),output_cv.k$T[,3],sd(output[,6]),output_length.k$T[,3],sd(output[,7])),
                         ci.min=c(output_sum.k$T[,4],quantile(output[,5],probs=0.025),output_cv.k$T[,4],quantile(output[,6],probs=0.025),output_length.k$T[,4],quantile(output[,7],probs=0.025)),
                         ci.max=c(output_sum.k$T[,5],quantile(output[,5],probs=0.975),output_cv.k$T[,5],quantile(output[,6],probs=0.975),output_length.k$T[,5],quantile(output[,7],probs=0.975))
)

#figure output
#setwd("D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX")
#pdf("Figure_SPD_sensitivity_2000_pd.pdf",height=6,width=8)
if(make.plot==T){
  layout(
    matrix(
      c(5,5,1,1,1,2,
        5,5,1,1,1,3,
        5,5,1,1,1,4), 
      nc=6, byrow = TRUE))
  #make output for the package
  par(oma=c(6,3,3,3))
  par(mar=c(0,2,0,0))
  plot(1,1,xlim=c(0.7,ncol(B)+0.3),ylim=c(0,1.2),xaxt="n",xlab="",ylab="",yaxt="n",col="white",cex.lab=1.2)
  abline(h=seq(0,1,0.2),col="grey")
  abline(v=seq(0,ncol(B),1),col="grey")
  axis(side=2,las=2,labels=F)
  lab<-colnames(B)
  axis(side=1,at=c(1:ncol(B)),labels=lab,las=2)
  legend("topright","SFD",bty="n",text.font=2,cex=2)
  mtext(side=3,"Environmental dependent",outer=TRUE,padj=-0.5)
  pal<-data.frame(class=c("param.sum","param.cv","param.length"),col=c("darkorange","cyan","darkblue"))
  loc<-data.frame(item=lab,loc=c(1:length(lab)))
  off<-data.frame(class=c("param.sum","param.cv","param.length"),off=c(-0.2,0,0.2))
  for(i in c(1:nrow(output.tot))){
    sel<-output.tot[i,]
    if(left(sel$class,4)=="stat"){next}
    lines(rep(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],2),
          c(sel$ci.min,sel$ci.max))
    points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
           sel$mean,
           pch=16,col=as.character(pal[which(as.character(pal$class)==as.character(sel$class)),"col"]),cex=1.5)
    points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
           sel$mean,
           pch=1,cex=1.5)
  }
  par(mar=c(0,0,0,3))
  plot(output.tot[which(output.tot$class=="stat.sum"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.sum"),"ci.max"]))
  axis(side=4,las=2)
  lines(c(1,1),c(output.tot[which(output.tot$class=="stat.sum"),"ci.min"],output.tot[which(output.tot$class=="stat.sum"),"ci.max"] ))
  points(1,output.tot[which(output.tot$class=="stat.sum"),"mean"],pch=16,cex=1.5,col="darkorange")
  points(1,output.tot[which(output.tot$class=="stat.sum"),"mean"],pch=1,cex=1.5)
  mtext(side=4,expression("Sum ("*cm^3*" "*cm^-2*" "*d^-1*")"),padj=2.5,cex=0.8)
  par(mar=c(0.2,0,0.2,3))
  plot(output.tot[which(output.tot$class=="stat.cv"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.cv"),"ci.max"]))
  axis(side=4,las=2)
  lines(c(1,1),c(output.tot[which(output.tot$class=="stat.cv"),"ci.min"],output.tot[which(output.tot$class=="stat.cv"),"ci.max"] ))
  points(1,output.tot[which(output.tot$class=="stat.cv"),"mean"],pch=16,cex=1.5,col="cyan")
  points(1,output.tot[which(output.tot$class=="stat.cv"),"mean"],pch=1,cex=1.5)
  mtext(side=4,expression("CV (%)"),padj=3,cex=0.8)
  par(mar=c(0,0,0,3))
  plot(output.tot[which(output.tot$class=="stat.length"),"mean"]  ,yaxt="n",xaxt="n",ylab="",xlab="",col="white",ylim=c(0,output.tot[which(output.tot$class=="stat.length"),"ci.max"]))
  axis(side=4,las=2)
  lines(c(1,1),c(output.tot[which(output.tot$class=="stat.length"),"ci.min"],output.tot[which(output.tot$class=="stat.length"),"ci.max"] ))
  points(1,output.tot[which(output.tot$class=="stat.length"),"mean"],pch=16,cex=1.5,col="darkblue")
  points(1,output.tot[which(output.tot$class=="stat.length"),"mean"],pch=1,cex=1.5)
  mtext(side=4,expression("Duration (h)"),padj=3,cex=0.8)
  
  par(mar=c(0,4,0,0))
  lab<-colnames(B)
  lab<-lab[-which(lab%in%c("a","b"))]
  plot(1,1,xlim=c(0.7,length(lab)+0.3),ylim=c(0,1.2),xaxt="n",ylab="Total sensitivity index (-)",xlab="",yaxt="n",col="white",cex.lab=1.2)
  abline(h=seq(0,1,0.2),col="grey")
  abline(v=seq(0,ncol(B),1),col="grey")
  axis(side=2,las=2)
  axis(side=1,at=c(1:length(lab)),labels=lab,las=2)
  legend("topleft",horiz=F,c("Sum","CV","Duration"),pch=16,col=c("darkorange","cyan","darkblue"),bty="n",cex=1.2)
  legend("topright","K",bty="n",text.font=2,cex=2)
  pal<-data.frame(class=c("param.sum","param.cv","param.length"),col=c("darkorange","cyan","darkblue"))
  loc<-data.frame(item=lab,loc=c(1:length(lab)))
  off<-data.frame(class=c("param.sum","param.cv","param.length"),off=c(-0.2,0,0.2))
  
  output.tot.k.gr<-output.tot.k[-which(output.tot.k$item%in%c("a","b")),]
  for(i in c(1:nrow(output.tot.k.gr))){
    #i<-1
    sel<-output.tot.k.gr[i,]
    if(left(sel$class,4)=="stat"){next}
    lines(rep(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],2),
          c(sel$ci.min,sel$ci.max))
    points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
           sel$mean,
           pch=16,col=as.character(pal[which(as.character(pal$class)==as.character(sel$class)),"col"]),cex=1.5)
    points(loc[which(as.character(loc$item)==as.character(sel$item)),"loc"]+off[which(as.character(off$class)==as.character(sel$class)),"off"],
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
param<-data.frame(method=method,start.input=as.character(index(input)[1]),end.input=as.character(index(input)[length(input)]),
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

#perform an uncertainty and sensitivity analysis on "dr" data processing
raw   <- example.data(type="doy")
input <- is.trex(raw,tz="GMT",time.format="%H:%M",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE,df=FALSE)
input <-time.step(input,time.int=15,start="2013-04-01 00:00",end="2013-11-01 00:00",max.gap=180,decimals=15)
plot(input)
output<- tdm_uncertain(input,probe.length=20,method="pd",n=100,sw.cor=32.28,sw.sd=16,log.a_mu=3.792436,
                       log.a_sd=0.4448937,b_mu=1.177099,b_sd=0.3083603,make.plot=TRUE)


