#uncertain
require(lhs)
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

#t= test
raw   <- example.data(type="doy", species="PCAB")
input <- is.trex(raw,tz="GMT",time.format="%H:%M",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE,df=FALSE)
plot(input)

#cleaining
window(input,start=as.POSIXct(as.character("(01/01/12 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"),
       end=as.POSIXct(as.character("(01/01/13 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"))[which(
         window(input,start=as.POSIXct(as.character("(01/01/12 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"),
                end=as.POSIXct(as.character("(01/01/13 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"))<0.5)]<-NA
window(input,start=as.POSIXct(as.character("(01/01/13 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"),
       end=as.POSIXct(as.character("(01/01/16 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"))[which(
         window(input,start=as.POSIXct(as.character("(01/01/13 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"),
                end=as.POSIXct(as.character("(01/01/16 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"))<0.64|
           window(input,start=as.POSIXct(as.character("(01/01/13 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"),
                  end=as.POSIXct(as.character("(01/01/16 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"))>0.81)]<-NA
input<-time.step(input,time.int=15,start="2013-04-01 00:00",end="2013-11-01 00:00",max.gap=180,decimals=15)
plot(input)


vpd<-read.table("D:/Documents/WSL/06_basic_data/1_database/Environmental_data/All_output_Tier3/Vapour_pressure_deficit.txt",header=TRUE,sep="\t")
sr<-read.table("D:/Documents/WSL/06_basic_data/1_database/Environmental_data/All_output_Tier3/Solar_radiance.txt",header=TRUE,sep="\t")
vpd<-vpd[,c("Date","N13")]
colnames(vpd)<-c("timestamp","value")
sr<-sr[,c("Timestamp","N13")]
colnames(sr)<-c("timestamp","value")
vpd_raw   <-is.trex(vpd,tz="GMT",time.format="(%m/%d/%y %H:%M:%S)",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
vpd.input <-time.step(input=vpd_raw,time.int=15,max.gap=180,decimals=15,df=F,start="2012-05-01 00:00",end="2012-10-01 00:00")
sr_raw   <-is.trex(sr,tz="GMT",time.format="(%m/%d/%y %H:%M:%S)",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
sr.input <-time.step(input=sr_raw,start="2012-05-01 00:00",end="2012-10-01 00:00",
                     time.int=15,max.gap=60,decimals=15,df=F)
plot(vpd.input)
par(new=T)
plot(input,col="red")
lines(sr_raw,col="red")

abline(h=100)
abline(h=100+100*0.3)
plot(vpd.input)


#d= default conditions
zero.end<-8*60
zero.start<-1*60
range.end<- 16# number of timesteps
range.start<- 16 # number of timesteps
  
probe.length<-20
sw.cor<-32.28
sw.sd<-16

log.a_mu<-3.792436 
log.a_sd<-0.4448937
b_mu<-1.177099
b_sd<-0.3083603

damp.cor<-0
k.threshold<-0.5

max.days_min<-1
max.days_max<-7

ed.window_min<-
ed.window_max<-

cirteria.vpd_min<-0.05
criteria.vpd_max<-0.5

cirteria.sr_mean <-30
criteria.sr_range<-30 #in percentage

criteria.cv_min<-0.5
criteria.cv_max<-1

  
input<-input
methods<-c("pd","mw","dr","ed")

min.sfd<-0.05
min.k<-0
n=2000

#e= error


#f= small functions
left = function(string, char){substr(string, 1,char)}
right = function (string, char){substr(string,nchar(string)-(char-1),nchar(string))}

#convert input to a tibble
minutes<-as.numeric(left(right(as.character(index(input)),8),2))*60+as.numeric(left(right(as.character(index(input)),5),2))
days<-as.numeric(floor(difftime(index(input),as.Date(index(input)[1]),units="days"))+1)

raw.input <- tibble(
  days = days,
  days.agg =days,
  minutes = minutes,
  value = as.numeric(as.character(input)),
  dt.max = as.numeric(as.character(input)),
  count = 1,
  gap = 1
)
length.input <- na.omit(tibble(days = days,value = as.numeric(as.character(input))))
raw.input$count<-full_join(raw.input,aggregate(length.input, list(length.input$days), FUN=length),by=c("days"="Group.1"))$value.y

#pd----
#only for pd uncertainty
#p= processing
A <- randomLHS(n,5) 
B <- matrix(nrow = nrow(A), ncol = ncol(A))
B[,1] <- ceiling(qunif(A[,1], min =-range.end/2, max = range.end/2))
B[,2] <- ceiling(qunif(A[,2], min =-range.start/2, max = range.start/2))
B[,3] <- qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
B[,4] <- qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
B[,5] <- qnorm(A[,5], mean = b_mu, sd= b_sd)
colnames(B)<-c("zero.end","zero.start","sw.cor","a","b")
X1<-data.frame(B)

A <- randomLHS(n,5) 
B <- matrix(nrow = nrow(A), ncol = ncol(A))
B[,1] <- ceiling(qunif(A[,1], min =-range.end/2, max = range.end/2))
B[,2] <- ceiling(qunif(A[,2], min =-range.start/2, max = range.start/2))
B[,3] <- qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
B[,4] <- qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
B[,5] <- qnorm(A[,5], mean = b_mu, sd= b_sd)
colnames(B)<-c("zero.end","zero.start","sw.cor","a","b")
X2<-data.frame(B)

x <- soboljansen(model = NULL , X1, X2, nboot = 100)
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
  c(i,mean(values[,2],na.rm=TRUE),(sd(d.sum[,2],na.rm=TRUE)/mean(d.sum[,2],na.rm=TRUE))*100,(mean(durat[,2]*median(diff(proc.2$minutes)))/60),
    mean(values.k[,2],na.rm=TRUE),(sd(d.sum.k[,2],na.rm=TRUE)/mean(d.sum.k[,2],na.rm=TRUE))*100,(mean(durat.k[,2]*median(diff(proc.2$minutes)))/60)))
}
stopImplicitCluster()

#o= output
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
setwd("D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX")
pdf("Figure_SPD_sensitivity_2000.pdf",height=6,width=8)
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
mtext(side=4,expression("SFD ("*cm^3*cm^-2*" "*d^-1*")"),padj=2.5,cex=0.8)
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
dev.off()
output.tot.k.gr<-output.tot.k.gr[-which(left(output.tot.k.gr$class,4)=="stat"),]
output.tot.gr<-output.tot[-which(left(output.tot$class,4)=="stat"),]
output.tot.k.gr$analysis<-rep("sensitivity",nrow(output.tot.k.gr))
output.tot.gr$analysis<-rep("sensitivity",nrow(output.tot.gr))
output.sen<-output.tot[which(left(output.tot$class,4)=="stat"),]
output.sen$analysis<-rep("uncertainty",nrow(output.sen))
output.data<-rbind(output.tot.k.gr,output.tot.gr,output.sen)
return(output.data)
#----

#mw----
n<-2000
A <- randomLHS(n, 6) 
B <- matrix(nrow = nrow(A), ncol = ncol(A))
B[,1] <- ceiling(qunif(A[,1], min =-range.end/2, max = range.end/2))
B[,2] <- ceiling(qunif(A[,2], min =-range.start/2, max = range.start/2))
B[,3] <- qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
B[,4] <- qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
B[,5] <- qnorm(A[,5], mean = b_mu, sd= b_sd)
B[,6] <- ceiling(qunif(A[,6], min =max.days_min, max = max.days_max))
colnames(B)<-c("zero.end","zero.start","sw.cor","a","b","max.days")
X1<-data.frame(B)

A <- randomLHS(n, 6) 
B <- matrix(nrow = nrow(A), ncol = ncol(A))
B[,1] <- ceiling(qunif(A[,1], min =-range.end/2, max = range.end/2))
B[,2] <- ceiling(qunif(A[,2], min =-range.start/2, max = range.start/2))
B[,3] <- qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
B[,4] <- qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
B[,5] <- qnorm(A[,5], mean = b_mu, sd= b_sd)
B[,6] <- ceiling(qunif(A[,6], min =max.days_min, max = max.days_max))
colnames(B)<-c("zero.end","zero.start","sw.cor","a","b","max.days")
X2<-data.frame(B)

x <- soboljansen(model = NULL , X1, X2, nboot = 100)
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
    c(i,mean(values[,2],na.rm=TRUE),(sd(d.sum[,2],na.rm=TRUE)/mean(d.sum[,2],na.rm=TRUE))*100,(mean(durat[,2]*median(diff(proc.2$minutes)))/60),
      mean(values.k[,2],na.rm=TRUE),(sd(d.sum.k[,2],na.rm=TRUE)/mean(d.sum.k[,2],na.rm=TRUE))*100,(mean(durat.k[,2]*median(diff(proc.2$minutes)))/60)))
}
stopImplicitCluster()

#o= output
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
pdf("Figure_SMW_sensitivity_2000.pdf",height=6,width=8)
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
mtext(side=4,expression("SFD ("*cm^3*cm^-2*" "*d^-1*")"),padj=2.5,cex=0.8)
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
dev.off()

output.tot.k.gr<-output.tot.k.gr[-which(left(output.tot.k.gr$class,4)=="stat"),]
output.tot.gr<-output.tot[-which(left(output.tot$class,4)=="stat"),]
output.tot.k.gr$analysis<-rep("sensitivity",nrow(output.tot.k.gr))
output.tot.gr$analysis<-rep("sensitivity",nrow(output.tot.gr))
output.sen<-output.tot[which(left(output.tot$class,4)=="stat"),]
output.sen$analysis<-rep("uncertainty",nrow(output.sen))
output.data<-rbind(output.tot.k.gr,output.tot.gr,output.sen)
return(output.data)

#----

#dr----
n<-2000
A <- randomLHS(n, 6) 
B <- matrix(nrow = nrow(A), ncol = ncol(A))
B[,1] <- ceiling(qunif(A[,1], min =-range.end/2, max = range.end/2))
B[,2] <- ceiling(qunif(A[,2], min =-range.start/2, max = range.start/2))
B[,3] <- qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
B[,4] <- qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
B[,5] <- qnorm(A[,5], mean = b_mu, sd= b_sd)
B[,6] <- ceiling(qunif(A[,6], min =max.days_min, max = max.days_max))
colnames(B)<-c("zero.end","zero.start","sw.cor","a","b","max.days")
X1<-data.frame(B)

A <- randomLHS(n, 6) 
B <- matrix(nrow = nrow(A), ncol = ncol(A))
B[,1] <- ceiling(qunif(A[,1], min =-range.end/2, max = range.end/2))
B[,2] <- ceiling(qunif(A[,2], min =-range.start/2, max = range.start/2))
B[,3] <- qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
B[,4] <- qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
B[,5] <- qnorm(A[,5], mean = b_mu, sd= b_sd)
B[,6] <- ceiling(qunif(A[,6], min =max.days_min, max = max.days_max))
colnames(B)<-c("zero.end","zero.start","sw.cor","a","b","max.days")
X2<-data.frame(B)

x <- soboljansen(model = NULL , X1, X2, nboot = 100)
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
    c(i,mean(values[,2],na.rm=TRUE),(sd(d.sum[,2],na.rm=TRUE)/mean(d.sum[,2],na.rm=TRUE))*100,(mean(durat[,2]*median(diff(proc.2$minutes)))/60),
      mean(values.k[,2],na.rm=TRUE),(sd(d.sum.k[,2],na.rm=TRUE)/mean(d.sum.k[,2],na.rm=TRUE))*100,(mean(durat.k[,2]*median(diff(proc.2$minutes)))/60)))
}
stopImplicitCluster()

#o= output
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
setwd("D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX")
pdf("Figure_sensitivity_2000.pdf",height=6,width=8)
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
mtext(side=4,expression("SFD ("*cm^3*cm^-2*" "*d^-1*")"),padj=2.5,cex=0.8)
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
dev.off()
output.tot.k.gr<-output.tot.k.gr[-which(left(output.tot.k.gr$class,4)=="stat"),]
output.tot.gr<-output.tot[-which(left(output.tot$class,4)=="stat"),]
output.tot.k.gr$analysis<-rep("sensitivity",nrow(output.tot.k.gr))
output.tot.gr$analysis<-rep("sensitivity",nrow(output.tot.gr))
output.sen<-output.tot[which(left(output.tot$class,4)=="stat"),]
output.sen$analysis<-rep("uncertainty",nrow(output.sen))
output.data<-rbind(output.tot.k.gr,output.tot.gr,output.sen)

write.table(output.data,"output.data.sen.txt",col.names=TRUE,row.names=F,sep="\t")
return(output.data)









x <- soboljansen(model = NULL , X1, X2, nboot = 100)
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
  ze<-zero.end+B[i,1]
  if(ze<0){ze<-24*60-ze}
  if(ze>24*60){ze<-ze-24*60}
  zs<-zero.start+B[i,2]
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

  m.day<-round(B[i,"max.days"],0)
  proc.1_2<-full_join(add,data.frame(days=c(1:max(add[,1])),rmax=NA),by=c("days.add"="days"))
  proc.1_2<-proc.1_2[order(proc.1_2$days.add),]
  proc.1_2$rmax<-zoo::na.locf(zoo::na.locf(rollmax(proc.1_2$ddt.max,m.day,align = c("center"),na.rm=TRUE,fill=NA),na.rm=F),fromLast=TRUE)
  proc.1_2<-na.omit(proc.1_2)
  proc.1_2$ddt.max<-proc.1_2$rmax
  add<-proc.1_2[,c(1,2)]
  
  proc.2<-full_join(proc.1,add,by=c("days.agg"="days.add"))
  proc.2[which(proc.2$ddt.max!=proc.2$dt.max|is.na(proc.2$dt.max)==TRUE),"ddt.max"]<-NA
  proc.2[which(is.na(proc.2$ddt.max)==FALSE),"gap"]<-NA
  proc.2$gap<-ave(proc.2$gap, rev(cumsum(rev(is.na(proc.2$gap)))), FUN=cumsum)*median(diff(proc.1$minutes)) 
  proc.2$ddt.max<-na.locf(zoo::na.locf(proc.2$ddt.max,na.rm=F),fromLast=TRUE)
  proc.2[which(is.na(proc.2$value)==TRUE|proc.2$gap>(60*24)|proc.2$count!=median(proc.2$count,na.rm=TRUE)),"ddt.max"]<-NA
  
  #HW correction
  #i<-4
  swt<-B[i,3]
  if(swt<5){swt<-5}
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
  values<-aggregate(proc.2$sfd,by=list(proc.2$days),mean,na.rm=TRUE)*24
  values[values[,2]=="NaN",2]<-NA
  d.sum<-suppressWarnings(aggregate(proc.2$sfd,by=list(proc.2$days),max,na.rm=TRUE))
  d.sum[which(d.sum[,2]=="-Inf"),2]<-NA
  return(c(i,mean(values[,2],na.rm=TRUE),(sd(d.sum[,2],na.rm=TRUE)/mean(d.sum[,2],na.rm=TRUE))*100))
}
stopImplicitCluster()

output_sum<-tell(x,as.numeric(output[,2]))
output_cv<-tell(x,output[,3])
output_cv<-tell(x,output[,3])

plot(output_sum)
plot(output_cv)
print(output_sum)




print(output_cv)
plot(output_cv)
str(output_sum)

print(x)
plot(x)

x <- fast99(model = NULL, factors = 3, n = 1000,
            q = "qunif", q.arg = list(min = -pi, max = pi))
y <- ishigami.fun(x$X)
tell(x, y)
print(x)
plot(x)

as.vector(output[,2])
plot(output[,2])
length(output[,2])
print(x)

plot(input)

plot(output[,2])
plot(output[,3])
hist(output[,2])
hist(output[,3])


# Example of use of fast99 with "model = NULL"
x <- fast99(model = NULL, factors = 3, n = 1000,
            q = "qunif", q.arg = list(min = -pi, max = pi))
y <- ishigami.fun(x$X)
tell(x, y)
print(x)
plot(x)


cube<-cbind(B,output)

plot(cube$zero.end,cube$mu_D.SFD)


write.table(cube,"cube_pd.txt",row.names=FALSE,col.names=TRUE,sep="\t")

plot()

plot(output[,3])





#----

#ed----
A <- randomLHS(n, 6) 
B <- matrix(nrow = nrow(A), ncol = ncol(A))
B[,1] <- ceiling(qunif(A[,1], min =-range.end/2, max = range.end/2))
B[,2] <- ceiling(qunif(A[,2], min =-range.start/2, max = range.start/2))
B[,3] <- qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
B[,4] <- qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
B[,5] <- qnorm(A[,5], mean = b_mu, sd= b_sd)


B[,6] <- ceiling(qunif(A[,6], min =max.days_min, max = max.days_max))
colnames(B)<-c("zero.end","zero.start","sw.cor","a","b","max.days")
X1<-data.frame(B)

A <- randomLHS(n, 6) 
B <- matrix(nrow = nrow(A), ncol = ncol(A))
B[,1] <- ceiling(qunif(A[,1], min =-range.end/2, max = range.end/2))
B[,2] <- ceiling(qunif(A[,2], min =-range.start/2, max = range.start/2))
B[,3] <- qtnorm(A[,3], mean = sw.cor, sd = sw.sd,lower=5)
B[,4] <- qlnorm(A[,4], meanlog = log.a_mu, sdlog = log.a_sd)
B[,5] <- qnorm(A[,5], mean = b_mu, sd= b_sd)
B[,6] <- ceiling(qunif(A[,6], min =max.days_min, max = max.days_max))
colnames(B)<-c("zero.end","zero.start","sw.cor","a","b","max.days")
X2<-data.frame(B)

x <- soboljansen(model = NULL , X1, X2, nboot = 100)
B<-x$X
n<-nrow(B)
#p= process
registerDoParallel(cores = detectCores() - 2)
cl<-makeCluster(detectCores() - 2)
registerDoSNOW(cl)
pb <- txtProgressBar(max = nrow(B), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)





#----


# Packages -------------------------------------------------------------------------






# to make it 'reproducible'
set.seed(123456789)

# Input parameter ranges -----------------------------------------------------------
zero.end = 8*60
zero.start = 1*60
range.end = 4*30
range.start = 4*30

#probe.length = 20
sw.cor = 32.28
sw.sd = 16

log.a_mu = 3.792436
log.a_sd = 0.4448937
b_mu = 1.177099
b_sd = 0.3083603

# we can decrease this number if it gets too computationally expensive (e.g. put 200)
n = 500

# Make the initial parameter matrices -----------------------------------------------
X1 <- data.frame(zero.end = runif(n, min =-range.end/2, max = range.end/2),
                 zero.start = runif(n, min =-range.start/2, max = range.start/2),
                 sw.cor = rnorm(n, mean = sw.cor, sd = sw.sd),
                 a = rlnorm(n, meanlog = log.a_mu, sdlog = log.a_sd),
                 b = rnorm(n, mean = b_mu, sd= b_sd)
)

X2 <- data.frame(zero.end = runif(n, min =-range.end/2, max = range.end/2),
                 zero.start = runif(n, min =-range.start/2, max = range.start/2),
                 sw.cor = rnorm(n, mean = sw.cor, sd = sw.sd),
                 a = rlnorm(n, meanlog = log.a_mu, sdlog = log.a_sd),
                 b = rnorm(n, mean = b_mu, sd= b_sd)
)


# Input parameter table for Sobol first and total indices ---------------------------
# Get the full matrix with the 'special design' in total (p+2)*n iterations/rows
# i.e., for this example: n=500, p=5 -> 3500 iterations

x <- soboljansen(model = NULL , X1, X2, nboot = 100)
x$X
# to access the final matrix of the parameters (constructed usin X1, X2) : x$X

write.table(x$X, file="/Users/cpappas/Documents/TREX_sensitivity_test.txt", col.names=T, row.names=F, quote=F, sep="\t")

# 'Offline' calculation of first and total order effects ----------------------------
# run the sap flow processing steps, store the outputs in vectrors/matrix and then do:
tell(x, output1) # where output1 is the vector of a given output metric (i.e., dimentions of output1: rows=(p+2)*n, column=1)
#print(x)
#plot(x)






hist(daily)

plot(daily_max)



plot(proc.2)

x<-seq(0,2,0.01)
y<-a*x^b
plot(x,y)




plot(proc.1$input)
points(proc.1$all.pd,col="red")


hist(B[,3])



head(B)

B[,2] <- qlnorm(A[,2], meanlog = 0.5, sdlog = 1)
B[,3] <- A[,3]
B[,4] <- qunif(A[,4], min = 7, max = 10)
B




range.end


#Latin Hypercube







#baseline determination
dt.max(input,method=c("pd"),zero.end=8*60,zero.start= 1*60,det.pd=TRUE)


methods=c("pd","mw","dr"),zero.end=8*60,zero.start= 1*60,interpolate=FALSE,det.pd=TRUE,max.days=7,ed.window=2*60,
vpd.input,sr.input,sel.max,criteria=c(sr=30,vpd=0.1,cv=0.5)
