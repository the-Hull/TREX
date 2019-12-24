require(zoo)

#damp
damp<-function(input,k.threshold= 0.05,make.plot=TRUE,df=FALSE){
#t= test
#test<-read.table("D:/Documents/WSL/06_basic_data/1_database/Physiological_data/Sapflow_most.txt",header=TRUE,sep="\t")
#test<-test[,c("TIMESTAMP","N13Ad_S1")]
#colnames(test)<-c("timestamp","value")
#test$timestamp<-as.character(test$timestamp)
#raw       <-is.trex(test,tz="GMT",time.format="(%m/%d/%y %H:%M:%S)",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
#raw       <-is.trex(example.data(type="doy", species="PCAB"),tz="GMT",time.format="%H:%M",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
#raw       <-is.trex(raw,tz="GMT",time.format="(%m/%d/%y %H:%M:%S)",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
#input     <-time.step(input=raw,time.int=15,max.gap=60,decimals=6,df=F)
#input<-dt.max(input, methods=c("pd","mw","dr"),det.pd=TRUE,interpolate=FALSE,max.days=10,sr.input=sr.input,vpd.input=vpd.input,
#                   ed.window=2*60,criteria=c(sr=30,vpd=0.1,cv=0.5),df=FALSE)
#make.plot<-TRUE
#df<-F
#k.threshold<-0.05
#input<-k.pd

#d= default conditions  
if(missing(make.plot)){make.plot<-F}
if(missing(df)){df=F}
if(missing(k.threshold)){k.threshold<-0.05}

#e= errors
if(df!=T&df!=F)stop("Unused argument, df needs to be TRUE|FALSE.")
if(is.numeric(k.threshold)==FALSE)stop("Unused argument, k. threshold needs to be numeric.")
if(make.plot!=T&make.plot!=F)stop("Unused argument, make.plot needs to be TRUE|FALSE.")
if(length(names(input))!=0){
if(length(which(names(input)%in%c("max.pd","max.mw","max.dr","max.ed","daily_max.pd","daily_max.mw","daily_max.dr","daily_max.ed","all.pd","all.ed",      
                                  "input","ed.criteria","methods","k.pd","k.mw","k.dr","k.ed")))!=17)stop("Invalid input data, data not originating from dt.max().")

#p= process
meth<-input[["methods"]]
#input[["damp.mod.pd"]]<-NA
#input[["damp.mod.mw"]]<-NA
#input[["damp.mod.dr"]]<-NA
#input[["damp.mod.ed"]]<-NA

for(i in c(1:length(meth))){
k<-input[[paste0("k.",meth[i])]]

  if(attributes(k)$class=="data.frame"){
    #e
    if(is.numeric(k$value)==F)stop("Invalid input data, values within the data.frame are not numeric.")
    if(is.character(k$timestamp)==F)stop("Invalid input data, timestamp within the data.frame are not numeric.")
    
    #p
    k<-zoo::zoo(k$value,order.by=base::as.POSIXct(k$timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC")) 
    
    #e
    if(as.character(index(k)[1])=="(NA NA)"|is.na(index(k)[1])==T)stop("No timestamp present, time.format is likely incorrect.")
  } 
  if(zoo::is.zoo(k)==TRUE){
    if(is.numeric(k)==F)stop("Invalid input data, zoo object does not contain numeric data.")
  }

day.max          <-suppressWarnings(aggregate(k,by=list(as.Date(index(k))),max,na.rm=TRUE))
day.max[which(day.max=="-Inf")]<-NA
day.max[which(day.max=="Inf")]<-NA
test.lenght<-length(na.omit(day.max))
initial.date     <-index(day.max)[1]
length.date      <-length(day.max)
zoo.date         <-index(day.max)
day.max[which(day.max<k.threshold)]<-NA

#e
if(length(na.omit(day.max))/test.lenght*100<25)stop("Unused argument, k. threshold removed over 75% of all data points.")

#p
doy<-zoo::zoo(as.numeric(strftime(index(day.max), format = "%j")),order.by=index(day.max))
run<-zoo::zoo(index(day.max)-initial.date+1,order.by=index(day.max))
proc.1<-na.omit(cbind(doy,day.max,run))

model.doy<-lm(day.max~doy+I(doy^2),data=proc.1)
proc.1$year<-as.numeric(substr(as.character(index(proc.1)), 1,4))
proc.1$day.max.cor<-NA

for(y in c(1:length(unique(proc.1$year)))){
  scaled    <-proc.1[which(proc.1$year==unique(proc.1$year)[y]),"day.max"]/quantile(proc.1[which(proc.1$year==unique(proc.1$year)[y]),"day.max"],probs=c(0.95),na.rm=TRUE)
  max.origin<-quantile(proc.1[which(proc.1$year==unique(proc.1$year)[1]),"day.max"],probs=c(0.95),na.rm=TRUE)
  min.scaled<-min(scaled,na.rm=TRUE)
  scaled<-(proc.1[which(proc.1$year==unique(proc.1$year)[y]),"day.max"]-(min(proc.1[which(proc.1$year==unique(proc.1$year)[y]),"day.max"],na.rm=TRUE)) )/
    (quantile(proc.1[which(proc.1$year==unique(proc.1$year)[y]),"day.max"],probs=c(0.95),na.rm=TRUE)-(min(proc.1[which(proc.1$year==unique(proc.1$year)[y]),"day.max"],na.rm=TRUE)))
  proc.1[which(proc.1$year==unique(proc.1$year)[y]),"day.max.cor"]<-scaled*(quantile(proc.1[which(proc.1$year==unique(proc.1$year)[1]),"day.max"],probs=c(0.95),na.rm=TRUE)-
                                                                              min(proc.1[which(proc.1$year==unique(proc.1$year)[y]),"day.max"],na.rm=TRUE))+min(proc.1[which(proc.1$year==unique(proc.1$year)[y]),"day.max"],na.rm=TRUE)}
model.doy          <-lm(day.max.cor~doy+I(doy^2),data=proc.1)
pred<-model.doy$coefficients[1]+proc.1$doy*model.doy$coefficients[2]+proc.1$doy^2*model.doy$coefficients[3]
proc.1$day.max.doy<-proc.1$day.max-pred
run<-proc.1$run
k.doy<-proc.1$day.max.doy
report<-try(
  nls(k.doy~ ((a+b*run)/(1+c*run+d*(run^2))), start = list(a=0.2,b =-0.002, c = 0.03, d = 0.00001),control = list(maxiter = 500)),silent=TRUE)
if(class(report)!="try-error"){
  model_nls<-nls(k.doy~ ((a+b*run)/(1+c*run+d*(run^2))), start = list(a=0.2,b =-0.002, c = 0.03, d = 0.00001),control = list(maxiter = 500))
}else{
  for(n in c(1:20)){
    b<-runif(1,min=-0.003,max=0.003)
    report<-try(nls(k.doy~ ((a+b*run)/(1+c*run+d*(run^2))), start = list(a=0.2,b =b, c = 0.03, d = 0.00001),control = list(maxiter = 500)),silent=TRUE)
    if(class(report)=="try-error"){next
    }else{model_nls<-nls(k.doy~ ((a+b*run)/(1+c*run+d*(run^2))), start = list(a=0.2,b =b, c = 0.03, d = 0.00001),control = list(maxiter = 500))}
    if(n==20){
      #e
      stop("Convergence error, no model fit was found for the dampening correction.")}
  }}

#p 
proc.2<-((coef(model_nls)[1]+coef(model_nls)[2]*c(1:length.date))/(1+coef(model_nls)[3]*c(1:length.date)+coef(model_nls)[4]*(c(1:length.date)^2)))
proc.2<-zoo::zoo(proc.2,order.by=zoo.date)

#e
stab<-diff(proc.2,na.rm=TRUE)
stab[which(stab<0)]<-1
stab[which(stab!=1)]<-0
if(length(which(diff(stab)!=0))>1){
  warning("Fitting error, no stable model fit was found for the dampening correction, use make.plot to visually inspect the validity of the fit.")
}
if(length(which(stab==0))==length(stab)){
  warning("Fitting error, no stable model fit was found for the dampening correction, use make.plot to visually inspect the validity of the fit.")
}

#g= graphics
if(make.plot==TRUE){
  plot(proc.1$run,base::scale(proc.1$day.max,center=TRUE,scale=FALSE),main=meth[i],type="p",yaxt="n",pch=16,ylab=expression(italic("K")*" (centered)"),xlab="Time since installing (days)")
  lines(c(1:length.date),base::scale(proc.2,center=TRUE,scale=FALSE),col="white",lwd=5)
  lines(c(1:length.date),base::scale(proc.2,center=TRUE,scale=FALSE),col="black",lwd=4)
  lines(c(1:length.date),base::scale(proc.2,center=TRUE,scale=FALSE),col="orange",lwd=2)
  axis(side=2,las=2)
  legend("topright",c(expression("Daily max. "*italic("K")),"Dampening model"),pch=c(16,NA),lty=c(NA,1),col=c("black","orange"),box.col="white",lwd=2)
  box()}

#p
proc.orig<-((coef(model_nls)[1]+coef(model_nls)[2]*proc.1$run)/(1+coef(model_nls)[3]*proc.1$run+coef(model_nls)[4]*(proc.1$run^2)))
center<-as.numeric(lm(proc.1$day.max~1+offset(proc.orig))$coefficients)
add<-zoo::zoo(proc.2+center, order.by= as.POSIXct(as.character(paste0(index(proc.2)," 00:00:00")),format="%Y-%m-%d %H:%M:%S",tz="GMT"))
damp<-zoo::na.locf(cbind(k,add)[,2])
proc.3<-cbind(k,damp)
proc.3$frac<-proc.3$k/proc.3$damp
rem<-k
rem[]<-1
proc.3<-cbind(proc.3,rem)
proc.3<-proc.3[which(proc.3$rem==1),c("k","damp","frac")]
proc.3$year<-as.numeric(substr(as.character(index(proc.3)),1,4))
first.max<-which(proc.3[which(proc.3$year==unique(proc.3$year)[1]),"k"]==max(proc.3[which(proc.3$year==unique(proc.3$year)[1]),"k"],na.rm=TRUE))[1]
proc.3$k_cor<-proc.3$frac*as.numeric(as.character((proc.3[first.max,"k"]/proc.3[first.max,"frac"])))

#g= graphics
if(make.plot==TRUE){
  plot(proc.3$k_cor,main=meth[i],type="l",yaxt="n",col="grey",ylab=expression(italic("K")),xlab="Time")
  lines(proc.3$k,col="black",lwd=1)
  axis(side=2,las=2)
  legend("topright",c(expression("Raw "*italic("K")),expression("Corrected "*italic("K"))),lty=c(1,1),col=c("grey","black"),box.col="white",lwd=2)
  box()
 }

#o
#input[[paste0("damp.mod.",meth[i])]]<-coef(model_nls)
input[[paste0("k.",meth[i])]]<-proc.3$k_cor
}

if(df==T){
  output.data<-list(data.frame(timestamp=as.character(index(input[["max.pd"]])),value=as.numeric(as.character(input[["max.pd"]]))),     
                    data.frame(timestamp=as.character(index(input[["max.mw"]])),value=as.numeric(as.character(input[["max.mw"]]))),
                    data.frame(timestamp=as.character(index(input[["max.dr"]])),value=as.numeric(as.character(input[["max.dr"]]))),
                    data.frame(timestamp=as.character(index(input[["max.ed"]])),value=as.numeric(as.character(input[["max.ed"]]))),
                    data.frame(timestamp=as.character(index(input[["daily_max.pd"]])),value=as.numeric(as.character(input[["daily_max.pd"]]))),
                    data.frame(timestamp=as.character(index(input[["daily_max.mw"]])),value=as.numeric(as.character(input[["daily_max.mw"]]))),
                    data.frame(timestamp=as.character(index(input[["daily_max.dr"]])),value=as.numeric(as.character(input[["daily_max.dr"]]))),
                    data.frame(timestamp=as.character(index(input[["daily_max.ed"]])),value=as.numeric(as.character(input[["daily_max.ed"]]))),
                    data.frame(timestamp=as.character(index(input[["all.pd"]])),value=as.numeric(as.character(input[["all.pd"]]))),
                    data.frame(timestamp=as.character(index(input[["all.ed"]])),value=as.numeric(as.character(input[["all.ed"]]))),
                    data.frame(timestamp=as.character(index(input[["input"]])),value=as.numeric(as.character(input[["input"]]))),criteria,methods,
                    data.frame(timestamp=as.character(index(input[["k.pd"]])),value=as.numeric(as.character(input[["k.pd"]]))),     
                    data.frame(timestamp=as.character(index(input[["k.mw"]])),value=as.numeric(as.character(input[["k.mw"]]))),
                    data.frame(timestamp=as.character(index(input[["k.dr"]])),value=as.numeric(as.character(input[["k.dr"]]))),
                    data.frame(timestamp=as.character(index(input[["k.ed"]])),value=as.numeric(as.character(input[["k.ed"]])))#,
                   # input[["damp.mod.pd"]],input[["damp.mod.mw"]],input[["damp.mod.dr"]],input[["damp.mod.ed"]]
                    
  )
  names(output.data)<-c("max.pd","max.mw","max.dr","max.ed",
                        "daily_max.pd","daily_max.mw","daily_max.dr","daily_max.ed",
                        "all.pd","all.ed","input","ed.criteria","methods",
                        "k.pd","k.mw","k.dr","k.ed"#,"damp.mod.pd","damp.mod.mw","damp.mod.dr","damp.mod.ed"
                        )
}else{
output.data<-input
names(output.data)<-c("max.pd","max.mw","max.dr","max.ed",
                      "daily_max.pd","daily_max.mw","daily_max.dr","daily_max.ed",
                      "all.pd","all.ed","input","ed.criteria","methods",
                      "k.pd","k.mw","k.dr","k.ed"#,"damp.mod.pd","damp.mod.mw","damp.mod.dr","damp.mod.ed"
                      )
}
}else{
  k<-input
  if(attributes(k)$class=="data.frame"){
    #e
    if(is.numeric(k$value)==F)stop("Invalid input data, values within the data.frame are not numeric.")
    if(is.character(k$timestamp)==F)stop("Invalid input data, timestamp within the data.frame are not numeric.")
    
    #p
    k<-zoo::zoo(k$value,order.by=base::as.POSIXct(k$timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC")) 
    
    #e
    if(as.character(index(k)[1])=="(NA NA)"|is.na(index(k)[1])==T)stop("No timestamp present, time.format is likely incorrect.")
  } 
  if(zoo::is.zoo(k)==TRUE){
    if(is.numeric(k)==F)stop("Invalid input data, zoo object does not contain numeric data.")
  }
  
  day.max          <-suppressWarnings(aggregate(k,by=list(as.Date(index(k))),max,na.rm=TRUE))
  day.max[which(day.max=="-Inf")]<-NA
  day.max[which(day.max=="Inf")]<-NA
  test.lenght<-length(na.omit(day.max))
  initial.date     <-index(day.max)[1]
  length.date      <-length(day.max)
  zoo.date         <-index(day.max)
  day.max[which(day.max<k.threshold)]<-NA
  
  #e
  if(length(na.omit(day.max))/test.lenght*100<25)stop("Unused argument, k. threshold removed over 75% of all data points.")
  
  #p
  doy<-zoo::zoo(as.numeric(strftime(index(day.max), format = "%j")),order.by=index(day.max))
  run<-zoo::zoo(index(day.max)-initial.date+1,order.by=index(day.max))
  cbind(doy,day.max,run)
  proc.1<-na.omit(cbind(doy,day.max,run))
  model.doy<-lm(day.max~doy+I(doy^2),data=proc.1)
  proc.1$year<-as.numeric(substr(as.character(index(proc.1)), 1,4))
  proc.1$day.max.cor<-NA
  
  for(y in c(1:length(unique(proc.1$year)))){
    scaled    <-proc.1[which(proc.1$year==unique(proc.1$year)[y]),"day.max"]/quantile(proc.1[which(proc.1$year==unique(proc.1$year)[y]),"day.max"],probs=c(0.95),na.rm=TRUE)
    max.origin<-quantile(proc.1[which(proc.1$year==unique(proc.1$year)[1]),"day.max"],probs=c(0.95),na.rm=TRUE)
    min.scaled<-min(scaled,na.rm=TRUE)
    scaled<-(proc.1[which(proc.1$year==unique(proc.1$year)[y]),"day.max"]-(min(proc.1[which(proc.1$year==unique(proc.1$year)[y]),"day.max"],na.rm=TRUE)) )/
      (quantile(proc.1[which(proc.1$year==unique(proc.1$year)[y]),"day.max"],probs=c(0.95),na.rm=TRUE)-(min(proc.1[which(proc.1$year==unique(proc.1$year)[y]),"day.max"],na.rm=TRUE)))
    proc.1[which(proc.1$year==unique(proc.1$year)[y]),"day.max.cor"]<-scaled*(quantile(proc.1[which(proc.1$year==unique(proc.1$year)[1]),"day.max"],probs=c(0.95),na.rm=TRUE)-
                                                                                min(proc.1[which(proc.1$year==unique(proc.1$year)[y]),"day.max"],na.rm=TRUE))+min(proc.1[which(proc.1$year==unique(proc.1$year)[y]),"day.max"],na.rm=TRUE)}
  model.doy          <-lm(day.max.cor~doy+I(doy^2),data=proc.1)
  pred<-model.doy$coefficients[1]+proc.1$doy*model.doy$coefficients[2]+proc.1$doy^2*model.doy$coefficients[3]
  proc.1$day.max.doy<-proc.1$day.max-pred
  run<-proc.1$run
  k.doy<-proc.1$day.max.doy
  report<-try(
    nls(k.doy~ ((a+b*run)/(1+c*run+d*(run^2))), start = list(a=0.2,b =-0.002, c = 0.03, d = 0.00001),control = list(maxiter = 500)),silent=TRUE)
  if(class(report)!="try-error"){
    model_nls<-nls(k.doy~ ((a+b*run)/(1+c*run+d*(run^2))), start = list(a=0.2,b =-0.002, c = 0.03, d = 0.00001),control = list(maxiter = 500))
  }else{
    for(n in c(1:20)){
      b<-runif(1,min=-0.003,max=0.003)
      report<-try(nls(k.doy~ ((a+b*run)/(1+c*run+d*(run^2))), start = list(a=0.2,b =b, c = 0.03, d = 0.00001),control = list(maxiter = 500)),silent=TRUE)
      if(class(report)=="try-error"){next
      }else{model_nls<-nls(k.doy~ ((a+b*run)/(1+c*run+d*(run^2))), start = list(a=0.2,b =b, c = 0.03, d = 0.00001),control = list(maxiter = 500))}
      if(n==20){
        #e
        stop("Convergence error, no model fit was found for the dampening correction.")}
    }}
  
  #p 
  proc.2<-((coef(model_nls)[1]+coef(model_nls)[2]*c(1:length.date))/(1+coef(model_nls)[3]*c(1:length.date)+coef(model_nls)[4]*(c(1:length.date)^2)))
  proc.2<-zoo::zoo(proc.2,order.by=zoo.date)
  
  #e
  stab<-diff(proc.2,na.rm=TRUE)
  stab[which(stab<0)]<-1
  stab[which(stab!=1)]<-0
  if(length(which(diff(stab)!=0))>2){
    warning("Fitting error, no stable model fit was found for the dampening correction, use make.plot to visually inspect the validity of the fit.")
  }
  if(length(which(stab==0))==length(stab)){
    warning("Fitting error, no stable model fit was found for the dampening correction, use make.plot to visually inspect the validity of the fit.")
  }
  
  #g= graphics
  if(make.plot==TRUE){
    plot(proc.1$run,base::scale(proc.1$day.max,center=TRUE,scale=FALSE),type="p",yaxt="n",pch=16,ylab=expression(italic("K")*" (centered)"),xlab="Time since installing (days)")
    lines(c(1:length.date),base::scale(proc.2,center=TRUE,scale=FALSE),col="white",lwd=5)
    lines(c(1:length.date),base::scale(proc.2,center=TRUE,scale=FALSE),col="black",lwd=4)
    lines(c(1:length.date),base::scale(proc.2,center=TRUE,scale=FALSE),col="orange",lwd=2)
    axis(side=2,las=2)
    legend("topright",c(expression("Daily max. "*italic("K")),"Dampening model"),pch=c(16,NA),lty=c(NA,1),col=c("black","orange"),box.col="white",lwd=2)
    box()}
  
  #p
  proc.orig<-((coef(model_nls)[1]+coef(model_nls)[2]*proc.1$run)/(1+coef(model_nls)[3]*proc.1$run+coef(model_nls)[4]*(proc.1$run^2)))
  center<-as.numeric(lm(proc.1$day.max~1+offset(proc.orig))$coefficients)
  add<-zoo::zoo(proc.2+center, order.by= as.POSIXct(as.character(paste0(index(proc.2)," 00:00:00")),format="%Y-%m-%d %H:%M:%S",tz="GMT"))
  damp<-zoo::na.locf(cbind(k,add)[,2])
  proc.3<-cbind(k,damp)
  proc.3$frac<-proc.3$k/proc.3$damp
  rem<-k
  rem[]<-1
  proc.3<-cbind(proc.3,rem)
  proc.3<-proc.3[which(proc.3$rem==1),c("k","damp","frac")]
  proc.3$year<-as.numeric(substr(as.character(index(proc.3)),1,4))
  first.max<-which(proc.3[which(proc.3$year==unique(proc.3$year)[1]),"k"]==max(proc.3[which(proc.3$year==unique(proc.3$year)[1]),"k"],na.rm=TRUE))[1]
  proc.3$k_cor<-proc.3$frac*as.numeric(as.character((proc.3[first.max,"k"]/proc.3[first.max,"frac"])))
  
  #g= graphics
  if(make.plot==TRUE){
    plot(proc.3$k_cor,type="l",yaxt="n",col="grey",ylab=expression(italic("K")),xlab="Time")
    lines(proc.3$k,col="black",lwd=1)
    axis(side=2,las=2)
    legend("topright",c(expression("Raw "*italic("K")),expression("Corrected "*italic("K"))),lty=c(1,1),col=c("grey","black"),box.col="white",lwd=2)
    box()
    }
    
  #o
  if(df==F){
    output.data<-list(proc.3$k_cor,proc.3$k,coef(model_nls))
    names(output.data)<-c("k.cor","k","damp.mod")
  }else{
    output.data<-list(
    data.frame(timestamp=as.character(index(proc.3)),value=as.numeric(as.character(proc.3$k_cor))),
    data.frame(timestamp=as.character(index(proc.3)),value=as.numeric(as.character(proc.3$k))),
    data.frame(a=coef(model_nls)[1],b=coef(model_nls)[2],c=coef(model_nls)[3],d=coef(model_nls)[4]))
    row.names(output.data[[3]])<-1
    names(output.data)<-c("k.cor","k","damp.mod")
  }
  }
return(output.data)
}

#correct for dampening of the signal
raw   <-is.trex(example.data(type="doy", species="PCAB"),tz="GMT",time.format="%H:%M",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
input <-time.step(input=raw,time.int=15,max.gap=60,decimals=6,df=FALSE)
input[which(input<0.2)]<-NA
input <-dt.max(input, methods=c("pd","mw","dr"),det.pd=TRUE,interpolate=FALSE,max.days=10,df=FALSE)
output.data<-damp(input,k.threshold=0.05,make.plot=TRUE,df=FALSE)
str(output.data)
head(output.data[["k.dr"]])
plot(output.data[["k.dr"]],ylab=expression(italic("K")))

#FIGURE
pdf("D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX/Figure 5.pdf",height=6,width=8)
test<-output.data$k.pd
test[which(test>0.3)]<-NA
par(mar=c(6,6,6,6))
plot(test,col="cyan",yaxt="n",ylab="",xlab="",cex.axis=1.2,ylim=c(0,0.5))
axis(side=2,las=2,cex=1.2,cex.axis=1.2)
test2<-input$k.pd
test2[which(test2>0.3)]<-NA
lines(test2)

par(new=TRUE)
run<-c(1:floor(length(test)/(24*4)))
y<-((output.data$damp.mod.pd[1]+output.data$damp.mod.pd[2]*run)/(1+output.data$damp.mod.pd[3]*run+output.data$damp.mod.pd[4]*(run^2)))
plot(run,y,ylab="",xlab="",xaxt="n",yaxt="n",bty="n",type="l",col="orange",ylim=c(-0.1,0.15),lwd=2)
lines(run,y,col='white',lwd=4)
lines(run,y,col='orange',lwd=3)
mtext(side=2,expression(italic("K")*" (-)"),padj=-3.5,cex=1.2)
mtext(side=1,expression("Time"),padj=3.5,cex=1.2)
legend("top",horiz=TRUE,c("Uncorrected","Corrected","Damp. model"),pch=c(15,15,NA),pt.cex=c(2,2,NA),lty=c(NA,NA,1),lwd=c(NA,NA,4),col=c("black","cyan","orange"),bty="n",cex=1.2)
dev.off()


max(y)
min(y)
