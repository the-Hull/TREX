#' Heartwood correction
#'
#' @description
#' The function corrects for the proportion of the probe that is installed
#' within the non-conductive heartwood according to Clearwater \emph{et al.} (1999).
#' The function requires \eqn{\Delta T_{max}}{\Delta Tmax}, the probe length and the
#' sapwood thickness. The correction is applied on the \eqn{\Delta T} (or \eqn{\Delta V}) values and \eqn{K}
#' is recalculated accordingly. When an \code{\link{is.trex}}-compliant object is
#' provided, the \eqn{K} values for each method are determined (see \code{\link{tdm_dt.max}}.
#'
#' @param input A \code{\link{tdm_dt.max}} ouput or \code{\link{is.trex}}-compliant object of \eqn{\Delta T }(or \eqn{\Delta V}) values containing
#'  a timestamp and a value column.
#' @param dt.max Optional \code{zoo} object or \code{data.frame} (columns = “timestamp” or “value”)
#' containing the \eqn{\Delta T_{max}}{\Delta Tmax} when no \code{\link{is.trex}}-compliant object is provided.
#' @param probe.length Numeric, the length of the TDM probes in mm.
#' @param sapwood.thickness Numeric, the sapwood thickness in mm.
#' @param df Logical; If \code{TRUE}, output is provided in a \code{data.frame} format
#'  with a timestamp and a value column. If \code{FALSE}, output
#'  is provided as a \code{zoo} vector object (default = \code{FALSE}).
#'
#' @usage tdm_hw.cor (input, dt.max, probe.length = 20,
#'             sapwood.thickness = 18, df = FALSE)
#'
#' @details The function applied the correction provided by Clearwater \emph{et al.} 1999.
#'  \eqn{\Delta T} (or \eqn{\Delta V}) was corrected (denoted as \eqn{\Delta T_{sw}}{\Delta Tsw}) for the proportion of
#'  the probe that was inserted into the conducting sapwood vs the
#'  proportion of the probe that was inserted into the nonconductive heartwood
#'  (\eqn{\gamma} in mm mm-1). Together with\eqn{\Delta T_{max}}{\Delta Tmax}, \eqn{\Delta T} was corrected
#'  according to the following equation:
#'  \deqn{\Delta T_{sw} = (\Delta T – (1 – \gamma)  \Delta T_{max}) / \gamma}{\Delta Tsw = (\Delta T – (1 – \gamma)  \Delta Tmax) / \gamma}
#'  \eqn{\Delta T_{sw}}{\Delta Tsw} together with \eqn{\Delta T_{max}}{\Delta Tmax} was then recalculated to \eqn{K}.
#'
#'
#' @return A \code{zoo} object or \code{data.frame} in the appropriate
#' format for other functionalities. See \code{\link{tdm_dt.max}} for output specifications.
#' All \eqn{K} values for each method are provided when an
#' \code{\link{is.trex}}-compliant object was provided.
#' If individual time series are provided for \code{input} and \code{dt.max} an alternative output is provided:
#'
#' \describe{
#'
#'  \item{input}{= \eqn{\Delta T} input data.}
#'  \item{dt.max}{ \eqn{\Delta T_{max}{\Delta Tmax}} input data.}
#'  \item{dtsw}{Corrected \eqn{\Delta T} data.}
#'  \item{k.value}{\eqn{K} values calculated according to Clearwater et al. (1999).}
#'  \item{settings}{data.frame of the applied \code{probe.length} and \code{sapwood.thickness}}
#'
#' }
#'
#' @references Clearwater MJ, Meinzer FC, Andrade JL, Goldstein G, Holbrook NM. 1999.
#' Potential errors in measurement of nonuniform sap flow using heat dissipation probes.
#' Tree Physiology 19:681–687 <doi: 10.1093/treephys/19.10.681>
#'
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' #correct for probes being inserted into the heartwood
#' raw   <-is.trex(example.data(type="doy"),
#'           tz="GMT",time.format="%H:%M",solar.time=TRUE,
#'           long.deg=7.7459,ref.add=FALSE)
#' input <- dt.steps(input=raw,
#'                    start="2014-05-08 00:00",
#'                    end="2014-07-25 00:50",
#'                   time.int=15,max.gap=60,decimals=6,df=F)
#' input[which(input<0.2)]<-NA
#' input <-tdm_dt.max(input, methods=c("pd","mw","dr"),
#'                  det.pd=TRUE,interpolate=FALSE,max.days=10,df=FALSE)
#' output.data<-tdm_hw.cor(input,probe.length=20,
#'                    sapwood.thickness=18,df=FALSE)
#' plot(output.data$k.dr,col="orange")
#' lines(input$k.dr)
#' }
#'
tdm_hw.cor<-function(input,dt.max,probe.length=20,sapwood.thickness=18,df=FALSE){


  #d= default conditions
  if(missing(probe.length)){probe.length=20}
  if(missing(dt.max)){dt.max=NA}
  if(missing(df)){df=F}

  #e= error
  if(is.numeric(probe.length)==FALSE)stop("Unused argument, probe.length is not numeric.")
  if(is.numeric(sapwood.thickness)==FALSE)stop("Unused argument, sapwood.thickness is not numeric.")
  if(df!=T&df!=F)stop("Unused argument, df needs to be TRUE|FALSE.")

  #p= process
  a<- sapwood.thickness/probe.length #probe fraction
  if(a=="Inf")stop("Unused argument, invalid probe.length or sapwood.thickness.")
  b<- 1-a

  #e
  if(is.na(dt.max[1])==TRUE){
    if(length(which(names(input)%in%c("max.pd","max.mw","max.dr","max.ed","daily_max.pd","daily_max.mw","daily_max.dr","daily_max.ed","all.pd","all.ed",
                                      "input","ed.criteria","methods","k.pd","k.mw","k.dr","k.ed")))!=17)stop("Invalid input data, data not originating from tdm_dt.max().")

    #p
    dt<-input$input
    if(attributes(input$input)$class=="data.frame"){dt<-zoo::zoo(dt,order.by=base::as.POSIXct(dt$timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC"))}

    for(i in c(1:length(input$methods))){
      dt.max<-input[[paste0("max.",input$methods[i])]]
      if(attributes(dt.max)$class=="data.frame"){dt.max<-zoo::zoo(dt.max,order.by=base::as.POSIXct(dt$timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC"))}
      proc.1<-cbind(dt,dt.max)
      if(a<1){
        dtsw               <-((proc.1$dt-b*proc.1$dt.max)/a)#here is clearwater correction applied
      }else{
        dtsw               <-proc.1$dt
        warning("No heartwood correction performed, the sapwood.thickness>probe.length.")
      }
      k.value                 <-((proc.1$dt.max-dtsw)/dtsw) #here the k values are calculated
      k.value[which(k.value<0)]<-0
      base::assign(paste0("k.",input$methods[i]),k.value)
      base::assign(paste0("dtsw.",input$methods[i]),dtsw)
    }

    if(length(which("pd"%in%input$methods))!=1){
      k.pd<-NA
      dtsw.pd<-NA}
    if(length(which("mw"%in%input$methods))!=1){
      k.mw<-NA
      dtsw.mw<-NA}
    if(length(which("dr"%in%input$methods))!=1){
      k.dr<-NA
      dtsw.dr<-NA}
    if(length(which("ed"%in%input$methods))!=1){
      k.ed<-NA
      dtsw.ed<-NA}

    #o= output
    if(df==F){
  output.data<-list(input$max.pd,      input$max.mw,      input$max.dr,      input$max.ed,
                      input$daily_max.pd,input$daily_max.mw,input$daily_max.dr,input$daily_max.ed,
                      input$all.pd,input$all.ed,input$input,input$criteria,input$methods,#data.frame(probe.length=probe.length,sapwood.thickness=sapwood.thickness),
                      #dtsw.pd,dtsw.mw,dtsw.dr,dtsw.ed,
                      k.pd,k.mw,k.dr,k.ed)
  }else{

    output.data<-list(data.frame(timestamp=as.character(zoo::index(input$max.pd)),value=as.numeric(as.character(input$max.pd))),
         data.frame(timestamp=as.character(zoo::index(input$max.mw)),value=as.numeric(as.character(input$max.mw))),
         data.frame(timestamp=as.character(zoo::index(input$max.dr)),value=as.numeric(as.character(input$max.dr))),
         data.frame(timestamp=as.character(zoo::index(input$max.ed)),value=as.numeric(as.character(input$max.ed))),
         data.frame(timestamp=as.character(zoo::index(input$daily_max.pd)),value=as.numeric(as.character(input$daily_max.pd))),
         data.frame(timestamp=as.character(zoo::index(input$daily_max.mw)),value=as.numeric(as.character(input$daily_max.mw))),
         data.frame(timestamp=as.character(zoo::index(input$daily_max.dr)),value=as.numeric(as.character(input$daily_max.dr))),
         data.frame(timestamp=as.character(zoo::index(input$daily_max.ed)),value=as.numeric(as.character(input$daily_max.ed))),
         data.frame(timestamp=as.character(zoo::index(input$all.pd)),value=as.numeric(as.character(input$all.pd))),
         data.frame(timestamp=as.character(zoo::index(input$all.ed)),value=as.numeric(as.character(input$all.ed))),
         data.frame(timestamp=as.character(zoo::index(input$input)),value=as.numeric(as.character(input$input))),input$criteria,input$methods,#data.frame(probe.length=probe.length,sapwood.thickness=sapwood.thickness),
         #data.frame(timestamp=as.character(zoo::index(dtsw.pd)),value=as.numeric(as.character(dtsw.pd))),
         #data.frame(timestamp=as.character(zoo::index(dtsw.mw)),value=as.numeric(as.character(dtsw.mw))),
         #data.frame(timestamp=as.character(zoo::index(dtsw.dr)),value=as.numeric(as.character(dtsw.dr))),
         #data.frame(timestamp=as.character(zoo::index(dtsw.ed)),value=as.numeric(as.character(dtsw.ed))),
         data.frame(timestamp=as.character(zoo::index(k.pd)),value=as.numeric(as.character(k.pd))),
         data.frame(timestamp=as.character(zoo::index(k.mw)),value=as.numeric(as.character(k.mw))),
         data.frame(timestamp=as.character(zoo::index(k.dr)),value=as.numeric(as.character(k.dr))),
         data.frame(timestamp=as.character(zoo::index(k.ed)),value=as.numeric(as.character(k.ed)))
    )}

    names(output.data)<-c("max.pd","max.mw","max.dr","max.ed",
                        "daily_max.pd","daily_max.mw","daily_max.dr","daily_max.ed",
                        "all.pd","all.ed","input","ed.criteria","methods",#"hw.cor",
                        #"dtsw.pd","dtsw.mw","dtsw.dr","dtsw.ed",
                        "k.pd","k.mw","k.dr","k.ed")
  return(output.data)

  }else{
    if(length(which(names(input)%in%c("max.pd","max.mw","max.dr","max.ed","daily_max.pd","daily_max.mw","daily_max.dr","daily_max.ed","all.pd","all.ed",
                                      "input","ed.criteria","methods","k.pd","k.mw","k.dr","k.ed")))==17){input<-input$input}
    if(attributes(input)$class=="data.frame"){#e
    if(is.numeric(input$value)==F)stop("Invalid input data, values within the data.frame are not numeric.")
    if(is.character(input$timestamp)==F)stop("Invalid input data, timestamp within the data.frame are not numeric.")
    #p
    input<-zoo::zoo(input$value,order.by=base::as.POSIXct(input$timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC"))
    #e
    if(as.character(zoo::index(input)[1])=="(NA NA)"|is.na(zoo::index(input)[1])==T)stop("No timestamp present, time.format is likely incorrect.")}

    if(attributes(dt.max)$class=="data.frame"){#e
    if(is.numeric(dt.max$value)==F)stop("Invalid dt.max data, values within the data.frame are not numeric.")
    if(is.character(dt.max$timestamp)==F)stop("Invalid dt.max data, timestamp within the data.frame are not numeric.")
    #p
    dt.max<-zoo::zoo(dt.max$value,order.by=base::as.POSIXct(dt.max$timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC"))
    #e
    if(as.character(zoo::index(dt.max)[1])=="(NA NA)"|is.na(zoo::index(dt.max)[1])==T)stop("No timestamp present, time.format is likely incorrect.")}
    if(is.numeric(input)==F)stop("Invalid input data, values within the data.frame are not numeric.")
    if(is.numeric(dt.max)==F)stop("Invalid dt.max data, values within the data.frame are not numeric.")
    if(nrow(stats::na.omit(cbind(input,dt.max)))<length(input)*1/2)stop("Invalid input and dt.max data, less than half of the data matches timestamps.")

    if(a<1){
      dtsw               <-((input-b*dt.max)/a)#here is clearwater correction applied
    }else{
      dtsw               <-input
    warning("No heartwood correction performed, the sapwood.thickness>probe.length.")
    }
    k.value                 <-((dt.max-dtsw)/dtsw) #here the k values are calculated
    k.value[which(k.value<0)]<-0

    #o
    if(df==F){
      output.data<-list(input,dt.max,dtsw,k.value,data.frame(probe.length=probe.length,sapwood.thickness=sapwood.thickness))
    }
    if(df==T){
      output.data<-list(data.frame(timestamp=as.character(zoo::index(input)),value=as.numeric(as.character(input))),
                        data.frame(timestamp=as.character(zoo::index(dt.max)),value=as.numeric(as.character(dt.max))),
                        data.frame(timestamp=as.character(zoo::index(dtsw)),value=as.numeric(as.character(dtsw))),
                        data.frame(timestamp=as.character(zoo::index(k.value)),value=as.numeric(as.character(k.value))),
                        data.frame(probe.length=probe.length,sapwood.thickness=sapwood.thickness)
                        )
    }
    names(output.data)<-c("input","dt.max","dtsw","k.value",
                          "settings")
    return(output.data)
}
}


