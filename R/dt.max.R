#' Calculate zero-flow conditions
#'
#' @param input An \code{is.trex}-compliant object of \eqn{K} values containing
#'  a timestamp and a value column.
#' @param methods Character vector of the requested \eqn{\Delta T_{max}} methods.
#' Options include “pd” (predawn), “mw” (moving window), “dr” (double regression),
#' and “ed” (environmental dependent; default= c(“pd”, “mw”, “dr”)).
#' @param zero.end Numeric, optionally defines the end of the predawn period.
#' Values should be in minutes (e.g. predawn conditions until 08:00 = 8 *60).
#' When not provided, the algorithm will automatically analyse the cyclic behaviour
#' of the data and define the day length.
#' @param zero.start Numeric, optionally defines the beginning of the predawn period.
#' Values should be in minutes (e.g., 01:00 = 1*60).
#' @param interpolate Logical: if \code{TRUE}, detected \eqn{\delta T_{max}} values are linearly
#' interpolated. If \code{FALSE}, constant \eqn{\delta T_{max}} values will be selected daily
#' (default = FALSE).
#' @param det.pd Logical; if \code{TRUE} and no zero.end and zero.start values are provided,
#'  predawn \eqn{\delta T_{max}} will be determined based on cyclic behaviour of the entire
#'  time-series (default = \code{TRUE}).
#' @param max.days Numeric, defines the number of days which the \code{mw} and \code{dr}
#' methods will consider for their moving-windows.
#' @param ed.window Numeric, defines the length of the period considered for assessing the
#'  environmental conditions and stable \eqn{\delta T_{max}} values.
#' @param vpd.input An \code{is.trex}-compliant object (\code{zoo} time-series or \code{data.frame})
#'  with a timestamp and a value column containing the vapour pressure deficit (\emph{vpd}; in kPa)
#'  with the same temporal extent and time steps as the input data.
#' @param sr.input An \code{is.trex}-compliant object (\code{zoo} time-series or \code{data.frame})
#'  with a timestamp and a value column the solar radiation data (\emph{sr}; e.g., global radiation or PAR)
#' @param sel.max Optional \code{zoo} time-series or \code{data.frame} with the specified \eqn{\delta T_{max}.
#'  This is included to change predawn \eqn{\delta T_{max} values selected with the \code{ed} method.
#' @param criteria Numeric vector, thresholds for the \code{ed} method.
#' Thresholds should be provided for all environmental data included in the function
#' (e.g. \emph{sr} = 30; \emph{vpd} = 0.1; coefficient of variation, \emph{cv} = 0.5)
#' @param df Logical; if \code{TRUE}, output is provided in a \code{data.frame}
#' format with a timestamp and a value (\eqn{\Delta T} or \eqn{\Delta V}) column.
#' If \code{FALSE}, output is provided as a \code{zoo} object (default = FALSE).
#'
#' @return A named \code{list} of \code{zoo} time series or \code{data.frame}
#' objects in the appropriate format for further processing.
#' List items include:
#'  \item{max.pd}{\eqn{\delta T_{max} time series as determined by the \code{pd} method.}
#'  \item{max.mw}{\eqn{\delta T_{max} time series as determined by the \code{mw} method.}
#'  \item{max.dr}{\eqn{\delta T_{max} time series as determined by the \code{dr} method.}
#'  \item{max.ed}{\eqn{\delta T_{max} time series as determined by the \code{ed} method.}
#'  \item{daily_max.pd}{daily predawn \eqn{\delta T_{max} as determined by \code{pd}.}
#'  \item{daily_max.mw}{daily predawn \eqn{\delta T_{max} as determined by \code{mw}.}
#'  \item{daily_max.dr}{daily predawn \eqn{\delta T_{max} as determined by \code{dr}.}
#'  \item{daily_max.ed}{daily predawn \eqn{\delta T_{max} as determined by \code{ed}.}
#'  \item{all.pd}{exact predawn \eqn{\delta T_{max} values detected with \code{pd}.}
#'  \item{all.pd}{exact predawn \eqn{\delta T_{max} values detected with \code{ed}.}
#'  \item{input}{\eqn{\delta T} input data.}
#'  \item{ed.criteria}{\code{data.frame} of the applied environmental and variability criteria used within \code{ed}.}
#'  \item{methods}{\code{data.frame} of applied methods to detect \eqn{\delta T_{max}.}
#'  \item{k.pd}{\eqn{K} values calculated by using the pd method.}
#'  \item{k.mw}{\eqn{K} values calculated by using the mw method.}
#'  \item{k.dr}{\eqn{K} values calculated by using the dr method.}
#'  \item{k.ed}{\eqn{K} values calculated by using the ed method.}
#' @export
#'
#' @examples
dt.max <-
  function(input,
           methods = c("pd", "mw", "dr"),
           zero.end = 8 * 60,
           zero.start = 1 * 60,
           interpolate = FALSE,
           det.pd = TRUE,
           max.days = 7,
           ed.window = 2 * 60,
           vpd.input,
           sr.input,
           sel.max,
           criteria = c(sr = 30, vpd = 0.1, cv = 0.5),
           df = FALSE) {
    #t= test
    #raw   <-is.trex(example.data(type="doy", species="PCAB"),tz="GMT",time.format="%H:%M",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
    #input   <-time.step(input=raw,start="2014-05-08 00:00",end="2015-06-25 00:50",
    #                   time.int=15,max.gap=60,decimals=6,df=F)
    #input[which(input<0.4)]<-NA
    #vpd<-read.table("D:/Documents/WSL/06_basic_data/1_database/Environmental_data/All_output_Tier3/Vapour_pressure_deficit.txt",header=TRUE,sep="\t")
    #sr<-read.table("D:/Documents/WSL/06_basic_data/1_database/Environmental_data/All_output_Tier3/Solar_radiance.txt",header=TRUE,sep="\t")
    #vpd<-vpd[,c("Date","N13")]
    #colnames(vpd)<-c("timestamp","value")
    #sr<-sr[,c("Timestamp","N13")]
    #colnames(sr)<-c("timestamp","value")
    #vpd_raw   <-is.trex(vpd,tz="GMT",time.format="(%m/%d/%y %H:%M:%S)",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
    #vpd.input <-time.step(input=vpd_raw,start="2014-05-08 00:00",end="2015-06-25 00:50",
    #                      time.int=15,max.gap=60,decimals=6,df=F)
    #sr_raw   <-is.trex(sr,tz="GMT",time.format="(%m/%d/%y %H:%M:%S)",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
    #sr.input <-time.step(input=sr_raw,start="2014-05-08 00:00",end="2015-06-25 00:50",
    #                     time.int=15,max.gap=60,decimals=6,df=F)
    #methods=c("pd","mw","dr","ed")
    #methods=c("dr")
    #zero.end= 8*60
    #zero.start= 1*60
    #interpolate= FALSE #interpolating within day values
    #det.pd=TRUE #automatic detection of pd max, no start or end time required
    #max.days=7 #days for rm and dr
    #ed.stats::window= 2*60 #number of hours before zero flow values where environmental conditions should be considered
    #criteria<-c(sr=30,vpd=0.1,cv=0.5) #criteria for thesholds
    #df<-F
    #sel.max<-sel.max
    #input<-SFD_add
    #plot(input)
    #input<-SFD_in

    #f= small functions
    left = function(string, char) {
      substr(string, 1, char)
    }
    right = function (string, char) {
      substr(string, nchar(string) - (char - 1), nchar(string))
    }

    #d= default conditions
    if (missing(methods)) {
      methods = c("pd")
    }
    if (missing(det.pd)) {
      det.pd = F
    }
    if (missing(zero.end)) {
      zero.end = 5
    }
    if (missing(zero.start)) {
      zero.start = 11
    }
    if (missing(interpolate)) {
      interpolate = F
    }
    if (missing(max.days)) {
      max.days = 7
    }
    if (missing(ed.stats::window)) {
      ed.stats::window = 2
    }
    if (missing(df)) {
      df = F
    }
    if (df != T &
        df != F)
      stop("Unused argument, df needs to be TRUE|FALSE.")

    #p= process
    if (attributes(input)$class == "data.frame") {
      #e
      if (is.numeric(input$value) == F)
        stop("Invalid input data, values within the data.frame are not numeric.")
      if (is.character(input$timestamp) == F)
        stop("Invalid input data, timestamp within the data.frame are not numeric.")

      #p
      input <-
        zoo::zoo(
          input$value,
          order.by = base::as.POSIXct(input$timestamp, format = "%Y-%m-%d %H:%M:%S", tz =
                                        "UTC")
        )

      #e
      if (as.character(zoo::index(input)[1]) == "(NA NA)" |
          is.na(zoo::index(input)[1]) == T)
        stop("No timestamp present, time.format is likely incorrect.")
    }

    #e= errors
    if (zoo::is.zoo(input) == F)
      stop(
        "Invalid input data, use a zoo file from is.trex or a zoo vector containing numeric values (tz= UTC)."
      )
    if (is.numeric(input) == F)
      stop("Invalid input data, values within the vector are not numeric.")
    if (interpolate != T &
        interpolate != F)
      stop("Unused argument, interpolate needs to be TRUE|FALSE.")
    if (det.pd != T &
        det.pd != F)
      stop("Unused argument, det.pd needs to be TRUE|FALSE.")
    if (length(which(methods %in% c("pd", "mw", "dr", "ed"))) == 0)
      stop(
        "Unused argument, dt.max methods should include a character with pd [pre-dawn], mw [moving stats::window], dr [double regression] or ed [environmental dependent]."
      )
    if (missing(zero.end) == T |
        missing(zero.start) == T) {
      if (missing(det.pd) == TRUE)
        stop(
          "Missing argument, either det.pd = TRUE or zero.start and zero.end have to be provided."
        )
    }

    #w= warnings
    if (difftime(zoo::index(input[length(input)]), zoo::index(input[1]), units = c("days")) <
        7) {
      warning("Selected input has a temporal extend of <7 days.")
    }

    #p
    #PD----
    if (det.pd == F) {
      #e
      if (is.numeric(zero.end) == F)
        stop("Unused argument, zero.end is not numeric.")
      if (is.numeric(zero.start) == F)
        stop("Unused argument, zero.start is not numeric.")
      if (zero.start >= 60 * 24)
        stop("Unused argument, zero.start is not between 0-1440 (24 hours *60 minutes).")
      if (zero.end >= 60 * 24)
        stop("Unused argument, zero.end is not between 0-1440 (24 hours *60 minutes).")
      if (zero.end == zero.start)
        stop("Unused argument, zero.start == zero.end.")

      minutes <-
        as.numeric(left(right(as.character(zoo::index(
          input
        )), 8), 2)) * 60 + as.numeric(left(right(as.character(zoo::index(
          input
        )), 5), 2))

      #e
      if (length(minutes) == 0)
        stop("Unused argument, difference zero.start and zero.end is too small.")

      #p
      proc.1 <- input
      if (zero.end > zero.start) {
        proc.1[which(minutes > zero.end | minutes < zero.start)] <- NA
        shift <- zoo::index(proc.1) + 0
      } else{
        proc.1[which(minutes > zero.end & minutes < zero.start)] <- NA
        shift <- zoo::index(proc.1) + (24 * 60 - zero.start) * 60
      }
      proc.2 <- zoo::zoo(proc.1, order.by = shift)
      daily_max.pd <-
        suppressWarnings(aggregate(
          zoo::zoo(proc.2),
          by = list(as.Date(zoo::index(proc.2))),
          max,
          na.rm = TRUE
        ))
      daily_max.pd <-
        daily_max.pd[which(as.character(daily_max.pd) != "-Inf")]
      proc.3 <-
        zoo::zoo(daily_max.pd, order.by = base::as.POSIXct(paste0(as.character(
          zoo::index(daily_max.pd)
        ), " 00:00:00"), tz = "UTC"))
      proc.3 <- cbind(proc.2, proc.3)
      proc.3$fill <- zoo::na.locf(proc.3$proc.3, na.rm = F)
      proc.3$diff <- NA
      proc.3[which((proc.3$fill - proc.3$proc.2) == 0), "diff"] <-
        proc.3[which((proc.3$fill - proc.3$proc.2) == 0), "proc.2"]
      proc.4 <- stats::na.omit(proc.3$diff)
      if (zero.end > zero.start) {
        max.pd <-
          cbind(input, zoo::zoo(proc.4, order.by = zoo::index(proc.4) - 0))[, 2]
      } else{
        max.pd <-
          cbind(input, zoo::zoo(proc.4, order.by = zoo::index(proc.4) - (24 * 60 - zero.start) *
                                  60))[, 2]
      }

    } else{
      step.min <-
        as.numeric(min(difftime(
          zoo::index(input)[-1], zoo::index(input)[-length(input)], units = c("mins")
        ), na.rm = TRUE))

      #e
      if (step.min > 60)
        stop("Minimum timestep is >1 hour, unable to perform automated pd dt.max detection.")

      #p
      k <-
        round(((60 * 10) / step.min), 0)#assumption in a 10 hour cycle you will find a cycle
      if ((as.integer(k) %% 2) == 0) {
        k <- k + 1
      }
      proc.1 <- diff(zoo::rollmean(input, k, align = c("center")))
      proc.1[which(proc.1 >= 0)] <- 1
      proc.1[which(proc.1 < 0)] <- 0
      proc.2 <- diff(proc.1)
      proc.2 <- proc.2[which(proc.2 == -1 | proc.2 == 1)]
      hour.cycle <-
        difftime(zoo::index(proc.2)[-1], zoo::index(proc.2)[-length(proc.2)], units = c("hours"))
      segment <-
        round(median(as.numeric(hour.cycle[which(hour.cycle < 24 |
                                                   hour.cycle > 3)])))
      k <- round(((60 * segment) / step.min), 0)
      if ((as.integer(k) %% 2) == 0) {
        k <- k + 1
      }
      transfer <- input
      transfer[which(is.na(transfer) == T)] <- -999
      proc.3 <- zoo::rollmax(transfer, k, align = c("center"))
      proc.4 <- cbind(input, proc.3)
      proc.4$proc.3 <-
        zoo::na.locf(zoo::na.locf(proc.4$proc.3, na.rm = FALSE), fromLast = TRUE)
      proc.4$diff <- proc.4$input - proc.4$proc.3
      proc.4[which(proc.4$diff != 0), "input"] <- NA
      proc.5 <- stats::na.omit(proc.4$input)
      test <- (zoo::rollmax(input, k, align = c("right")))
      proc.6 <-
        cbind(proc.5, test)[which(is.na(cbind(proc.5, test)$proc.5) == FALSE), ]
      proc.6[which(is.na(proc.6$test) == TRUE), "test"] <-
        proc.6[which(is.na(proc.6$test) == TRUE), "proc.5"]
      proc.6 <- proc.6[which(proc.6$proc.5 == proc.6$test), ]
      max.pd <- cbind(input, proc.6)[, 2]
      pd.hour <-
        median(as.numeric(left(right(
          as.character(zoo::index(stats::na.omit(max.pd))), 8
        ), 2)))
      pd.min <-
        median(as.numeric(left(right(
          as.character(zoo::index(stats::na.omit(max.pd))), 5
        ), 2)))
      if (nchar(as.character(pd.hour)) == 1) {
        pd.hour <- paste0("0", pd.hour)
      } else{
        pd.hour <- as.character(pd.hour)
      }
      if (nchar(as.character(pd.min)) == 1) {
        pd.min <- paste0("0", pd.min)
      } else{
        pd.min <- as.character(pd.min)
      }

      daily_max.pd <-
        suppressWarnings(aggregate(
          zoo::zoo(max.pd, order.by = zoo::index(max.pd) + (segment * 60 * 60)),
          by = list(as.Date(zoo::index(max.pd) + (segment * 60 * 60))),
          max,
          na.rm = TRUE
        ))
      daily_max.pd[which(daily_max.pd == "-Inf")] <- NA
      daily_max.pd[which(daily_max.pd == "Inf")] <- NA
      max.add <-
        suppressWarnings(aggregate(
          zoo::zoo(max.pd, order.by = zoo::index(max.pd)),
          by = list(as.Date(zoo::index(max.pd))),
          max,
          na.rm = TRUE
        ))
      max.add[which(max.add == "-Inf")] <- NA
      max.add[which(max.add == "Inf")] <- NA
      day.add <- max.add
      zoo::index(day.add) <- zoo::index(day.add) + 1
      daily_max.pd <- cbind(daily_max.pd, max.add, day.add)
      daily_max.pd[which(daily_max.pd[, 1] == daily_max.pd[, 3] &
                           is.na(daily_max.pd[, 2]) == T), 1] <- NA
      daily_max.pd[which(is.na(daily_max.pd[, 1]) == T), 1] <-
        daily_max.pd[which(is.na(daily_max.pd[, 1]) == T), 2]
      daily_max.pd[which(is.na(daily_max.pd[, 1]) == T), 1] <-
        daily_max.pd[which(is.na(daily_max.pd[, 1]) == T), 3]
      daily_max.pd <- daily_max.pd[, 1]

      if (length(which(daily_max.pd == "-Inf")) > 0) {
        add <-
          zoo::zoo(NA,
                   order.by = base::as.POSIXct(
                     paste0(as.character(zoo::index(
                       daily_max.pd[which(daily_max.pd == "-Inf")]
                     )), " ", pd.hour, ":", pd.min, ":00"),
                     format = "%Y-%m-%d %H:%M:%S",
                     tz = "UTC"
                   ))
        for (i in c(1:length(which(daily_max.pd == "-Inf")))) {
          add.sel <-
            stats::stats::window(
              input,
              start = base::as.POSIXct(
                paste0(as.character(zoo::index(
                  daily_max.pd[which(daily_max.pd == "-Inf")]
                )), " ", pd.hour, ":", pd.min, ":00"),
                format = "%Y-%m-%d %H:%M:%S",
                tz = "UTC"
              )[i] - ((segment / 2) * 60 * 60),
              end = base::as.POSIXct(
                paste0(as.character(zoo::index(
                  daily_max.pd[which(daily_max.pd == "-Inf")]
                )), " ", pd.hour, ":", pd.min, ":00"),
                format = "%Y-%m-%d %H:%M:%S",
                tz = "UTC"
              )[i] + ((segment / 2) * 60 * 60)
            )
          if (length(add.sel) == 0) {
            next
          }
          max.pd[zoo::index(add.sel[which(add.sel == suppressWarnings(max(add.sel, na.rm =
                                                                       TRUE)))])] <-
            input[zoo::index(add.sel[which(add.sel == suppressWarnings(max(add.sel, na.rm =
                                                                        TRUE)))])]
        }
        daily_max.pd <-
          suppressWarnings(aggregate(
            zoo::zoo(max.pd, order.by = zoo::index(max.pd) + ((segment) * 1 * 60 * 60)),
            by = list(as.Date(zoo::index(max.pd) + ((segment) * 1 * 60 * 60
            ))),
            max,
            na.rm = TRUE
          ))
      }
      daily_max.pd[which(daily_max.pd == "-Inf")] <- NA
    }

    #w
    if (length(which(is.na(daily_max.pd) == F)) == 0) {
      warning("Days without max.pd present.")
    }

    #e
    if (length(stats::na.omit(max.pd)) == 0)
      stop("Invalid input data, no max.pd values detected.")

    #p
    gap <-
      suppressWarnings(aggregate(input, by = list(as.Date(zoo::index(
        input
      ))), max, na.rm = TRUE))
    gap[which(gap == "-Inf")] <- NA
    gap[which(gap == "Inf")] <- NA

    daily_max.pd[which(daily_max.pd == "-Inf")] <- NA
    all.pd <- max.pd

    if (length(zoo::index(stats::na.omit(gap))[which(diff(zoo::index(stats::na.omit(gap))) > 1)]) !=
        0) {
      gaps <-
        paste0(c(zoo::index(gap)[1] - 2, (zoo::index(stats::na.omit(
          gap
        ))[which(diff(zoo::index(stats::na.omit(gap))) > 1)] + 2), zoo::index(gap)[length(gap)] +
          1), " 00:00:00")
    } else{
      gaps <-
        paste0(c(zoo::index(gap)[1] - 2, zoo::index(gap)[length(gap)] + 1), " 00:00:00")
    }

    for (g in c(1:(length(gaps) - 1))) {
      st.g <-
        (base::as.POSIXct(gaps[g], format = "%Y-%m-%d %H:%M:%S", tz = "UTC") - 1)
      st.e <-
        (base::as.POSIXct(gaps[g + 1], format = "%Y-%m-%d %H:%M:%S", tz = "UTC") -
           1)
      proc.g <- stats::window(max.pd, start = st.g, end = st.e)
      if (interpolate == T) {
        fill.pd <-
          zoo::na.locf(zoo::na.locf(zoo::na.approx(proc.g), na.rm = F), fromLast =
                         T)
      } else{
        fill.pd <- zoo::na.locf(zoo::na.locf(proc.g, na.rm = F), fromLast = T)
      }
      stats::window(max.pd, start = st.g, end = st.e) <- fill.pd
    }

    k.pd                 <- ((max.pd - input) / input)
    k.pd[which(k.pd < 0)] <- 0
    #----
    max.pd[which(is.na(input) == T)] <- NA

    #MW----
    if (length(which("mw" %in% methods)) == 1) {
      max.days <- max.days
      #e
      if (is.numeric(max.days) == FALSE)
        stop("Unused argument, max.days is not numeric.")
      if (max.days > floor(as.numeric(base::difftime(
        zoo::index(input[length(input)]), zoo::index(input[1]), units = c("days")
      ))))
        stop("Unused argument, max.days is larger then number of days present in the input.")

      #p
      step.min <-
        as.numeric(min(difftime(
          zoo::index(input)[-1], zoo::index(input)[-length(input)], units = c("mins")
        ), na.rm = TRUE))
      k <-
        round(((60 * 10) / step.min), 0)#assumption in a 10 hour cycle you will find a cycle
      if ((as.integer(k) %% 2) == 0) {
        k <- k + 1
      }
      proc.1 <- diff(zoo::rollmean(input, k, align = c("center")))
      proc.1[which(proc.1 >= 0)] <- 1
      proc.1[which(proc.1 < 0)] <- 0
      proc.2 <- diff(proc.1)
      proc.2 <- proc.2[which(proc.2 == -1 | proc.2 == 1)]
      hour.cycle <-
        base::difftime(zoo::index(proc.2)[-1], zoo::index(proc.2)[-length(proc.2)], units =
                         c("hours"))
      segment <-
        round(median(as.numeric(hour.cycle[which(hour.cycle < 24 |
                                                   hour.cycle > 3)])))
      rmean <-
        zoo::zoo(NA, order.by = seq(
          from = zoo::index(daily_max.pd)[1],
          to = zoo::index(daily_max.pd)[length(daily_max.pd)],
          by = 1
        ))
      proc.1 <- cbind(daily_max.pd, rmean)

      #here we need to generate segments
      if (length(zoo::index(stats::na.omit(gap))[which(diff(zoo::index(stats::na.omit(gap))) > 1)]) !=
          0) {
        gaps <-
          paste0(c(zoo::index(gap)[1] - 2, (zoo::index(stats::na.omit(
            gap
          ))[which(diff(zoo::index(stats::na.omit(gap))) > 1)] + 2), zoo::index(gap)[length(gap)] +
            1), " 00:00:00")
      } else{
        gaps <-
          paste0(c(zoo::index(gap)[1] - 2, zoo::index(gap)[length(gap)] + 1), " 00:00:00")
      }


      for (g in c(1:(length(gaps) - 1))) {
        st.g <-
          as.Date(base::as.POSIXct(gaps[g], format = "%Y-%m-%d %H:%M:%S", tz = "UTC") +
                    1)
        st.e <-
          as.Date(base::as.POSIXct(gaps[g + 1], format = "%Y-%m-%d %H:%M:%S", tz =
                                     "UTC") - 1)
        proc.g <- stats::window(proc.1, start = st.g, end = st.e)
        max.g <-
          zoo::na.locf(zoo::na.locf(
            rollmax(
              proc.g$daily_max.pd,
              max.days,
              align = c("center"),
              na.rm = TRUE,
              fill = NA
            ),
            na.rm = F
          ), fromLast = TRUE)
        stats::window(proc.1$rmean, start = st.g, end = st.e) <- max.g
      }
      daily_max.mw <- proc.1$rmean
      proc.2 <-
        zoo::zoo(daily_max.mw,
                 order.by = base::as.POSIXct(paste0(as.character(
                   zoo::index(daily_max.mw)
                 ), " 00:00:00"), tz = "UTC") - ((segment * (1 / 3)) * 60 * 60))
      proc.2 <- cbind(all.pd, proc.2)$proc.2

      for (g in c(1:(length(gaps) - 1))) {
        st.g <-
          (base::as.POSIXct(gaps[g], format = "%Y-%m-%d %H:%M:%S", tz = "UTC") - 1)
        st.e <-
          (base::as.POSIXct(gaps[g + 1], format = "%Y-%m-%d %H:%M:%S", tz = "UTC") -
             1)
        proc.g <- stats::window(proc.2, start = st.g, end = st.e)
        fill.g <- zoo::na.locf(zoo::na.locf(proc.g, na.rm = F), fromLast =
                                 T)
        stats::window(proc.2, start = st.g, end = st.e) <- fill.g
      }

      proc.3 <- cbind(all.pd, proc.2)
      proc.3[which(is.na(proc.3$all.pd) == T), "proc.2"] <- NA
      all.mw <- proc.3$proc.2
      max.mw <- all.mw

      for (g in c(1:(length(gaps) - 1))) {
        st.g <-
          (base::as.POSIXct(gaps[g], format = "%Y-%m-%d %H:%M:%S", tz = "UTC") - 1)
        st.e <-
          (base::as.POSIXct(gaps[g + 1], format = "%Y-%m-%d %H:%M:%S", tz = "UTC") -
             1)
        proc.g <- stats::window(max.mw, start = st.g, end = st.e)

        if (interpolate == T) {
          fill.dr <-
            zoo::na.locf(zoo::na.locf(zoo::na.approx(proc.g), na.rm = F), fromLast =
                           T)
        } else{
          fill.dr <- zoo::na.locf(zoo::na.locf(proc.g, na.rm = F), fromLast = T)
        }
        stats::window(max.mw, start = st.g, end = st.e) <- fill.dr
      }

      k.mw                 <- ((max.mw - input) / input)
      k.mw[which(k.mw < 0)] <- 0
    }
    #----

    #DR----
    if (length(which("dr" %in% methods)) == 1) {
      max.days <- max.days
      #e
      if (is.numeric(max.days) == FALSE)
        stop("Unused argument, max.days is not numeric.")
      if (max.days > floor(as.numeric(base::difftime(
        zoo::index(input[length(input)]), zoo::index(input[1]), units = c("days")
      ))))
        stop("Unused argument, max.days is larger then number of days present in the input.")

      #p
      step.min <-
        as.numeric(min(difftime(
          zoo::index(input)[-1], zoo::index(input)[-length(input)], units = c("mins")
        ), na.rm = TRUE))
      k <-
        round(((60 * 10) / step.min), 0)#assumption in a 10 hour cycle you will find a cycle
      if ((as.integer(k) %% 2) == 0) {
        k <- k + 1
      }
      proc.1 <- diff(zoo::rollmean(input, k, align = c("center")))
      proc.1[which(proc.1 >= 0)] <- 1
      proc.1[which(proc.1 < 0)] <- 0
      proc.2 <- diff(proc.1)
      proc.2 <- proc.2[which(proc.2 == -1 | proc.2 == 1)]
      hour.cycle <-
        base::difftime(zoo::index(proc.2)[-1], zoo::index(proc.2)[-length(proc.2)], units =
                         c("hours"))
      segment <-
        round(median(as.numeric(hour.cycle[which(hour.cycle < 24 |
                                                   hour.cycle > 3)])))
      dmean <-
        zoo::zoo(NA, order.by = seq(
          from = zoo::index(daily_max.pd)[1],
          to = zoo::index(daily_max.pd)[length(daily_max.pd)],
          by = 1
        ))
      proc.1 <- cbind(daily_max.pd, dmean)

      if (length(zoo::index(stats::na.omit(gap))[which(diff(zoo::index(stats::na.omit(gap))) > 1)]) !=
          0) {
        gaps <-
          paste0(c(zoo::index(gap)[1] - 2, (zoo::index(stats::na.omit(
            gap
          ))[which(diff(zoo::index(stats::na.omit(gap))) > 1)] + 2), zoo::index(gap)[length(gap)] +
            1), " 00:00:00")
      } else{
        gaps <-
          paste0(c(zoo::index(gap)[1] - 2, zoo::index(gap)[length(gap)] + 1), " 00:00:00")
      }

      for (g in c(1:(length(gaps) - 1))) {
        st.g <-
          as.Date(base::as.POSIXct(gaps[g], format = "%Y-%m-%d %H:%M:%S", tz = "UTC") +
                    1)
        st.e <-
          as.Date(base::as.POSIXct(gaps[g + 1], format = "%Y-%m-%d %H:%M:%S", tz =
                                     "UTC") - 1)
        proc.g <- stats::window(proc.1, start = st.g, end = st.e)
        mean.g <-
          zoo::na.locf(zoo::na.locf(
            rollmean(
              proc.g$daily_max.pd,
              max.days,
              align = c("center"),
              na.rm = TRUE,
              fill = NA
            ),
            na.rm = F
          ), fromLast = TRUE)
        stats::window(proc.1$dmean, start = st.g, end = st.e) <- mean.g
      }

      proc.1 <- cbind(proc.1$dmean, proc.1$daily_max.pd)
      colnames(proc.1) <- c("dmean", "daily_max.pd")

      proc.1[which(proc.1$dmean > proc.1$daily_max.pd), "daily_max.pd"] <-
        NA
      for (g in c(1:(length(gaps) - 1))) {
        st.g <-
          as.Date(base::as.POSIXct(gaps[g], format = "%Y-%m-%d %H:%M:%S", tz = "UTC") +
                    1)
        st.e <-
          as.Date(base::as.POSIXct(gaps[g + 1], format = "%Y-%m-%d %H:%M:%S", tz =
                                     "UTC") - 1)
        proc.g <- stats::window(proc.1, start = st.g, end = st.e)
        mean.g <-
          zoo::na.locf(zoo::na.locf(
            rollmean(
              proc.g$daily_max.pd,
              max.days,
              align = c("center"),
              na.rm = TRUE,
              fill = NA
            ),
            na.rm = F
          ), fromLast = TRUE)
        stats::window(proc.1$dmean, start = st.g, end = st.e) <- mean.g
      }

      daily_max.dr <- (proc.1$dmean)
      proc.2 <-
        zoo::zoo(daily_max.dr,
                 order.by = base::as.POSIXct(paste0(as.character(
                   zoo::index(daily_max.dr)
                 ), " 00:00:00"), tz = "UTC") - ((segment * (1 / 3)) * 60 * 60))
      proc.2 <- cbind(all.pd, proc.2)$proc.2
      #proc.2<-zoo::na.locf(zoo::na.locf(proc.2,na.rm=F),fromLast=T)

      for (g in c(1:(length(gaps) - 1))) {
        st.g <-
          (base::as.POSIXct(gaps[g], format = "%Y-%m-%d %H:%M:%S", tz = "UTC") - 1)
        st.e <-
          (base::as.POSIXct(gaps[g + 1], format = "%Y-%m-%d %H:%M:%S", tz = "UTC") -
             1)
        proc.g <- stats::window(proc.2, start = st.g, end = st.e)
        fill.g <-
          zoo::na.locf(zoo::na.locf(proc.g, na.rm = F), fromLast = T)
        stats::window(proc.2, start = st.g, end = st.e) <- fill.g
      }

      proc.3 <- cbind(all.pd, proc.2)
      proc.3[which(is.na(proc.3$all.pd) == T), "proc.2"] <- NA
      all.dr <- proc.3$proc.2
      max.dr <- all.dr

      for (g in c(1:(length(gaps) - 1))) {
        st.g <-
          (base::as.POSIXct(gaps[g], format = "%Y-%m-%d %H:%M:%S", tz = "UTC") - 1)
        st.e <-
          (base::as.POSIXct(gaps[g + 1], format = "%Y-%m-%d %H:%M:%S", tz = "UTC") -
             1)
        proc.g <- stats::window(max.dr, start = st.g, end = st.e)

        if (interpolate == T) {
          fill.dr <-
            zoo::na.locf(zoo::na.locf(zoo::na.approx(proc.g), na.rm = F), fromLast =
                           T)
        } else{
          fill.dr <- zoo::na.locf(zoo::na.locf(proc.g, na.rm = F), fromLast = T)
        }
        stats::window(max.dr, start = st.g, end = st.e) <- fill.dr
      }

      k.dr                 <- ((max.dr - input) / input)
      k.dr[which(k.dr < 0)] <- 0
    }
    #----

    #ED----
    if (length(which("ed" %in% methods)) == 1) {
      #c
      if (missing(criteria)) {
        criteria <- c(sr = 30, vpd = 0.1, cv = 0.5)
      }
      sr <- criteria["sr"]
      vpd <- criteria["vpd"]
      cv <- criteria["cv"]
      if (is.na(sr) == TRUE) {
        sr <- 30
      }
      if (is.na(vpd) == TRUE) {
        vpd <- 0.1
      }
      if (is.na(cv) == TRUE) {
        cv <- 0.5
      }
      criteria <- c(sr = sr, vpd = vpd, cv = cv)
      names(criteria) <- c("sr", "vpd", "cv")

      if (missing(sel.max)) {
        if (missing(vpd.input)) {
          warning(paste0("No vpd.input data included."))
          vpd.input <- zoo::zoo(0, order.by = zoo::index(input))
        }
        if (missing(sr.input)) {
          warning(paste0("No sr.input data included."))
          sr.input <- zoo::zoo(0, order.by = zoo::index(input))
        }
        if (missing(vpd.input) &
            missing(sr.input))
          stop("Invalid input data, no sr.input nor vpd.input provided.")

        #e
        if (is.numeric(ed.stats::window) == F)
          stop("Invalid input data, ed.stats::window is not numeric.")
        if (ed.stats::window < 1 * 60 |
            ed.stats::window > 12 * 60)
          stop("Invalid input data, ed.stats::window has to fall between 1-12 hours.")
        if (attributes(vpd.input)$class == "data.frame") {
          #e
          if (is.numeric(vpd.input$value) == F)
            stop("Invalid vpd.input data, values within the data.frame are not numeric.")
          if (is.character(vpd.input$timestamp) == F)
            stop("Invalid vpd.input data, timestamp within the data.frame are not numeric.")

          #p
          vpd.input <-
            zoo::zoo(
              vpd.input$value,
              order.by = base::as.POSIXct(
                vpd.input$timestamp,
                format = "%Y-%m-%d %H:%M:%S",
                tz = "UTC"
              )
            )

          #e
          if (as.character(zoo::index(vpd.input)[1]) == "(NA NA)" |
              is.na(zoo::index(vpd.input)[1]) == T)
            stop("No timestamp present, time.format is likely incorrect for vpd.input.")
        }
        if (is.zoo(vpd.input) == FALSE)
          stop("Invalid input data, vpd.input must be a zoo file (use is.trex).")

        if (attributes(sr.input)$class == "data.frame") {
          #e
          if (is.numeric(sr.input$value) == F)
            stop("Invalid sr.input data, values within the data.frame are not numeric.")
          if (is.character(sr.input$timestamp) == F)
            stop("Invalid sr.input data, timestamp within the data.frame are not numeric.")

          #p
          sr.input <-
            zoo::zoo(
              sr.input$value,
              order.by = base::as.POSIXct(sr.input$timestamp, format = "%Y-%m-%d %H:%M:%S", tz =
                                            "UTC")
            )

          #e
          if (as.character(zoo::index(sr.input)[1]) == "(NA NA)" |
              is.na(zoo::index(sr.input)[1]) == T)
            stop("No timestamp present, time.format is likely incorrect for sr.input.")
        }
        if (is.zoo(sr.input) == FALSE)
          stop("Invalid input data, sr.input must be a zoo file (use is.trex).")

        #p
        step.min <-
          as.numeric(min(difftime(
            zoo::index(input)[-1], zoo::index(input)[-length(input)], units = c("mins")
          ), na.rm = TRUE))
        step.sr <-
          as.numeric(min(difftime(
            zoo::index(sr.input)[-1], zoo::index(sr.input)[-length(sr.input)], units = c("mins")
          ), na.rm = TRUE))
        step.vpd <-
          as.numeric(min(difftime(
            zoo::index(vpd.input)[-1], zoo::index(vpd.input)[-length(vpd.input)], units = c("mins")
          ), na.rm = TRUE))

        #w
        if (step.min != step.sr | step.min != step.vpd) {
          warning(
            paste0(
              "time steps between input and vpd.input/sr.input differ, results might not be correctly aggregated."
            )
          )
        }

        k <-
          round(((60 * 10) / step.min), 0)#assumption in a 10 hour cycle you will find a cycle
        if ((as.integer(k) %% 2) == 0) {
          k <- k + 1
        }
        proc.1 <- diff(zoo::rollmean(input, k, align = c("center")))
        proc.1[which(proc.1 >= 0)] <- 1
        proc.1[which(proc.1 < 0)] <- 0
        proc.2 <- diff(proc.1)
        proc.2 <- proc.2[which(proc.2 == -1 | proc.2 == 1)]
        hour.cycle <-
          base::difftime(zoo::index(proc.2)[-1], zoo::index(proc.2)[-length(proc.2)], units =
                           c("hours"))
        segment <-
          round(median(as.numeric(hour.cycle[which(hour.cycle < 24 |
                                                     hour.cycle > 3)])))

        k <- round(ed.stats::window / step.min)
        #w
        if (nchar(as.character(round(ed.stats::window / step.min))) != nchar(as.character(ed.stats::window /
                                                                                   step.min))) {
          warning(
            paste0(
              "Value selected for ed.stats::window does not allow for the selection of full timesteps (minimum time step= ",
              step.min,
              "minutes), k value for rollmean was rounded."
            )
          )
        }

        #p
        #adding all time steps for all.pd
        ind.ed <-
          seq(zoo::index(all.pd)[1], zoo::index(all.pd)[length(all.pd)], by = step.min * 60)
        padd <-
          zoo::zoo(
            rep(NA, length(ind.ed)),
            order.by = base::as.POSIXct(ind.ed, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
          )
        all.pd <- cbind(all.pd, padd)[, "all.pd"]
        proc.1 <- cbind(all.pd, input, sr.input, vpd.input)
        #e
        if (nrow(proc.1) / 4 > nrow(stats::na.omit(proc.1[, -1])))
          stop(
            "Environmental data covers matches with less than 25% of the TDP data, check both time step and extend of sr.input/vpd.input."
          )

        #p
        proc.1$sr_input <-
          zoo::rollmean(
            proc.1$sr.input,
            k,
            align = c("right"),
            na.rm = TRUE,
            fill = NA
          )
        proc.1$vpd_input <-
          zoo::rollmean(
            proc.1$vpd.input,
            k,
            align = c("right"),
            na.rm = TRUE,
            fill = NA
          )
        proc.1$cv <-
          (
            zoo::rollapply(
              proc.1$input,
              width = k,
              FUN = sd,
              align = "right",
              na.rm = TRUE,
              fill = NA
            ) / rollapply(
              proc.1$input,
              width = k,
              FUN = mean,
              align = "right",
              na.rm = TRUE,
              fill = NA
            )
          ) * 100

        if (length(zoo::index(stats::na.omit(gap))[which(diff(zoo::index(stats::na.omit(gap))) > 1)]) !=
            0) {
          gaps <-
            paste0(c(zoo::index(gap)[1] - 2, (zoo::index(
              stats::na.omit(gap)
            )[which(diff(zoo::index(stats::na.omit(gap))) > 1)] + 2), zoo::index(gap)[length(gap)] +
              1), " 00:00:00")
        } else{
          gaps <-
            paste0(c(zoo::index(gap)[1] - 2, zoo::index(gap)[length(gap)] + 1), " 00:00:00")
        }

        proc.2 <- proc.1
        proc.2[which(proc.1$sr_input > criteria["sr"] |
                       proc.1$vpd_input > criteria["vpd"] |
                       proc.1$cv > criteria["cv"]), "all.pd"] <- NA
        proc.2[which(is.na(proc.2$sr_input) == T |
                       is.na(proc.2$vpd_input) == T | is.na(proc.2$cv) == T), "all.pd"] <-
          NA
        if (is.nan(mean(proc.2$all.pd, na.rm = TRUE)) == TRUE)
          stop("No pd.max values have been selected, change criteria or sr.input/vpd.input.")

        all.ed <- proc.2$all.pd
        daily_max.ed <-
          suppressWarnings(aggregate(
            zoo::zoo(all.ed, order.by = zoo::index(all.ed) + (segment * 60 * 60)),
            by = list(as.Date(zoo::index(all.ed) + (
              segment * 60 * 60
            ))),
            max,
            na.rm = TRUE
          ))
        daily_max.ed[which(daily_max.ed == "-Inf")] <- NA
        daily_max.ed[which(daily_max.ed == "Inf")] <- NA

        for (g in c(1:(length(gaps) - 1))) {
          st.g <-
            as.Date(base::as.POSIXct(gaps[g], format = "%Y-%m-%d %H:%M:%S", tz = "UTC") +
                      1)
          st.e <-
            as.Date(base::as.POSIXct(gaps[g + 1], format = "%Y-%m-%d %H:%M:%S", tz =
                                       "UTC") - 1)
          daily.max.g <- stats::window(daily_max.ed, start = st.g, end = st.e)
          stats::window(daily_max.ed, start = st.g, end = st.e) <-
            zoo::na.locf(zoo::na.locf(na.approx(daily.max.g, na.rm = F), na.rm = F), fromLast =
                           T)
        }

        proc.3 <-
          zoo::zoo(daily_max.ed,
                   order.by = base::as.POSIXct(paste0(as.character(
                     zoo::index(daily_max.ed)
                   ), " 00:00:00"), tz = "UTC") - ((segment * 60 * 60)))
        proc.3 <- cbind(proc.2, proc.3)
        proc.3$proc.3 <-
          zoo::na.locf(zoo::na.locf(proc.3$proc.3, na.rm = F), fromLast = TRUE)
        proc.3$all.raw <- all.pd
        proc.3[which(is.na(proc.3$all.raw) == T), "proc.3"] <- NA

        #add gap interpolation
        max.ed <- proc.3$proc.3

        for (g in c(1:(length(gaps) - 1))) {
          st.g <-
            (base::as.POSIXct(gaps[g], format = "%Y-%m-%d %H:%M:%S", tz = "UTC") - 1)
          st.e <-
            (base::as.POSIXct(gaps[g + 1], format = "%Y-%m-%d %H:%M:%S", tz = "UTC") -
               1)
          proc.g <- stats::window(max.ed, start = st.g, end = st.e)
          if (interpolate == T) {
            fill.pd <-
              zoo::na.locf(zoo::na.locf(zoo::na.approx(proc.g), na.rm = F), fromLast =
                             T)
          } else{
            fill.pd <- zoo::na.locf(zoo::na.locf(proc.g, na.rm = F), fromLast = T)
          }
          stats::window(max.ed, start = st.g, end = st.e) <- fill.pd
        }

      } else{
        max.pd <- sel.max

        #e
        if (attributes(max.pd)$class == "data.frame") {
          #e
          if (is.numeric(max.pd$value) == F)
            stop("Invalid max.pd data, values within the data.frame are not numeric.")
          if (is.character(max.pd$timestamp) == F)
            stop("Invalid max.pd data, timestamp within the data.frame are not numeric.")

          #p
          max.pd <-
            zoo::zoo(
              max.pd$value,
              order.by = base::as.POSIXct(max.pd$timestamp, format = "%Y-%m-%d %H:%M:%S", tz =
                                            "UTC")
            )

          #e
          if (as.character(zoo::index(max.pd)[1]) == "(NA NA)" |
              is.na(zoo::index(max.pd)[1]) == T)
            stop("No timestamp present, time.format is likely incorrect for max.pd.")
        }
        if (is.zoo(max.pd) == FALSE)
          stop("Invalid input data, max.pd must be a zoo file (use is.trex).")
        step.min <-
          as.numeric(min(difftime(
            zoo::index(input)[-1], zoo::index(input)[-length(input)], units = c("mins")
          ), na.rm = TRUE))
        step.max <-
          as.numeric(min(difftime(
            zoo::index(max.pd)[-1], zoo::index(max.pd)[-length(max.pd)], units = c("mins")
          ), na.rm = TRUE))
        if (step.min != step.max)
          stop("Invalid input data, max.pd does not have the same minimum time step.")

        #p
        k <-
          round(((60 * 10) / step.min), 0)#assumption in a 10 hour cycle you will find a cycle
        if ((as.integer(k) %% 2) == 0) {
          k <- k + 1
        }
        proc.1 <- diff(zoo::rollmean(input, k, align = c("center")))
        proc.1[which(proc.1 >= 0)] <- 1
        proc.1[which(proc.1 < 0)] <- 0
        proc.2 <- diff(proc.1)
        proc.2 <- proc.2[which(proc.2 == -1 | proc.2 == 1)]
        hour.cycle <-
          difftime(zoo::index(proc.2)[-1], zoo::index(proc.2)[-length(proc.2)], units = c("hours"))
        segment <-
          round(median(as.numeric(hour.cycle[which(hour.cycle < 24 |
                                                     hour.cycle > 3)])))

        daily_max.ed <-
          suppressWarnings(aggregate(
            zoo::zoo(max.pd, order.by = zoo::index(max.pd) + (segment * 60 * 60)),
            by = list(as.Date(zoo::index(max.pd) + (
              segment * 60 * 60
            ))),
            max,
            na.rm = TRUE
          ))
        daily_max.ed[which(daily_max.ed == "-Inf")] <- NA
        daily_max.ed[which(daily_max.ed == "Inf")] <- NA

        if (length(zoo::index(stats::na.omit(gap))[which(diff(zoo::index(stats::na.omit(gap))) > 1)]) !=
            0) {
          gaps <-
            paste0(c(zoo::index(gap)[1] - 2, (zoo::index(
              stats::na.omit(gap)
            )[which(diff(zoo::index(stats::na.omit(gap))) > 1)] + 2), zoo::index(gap)[length(gap)] +
              1), " 00:00:00")
        } else{
          gaps <-
            paste0(c(zoo::index(gap)[1] - 2, zoo::index(gap)[length(gap)] + 1), " 00:00:00")
        }

        for (g in c(1:(length(gaps) - 1))) {
          st.g <-
            as.Date(base::as.POSIXct(gaps[g], format = "%Y-%m-%d %H:%M:%S", tz = "UTC") +
                      1)
          st.e <-
            as.Date(base::as.POSIXct(gaps[g + 1], format = "%Y-%m-%d %H:%M:%S", tz =
                                       "UTC") - 1)
          daily.max.g <- stats::window(daily_max.ed, start = st.g, end = st.e)
          stats::window(daily_max.ed, start = st.g, end = st.e) <-
            zoo::na.locf(zoo::na.locf(na.approx(daily.max.g, na.rm = F), na.rm = F), fromLast =
                           T)
        }

        proc.3 <-
          zoo::zoo(daily_max.ed,
                   order.by = base::as.POSIXct(paste0(as.character(
                     zoo::index(daily_max.ed)
                   ), " 00:00:00"), tz = "UTC") - ((segment * 60 * 60)))
        proc.2 <- cbind(all.pd, input)
        proc.3 <- cbind(proc.2, proc.3)
        proc.3$proc.3 <-
          zoo::na.locf(zoo::na.locf(proc.3$proc.3, na.rm = F), fromLast = TRUE)
        proc.3$all.raw <- all.pd
        proc.3[which(is.na(proc.3$all.raw) == T), "proc.3"] <- NA

        #add gap interpolation
        max.ed <- proc.3$proc.3

        for (g in c(1:(length(gaps) - 1))) {
          #g<-2
          st.g <-
            (base::as.POSIXct(gaps[g], format = "%Y-%m-%d %H:%M:%S", tz = "UTC") - 1)
          st.e <-
            (base::as.POSIXct(gaps[g + 1], format = "%Y-%m-%d %H:%M:%S", tz = "UTC") -
               1)
          proc.g <- stats::window(max.ed, start = st.g, end = st.e)
          if (interpolate == T) {
            fill.pd <-
              zoo::na.locf(zoo::na.locf(zoo::na.approx(proc.g), na.rm = F), fromLast =
                             T)
          } else{
            fill.pd <- zoo::na.locf(zoo::na.locf(proc.g, na.rm = F), fromLast = T)
          }
          stats::window(max.ed, start = st.g, end = st.e) <- fill.pd
        }
      }

      k.ed                 <- ((max.ed - input) / input)
      k.ed[which(k.ed < 0)] <- 0
    }

    #----

    #o= output
    if (length(which("mw" %in% methods)) == 0) {
      max.mw <- NA
      daily_max.mw <- NA
      k.mw <- NA
    }
    if (length(which("dr" %in% methods)) == 0) {
      max.dr <- NA
      daily_max.dr <- NA
      k.dr <- NA
    }
    if (length(which("ed" %in% methods)) == 0) {
      max.ed <- NA
      daily_max.ed <- NA
      all.ed <- NA
      criteria <- NA
      k.ed <- NA
    }

    if (df == F) {
      output.data <- list(
        max.pd,
        max.mw,
        max.dr,
        max.ed,
        daily_max.pd,
        daily_max.mw,
        daily_max.dr,
        daily_max.ed,
        all.pd,
        all.ed,
        input,
        criteria,
        methods,
        k.pd,
        k.mw,
        k.dr,
        k.ed
      )
    }
    if (df == T) {
      output.data <-
        list(
          data.frame(
            timestamp = as.character(zoo::index(max.pd)),
            value = as.numeric(as.character(max.pd))
          ),
          data.frame(
            timestamp = as.character(zoo::index(max.mw)),
            value = as.numeric(as.character(max.mw))
          ),
          data.frame(
            timestamp = as.character(zoo::index(max.dr)),
            value = as.numeric(as.character(max.dr))
          ),
          data.frame(
            timestamp = as.character(zoo::index(max.ed)),
            value = as.numeric(as.character(max.ed))
          ),
          data.frame(
            timestamp = as.character(zoo::index(daily_max.pd)),
            value = as.numeric(as.character(daily_max.pd))
          ),
          data.frame(
            timestamp = as.character(zoo::index(daily_max.mw)),
            value = as.numeric(as.character(daily_max.mw))
          ),
          data.frame(
            timestamp = as.character(zoo::index(daily_max.dr)),
            value = as.numeric(as.character(daily_max.dr))
          ),
          data.frame(
            timestamp = as.character(zoo::index(daily_max.ed)),
            value = as.numeric(as.character(daily_max.ed))
          ),
          data.frame(
            timestamp = as.character(zoo::index(all.pd)),
            value = as.numeric(as.character(all.pd))
          ),
          data.frame(
            timestamp = as.character(zoo::index(all.ed)),
            value = as.numeric(as.character(all.ed))
          ),
          data.frame(
            timestamp = as.character(zoo::index(input)),
            value = as.numeric(as.character(input))
          ),
          criteria,
          methods,
          data.frame(
            timestamp = as.character(zoo::index(k.pd)),
            value = as.numeric(as.character(k.pd))
          ),
          data.frame(
            timestamp = as.character(zoo::index(k.mw)),
            value = as.numeric(as.character(k.mw))
          ),
          data.frame(
            timestamp = as.character(zoo::index(k.dr)),
            value = as.numeric(as.character(k.dr))
          ),
          data.frame(
            timestamp = as.character(zoo::index(k.ed)),
            value = as.numeric(as.character(k.ed))
          )
        )
    }
    names(output.data) <- c(
      "max.pd",
      "max.mw",
      "max.dr",
      "max.ed",
      "daily_max.pd",
      "daily_max.mw",
      "daily_max.dr",
      "daily_max.ed",
      "all.pd",
      "all.ed",
      "input",
      "ed.criteria",
      "methods",
      "k.pd",
      "k.mw",
      "k.dr",
      "k.ed"
    )
    return(output.data)
  }
































#adopt function
raw   <- example.data(type = "doy", species = "PCAB")
input <-
  is.trex(
    raw,
    tz = "GMT",
    time.format = "%H:%M",
    solar.time = TRUE,
    long.deg = 7.7459,
    ref.add = FALSE,
    df = FALSE
  )

#cleaning
stats::window(
  input,
  start = as.POSIXct(
    as.character("(01/01/12 00:00:00)"),
    format = "(%m/%d/%y %H:%M:%S)",
    tz = "GMT"
  ),
  end = as.POSIXct(
    as.character("(01/01/13 00:00:00)"),
    format = "(%m/%d/%y %H:%M:%S)",
    tz = "GMT"
  )
)[which(stats::window(
  input,
  start = as.POSIXct(
    as.character("(01/01/12 00:00:00)"),
    format = "(%m/%d/%y %H:%M:%S)",
    tz = "GMT"
  ),
  end = as.POSIXct(
    as.character("(01/01/13 00:00:00)"),
    format = "(%m/%d/%y %H:%M:%S)",
    tz = "GMT"
  )
) < 0.5)] <- NA
stats::window(
  input,
  start = as.POSIXct(
    as.character("(01/01/13 00:00:00)"),
    format = "(%m/%d/%y %H:%M:%S)",
    tz = "GMT"
  ),
  end = as.POSIXct(
    as.character("(01/01/16 00:00:00)"),
    format = "(%m/%d/%y %H:%M:%S)",
    tz = "GMT"
  )
)[which(
  stats::window(
    input,
    start = as.POSIXct(
      as.character("(01/01/13 00:00:00)"),
      format = "(%m/%d/%y %H:%M:%S)",
      tz = "GMT"
    ),
    end = as.POSIXct(
      as.character("(01/01/16 00:00:00)"),
      format = "(%m/%d/%y %H:%M:%S)",
      tz = "GMT"
    )
  ) < 0.64 |
    stats::window(
      input,
      start = as.POSIXct(
        as.character("(01/01/13 00:00:00)"),
        format = "(%m/%d/%y %H:%M:%S)",
        tz = "GMT"
      ),
      end = as.POSIXct(
        as.character("(01/01/16 00:00:00)"),
        format = "(%m/%d/%y %H:%M:%S)",
        tz = "GMT"
      )
    ) > 0.81
)] <- NA
input <- time.step(input,
                   time.int = 15,
                   max.gap = 180,
                   decimals = 10)

test <- dt.max(input, methods = c("dr"), max.days = 7)
lines(test$max.dr, col = "orange")
plot(test$k.dr)
str(test)

#perform delta T maximum calculations
raw   <-
  is.trex(
    example.data(type = "doy", species = "PCAB"),
    tz = "GMT",
    time.format = "%H:%M",
    solar.time = TRUE,
    long.deg = 7.7459,
    ref.add = FALSE
  )
input   <-
  time.step(
    input = raw,
    start = "2014-05-08 00:00",
    end = "2014-07-25 00:50",
    time.int = 15,
    max.gap = 60,
    decimals = 6,
    df = F
  )
input[which(input < 0.2)] <- NA

vpd <-
  read.table(
    "D:/Documents/WSL/06_basic_data/1_database/Environmental_data/All_output_Tier3/Vapour_pressure_deficit.txt",
    header = TRUE,
    sep = "\t"
  )
sr <-
  read.table(
    "D:/Documents/WSL/06_basic_data/1_database/Environmental_data/All_output_Tier3/Solar_radiance.txt",
    header = TRUE,
    sep = "\t"
  )
vpd <- vpd[, c("Date", "N13")]
colnames(vpd) <- c("timestamp", "value")
sr <- sr[, c("Timestamp", "N13")]
colnames(sr) <- c("timestamp", "value")
vpd_raw   <-
  is.trex(
    vpd,
    tz = "GMT",
    time.format = "(%m/%d/%y %H:%M:%S)",
    solar.time = TRUE,
    long.deg = 7.7459,
    ref.add = FALSE
  )
vpd.input <-
  time.step(
    input = vpd_raw,
    start = "2014-05-08 00:00",
    end = "2014-07-25 00:50",
    time.int = 15,
    max.gap = 60,
    decimals = 6,
    df = F
  )
sr_raw   <-
  is.trex(
    sr,
    tz = "GMT",
    time.format = "(%m/%d/%y %H:%M:%S)",
    solar.time = TRUE,
    long.deg = 7.7459,
    ref.add = FALSE
  )
sr.input <-
  time.step(
    input = sr_raw,
    start = "2014-05-08 00:00",
    end = "2014-07-25 00:50",
    time.int = 15,
    max.gap = 60,
    decimals = 6,
    df = F
  )

output.max <-
  dt.max(
    input,
    methods = c("pd", "mw", "dr", "ed"),
    det.pd = TRUE,
    interpolate = FALSE,
    max.days = 10,
    sr.input = sr.input,
    vpd.input = vpd.input,
    ed.stats::window = 2 * 60,
    criteria = c(sr = 30, vpd = 0.1, cv = 0.5),
    df = FALSE
  )

str(output.max)

plot(output.max$input, ylab = expression(Delta * italic("V")))
lines(output.max$max.pd, col = "green")
lines(output.max$max.mw, col = "blue")
lines(output.max$max.dr, col = "orange")
lines(output.max$max.ed, col = "purple")
points(output.max$all.pd, col = "green", pch = 16)
points(output.max$all.ed, col = "purple", pch = 16)
legend(
  "bottomright",
  c("raw", "max.pd", "max.mw", "max.dr", "max.ed"),
  lty = 1,
  col = c("black", "green", "blue", "orange", "purple")
)

#adding a point (BUGS)
steps <-
  zoo(c(1:length(output.max$all.pd)), order.by = zoo::index(output.max$all.pd))
View(cbind(output.max$all.ed, output.max$all.pd, steps))
sel.max <- output.max$all.ed
sel.max[c(5185)] <- output.max$all.pd[c(5185)]
sel.max[c(2697:2710)] <- NA

output.max2 <-
  dt.max(
    input,
    methods = c("pd", "mw", "dr", "ed"),
    det.pd = TRUE,
    interpolate = FALSE,
    max.days = 10,
    sr.input = sr.input,
    vpd.input = vpd.input,
    ed.stats::window = 2 * 60,
    criteria = c(sr = 30, vpd = 0.1, cv = 0.5),
    df = FALSE,
    sel.max = sel.max
  )

plot(output.max2$input, ylab = expression(Delta * italic("V")))
lines(output.max2$max.pd, col = "green")
lines(output.max2$max.mw, col = "blue")
lines(output.max2$max.dr, col = "orange")
lines(output.max2$max.ed, col = "purple")
points(output.max2$all.pd, col = "green", pch = 16)
points(output.max2$all.ed, col = "red", pch = 16)
points(output.max2$all.ed, col = "black", pch = 1)
points(output.max$all.ed,
       col = "purple",
       pch = 16,
       cex = 1)
legend(
  "bottomright",
  c("raw", "max.pd", "max.mw", "max.dr", "max.ed"),
  lty = 1,
  col = c("black", "green", "blue", "orange", "purple")
)


#FIGURE 4
#perform delta T maximum calculations
raw   <-
  is.trex(
    example.data(type = "doy", species = "PCAB"),
    tz = "GMT",
    time.format = "%H:%M",
    solar.time = TRUE,
    long.deg = 7.7459,
    ref.add = FALSE
  )
input   <-
  time.step(
    input = raw,
    start = "2014-05-01 00:00",
    end = "2014-06-01 00:00",
    time.int = 15,
    max.gap = 60,
    decimals = 15,
    df = FALSE
  )

vpd <-
  read.table(
    "D:/Documents/WSL/06_basic_data/1_database/Environmental_data/All_output_Tier3/Vapour_pressure_deficit.txt",
    header = TRUE,
    sep = "\t"
  )
sr <-
  read.table(
    "D:/Documents/WSL/06_basic_data/1_database/Environmental_data/All_output_Tier3/Solar_radiance.txt",
    header = TRUE,
    sep = "\t"
  )
vpd <- vpd[, c("Date", "N13")]
colnames(vpd) <- c("timestamp", "value")
sr <- sr[, c("Timestamp", "N13")]
colnames(sr) <- c("timestamp", "value")
vpd_raw   <-
  is.trex(
    vpd,
    tz = "GMT",
    time.format = "(%m/%d/%y %H:%M:%S)",
    solar.time = TRUE,
    long.deg = 7.7459,
    ref.add = FALSE
  )
vpd.input <-
  time.step(
    input = vpd_raw,
    start = "2014-05-01 00:00",
    end = "2014-06-01 00:00",
    time.int = 15,
    max.gap = 60,
    decimals = 15,
    df = F
  )
sr_raw   <-
  is.trex(
    sr,
    tz = "GMT",
    time.format = "(%m/%d/%y %H:%M:%S)",
    solar.time = TRUE,
    long.deg = 7.7459,
    ref.add = FALSE
  )
sr.input <-
  time.step(
    input = sr_raw,
    start = "2014-05-01 00:00",
    end = "2014-06-01 00:00",
    time.int = 15,
    max.gap = 60,
    decimals = 15,
    df = F
  )

output.max <-
  dt.max(
    input,
    methods = c("pd", "mw", "dr", "ed"),
    det.pd = TRUE,
    interpolate = FALSE,
    max.days = 10,
    sr.input = sr.input,
    vpd.input = vpd.input,
    ed.stats::window = 2 * 60,
    criteria = c(sr = 30, vpd = 0.1, cv = 0.05),
    df = FALSE
  )

pdf(
  "D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX/Figure 4.pdf",
  height = 6,
  width = 8
)
Sys.setlocale("LC_ALL", "English")
layout(matrix(c(1, 1, 1, 2,
                1, 1, 1, 2),
              nc = 4, byrow = TRUE))
par(mar = c(7, 7, 7, 0))
plot(
  input,
  yaxt = "n",
  xlab = "",
  ylab = "",
  cex.axis = 1.5
)
mtext(side = 1,
      "Year = 2014",
      padj = 3.5,
      cex = 1.2)
mtext(
  side = 2,
  expression(Delta * italic("V") * " (mV)"),
  padj = -3.5,
  cex = 1.2
)
axis(side = 2,
     las = 2,
     cex.axis = 1.5)
polygon(
  c(as.numeric(zoo::index(output.max$input)), rev(as.numeric(
    zoo::index(output.max$max.mw)
  ))),
  c(as.numeric(output.max$input), as.numeric(rev(output.max$max.mw))),
  col = "darkgrey",
  border = FALSE
)
polygon(
  c(as.numeric(zoo::index(output.max$input)), rev(as.numeric(
    zoo::index(output.max$max.pd)
  ))),
  c(as.numeric(output.max$input), as.numeric(rev(output.max$max.pd))),
  col = "lightgrey",
  border = FALSE
)
lines(output.max$input, lwd = 1.5, lty = 2)
lines(output.max$max.pd, col = "black", lwd = 3)
lines(output.max$max.pd, col = "orange")
lines(output.max$max.mw, col = "black", lwd = 3)
lines(output.max$max.mw, col = "purple")
lines(output.max$max.dr, col = "black", lwd = 3)
lines(output.max$max.dr, col = "cyan")
points(output.max$all.ed,
       col = "black",
       pch = 16,
       cex = 2)
points(output.max$all.ed,
       col = "blue",
       pch = 16,
       cex = 1.5)

par(mar = c(7, 0, 7, 0))
plot(
  1,
  1,
  xaxt = "n",
  yaxt = "n",
  ylab = "",
  xlab = "",
  bty = "n",
  col = "white"
)
legend(
  "topleft",
  c(
    expression("Raw "),
    expression("PD   "),
    expression("MW "),
    expression("DR   "),
    expression("ED   ")
  ),
  col = c("black", "orange", "purple", "cyan", "blue"),
  lty = c(2, 1, 1, 1, NA),
  lwd = c(2, 3, 3, 3, NA),
  pch = c(NA, NA, NA, NA, 16),
  pt.cex = c(NA, NA, NA, NA, 2)
  ,
  cex = 1.8,
  bty = "n",
  border = FALSE
)
dev.off()
