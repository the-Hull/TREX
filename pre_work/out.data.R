#' FILL - saves output data
#'
#' @description FILL this function allow saving plots and making cool data.
#'
#' @param input An \code{\link{is.trex}}-compliant time series from \code{cal.sfd} outputs
#' (e.g., \code{X$sfd.mw$sfd})
#' @param vpd.input An \code{\link{is.trex}}-compliant object (\code{zoo} time-series or \code{data.frame})
#'  with a timestamp and a value column containing the vapour pressure deficit (\emph{vpd}; in kPa)
#'  with the same temporal extent and time steps as the input data.
#' @param sr.input  An \code{\link{is.trex}}-compliant object (\code{zoo} time-series or \code{data.frame})
#'  with a timestamp and a value column the solar radiation data (\emph{sr}; e.g., global radiation or \emph{PAR})
#' @param prec.input An \code{\link{is.trex}}-compliant object (\code{zoo} time-series or \code{data.frame})
#'  with a timestamp and a value column the precipitation data (\emph{sr}; e.g., global radiation or \emph{PAR})
#' @param peak.hours FILL
#' @param low.sr FILL
#' @param peak.sr FILL
#' @param vpd.cutoff FILL
#' @param prec.lim FILL
#' @param method FILL
#' @param make.plot Logical; if \code{TRUE}, a plot is generated presenting the FILL.
#'
#' @return FILL returns output data
#' @export
#'
#' @examples
#' #Gc response function
#' raw   <-
#'   is.trex(
#'     example.data(type = "doy", species = "PCAB"),
#'     tz = "GMT",
#'     time.format = "%H:%M",
#'     solar.time = TRUE,
#'     long.deg = 7.7459,
#'     ref.add = FALSE
#'   )
#' input <-
#'   time_step(
#'     input = raw,
#'     start = "2013-05-01 00:00",
#'     end = "2013-11-01 00:00",
#'     time.int = 15,
#'     max.gap = 60,
#'     decimals = 10,
#'     df = FALSE
#'   )
#' input[which(input < 0.2)] <- NA
#' input <-
#'   dt.max(
#'     input,
#'     methods = c("dr"),
#'     det.pd = TRUE,
#'     interpolate = FALSE,
#'     max.days = 10,
#'     df = FALSE
#'   )
#' output.data <-
#'   cal.sfd(input,
#'           make.plot = TRUE,
#'           df = FALSE,
#'           wood = "Coniferous")
#'
#'
#' vpd_raw   <-
#'   is.trex(
#'     vpd,
#'     tz = "GMT",
#'     time.format = "(%m/%d/%y %H:%M:%S)",
#'     solar.time = TRUE,
#'     long.deg = 7.7459,
#'     ref.add = FALSE
#'   )
#' vpd.input <-
#'   time_step(
#'     input = vpd_raw,
#'     start = "2013-05-01 00:00",
#'     end = "2013-11-01 00:00",
#'     time.int = 15,
#'     max.gap = 60,
#'     decimals = 10,
#'     df = FALSE
#'   )
#' sr_raw   <-
#'   is.trex(
#'     sr,
#'     tz = "GMT",
#'     time.format = "(%m/%d/%y %H:%M:%S)",
#'     solar.time = TRUE,
#'     long.deg = 7.7459,
#'     ref.add = FALSE
#'   )
#' sr.input <-
#'   time_step(
#'     input = sr_raw,
#'     start = "2013-05-01 00:00",
#'     end = "2013-11-01 00:00",
#'     time.int = 15,
#'     max.gap = 60,
#'     decimals = 10,
#'     df = FALSE
#'   )
#'
#' input <- (output.data$sfd.dr$sfd)
#'
#'
#' prec.in <- is.trex(preci, time.format = "%d/%m/%y", tz = "UTC")
#' prec.input <- window(
#'   prec.in,
#'   start = as.POSIXct(as.character(index(input)[1]), format =
#'                        "%Y-%m-%d", tz = "UTC"),
#'   end = as.POSIXct(as.character(index(input)[length(input)]), format =
#'                      "%Y-%m-%d", tz = "UTC")
#' )
#'
#' output <-
#'   out.data(
#'     input,
#'     vpd.input = vpd.input,
#'     sr.input = sr.input,
#'     prec.input = prec.input,
#'     prec.lim = 1,
#'     method = "env.filt",
#'     make.plot = TRUE
#'   )
#' head(output)
#'
#'
out.data <-
  function(input,
           vpd.input = vpd,
           sr.input = sr,
           prec.input = prec,
           peak.hours = c(10:14),
           low.sr = 0.15,
           peak.sr = 0.75,
           vpd.cutoff = 0.5,
           prec.lim = 1,
           method = "env.filt",
           make.plot = TRUE) {
    #t= test
    #setwd("D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX/R_package/TREX - Construction")
    #calib<-read.table("D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX/R_package/TREX - Construction/cal.data.txt",header=TRUE,sep="\t")
    #raw   <-is.trex(example.data(type="doy", species="PCAB"),tz="GMT",time.format="%H:%M",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
    #input <-time_step(input=raw,start="2013-05-01 00:00",end="2013-11-01 00:00",
    #                  time.int=15,max.gap=60,decimals=10,df=FALSE)
    #input[which(input<0.2)]<-NA
    #input <-dt.max(input, methods=c("dr"),det.pd=TRUE,interpolate=FALSE,max.days=10,df=FALSE)
    #output.data<-cal.sfd(input,make.plot=TRUE,df=FALSE,wood="Coniferous")
    #
    #vpd<-read.table("D:/Documents/WSL/06_basic_data/1_database/Environmental_data/All_output_Tier3/Vapour_pressure_deficit.txt",header=TRUE,sep="\t")
    #sr<-read.table("D:/Documents/WSL/06_basic_data/1_database/Environmental_data/All_output_Tier3/Solar_radiance.txt",header=TRUE,sep="\t")
    #vpd<-vpd[,c("Date","N13")]
    #colnames(vpd)<-c("timestamp","value")
    #sr<-sr[,c("Timestamp","N13")]
    #colnames(sr)<-c("timestamp","value")
    #vpd_raw   <-is.trex(vpd,tz="GMT",time.format="(%m/%d/%y %H:%M:%S)",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
    #vpd.input <-time_step(input=vpd_raw,start="2013-05-01 00:00",end="2013-11-01 00:00",
    #                      time.int=15,max.gap=60,decimals=10,df=FALSE)
    #sr_raw   <-is.trex(sr,tz="GMT",time.format="(%m/%d/%y %H:%M:%S)",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
    #sr.input <-time_step(input=sr_raw,start="2013-05-01 00:00",end="2013-11-01 00:00",
    #                     time.int=15,max.gap=60,decimals=10,df=FALSE)
    #
    #input<-(output.data$sfd.dr$sfd)
    #prec_raw<-read.table("D:/Documents/WSL/06_basic_data/1_database/Environmental_data/All_output_Tier3/Precipitation.txt",header=TRUE,sep="\t")
    #colnames(prec_raw)<-c("timestamp","value")
    #prec.in<-is.trex(prec_raw,time.format="%d/%m/%y",tz="UTC")
    #prec.input<-window(prec.in,
    #  start=as.POSIXct(as.character(index(input)[1]),format="%Y-%m-%d",tz="UTC"),
    #  end=as.POSIXct(as.character(index(input)[length(input)]),format="%Y-%m-%d",tz="UTC"))
    #
    #method = 'env.filt'
    #peak.hours = c(10:14) # hours that are considered as 'peak of the day'
    #sr.input # kPa time-1
    #vpd.input # kPa time-1
    #prec.input # mm d-1
    #low.sr = 0.15
    #peak.sr=0.75
    #vpd.cutoff= 0.5 # kPa
    #prec.lim= 1
    #make.plot=T

    #f= small functions
    left = function(string, char) {
      substr(string, 1, char)
    }
    right = function (string, char) {
      substr(string, nchar(string) - (char - 1), nchar(string))
    }

    #d= default conditions
    if (missing(method)) {
      method = "stat"
    }
    if (missing(peak.hours)) {
      peak.hours = c(10:14)
    }
    if (missing(low.sr)) {
      low.sr = 0.25
    }
    if (missing(peak.sr)) {
      peak.sr = 0.75
    }
    if (missing(vpd.cutoff)) {
      vpd.cutoff = 0.5
    }
    if (missing(prec.lim)) {
      prec.lim = 1
    }
    if (missing(make.plot)) {
      make.plot = F
    }
    if (missing(sr.input)) {
      sr.input = input[]
      sr.input[] <- NA
    }
    if (missing(prec.input)) {
      prec.input <- aggregate(input[], mean, na.rm = T, by = as.Date(index(input)))
      prec.input[] <- 0
      prec.lim <- 100
    }

    #e
    if (method != "stat" &
        method != "env.filt")
      stop("Unused argument, method needs to be a character of stat|env.filt.")
    if (is.numeric(low.sr) == F)
      stop("Unused argument, low.sr need to be numeric.")
    if (is.numeric(peak.sr) == F)
      stop("Unused argument, peak.sr need to be numeric.")
    if (is.numeric(vpd.cutoff) == F)
      stop("Unused argument, vpd.cutoff need to be numeric.")
    if (is.numeric(prec.lim) == F)
      stop("Unused argument, prec.lim need to be numeric.")
    if (make.plot != T &
        make.plot != F)
      stop("Unused argument, make.plot needs to be TRUE|FALSE.")

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
      if (as.character(index(input)[1]) == "(NA NA)" |
          is.na(index(input)[1]) == T)
        stop("No timestamp present, time.format is likely incorrect.")
    }

    #e= errors
    if (zoo::is.zoo(input) == F)
      stop(
        "Invalid input data, use a zoo file from is.trex or a zoo vector containing numeric values (tz= UTC)."
      )
    if (is.numeric(input) == F)
      stop("Invalid input data, values within the vector are not numeric.")

    #w= warnings
    if (difftime(index(input[length(input)]), index(input[1]), units = c("days")) <
        30) {
      warning("Selected input has a temporal extend of <30 days.")
    }

    if (missing(vpd.input))
      stop("No vpd.input data included.")
    if (missing(sr.input)) {
      warning(paste0("No sr.input data included."))
      sr.input <- zoo::zoo(0, order.by = index(input))
    }

    #e
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
          order.by = base::as.POSIXct(vpd.input$timestamp, format = "%Y-%m-%d %H:%M:%S", tz =
                                        "UTC")
        )

      #e
      if (as.character(index(vpd.input)[1]) == "(NA NA)" |
          is.na(index(vpd.input)[1]) == T)
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
      if (as.character(index(sr.input)[1]) == "(NA NA)" |
          is.na(index(sr.input)[1]) == T)
        stop("No timestamp present, time.format is likely incorrect for sr.input.")
    }
    if (is.zoo(sr.input) == FALSE)
      stop("Invalid input data, sr.input must be a zoo file (use is.trex).")

    if (attributes(prec.input)$class == "data.frame") {
      #e
      if (is.numeric(prec.input$value) == F)
        stop("Invalid prec.input data, values within the data.frame are not numeric.")
      if (is.character(prec.input$timestamp) == F)
        stop("Invalid prec.input data, timestamp within the data.frame are not numeric.")

      #p
      prec.input <-
        zoo::zoo(
          prec.input$value,
          order.by = base::as.POSIXct(prec.input$timestamp, format = "%Y-%m-%d", tz =
                                        "UTC")
        )

      #e
      if (as.character(index(prec.input)[1]) == "(NA NA)" |
          is.na(index(prec.input)[1]) == T)
        stop("No timestamp present, time.format is likely incorrect for vpd.input.")
    }
    if (is.zoo(prec.input) == FALSE)
      stop("Invalid input data, vpd.input must be a zoo file (use is.trex).")

    #p
    step.min <-
      as.numeric(min(difftime(
        index(input)[-1], index(input)[-length(input)], units = c("mins")
      ), na.rm = TRUE))
    step.sr <-
      as.numeric(min(difftime(
        index(sr.input)[-1], index(sr.input)[-length(sr.input)], units = c("mins")
      ), na.rm = TRUE))
    step.vpd <-
      as.numeric(min(difftime(
        index(vpd.input)[-1], index(vpd.input)[-length(vpd.input)], units = c("mins")
      ), na.rm = TRUE))

    #w
    if (step.min != step.sr | step.min != step.vpd) {
      warning(
        paste0(
          "time steps between input and vpd.input/sr.input differ, results might not be correctly aggregated."
        )
      )
    }

    #p
    sfd <-
      ((input * 10000 / 3600) / 18.01528) * 1000 #cm3 cm-2 h-1 to cm3 m-2 s-1 to mmol m-2 s-1
    doy <- zoo::zoo(lubridate::yday(index(input)), order.by = index(input))
    h  <- zoo::zoo(lubridate::hour(index(input)), order.by = index(input))
    y  <- zoo::zoo(lubridate::year(index(input)), order.by = index(input))
    doy_y <-
      zoo::zoo(as.numeric(paste(doy, y, sep = "")), order.by = index(input))

    if (method == "stat") {
      sfd_df <- cbind(sfd, vpd.input, sr.input, doy, h, y, doy_y)
      sfd_df$gc <- sfd_df$sfd / sfd_df$vpd.input

      ##non-rainy days (P_daily < 1 mm)
      prec_df = as.data.frame(prec.input)
      prec_df$doy = lubridate::yday(index(prec.input))
      prec_df$y = lubridate::year(index(prec.input))
      prec_df$doy_y = as.numeric(paste(prec_df$doy, prec_df$y, sep = ""))
      add <-
        data.frame(
          timestamp = as.character(index(sfd_df)),
          sfd = as.numeric(sfd_df$sfd),
          doy_y = as.numeric(sfd_df$doy_y)
        )
      add2 <-
        data.frame(prec = as.numeric(prec_df$prec.input),
                   doy_y = as.numeric(prec_df$doy_y))
      adding <- merge(add, add2, by = "doy_y")
      prec_day <-
        zoo::zoo(
          adding$prec,
          order.by = base::as.POSIXct(adding$timestamp, format = "%Y-%m-%d %H:%M:%S", tz =
                                        "UTC")
        )

      #add dummy
      sfd_df$sr_day <- sfd_df$sr.input
      sfd_df$prec_day <- prec_day
      sfd_df$gc_filt <- sfd_df$gc
      sfd_df$vpd_filt <- sfd_df$vpd.input
      sfd_df[which(sfd_df$h %in% peak.hours == F), "gc_filt"] <- NA
      sfd_df[which(sfd_df$h %in% peak.hours == F), "vpd_filt"] <- NA

      ##4. aggregate to daily values
      sfd_df_peak_daily = aggregate(sfd_df[, ],
                                    mean,
                                    na.rm = T,
                                    by = list(doy_y = sfd_df$doy_y))
      sfd_df_peak_daily[which(sfd_df_peak_daily$sr.input == "NaN"), "sr.input"] <-
        NA
      sfd_df_peak_daily[which(sfd_df_peak_daily$sr_day == "NaN"), "sr_day"] <-
        NA
    }

    if (method == 'env_filt') {
      ## 0. calculate conductance (SFD/VPD) from all data
      sfd_df <- cbind(sfd, vpd.input, sr.input, doy, h, y, doy_y)
      sfd_df$gc <- sfd_df$sfd / sfd_df$vpd.input

      ## 1. non-rainy days (P_daily < 1 mm)
      prec_df = as.data.frame(prec.input)
      prec_df$doy = lubridate::yday(index(prec.input))
      prec_df$y = lubridate::year(index(prec.input))
      prec_df$doy_y = as.numeric(paste(prec_df$doy, prec_df$y, sep = ""))
      add <-
        data.frame(
          timestamp = as.character(index(sfd_df)),
          sfd = as.numeric(sfd_df$sfd),
          doy_y = as.numeric(sfd_df$doy_y)
        )
      add2 <-
        data.frame(prec = as.numeric(prec_df$prec.input),
                   doy_y = as.numeric(prec_df$doy_y))
      adding <- merge(add, add2, by = "doy_y")
      prec_day <-
        zoo::zoo(
          adding$prec,
          order.by = base::as.POSIXct(adding$timestamp, format = "%Y-%m-%d %H:%M:%S", tz =
                                        "UTC")
        )

      ## 2. non-cloudy days (SW > q25) #(PAR_daily > 300 umol/m2/s)
      sr_df = as.data.frame(sr.input)
      sr_df$doy = lubridate::yday(index(sr.input))
      sr_df$y = lubridate::year(index(sr.input))
      sr_df$doy_y = as.numeric(paste(sr_df$doy, sr_df$y, sep = ""))
      sr_df_daily = aggregate(sr_df$sr.input,
                              mean,
                              na.rm = T,
                              by = list(doy_y = sr_df$doy_y))
      add2 <-
        data.frame(sr = as.numeric(sr_df_daily$x),
                   doy_y = as.numeric(sr_df_daily$doy_y))
      adding <- merge(add, add2, by = "doy_y")
      sr_day <-
        zoo::zoo(
          adding$sr,
          order.by = base::as.POSIXct(adding$timestamp, format = "%Y-%m-%d %H:%M:%S", tz =
                                        "UTC")
        )
      sr_limit <- quantile(sr_df_daily$x, probs = low.sr)

      ##3. merge all data together and select the criteria
      sfd_df$sr_day <- sr_day
      sfd_df$prec_day <- prec_day
      sfd_df$gc_filt <- sfd_df$gc
      sfd_df$vpd_filt <- sfd_df$vpd.input
      sfd_df[which(sfd_df$prec_day > as.numeric(prec.lim)), "gc_filt"] <-
        NA
      sfd_df[which(sfd_df$sr_day < as.numeric(sr_limit)), "gc_filt"] <- NA
      sfd_df[which(sfd_df$prec_day > as.numeric(prec.lim)), "vpd_filt"] <-
        NA
      sfd_df[which(sfd_df$sr_day < as.numeric(sr_limit)), "vpd_filt"] <-
        NA
      sfd_df[which(sfd_df$sr.input < quantile(sfd_df$sr.input, probs = peak.sr, na.rm =
                                                T)), "gc_filt"] <- NA
      sfd_df[which(sfd_df$sr.input < quantile(sfd_df$sr.input, probs = peak.sr, na.rm =
                                                T)), "vpd_filt"] <- NA

      ##4. aggregate to daily values
      sfd_df_peak_daily = aggregate(sfd_df[, ],
                                    mean,
                                    na.rm = T,
                                    by = list(doy_y = sfd_df$doy_y))
    }
    sfd_df_peak_daily <- zoo::fortify.zoo(sfd_df_peak_daily)
    sfd_df_peak_daily <-
      sfd_df_peak_daily[which(sfd_df_peak_daily$gc_fil != "NaN" |
                                sfd_df_peak_daily$vpd_fil != "NaN"), ]

    ## 5. Filter low VPD conditions (i.e., VPD_peakdaymean <= 0.5 kPa)
    sfd_df_4gcfit = sfd_df_peak_daily[which(sfd_df_peak_daily$vpd_filt > vpd.cutoff), ]

    ## 6. normalize Gc in [0,1]
    sfd_df_4gcfit$gc_norm = as.numeric(sfd_df_4gcfit$gc_filt) / max(sfd_df_4gcfit$gc_filt, na.rm =
                                                                      T)

    ## 7. Model fitting preparation
    df = data.frame(x = sfd_df_4gcfit$vpd_filt, y = sfd_df_4gcfit$gc_filt)
    df_n = data.frame(x = sfd_df_4gcfit$vpd_filt, y = sfd_df_4gcfit$gc_norm)

    #model fitting
    mod <- nls(y ~ a + d * x ^ (-0.5), start = list(a = 1, d = 1), data = df)
    mod_n <-
      nls(y ~ a + d * x ^ (-0.5), start = list(a = 1, d = 1), data = df_n)
    Alpha <- round(summary(mod)$coef[1, 1], 3)
    Beta <- round(summary(mod)$coef[2, 1], 3)

    #plotting output
    if (make.plot == T) {
      par(oma = c(1, 1, 1, 1))
      par(mfrow = c(1, 1))
      plot(
        df$x,
        df$y,
        pch = 16,
        col = "white",
        cex = 2.5,
        yaxt = "n",
        ylab = "",
        xlab = "",
        ylim = c(0, max(df$y))
      )
      axis(side = 2, las = 2)
      points(df$x,
             df$y,
             col = "grey",
             pch = 16,
             cex = 1.8)
      points(df$x,
             df$y,
             col = "white",
             pch = 1,
             cex = 1.8)
      points(df$x,
             df$y,
             col = "grey",
             pch = 16,
             cex = 1.6)
      newdat <-
        data.frame(x = seq(round((min(
          df$x
        )), 2), round(max(df$x), 2), 0.1))
      lines(
        seq(round((min(
          df$x
        )), 2), round(max(df$x), 2), 0.1),
        predict(mod, newdata = newdat),
        col = "black",
        lwd = 7
      )
      lines(
        seq(round((min(
          df$x
        )), 2), round(max(df$x), 2), 0.1),
        predict(mod, newdata = newdat),
        col = "orange",
        lwd = 5
      )
      legend("top", c(
        expression(
          italic("G")[c] * " = " * italic(alpha) * " + " * italic(beta) * " VPD" ^
            0.5
        ),
        substitute(
          paste(
            italic(alpha),
            " = ",
            Alpha,
            " mmol ",
            " ",
            m ^ -2,
            " ",
            s ^ -1,
            " ",
            kPa ^ -1
          ),
          list(Alpha = (Alpha))
        ),
        substitute(paste(italic(beta), " = ", Beta, " ", kPa ^ -0.5), list(Beta =
                                                                             (Beta)))
      ), bty = "n")
      mtext(
        side = 2,
        expression(italic(G)[c] * " (" * "mmol " * m ^ -2 * " " * s ^ -1 * " " *
                     kPa ^ -1 * ")"),
        padj = -2.5
      )
      mtext(side = 1,
            expression(VPD * " (kPa)"),
            padj = 3)
    }

    #o= output
    raw <- zoo::fortify.zoo(sfd_df)
    colnames(raw) <-
      c(
        "timestamp",
        "sfd",
        "vpd",
        "sr",
        "doy",
        "hour",
        "year",
        "doy.y",
        "gc",
        "sr.day",
        "prec.day",
        "gc.filt",
        "vpd.filt"
      )
    raw <- raw[, c(1, 7, 5, 6, 4, 10, 3, 13, 11, 2, 9, 12)]
    peak.mean <- sfd_df_4gcfit
    colnames(peak.mean) <-
      c(
        "timestamp",
        "sfd",
        "vpd",
        "sr",
        "doy",
        "hour",
        "year",
        "doy.y",
        "gc",
        "sr.day",
        "prec.day",
        "gc.filt",
        "vpd.filt",
        "gc.norm"
      )
    peak.mean[, 1] <- row.names(peak.mean)
    row.names(peak.mean) <- c(1:nrow(peak.mean))
    peak.mean <- peak.mean[, c(1, 12, 13, 14)]
    sum.mod <- summary(mod)
    sum.mod.norm <- summary(mod_n)
    output.data <- list(raw, peak.mean, sum.mod, sum.mod.norm)
    names <- c("raw", "peak.mean", "sum.mod", 'sum.mod.norm')
    return(output.data)
  }





