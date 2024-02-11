#' Signal dampening correction
#'
#' @description When long-term \eqn{K} time series (~3 years) are provided, one can perform
#'  a signal dampening correction (when sensors were not re-installed;
#'  see Peters \emph{et al.} 2018). Applying the signal dampening
#'  correction requires  visually inspecting the correction curve
#'  (see \code{make.plot = TRUE}). The correction curve is constructed with
#'  the day since installation and the day of year (DOY) to account for seasonal changes in \eqn{K}
#'  values. The function returns corrected \eqn{K} values and the applied correction curve.
#'
#' @param input An \code{\link{is.trex}}-compliant object (\code{zoo} vector, \code{data.frame}) of \eqn{K} values containing
#'  a timestamp and a value column.
#' @param k.threshold Numeric, the threshold below which daily maximum \eqn{K} values should not be considered (default = 0.05).
#' @param make.plot Logical; if \code{TRUE}, a plot is generated presenting the correction curve and the \eqn{K} time series.
#' @param df Logical; If \code{TRUE}, output is provided in a \code{data.frame} format
#'  with a timestamp and a value column. If \code{FALSE}, output
#'  is provided as a \code{zoo} vector object (default = FALSE).
#'
#'
#'  @details The function fits a correction curve for signal dampening (e.g., due to wounding)
#'  according to Peters \emph{et al.} (2018). A sensor specific function is fitted to daily maximum
#'  \eqn{K} values (considering a minimum cut-off threshold; see \code{k.threshold}). Dependent variables
#'  for the function include seasonality (DOY) and days since installation (\eqn{t}).
#'  First, seasonal effects are removed by correcting the \eqn{K} series (residuals; \eqn{Kresid})
#'  to a second-order polynomial with DOY. These residuals are then used within a
#'  non-linear model:
#'  \deqn{K_{resid} = (a + b * t)/(1 + c * t + d * t^{2})}{Kresid = (a + b * t)/(1 + c * t + d * t2)}
#'
#'  The fitted parameters for \eqn{t} (with \eqn{a}, \eqn{b}, \eqn{c} and \eqn{d}) are used to
#'  correct \eqn{K} and scale it to the maximum within the first year of installation.
#'  \strong{Note, that the stability of the fit has to be visually inspected before using the output data}.
#'
#' @return A \code{zoo} object or \code{data.frame} in the appropriate
#'  format for other functionalities.
#'  See \code{\link{tdm_dt.max}} output specifications.
#'  All \eqn{K} values for each method are provided when an \code{\link{is.trex}}-object was used as input.
#'  If an individual time series was provided for input with \eqn{K} values an alternative output is given:
#'
#'  \describe{
#'    \item{k.cor}{corrected \eqn{K} values according to the correction curve.}
#'    \item{k}{\eqn{K} values provided as input.}
#'    \item{damp.mod}{\code{data.frame} with the coefficients of the correction curve.}
#'
#'  }
#'
#' @references Peters RL, Fonti P, Frank DC, Poyatos R, Pappas C, Kahmen A, Carraro V,
#' Prendin AL, Schneider L, Baltzer JL, Baron-Gafford GA, Dietrich L, Heinrich I,
#' Minor RL, Sonnentag O, Matheny AM, Wightman MG, Steppe K. 2018.
#' Quantification of uncertainties in conifer sap flow measured with the thermal
#' dissipation method. New Phytologist 219:1283-1299 \doi{10.1111/nph.15241}
#'
#'
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#'  #correct for dampening of the signal
#' raw   <-
#'   is.trex(
#'     example.data(type = "doy"),
#'     tz = "GMT",
#'     time.format = "%H:%M",
#'     solar.time = TRUE,
#'     long.deg = 7.7459,
#'     ref.add = FALSE
#'   )
#' input <-
#'   dt.steps(
#'     input = raw,
#'     time.int = 15,
#'     max.gap = 60,
#'     decimals = 6,
#'     df = FALSE
#'   )
#' input[which(input < 0.2)] <- NA
#' input <-
#'   tdm_dt.max(
#'     input,
#'     methods = c("pd", "mw", "dr"),
#'     det.pd = TRUE,
#'     interpolate = FALSE,
#'     max.days = 10,
#'     df = FALSE
#'   )
#' output.data <- tdm_damp(input,
#'                     k.threshold = 0.05,
#'                     make.plot = TRUE,
#'                     df = FALSE)
#' str(output.data)
#' head(output.data[["k.dr"]])
#' plot(output.data[["k.dr"]], ylab = expression(italic("K")))
#' }
#'
tdm_damp <- function(input,
                     k.threshold = 0.05,
                     make.plot = TRUE,
                     df = FALSE) {

  #test
  #input<-K_out
  #k.threshold = 0.01
  #make.plot = TRUE
  #df = FALSE

  #d= default conditions
  if (missing(make.plot)) {
    make.plot <- F
  }
  if (missing(df)) {
    df = F
  }
  if (missing(k.threshold)) {
    k.threshold <- 0.05
  }

  #e= errors
  if (df != T &
      df != F)
    stop("Unused argument, df needs to be TRUE|FALSE.")
  if (is.numeric(k.threshold) == FALSE)
    stop("Unused argument, k. threshold needs to be numeric.")
  if (make.plot != T &
      make.plot != F)
    stop("Unused argument, make.plot needs to be TRUE|FALSE.")
  if (length(names(input)) != 0) {
    if (length(which(
      names(input) %in% c(
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
    )) != 17)
    stop("Invalid input data, data not originating from tdm_dt.max().")

    #p= process
    meth <- input[["methods"]]
    #input[["damp.mod.pd"]]<-NA
    #input[["damp.mod.mw"]]<-NA
    #input[["damp.mod.dr"]]<-NA
    #input[["damp.mod.ed"]]<-NA

    for (i in c(1:length(meth))) {
      k <- input[[paste0("k.", meth[i])]]

      if (attributes(k)$class == "data.frame") {
        #e
        if (is.numeric(k$value) == F)
          stop("Invalid input data, values within the data.frame are not numeric.")
        if (is.character(k$timestamp) == F)
          stop("Invalid input data, timestamp within the data.frame are not numeric.")

        #p
        k <-
          zoo::zoo(
            k$value,
            order.by = base::as.POSIXct(k$timestamp, format = "%Y-%m-%d %H:%M:%S", tz =
                                          "UTC")
          )

        #e
        if (as.character(zoo::index(k)[1]) == "(NA NA)" |
            is.na(zoo::index(k)[1]) == T)
          stop("No timestamp present, time.format is likely incorrect.")
      }
      if (zoo::is.zoo(k) == TRUE) {
        if (is.numeric(k) == F)
          stop("Invalid input data, zoo object does not contain numeric data.")
      }

      day.max          <-
        suppressWarnings(stats::aggregate(k, by = list(as.Date(zoo::index(
          k
        ))), max, na.rm = TRUE))
      day.max[which(day.max == "-Inf")] <- NA
      day.max[which(day.max == "Inf")] <- NA
      test.lenght <- length(stats::na.omit(day.max))
      initial.date     <- zoo::index(day.max)[1]
      length.date      <- length(day.max)
      zoo.date         <- zoo::index(day.max)
      day.max[which(day.max < k.threshold)] <- NA

      #e
      if (length(stats::na.omit(day.max)) / test.lenght * 100 < 25)
        stop("Unused argument, k. threshold removed over 75% of all data points.")

      #p
      doy <-
        zoo::zoo(as.numeric(strftime(zoo::index(day.max), format = "%j")), order.by =
                   zoo::index(day.max))
      run <- zoo::zoo(zoo::index(day.max) - initial.date + 1, order.by = zoo::index(day.max))
      run_test<-as.numeric(as.character(run))
      proc.1 <- stats::na.omit(cbind(doy, day.max, run))
      model.doy <- stats::lm(day.max ~ doy + I(doy ^ 2), data = proc.1)
      proc.1$year <- as.numeric(substr(as.character(zoo::index(proc.1)), 1, 4))
      proc.1$day.max.cor <- NA

      for (y in c(1:length(unique(proc.1$year)))) {
        scaled    <-
          proc.1[which(proc.1$year == unique(proc.1$year)[y]), "day.max"] / stats::quantile(proc.1[which(proc.1$year ==
                                                                                                           unique(proc.1$year)[y]), "day.max"], probs = c(0.95), na.rm = TRUE)
        max.origin <-
          stats::quantile(proc.1[which(proc.1$year == unique(proc.1$year)[1]), "day.max"], probs =
                            c(0.95), na.rm = TRUE)
        min.scaled <- min(scaled, na.rm = TRUE)
        scaled <-
          (proc.1[which(proc.1$year == unique(proc.1$year)[y]), "day.max"] - (min(proc.1[which(proc.1$year ==
                                                                                                 unique(proc.1$year)[y]), "day.max"], na.rm = TRUE))) /
          (stats::quantile(proc.1[which(proc.1$year == unique(proc.1$year)[y]), "day.max"], probs =
                             c(0.95), na.rm = TRUE) - (min(proc.1[which(proc.1$year == unique(proc.1$year)[y]), "day.max"], na.rm =
                                                             TRUE)))
        proc.1[which(proc.1$year == unique(proc.1$year)[y]), "day.max.cor"] <-
          scaled * (stats::quantile(proc.1[which(proc.1$year == unique(proc.1$year)[1]), "day.max"], probs =
                                      c(0.95), na.rm = TRUE) -
                      min(proc.1[which(proc.1$year ==
                                         unique(proc.1$year)[y]), "day.max"], na.rm = TRUE)) + min(proc.1[which(proc.1$year ==
                                                                                                                  unique(proc.1$year)[y]), "day.max"], na.rm = TRUE)
      }
      model.doy          <- stats::lm(day.max.cor ~ doy + I(doy ^ 2), data = proc.1)
      pred <-
        model.doy$coefficients[1] + proc.1$doy * model.doy$coefficients[2] + proc.1$doy ^
        2 * model.doy$coefficients[3]
      proc.1$day.max.doy <- proc.1$day.max - pred
      run <- proc.1$run
      k.doy <- proc.1$day.max.doy
      report <- try(stats::nls(
        k.doy ~ ((a + b * run) / (1 + c * run + d * (run ^ 2))),
        start = list(
          a = 0.2,
          b = -0.002,
          c = 0.03,
          d = 0.00001
        ),
        control = list(maxiter = 500)
      ), silent = TRUE)
      if (!inherits(report, "try-error")) {
        model_nls <-
          stats::nls(
            k.doy ~ ((a + b * run) / (1 + c * run + d * (run ^ 2))),
            start = list(
              a = 0.2,
              b = -0.002,
              c = 0.03,
              d = 0.00001
            ),
            control = list(maxiter = 500)
          )
      } else{
        for (n in c(1:20)) {
          b <- stats::runif(1, min = -0.003, max = 0.003)
          report <-
            try(stats::nls(
              k.doy ~ ((a + b * run) / (1 + c * run + d * (run ^ 2))),
              start = list(
                a = 0.2,
                b = b,
                c = 0.03,
                d = 0.00001
              ),
              control = list(maxiter = 500)
            ), silent = TRUE)
          if (inherits(report, "try-error")) {
            next
          } else{
            model_nls <-
              stats::nls(
                k.doy ~ ((a + b * run) / (1 + c * run + d * (run ^ 2))),
                start = list(
                  a = 0.2,
                  b = b,
                  c = 0.03,
                  d = 0.00001
                ),
                control = list(maxiter = 500)
              )
          }
          if (n == 20) {
            #e
            stop("Convergence error, no model fit was found for the dampening correction.")
          }
        }
      }

      #p
      proc.2 <-
        ((stats::coef(model_nls)[1] + stats::coef(model_nls)[2] * run_test) / (
          1 + stats::coef(model_nls)[3] * run_test + stats::coef(model_nls)[4] * (run_test ^
                                                                                            2)
        ))
      proc.2 <- zoo::zoo(proc.2, order.by = zoo.date)

      #e
      stab <- diff(proc.2, na.rm = TRUE)
      stab[which(stab < 0)] <- 1
      stab[which(stab != 1)] <- 0
      if (length(which(diff(stab) != 0)) > 1) {
        warning(
          "Fitting error, no stable model fit was found for the dampening correction, use make.plot to visually inspect the validity of the fit."
        )
      }
      if (length(which(stab == 0)) == length(stab)) {
        warning(
          "Fitting error, no stable model fit was found for the dampening correction, use make.plot to visually inspect the validity of the fit."
        )
      }

      #g= graphics
      if (make.plot == TRUE) {
        graphics::par(mfrow=c(2,1))
        graphics::par(mar=c(5,5,1,5))
        graphics::plot(
          proc.1$run,
          base::scale(proc.1$day.max, center = TRUE, scale = FALSE),
          main = meth[i],
          type = "p",
          yaxt = "n",
          pch = 16,
          ylab = expression(italic("K") * " (centered)"),
          xlab = "Time since installing (days)"
        )
        graphics::lines(
          run_test,
          base::scale(proc.2, center = TRUE, scale = FALSE),
          col = "white",
          lwd = 5
        )
        graphics::lines(
          run_test,
          base::scale(proc.2, center = TRUE, scale = FALSE),
          col = "black",
          lwd = 4
        )
        graphics::lines(
          run_test,
          base::scale(proc.2, center = TRUE, scale = FALSE),
          col = "orange",
          lwd = 2
        )
        graphics::points(
          run_test,
          base::scale(proc.2, center = TRUE, scale = FALSE),
          col = "black",
          pch = 16,cex=1.2
        )
        graphics::points(
          run_test,
          base::scale(proc.2, center = TRUE, scale = FALSE),
          col = "orange",
          pch = 16,cex=0.8
        )
        graphics::axis(side = 2, las = 2)
        graphics::legend(
          "topright",
          c(expression("Daily max. " * italic("K")), "Dampening model"),
          pch = c(16, NA),
          lty = c(NA, 1),
          col = c("black", "orange"),
          box.col = "white",
          lwd = 2
        )
        graphics::box()
      }

      #p
      proc.orig <-
        ((stats::coef(model_nls)[1] + stats::coef(model_nls)[2] * proc.1$run) / (
          1 + stats::coef(model_nls)[3] * proc.1$run + stats::coef(model_nls)[4] * (proc.1$run ^
                                                                                      2)
        ))
      center <-
        as.numeric(stats::lm(proc.1$day.max ~ 1 + stats::offset(proc.orig))$coefficients)
      add <-
        zoo::zoo(
          proc.2 + center,
          order.by = as.POSIXct(
            as.character(paste0(zoo::index(proc.2), " 00:00:00")),
            format = "%Y-%m-%d %H:%M:%S",
            tz = "GMT"
          )
        )
      damp <- zoo::na.locf(cbind(k, add)[, 2])
      proc.3 <- cbind(k, damp)
      proc.3$frac <- proc.3$k / proc.3$damp
      rem <- k
      rem[] <- 1
      proc.3 <- cbind(proc.3, rem)
      proc.3 <- proc.3[which(proc.3$rem == 1), c("k", "damp", "frac")]
      proc.3$year <- as.numeric(substr(as.character(zoo::index(proc.3)), 1, 4))
      first.max <-
        which(proc.3[which(proc.3$year == unique(proc.3$year)[1]), "k"] == max(proc.3[which(proc.3$year ==
                                                                                              unique(proc.3$year)[1]), "k"], na.rm = TRUE))[1]
      proc.3$k_cor <-
        proc.3$frac * as.numeric(as.character((proc.3[first.max, "k"] / proc.3[first.max, "frac"])))

      #g= graphics
      if (make.plot == TRUE) {
        graphics::plot(
          proc.3$k_cor,
          main = meth[i],
          type = "l",
          yaxt = "n",
          col = "grey",
          ylab = expression(italic("K")),
          xlab = "Time"
        )
        graphics::lines(proc.3$k, col = "black", lwd = 1)
        graphics::axis(side = 2, las = 2)
        graphics::legend(
          "topright",
          c(
            expression("Raw " * italic("K")),
            expression("Corrected " * italic("K"))
          ),
          lty = c(1, 1),
          col = c("black", "grey"),
          box.col = "white",
          lwd = 2
        )
        graphics::box()
      }

      #o
      #input[[paste0("damp.mod.",meth[i])]]<-stats::coef(model_nls)
      input[[paste0("k.", meth[i])]] <- proc.3$k_cor
    }

    if (df == T) {
      output.data <-
        list(
          data.frame(
            timestamp = as.character(zoo::index(input[["max.pd"]])),
            value = as.numeric(as.character(input[["max.pd"]]))
          ),
          data.frame(
            timestamp = as.character(zoo::index(input[["max.mw"]])),
            value = as.numeric(as.character(input[["max.mw"]]))
          ),
          data.frame(
            timestamp = as.character(zoo::index(input[["max.dr"]])),
            value = as.numeric(as.character(input[["max.dr"]]))
          ),
          data.frame(
            timestamp = as.character(zoo::index(input[["max.ed"]])),
            value = as.numeric(as.character(input[["max.ed"]]))
          ),
          data.frame(
            timestamp = as.character(zoo::index(input[["daily_max.pd"]])),
            value = as.numeric(as.character(input[["daily_max.pd"]]))
          ),
          data.frame(
            timestamp = as.character(zoo::index(input[["daily_max.mw"]])),
            value = as.numeric(as.character(input[["daily_max.mw"]]))
          ),
          data.frame(
            timestamp = as.character(zoo::index(input[["daily_max.dr"]])),
            value = as.numeric(as.character(input[["daily_max.dr"]]))
          ),
          data.frame(
            timestamp = as.character(zoo::index(input[["daily_max.ed"]])),
            value = as.numeric(as.character(input[["daily_max.ed"]]))
          ),
          data.frame(
            timestamp = as.character(zoo::index(input[["all.pd"]])),
            value = as.numeric(as.character(input[["all.pd"]]))
          ),
          data.frame(
            timestamp = as.character(zoo::index(input[["all.ed"]])),
            value = as.numeric(as.character(input[["all.ed"]]))
          ),
          data.frame(
            timestamp = as.character(zoo::index(input[["input"]])),
            value = as.numeric(as.character(input[["input"]]))
          ),
          # criteria,
          # methods,
          data.frame(
            timestamp = as.character(zoo::index(input[["ed.criteria"]])),
            value = as.numeric(as.character(input[["ed.criteria"]]))
          ),

          data.frame(
            timestamp = as.character(zoo::index(input[["methods"]])),
            value = as.numeric(as.character(input[["methods"]]))
          ),



          data.frame(
            timestamp = as.character(zoo::index(input[["k.pd"]])),
            value = as.numeric(as.character(input[["k.pd"]]))
          ),
          data.frame(
            timestamp = as.character(zoo::index(input[["k.mw"]])),
            value = as.numeric(as.character(input[["k.mw"]]))
          ),
          data.frame(
            timestamp = as.character(zoo::index(input[["k.dr"]])),
            value = as.numeric(as.character(input[["k.dr"]]))
          ),
          data.frame(
            timestamp = as.character(zoo::index(input[["k.ed"]])),
            value = as.numeric(as.character(input[["k.ed"]]))
          )#,
          # input[["damp.mod.pd"]],input[["damp.mod.mw"]],input[["damp.mod.dr"]],input[["damp.mod.ed"]]

        )
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
        "k.ed"#,"damp.mod.pd","damp.mod.mw","damp.mod.dr","damp.mod.ed"
      )
    } else{
      output.data <- input
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
        "k.ed"#,"damp.mod.pd","damp.mod.mw","damp.mod.dr","damp.mod.ed"
      )
    }
  } else{
    k <- input
    if (attributes(k)$class == "data.frame") {
      #e
      if (is.numeric(k$value) == F)
        stop("Invalid input data, values within the data.frame are not numeric.")
      if (is.character(k$timestamp) == F)
        stop("Invalid input data, timestamp within the data.frame are not numeric.")

      #p
      k <-
        zoo::zoo(
          k$value,
          order.by = base::as.POSIXct(k$timestamp, format = "%Y-%m-%d %H:%M:%S", tz =
                                        "UTC")
        )

      #e
      if (as.character(zoo::index(k)[1]) == "(NA NA)" |
          is.na(zoo::index(k)[1]) == T)
        stop("No timestamp present, time.format is likely incorrect.")
    }
    if (zoo::is.zoo(k) == TRUE) {
      if (is.numeric(k) == F)
        stop("Invalid input data, zoo object does not contain numeric data.")
    }

    day.max          <-
      suppressWarnings(stats::aggregate(k, by = list(as.Date(zoo::index(
        k
      ))), max, na.rm = TRUE))
    day.max[which(day.max == "-Inf")] <- NA
    day.max[which(day.max == "Inf")] <- NA
    test.lenght <- length(stats::na.omit(day.max))
    initial.date     <- zoo::index(day.max)[1]
    length.date      <- length(day.max)
    zoo.date         <- zoo::index(day.max)
    day.max[which(day.max < k.threshold)] <- NA

    #e
    if (length(stats::na.omit(day.max)) / test.lenght * 100 < 25)
      stop("Unused argument, k. threshold removed over 75% of all data points.")

    #p
    doy <-
      zoo::zoo(as.numeric(strftime(zoo::index(day.max), format = "%j")), order.by =
                 zoo::index(day.max))
    run <-
      zoo::zoo(zoo::index(day.max) - initial.date + 1, order.by = zoo::index(day.max))
    run_test<-as.numeric(as.character(run))
    proc.1 <- stats::na.omit(cbind(doy, day.max, run))
    model.doy <- stats::lm(day.max ~ doy + I(doy ^ 2), data = proc.1)
    proc.1$year <- as.numeric(substr(as.character(zoo::index(proc.1)), 1, 4))
    proc.1$day.max.cor <- NA

    for (y in c(1:length(unique(proc.1$year)))) {
      scaled    <-
        proc.1[which(proc.1$year == unique(proc.1$year)[y]), "day.max"] / stats::quantile(proc.1[which(proc.1$year ==
                                                                                                         unique(proc.1$year)[y]), "day.max"], probs = c(0.95), na.rm = TRUE)
      max.origin <-
        stats::quantile(proc.1[which(proc.1$year == unique(proc.1$year)[1]), "day.max"], probs =
                          c(0.95), na.rm = TRUE)
      min.scaled <- min(scaled, na.rm = TRUE)
      scaled <-
        (proc.1[which(proc.1$year == unique(proc.1$year)[y]), "day.max"] - (min(proc.1[which(proc.1$year ==
                                                                                               unique(proc.1$year)[y]), "day.max"], na.rm = TRUE))) /
        (stats::quantile(proc.1[which(proc.1$year == unique(proc.1$year)[y]), "day.max"], probs =
                           c(0.95), na.rm = TRUE) - (min(proc.1[which(proc.1$year == unique(proc.1$year)[y]), "day.max"], na.rm =
                                                           TRUE)))
      proc.1[which(proc.1$year == unique(proc.1$year)[y]), "day.max.cor"] <-
        scaled * (stats::quantile(proc.1[which(proc.1$year == unique(proc.1$year)[1]), "day.max"], probs =
                                    c(0.95), na.rm = TRUE) -
                    min(proc.1[which(proc.1$year ==
                                       unique(proc.1$year)[y]), "day.max"], na.rm = TRUE)) + min(proc.1[which(proc.1$year ==
                                                                                                                unique(proc.1$year)[y]), "day.max"], na.rm = TRUE)
    }
    model.doy          <- stats::lm(day.max.cor ~ doy + I(doy ^ 2), data = proc.1)
    pred <-
      model.doy$coefficients[1] + proc.1$doy * model.doy$coefficients[2] + proc.1$doy ^
      2 * model.doy$coefficients[3]
    proc.1$day.max.doy <- proc.1$day.max - pred
    run <- proc.1$run
    k.doy <- proc.1$day.max.doy
    report <- try(stats::nls(
      k.doy ~ ((a + b * run) / (1 + c * run + d * (run ^ 2))),
      start = list(
        a = 0.2,
        b = -0.002,
        c = 0.03,
        d = 0.00001
      ),
      control = list(maxiter = 500)
    ), silent = TRUE)
    if (!inherits(report, "try-error")) {
      model_nls <-
        stats::nls(
          k.doy ~ ((a + b * run) / (1 + c * run + d * (run ^ 2))),
          start = list(
            a = 0.2,
            b = -0.002,
            c = 0.03,
            d = 0.00001
          ),
          control = list(maxiter = 500)
        )
    } else{
      for (n in c(1:20)) {
        b <- stats::runif(1, min = -0.003, max = 0.003)
        report <-
          try(stats::nls(
            k.doy ~ ((a + b * run) / (1 + c * run + d * (run ^ 2))),
            start = list(
              a = 0.2,
              b = b,
              c = 0.03,
              d = 0.00001
            ),
            control = list(maxiter = 500)
          ), silent = TRUE)
        if (inherits(report, "try-error")) {
          next
        } else{
          model_nls <-
            stats::nls(
              k.doy ~ ((a + b * run) / (1 + c * run + d * (run ^ 2))),
              start = list(
                a = 0.2,
                b = b,
                c = 0.03,
                d = 0.00001
              ),
              control = list(maxiter = 500)
            )
        }
        if (n == 20) {
          #e
          stop("Convergence error, no model fit was found for the dampening correction.")
        }
      }
    }


    #p
    proc.2 <-
      ((stats::coef(model_nls)[1] + stats::coef(model_nls)[2] * run_test) / (
        1 + stats::coef(model_nls)[3] * run_test + stats::coef(model_nls)[4] * (run_test ^
                                                                                          2)
      ))

    #proc.2 <-
    #  ((stats::coef(model_nls)[1] + stats::coef(model_nls)[2] * c(1:length.date)) / (
    #    1 + stats::coef(model_nls)[3] * c(1:length.date) + stats::coef(model_nls)[4] * (c(1:length.date) ^
    #                                                                                      2)
    #  ))

    proc.2 <- zoo::zoo(proc.2, order.by = zoo.date)

    #e
    stab <- diff(proc.2, na.rm = TRUE)
    stab[which(stab < 0)] <- 1
    stab[which(stab != 1)] <- 0
    if (length(which(diff(stab) != 0)) > 2) {
      warning(
        "Fitting error, no stable model fit was found for the dampening correction, use make.plot to visually inspect the validity of the fit."
      )
    }
    if (length(which(stab == 0)) == length(stab)) {
      warning(
        "Fitting error, no stable model fit was found for the dampening correction, use make.plot to visually inspect the validity of the fit."
      )
    }

    #g= graphics
    if (make.plot == TRUE) {
      graphics::par(mfrow=c(2,1))
      graphics::par(mar=c(5,5,1,5))
      graphics::plot(
        proc.1$run,
        base::scale(proc.1$day.max, center = TRUE, scale = FALSE),
        type = "p",
        yaxt = "n",
        pch = 16,
        ylab = expression(italic("K") * " (centered)"),
        xlab = "Time since installing (days)"
      )
      graphics::lines(
        run_test,
        base::scale(proc.2, center = TRUE, scale = FALSE),
        col = "white",
        lwd = 5
      )
      graphics::lines(
        run_test,
        base::scale(proc.2, center = TRUE, scale = FALSE),
        col = "black",
        lwd = 4
      )
      graphics::lines(
        run_test,
        base::scale(proc.2, center = TRUE, scale = FALSE),
        col = "orange",
        lwd = 2
      )
      graphics::points(
        run_test,
        base::scale(proc.2, center = TRUE, scale = FALSE),
        col = "black",
        pch = 16,cex=1.2
      )
      graphics::points(
        run_test,
        base::scale(proc.2, center = TRUE, scale = FALSE),
        col = "orange",
        pch = 16,cex=0.8
      )
      graphics::axis(side = 2, las = 2)
      graphics::legend(
        "topright",
        c(expression("Daily max. " * italic("K")), "Dampening model"),
        pch = c(16, NA),
        lty = c(NA, 1),
        col = c("black", "orange"),
        box.col = "white",
        lwd = 2
      )
      graphics::box()
    }

    #p
    proc.orig <-
      ((stats::coef(model_nls)[1] + stats::coef(model_nls)[2] * proc.1$run) / (
        1 + stats::coef(model_nls)[3] * proc.1$run + stats::coef(model_nls)[4] * (proc.1$run ^
                                                                                    2)
      ))
    center <-
      as.numeric(stats::lm(proc.1$day.max ~ 1 + stats::offset(proc.orig))$coefficients)
    add <-
      zoo::zoo(proc.2 + center,
               order.by = as.POSIXct(as.character(paste0(
                 zoo::index(proc.2), " 00:00:00"
               )), format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
    damp <- zoo::na.locf(cbind(k, add)[, 2])
    proc.3 <- cbind(k, damp)
    proc.3$frac <- proc.3$k / proc.3$damp
    rem <- k
    rem[] <- 1
    proc.3 <- cbind(proc.3, rem)
    proc.3 <- proc.3[which(proc.3$rem == 1), c("k", "damp", "frac")]
    proc.3$year <- as.numeric(substr(as.character(zoo::index(proc.3)), 1, 4))
    first.max <-
      which(proc.3[which(proc.3$year == unique(proc.3$year)[1]), "k"] == max(proc.3[which(proc.3$year ==
                                                                                            unique(proc.3$year)[1]), "k"], na.rm = TRUE))[1]
    proc.3$k_cor <-
      proc.3$frac * as.numeric(as.character((proc.3[first.max, "k"] / proc.3[first.max, "frac"])))

    #g= graphics
    if (make.plot == TRUE) {
      graphics::plot(
        proc.3$k_cor,
        type = "l",
        yaxt = "n",
        col = "grey",
        ylab = expression(italic("K")),
        xlab = "Time"
      )
      graphics::lines(proc.3$k, col = "black", lwd = 1)
      graphics::axis(side = 2, las = 2)
      graphics::legend(
        "topright",
        c(
          expression("Raw " * italic("K")),
          expression("Corrected " * italic("K"))
        ),
        lty = c(1, 1),
        col = c("black", "grey"),
        box.col = "white",
        lwd = 2
      )
      graphics::box()
    }

    #o
    if (df == F) {
      output.data <- list(proc.3$k_cor, proc.3$k, stats::coef(model_nls))
      names(output.data) <- c("k.cor", "k", "damp.mod")
    } else{
      output.data <- list(
        data.frame(
          timestamp = as.character(zoo::index(proc.3)),
          value = as.numeric(as.character(proc.3$k_cor))
        ),
        data.frame(
          timestamp = as.character(zoo::index(proc.3)),
          value = as.numeric(as.character(proc.3$k))
        ),
        data.frame(
          a = stats::coef(model_nls)[1],
          b = stats::coef(model_nls)[2],
          c = stats::coef(model_nls)[3],
          d = stats::coef(model_nls)[4]
        )
      )
      row.names(output.data[[3]]) <- 1
      names(output.data) <- c("k.cor", "k", "damp.mod")
    }
  }
  return(output.data)
}

