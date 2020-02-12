#' Calculate sap flux density
#'
#' @description The acquired \eqn{K} values are calculated to sap flux density
#'  (SFD in \eqn{cm^3 cm^{-2} h^{-1}}{cm3 cm-2 h-1}). As many calibration curves exist
#'  (see Peters \emph{et al}. 2018; Flo \emph{et al.} 2019), the function provides the option to
#'  calculate \eqn{SFD} using all available calibration experiment data
#'  (see \code{\link{cal.data}}; Flo \emph{et al.} 2019). Additionally,
#'  raw calibration data can be provided or parameters \eqn{a} and \eqn{b}
#'  for a specific calibration function (\eqn{aK^b}) can be provided.
#'  The algorithm determines for each calibration experiment dataset
#'  the calibration curve (\eqn{SFD = aK^b}) and calculates \eqn{SFD} from
#'  either the mean of all curves and the \eqn{95}% confidence interval
#'  of either all curves, or bootstrapped resampled uncertainty around
#'  the raw calibration experiment data when one calibration dataset is selected.
#'
#' @param input An \code{\link{is.trex}}-compliant object (\code{zoo} vector,
#' \code{data.frame}) of \eqn{K} values containing a timestamp and a value column.
#' @param genus Optional, character vector specifying genus-specific calibration
#' data that should be used (e.g., \code{c("Picea", "Larix")}). See \code{\link{cal.data}}
#' for the specific labels (default = Use all).
#' @param species Optional, character vector of species specific calibration data that should be used,
#'   e.g. \code{c("Picea abies")}. See \code{\link{cal.data}} for the specific labels (default = Use all).
#' @param study Optional character vector of study specific calibration data that
#' should be used (e.g., \code{c("Peters et al. 2018"}) ). See \code{\link{cal.data}}
#' for the specific labels (default= Use all).
#' @param wood Optional, character vector of wood type specific calibration
#' data that should be used (one of \code{c("Diffuse-porous", "Ring-porous", "Coniferous")}).
#' See \code{\link{cal.data}} for the specific labels (default= Use all).
#' @param calib Optional \code{data.frame} containing raw calibration experiment values.
#' Required columns include: \code{[ ,1]} \eqn{K = K} values measured with the probe (numeric),
#' and [,2] \eqn{SFD =} Gravimetrically measured sap flux density (\eqn{cm^3 cm^{-2} h^{-1}}{cm3 cm-2 h-1})) (numeric).
#' If not provided, literature values are used.
#' @param a Optional, numeric value for the calibration curve (\eqn{SFD = aK^b}).
#' No uncertainty can be calculated when this value is provided.
#' @param b Optional, numeric value for the calibration curve (\eqn{SFD = aK^b}).
#' No uncertainty can be calculated when this value is provided.
#' @param decimals Integer, the number of decimals of the output (default = 10).
#' @param make.plot Logical; if \code{TRUE}, a plot is generated showing
#' the calibration curve with \eqn{K vs sap flux density} (\eqn{cm^3 cm^{-2} h^{-1}}{cm3 cm-2 h-1})).
#' @param df Logical; If \code{TRUE}, output is provided in a \code{data.frame} format
#'  with a timestamp and a value column. If \code{FALSE}, output
#'  is provided as a \code{zoo} vector object (default = FALSE).
#'
#'
#' @details The function fits a calibration curve (\eqn{SFD = aK^b}{SFD = aK^b})
#' through all selected raw calibration data. If multiple studies are provided,
#' multiple calibration curves are fitted. In case a single calibration dataset
#' is provided a bootstrap resampling is applied (n = 100) to determined the
#' mean and 95% confidence interval of the fit. When multiple calibration curves
#' are requested the mean and 95% confidence interval is determined on the fitted functions.
#' The mean and confidence interval are used to calculate \eqn{SFD} from \eqn{K}.
#'
#' @return A list containing either a \code{zoo} object or \code{data.frame} in the appropriate format
#'  for other functionalities (see \code{\link{tdm_dt.max}} output specifications), as well as
#'  all \eqn{SFD} values for each method are provided and added to the
#'  \code{\link{is.trex}}-compliant object (e.g., [['sfd.pd']], [['sfd.mw']])
#'  if this format was provided as an input, and,
#'  finally, a \code{data.frame} is provided with the mean and 95% confidence
#'  interval of the applied calibration functions (see [['model.ens']]).
#'  If an individual time series is provided for input with \eqn{K} values an alternative output is provided:
#'
#'  \describe{
#'    \item{input}{K values provided as input.}
#'    \item{sfd.input}{\eqn{SFD} values calculated for the input according to the mean of the calibration function.}
#'    \item{model.ens}{A \code{data.frame} providing the mean and 95\% confidence interval of the applied calibration function.}
#'    \item{out.param}{A \code{data.frame} with the coefficients of calibration function.}
#'  }
#'
#'
#'
#' @references
#'  Peters RL, Fonti P, Frank DC, Poyatos R, Pappas C, Kahmen A, Carraro V, Prendin AL, Schneider L, Baltzer JL,
#'  Baron-Gafford GA, Dietrich L, Heinrich I, Minor RL, Sonnentag O, Matheny AM, Wightman MG, Steppe K. 2018.
#'  Quantification of uncertainties in conifer sap flow measured with the thermal dissipation method.
#'  New Phytologist 219:1283-1299 <doi: 10.1111/nph.15241>
#'
#'  Flo V, Martinez-Vilalta J, Steppe K, Schuldt B, Poyatos, R. 2019.
#'  A synthesis of bias and uncertainty in sap flow methods. A
#'  gricultural and Forest Meteorology 271:362-374 <doi: 10.1016/j.agrformet.2019.03.012>
#'
#'
#' @export
#'
#' @examples
#' #calculating sap flux density
#' \dontrun{
#' raw   <-is.trex(example.data(type="doy"),
#'     tz="GMT",time.format="%H:%M",
#'     solar.time=TRUE,long.deg=7.7459,
#'     ref.add=FALSE)
#'
#' input <-dt.steps(input=raw,start="2014-05-08 00:00",
#' end="2014-07-25 00:50",
#'      time.int=15,max.gap=60,decimals=10,df=FALSE)
#'
#' input[which(input<0.2)]<-NA
#'
#' input <-tdm_dt.max(input, methods=c("pd","mw","dr"),
#'      det.pd=TRUE,interpolate=FALSE,max.days=10,df=FALSE)
#'
#' output.data<-tdm_cal.sfd(input,make.plot=TRUE,df=FALSE,
#' wood="Coniferous")
#'
#'
#' str(output.data)
#' plot(output.data$sfd.pd$sfd,ylim=c(0,10))
#' lines(output.data$sfd.pd$q025,lty=1,col="grey")
#' lines(output.data$sfd.pd$q975,lty=1,col="grey")
#' lines(output.data$sfd.pd$sfd)
#'
#' output.data$out.param
#' }
#'
tdm_cal.sfd <-
  function(input,
           genus,
           species,
           study,
           wood,
           calib,
           a,
           b,
           decimals,
           make.plot = TRUE,
           df = FALSE) {
    #t= test
    #raw   <-is.trex(example.data(type="doy"),tz="GMT",time.format="%H:%M",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE)
    #input <-dt.steps(input=raw,start="2014-05-08 00:00",end="2014-07-25 00:50",
    #                  time.int=15,max.gap=60,decimals=10,df=F)
    #input[which(input<0.2)]<-NA
    #input <-tdm_dt.max(input, methods=c("pd","mw","dr"),det.pd=TRUE,interpolate=FALSE,max.days=10,sr.input=sr.input,vpd.input=vpd.input,
    #               ed.window=2*60,criteria=c(sr=30,vpd=0.1,cv=0.5),df=FALSE)
    #input<-input$k.pd


    #genus<-unique(calib$Genus)
    #genus<-"Picea"
    #species<-unique(calib$Species)
    #study<-unique(calib$Study)
    #wood<-unique(calib$Wood)[c(1,2,3,4)]
    #make.plot<-TRUE
    #decimals<-6
    #a<-NA
    #b<-NA

    #d= default conditions
    if (missing(calib)) {
      calib <- getExportedValue('TREX', 'cal.data')
    }
    if (ncol(calib) == 10) {
      if (missing(genus)) {
        genus <- unique(calib$Genus)
      }
      if (missing(species)) {
        species <- unique(calib$Species)
      }
      if (missing(study)) {
        study <- unique(calib$Study)
      }
      if (missing(wood)) {
        wood <- unique(calib$Wood)
      }
    }
    if (missing(make.plot)) {
      make.plot <- F
    }
    if (missing(df)) {
      df <- F
    }
    if (missing(a)) {
      a <- NA
    }
    if (missing(b)) {
      b <- NA
    }
    if (missing(decimals)) {
      decimals <- 6
    }

    #e=errors
    if (df != T &
        df != F)
      stop("Unused argument, df needs to be TRUE|FALSE.")
    if (make.plot != T &
        make.plot != F)
      stop("Unused argument, make.plot needs to be TRUE|FALSE.")
    if (is.numeric(decimals) == F)
      stop("Unused argument, decimals should be numeric.")
    if (decimals > 10)
      stop("Unused argument, decimals should smaller then 10.")

    if (is.na(a) == F | is.na(b) == F) {
      if (missing(calib) == F) {
        if (ncol(calib) != 10)
          stop("Unused arguments, cannot provide both a/b and calib.")
      }
    }
    if (missing(calib) == F) {
      if (ncol(calib) != 10) {
        if (is.na(a) == F |
            is.na(b) == F)
          stop("Unused arguments, cannot provide both a/b and calib.")
      }
    }


    #p= processing when TREX file is provided
    if (length(names(input)) != 0) {
      #e=
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

      #o= output
      methods <- input$methods

      #p= process (when no a, b or calib are provided)
      if (is.na(a) == TRUE & is.na(b) == TRUE & ncol(calib) == 10) {
        #o = output
        output.data <-
          list(
            input$max.pd,
            input$max.mw,
            input$max.dr,
            input$max.ed,
            input$daily_max.pd,
            input$daily_max.mw,
            input$daily_max.dr,
            input$daily_max.ed,
            input$all.pd,
            input$all.ed,
            input$input,
            input$ed.criteria,
            input$methods,
            input$k.pd,
            input$k.mw,
            input$k.dr,
            input$k.ed,
            sfd.pd <-
              NA,
            sfd.mw <- NA,
            sfd.pd <- NA,
            sfd.ed <- NA,
            model.ens <- NA,
            out.param <- NA
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
          "k.ed",
          "sfd.pd",
          "sfd.mw",
          "sfd.dr",
          "sfd.ed",
          "model.ens",
          "out.param"
        )

        #p= process
        sel.calib <-
          calib[which(
            as.character(calib$Genus) %in% as.character(genus) &
              as.character(calib$Species) %in% as.character(species) &
              as.character(calib$Wood.porosity) %in% as.character(wood)
          ), ]

        for (i in c(1:length(methods))) {
          #i<-1
          k <- input[[paste0("k.", methods[i])]]

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

          output.sub <- data.frame(a = NA, b = NA, sd = NA)
          for (m in c(1:length(unique(paste0((sel.calib$Study), (sel.calib$Species)
          ))))) {
            sel.sub <-
              sel.calib[which(as.character(paste0(
                sel.calib$Study, sel.calib$Species
              )) == as.character(unique(paste0((sel.calib$Study), (sel.calib$Species)
              )))[m]), ]
            if (as.character(unique(paste0((sel.calib$Study), (sel.calib$Species)
            )))[m] == "Paudel et al. 2013Prunus persica") {
              model.calib <- stats::nls(SFD ~ a * k ^ b,
                                        start = list(a = 100, b = 1.231),
                                        data = sel.sub)
            } else{
              model.calib <- stats::nls(SFD ~ a * k ^ b,
                                        start = list(a = 42.84, b = 1.231),
                                        data = sel.sub)
            }
            output.sub[m, "a"] <- stats::coef(model.calib)[1]
            output.sub[m, "b"] <- stats::coef(model.calib)[2]
            output.sub[m, "sd"] <- stats::sd(stats::resid(model.calib))
          }

          #o= output
          output.data[["out.param"]] <-
            data.frame(
              n = nrow(output.sub),
              a_mu = base::mean(output.sub$a),
              a_sd = stats::sd(output.sub$a),
              log.a_mu = base::mean(log(output.sub$a)),
              log.a_sd = stats::sd(log(output.sub$a)),
              b_mu = base::mean(output.sub$b),
              b_sd = stats::sd(output.sub$b)
            )

          output.model <-
            data.frame(k = seq(0, ceiling(max(k, na.rm = TRUE)), 0.001), SFD = NA)
          for (m in c(1:length(unique(
            paste0(sel.calib$Study, sel.calib$Species)
          )))) {
            output.model[, m + 1] <-
              output.sub[m, "a"] * seq(0, ceiling(max(k, na.rm = TRUE)), 0.001) ^ output.sub[m, "b"]
          }

          if (length(paste0(unique(
            paste0(sel.calib$Study, sel.calib$Species)
          ))) == 1) {
            model.means <- output.model[, -1]
          } else{
            model.means <- rowMeans(output.model[, -1])
          }
          model.quant.0.025 <- NA
          model.quant.0.975 <- NA
          model.sd <- NA

          for (r in c(1:nrow(output.model))) {
            model.quant.0.025[r] <-
              as.numeric(stats::quantile(output.model[r, -1], probs = c(0.025))[1])
            model.quant.0.975[r] <-
              as.numeric(stats::quantile(output.model[r, -1], probs = c(0.975))[1])
            model.sd[r] <- as.numeric(as.numeric(stats::sd(output.model[r, -1])))
          }
          quant.0.025 <- model.quant.0.025
          quant.0.975 <- model.quant.0.975

          if (length(unique(paste0(
            sel.calib$Study, sel.calib$Species
          ))) == 1) {
            sel.sub <-
              sel.calib[which(as.character(paste0(
                sel.calib$Study, sel.calib$Species
              )) == as.character(unique(paste0((sel.calib$Study), (sel.calib$Species)
              )))[1]), ]

            output.sub <- data.frame(a = NA,
                                     b = NA,
                                     sd = NA)
            for (n in c(1:100)) {
              sel.n  <-
                sel.sub[sample(c(1:nrow(sel.sub)), nrow(sel.sub), replace = T), ]
              model.n <- stats::nls(SFD ~ a * k ^ b,
                                    start = list(a = 100, b = 1.231),
                                    data = sel.n)
              output.sub[n, "a"] <- stats::coef(model.n)[1]
              output.sub[n, "b"] <- stats::coef(model.n)[2]
              output.sub[n, "sd"] <- stats::sd(stats::resid(model.n))
              if (n == 1) {
                n.mod <-
                  as.numeric(stats::predict(model.n, newdat = data.frame(k = seq(
                    0, ceiling(max(k, na.rm = TRUE)), 0.001
                  ))))
              } else{
                n.mod <-
                  cbind(n.mod, as.numeric(stats::predict(model.n, newdat = data.frame(
                    k = seq(0, ceiling(max(
                      k, na.rm = TRUE
                    )), 0.001)
                  ))))
              }
            }
            #o= output
            output.data[["out.param"]] <-
              data.frame(
                n = nrow(output.sub),
                a_mu = base::mean(output.sub$a),
                a_sd = stats::sd(output.sub$a),
                log.a_mu = base::mean(log(output.sub$a)),
                log.a_sd = stats::sd(log(output.sub$a)),
                b_mu = base::mean(output.sub$b),
                b_sd = stats::sd(output.sub$b)
              )

            model.n.0.025 <- NA
            model.n.0.975 <- NA
            for (r in c(1:nrow(n.mod))) {
              model.n.0.025[r] <-
                as.numeric(stats::quantile(as.numeric(n.mod[r, ]), probs = c(0.025))[1])
              model.n.0.975[r] <-
                as.numeric(stats::quantile(as.numeric(n.mod[r, ]), probs = c(0.975))[1])
            }
            quant.0.025 <- model.n.0.025
            quant.0.975 <- model.n.0.975
          }
          if (i == 1) {
            if (length(unique(paste0(
              sel.calib$Study, sel.calib$Species
            ))) != 1) {
              #g= graphics
              if (make.plot == TRUE) {
                graphics::plot(
                  sel.calib$k,
                  sel.calib$SFD,
                  ylim = c(0, 400),
                  xlim = c(0, ceiling(max(
                    k, na.rm = TRUE
                  ))),
                  pch = 16,
                  xlab = expression(italic("K") * " (-)"),
                  ylab = "",
                  yaxt = "n",
                  col = "lightgrey"
                )
                graphics::mtext(
                  side = 2,
                  expression(italic("SFD") * " (" * cm ^ 3 * cm ^ -2 * h ^ -1 * ")"),
                  padj = -2.5
                )
                graphics::mtext(
                  side = 3,
                  paste(as.character(wood), collapse = " | "),
                  font = 3,
                  cex = 1.2,
                  padj = -1
                )
                graphics::axis(side = 2, las = 2)
                graphics::polygon(
                  c(
                    min(k, na.rm = TRUE),
                    min(k, na.rm = TRUE),
                    max(k, na.rm = TRUE),
                    max(k, na.rm = TRUE)
                  ),
                  c(-1000, 1000, 1000, -1000),
                  col = "darkgrey",
                  border = "white"
                )
                graphics::points(
                  sel.calib$k,
                  sel.calib$SFD,
                  pch = 16,
                  col = "white",
                  cex = 1.5
                )
                graphics::points(
                  sel.calib$k,
                  sel.calib$SFD,
                  pch = 16,
                  col = "lightgrey",
                  cex = 1.2
                )


                for (m in c(1:nrow(output.sub))) {
                  graphics::lines(
                    seq(0, ceiling(max(
                      k, na.rm = TRUE
                    )), 0.001),
                    output.sub[m, "a"] * seq(0, ceiling(max(
                      k, na.rm = TRUE
                    )), 0.001) ^ output.sub[m, "b"],
                    col = "grey",
                    lwd = 1,
                    lty = 1
                  )
                }

                graphics::lines(seq(0, ceiling(max(
                  k, na.rm = TRUE
                )), 0.001),
                model.means,
                col = "white",
                lwd = 5)
                graphics::lines(seq(0, ceiling(max(
                  k, na.rm = TRUE
                )), 0.001),
                model.means,
                col = "black",
                lwd = 4)
                graphics::lines(seq(0, ceiling(max(
                  k, na.rm = TRUE
                )), 0.001),
                model.means,
                col = "orange",
                lwd = 2)
                graphics::lines(
                  seq(0, ceiling(max(
                    k, na.rm = TRUE
                  )), 0.001),
                  quant.0.025,
                  col = "black",
                  lwd = 2,
                  lty = 2
                )
                graphics::lines(
                  seq(0, ceiling(max(
                    k, na.rm = TRUE
                  )), 0.001),
                  quant.0.975,
                  col = "black",
                  lwd = 2,
                  lty = 2
                )
                graphics::legend(
                  "topright",
                  c(
                    "Raw data",
                    "Individual fit",
                    expression(mu * " fit"),
                    "95% CI"
                  ),
                  border = "white",
                  bg = "white",
                  pch = c(16, NA, NA, NA),
                  lty = c(NA, 1, 1, 2),
                  lwd = c(NA, 1, 3, 2),
                  col = c("lightgrey", "grey", "orange", "black"),
                  box.col = "white"
                )
                graphics::text(
                  min(k, na.rm = TRUE),
                  340,
                  expression(italic("K") * " Range"),
                  col = "white",
                  pos = 4,
                  cex = 1.5
                )
                graphics::text(
                  min(k, na.rm = TRUE),
                  380,
                  paste0("Method: ", methods[i]),
                  col = "white",
                  pos = 4,
                  cex = 1.5
                )
                graphics::box()
              }
            } else{
              if (make.plot == TRUE) {
                graphics::plot(
                  sel.calib$k,
                  sel.calib$SFD,
                  ylim = c(0, 400),
                  xlim = c(0, ceiling(max(
                    k, na.rm = TRUE
                  ))),
                  pch = 16,
                  xlab = expression(italic("K") * " (-)"),
                  ylab = "",
                  yaxt = "n",
                  col = "lightgrey"
                )
                graphics::mtext(
                  side = 3,
                  unique(
                    paste0(sel.calib$Study, " | ", sel.calib$Species)
                  )[m],
                  font = 3,
                  cex = 1.2,
                  padj = -1
                )
                graphics::mtext(
                  side = 2,
                  expression(italic("SFD") * " (" * cm ^ 3 * cm ^ -2 * h ^ -1 * ")"),
                  padj = -2.5
                )
                graphics::axis(side = 2, las = 2)
                graphics::polygon(
                  c(
                    min(k, na.rm = TRUE),
                    min(k, na.rm = TRUE),
                    max(k, na.rm = TRUE),
                    max(k, na.rm = TRUE)
                  ),
                  c(-1000, 1000, 1000, -1000),
                  col = "darkgrey",
                  border = "white"
                )
                graphics::points(
                  sel.calib$k,
                  sel.calib$SFD,
                  pch = 16,
                  col = "white",
                  cex = 1.5
                )
                graphics::points(
                  sel.calib$k,
                  sel.calib$SFD,
                  pch = 16,
                  col = "lightgrey",
                  cex = 1.2
                )

                for (m in c(1:nrow(output.sub))) {
                  graphics::lines(
                    seq(0, ceiling(max(
                      k, na.rm = TRUE
                    )), 0.001),
                    output.sub[m, "a"] * seq(0, ceiling(max(
                      k, na.rm = TRUE
                    )), 0.001) ^ output.sub[m, "b"],
                    col = "grey",
                    lwd = 1,
                    lty = 1
                  )
                }
                graphics::lines(
                  seq(0, ceiling(max(
                    k, na.rm = TRUE
                  )), 0.001),
                  quant.0.025,
                  col = "black",
                  lwd = 2,
                  lty = 2
                )
                graphics::lines(
                  seq(0, ceiling(max(
                    k, na.rm = TRUE
                  )), 0.001),
                  quant.0.975,
                  col = "black",
                  lwd = 2,
                  lty = 2
                )
                graphics::lines(seq(0, ceiling(max(
                  k, na.rm = TRUE
                )), 0.001),
                model.means,
                col = "white",
                lwd = 5)
                graphics::lines(seq(0, ceiling(max(
                  k, na.rm = TRUE
                )), 0.001),
                model.means,
                col = "black",
                lwd = 4)
                graphics::lines(seq(0, ceiling(max(
                  k, na.rm = TRUE
                )), 0.001),
                model.means,
                col = "orange",
                lwd = 2)
                graphics::legend(
                  "topright",
                  c(
                    "Raw data",
                    "Individual fit",
                    expression("Fit"),
                    "95% CI"
                  ),
                  border = "white",
                  bg = "white",
                  pch = c(16, NA, NA, NA),
                  lty = c(NA, 1, 1, 2),
                  lwd = c(NA, 1, 3, 2),
                  col = c("lightgrey", "grey", "orange", "black"),
                  box.col = "white"
                )
                graphics::text(
                  min(k, na.rm = TRUE),
                  340,
                  expression(italic("K") * " Range"),
                  col = "white",
                  pos = 4,
                  cex = 1.5
                )
                graphics::text(
                  min(k, na.rm = TRUE),
                  380,
                  paste0("Method: ", methods[i]),
                  col = "white",
                  pos = 4,
                  cex = 1.5
                )
                graphics::box()
              }
            }
          }

          #o= output
          model.ens <-
            data.frame(
              k = seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
              sfd = model.means,
              q025 = quant.0.025,
              q975 = quant.0.975
            )
          model.ens <-
            merge(data.frame(k = seq(
              0, ceiling(max(k, na.rm = TRUE)), 1 / (10 ^ decimals)
            )),
            model.ens,
            by = "k",
            all = T)
          model.ens$sfd <- zoo::na.approx(model.ens$sfd)
          model.ens$q025 <- zoo::na.approx(model.ens$q025)
          model.ens$q975 <- zoo::na.approx(model.ens$q975)
          if (i == 1) {
            output.data[["model.ens"]] <- model.ens
          }
          k.round <- round(k, decimals)
          model.zoo <-
            zoo::zoo(model.ens[match(as.character(k.round), as.character(model.ens$k)), ], order.by =
                       zoo::index(k))
          output.data[[paste0("sfd.", methods[i])]] <- model.zoo
        }

        if (df == T) {
          output.data <-
            list(
              data.frame(
                timestamp = as.character(zoo::index(input$max.pd)),
                value = as.numeric(as.character(input$max.pd))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$max.mw)),
                value = as.numeric(as.character(input$max.mw))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$max.dr)),
                value = as.numeric(as.character(input$max.dr))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$max.ed)),
                value = as.numeric(as.character(input$max.ed))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$daily_max.pd)),
                value = as.numeric(as.character(input$daily_max.pd))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$daily_max.mw)),
                value = as.numeric(as.character(input$daily_max.mw))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$daily_max.dr)),
                value = as.numeric(as.character(input$daily_max.dr))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$daily_max.ed)),
                value = as.numeric(as.character(input$daily_max.ed))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$all.pd)),
                value = as.numeric(as.character(input$all.pd))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$all.ed)),
                value = as.numeric(as.character(input$all.ed))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$input)),
                value = as.numeric(as.character(input$input))
              ),
              input$ed.criteria,
              input$methods,
              #data.frame(probe.length=probe.length,sapwood.thickness=sapwood.thickness),
              data.frame(
                timestamp = as.character(zoo::index(input$k.pd)),
                value = as.numeric(as.character(input$k.pd))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$k.mw)),
                value = as.numeric(as.character(input$k.mw))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$k.dr)),
                value = as.numeric(as.character(input$k.dr))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$k.ed)),
                value = as.numeric(as.character(input$k.ed))
              ),
              data.frame(
                timestamp = as.character(zoo::index(sfd.pd)),
                value = as.numeric(as.character(sfd.pd))
              ),
              data.frame(
                timestamp = as.character(zoo::index(sfd.mw)),
                value = as.numeric(as.character(sfd.mw))
              ),
              data.frame(
                timestamp = as.character(zoo::index(sfd.dr)),
                value = as.numeric(as.character(sfd.dr))
              ),
              data.frame(
                timestamp = as.character(zoo::index(sfd.ed)),
                value = as.numeric(as.character(sfd.ed))
              ),
              model.ens,
              out.param
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
            "k.ed",
            "sfd.pd",
            "sfd.mw",
            "sfd.dr",
            "sfd.ed",
            "model.ens",
            "out.param"
          )
        }

        return(output.data)
      }

      #process when calibration data is provided
      calib <- getExportedValue('TREX', 'cal.data')
      calib <- calib[which(calib$Study == "Peters et al. 2017"), ]
      calib <- data.frame(K = calib$k, SFD = calib$SFD)

      if (length(which(colnames(calib)[c(1, 2)] %in% c("K", "SFD")))) {
        #e=
        if (is.numeric(calib$K) == F)
          stop("Invalid input data, K in calib is not numeric.")
        if (is.numeric(calib$SFD) == F)
          stop("Invalid input data, SFD in calib is not numeric.")

        #o=
        output.data <-
          list(
            input$max.pd,
            input$max.mw,
            input$max.dr,
            input$max.ed,
            input$daily_max.pd,
            input$daily_max.mw,
            input$daily_max.dr,
            input$daily_max.ed,
            input$all.pd,
            input$all.ed,
            input$input,
            input$ed.criteria,
            input$methods,
            input$k.pd,
            input$k.mw,
            input$k.dr,
            input$k.ed,
            sfd.pd <-
              NA,
            sfd.mw <- NA,
            sfd.pd <- NA,
            sfd.ed <- NA,
            model.ens <- NA,
            out.param <- NA
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
          "k.ed",
          "sfd.pd",
          "sfd.mw",
          "sfd.dr",
          "sfd.ed",
          "model.ens",
          "out.param"
        )

        #p=
        for (i in c(1:length(methods))) {
          #i<-1
          k <- input[[paste0("k.", methods[i])]]

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

          sel.sub <- calib

          model.calib <- stats::nls(SFD ~ a * K ^ b,
                                    start = list(a = 100, b = 1.231),
                                    data = sel.sub)
          model.means <-
            stats::predict(model.calib, newdata = data.frame(K = data.frame(K = seq(
              0, ceiling(max(k, na.rm = TRUE)), 0.001
            ))))

          output.sub <- data.frame(a = NA, b = NA, sd = NA)
          for (n in c(1:100)) {
            sel.n  <-
              sel.sub[sample(c(1:nrow(sel.sub)), nrow(sel.sub), replace = T), ]
            model.n <- stats::nls(SFD ~ a * k ^ b,
                                  start = list(a = 100, b = 1.231),
                                  data = sel.n)
            output.sub[n, "a"] <- stats::coef(model.n)[1]
            output.sub[n, "b"] <- stats::coef(model.n)[2]
            output.sub[n, "sd"] <- stats::sd(stats::resid(model.n))
            if (n == 1) {
              n.mod <-
                as.numeric(stats::predict(model.n, newdat = data.frame(k = seq(
                  0, ceiling(max(k, na.rm = TRUE)), 0.001
                ))))
            } else{
              n.mod <-
                cbind(n.mod, as.numeric(stats::predict(model.n, newdat = data.frame(
                  k = seq(0, ceiling(max(
                    k, na.rm = TRUE
                  )), 0.001)
                ))))
            }
          }
          #o= output
          output.data[["out.param"]] <-
            data.frame(
              n = nrow(output.sub),
              a_mu = base::mean(output.sub$a),
              a_sd = stats::sd(output.sub$a),
              log.a_mu = base::mean(log(output.sub$a)),
              log.a_sd = stats::sd(log(output.sub$a)),
              b_mu = base::mean(output.sub$b),
              b_sd = stats::sd(output.sub$b)
            )

          model.n.0.025 <- NA
          model.n.0.975 <- NA
          for (r in c(1:nrow(n.mod))) {
            model.n.0.025[r] <-
              as.numeric(stats::quantile(as.numeric(n.mod[r, ]), probs = c(0.025))[1])
            model.n.0.975[r] <-
              as.numeric(stats::quantile(as.numeric(n.mod[r, ]), probs = c(0.975))[1])
          }
          quant.0.025 <- model.n.0.025
          quant.0.975 <- model.n.0.975

          if (i == 1) {
            if (make.plot == TRUE) {
              graphics::plot(
                calib$K,
                calib$SFD,
                ylim = c(0, 400),
                xlim = c(0, ceiling(max(
                  k, na.rm = TRUE
                ))),
                pch = 16,
                xlab = expression(italic("K") * " (-)"),
                ylab = "",
                yaxt = "n",
                col = "lightgrey"
              )
              graphics::mtext(
                side = 3,
                "Calibration data",
                font = 3,
                cex = 1.2,
                padj = -1
              )
              graphics::mtext(
                side = 2,
                expression(italic("SFD") * " (" * cm ^ 3 * cm ^ -2 * h ^ -1 * ")"),
                padj = -2.5
              )
              graphics::axis(side = 2, las = 2)
              graphics::polygon(
                c(
                  min(k, na.rm = TRUE),
                  min(k, na.rm = TRUE),
                  max(k, na.rm = TRUE),
                  max(k, na.rm = TRUE)
                ),
                c(-1000, 1000, 1000, -1000),
                col = "darkgrey",
                border = "white"
              )
              graphics::points(
                calib$K,
                calib$SFD,
                pch = 16,
                col = "white",
                cex = 1.5
              )
              graphics::points(
                calib$K,
                calib$SFD,
                pch = 16,
                col = "lightgrey",
                cex = 1.2
              )
              graphics::lines(
                seq(0, ceiling(max(
                  k, na.rm = TRUE
                )), 0.001),
                quant.0.025,
                col = "black",
                lwd = 2,
                lty = 2
              )
              graphics::lines(
                seq(0, ceiling(max(
                  k, na.rm = TRUE
                )), 0.001),
                quant.0.975,
                col = "black",
                lwd = 2,
                lty = 2
              )
              graphics::lines(seq(0, ceiling(max(
                k, na.rm = TRUE
              )), 0.001),
              model.means,
              col = "white",
              lwd = 5)
              graphics::lines(seq(0, ceiling(max(
                k, na.rm = TRUE
              )), 0.001),
              model.means,
              col = "black",
              lwd = 4)
              graphics::lines(seq(0, ceiling(max(
                k, na.rm = TRUE
              )), 0.001),
              model.means,
              col = "orange",
              lwd = 2)
              graphics::legend(
                "topright",
                c(
                  "Raw data",
                  "Individual fit",
                  expression("Fit"),
                  "95% CI"
                ),
                border = "white",
                bg = "white",
                pch = c(16, NA, NA, NA),
                lty = c(NA, 1, 1, 2),
                lwd = c(NA, 1, 3, 2),
                col = c("lightgrey", "grey", "orange", "black"),
                box.col = "white"
              )
              graphics::text(
                min(k, na.rm = TRUE),
                340,
                expression(italic("K") * " Range"),
                col = "white",
                pos = 4,
                cex = 1.5
              )
              graphics::text(
                min(k, na.rm = TRUE),
                380,
                paste0("Method: ", methods[i]),
                col = "white",
                pos = 4,
                cex = 1.5
              )
              graphics::box()
            }
          }
          #o= output
          model.ens <-
            data.frame(
              k = seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
              sfd = model.means,
              q025 = quant.0.025,
              q975 = quant.0.975
            )
          model.ens <-
            merge(data.frame(k = seq(
              0, ceiling(max(k, na.rm = TRUE)), 1 / (10 ^ decimals)
            )),
            model.ens,
            by = "k",
            all = T)
          model.ens$sfd <- zoo::na.approx(model.ens$sfd)
          model.ens$q025 <- zoo::na.approx(model.ens$q025)
          model.ens$q975 <- zoo::na.approx(model.ens$q975)
          if (i == 1) {
            output.data[["model.ens"]] <- model.ens
          }
          k.round <- round(k, decimals)
          model.zoo <-
            zoo::zoo(model.ens[match(as.character(k.round), as.character(model.ens$k)), ], order.by =
                       zoo::index(k))
          output.data[[paste0("sfd.", methods[i])]] <- model.zoo
        }

        if (df == T) {
          output.data <-
            list(
              data.frame(
                timestamp = as.character(zoo::index(input$max.pd)),
                value = as.numeric(as.character(input$max.pd))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$max.mw)),
                value = as.numeric(as.character(input$max.mw))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$max.dr)),
                value = as.numeric(as.character(input$max.dr))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$max.ed)),
                value = as.numeric(as.character(input$max.ed))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$daily_max.pd)),
                value = as.numeric(as.character(input$daily_max.pd))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$daily_max.mw)),
                value = as.numeric(as.character(input$daily_max.mw))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$daily_max.dr)),
                value = as.numeric(as.character(input$daily_max.dr))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$daily_max.ed)),
                value = as.numeric(as.character(input$daily_max.ed))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$all.pd)),
                value = as.numeric(as.character(input$all.pd))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$all.ed)),
                value = as.numeric(as.character(input$all.ed))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$input)),
                value = as.numeric(as.character(input$input))
              ),
              input$ed.criteria,
              input$methods,
              #data.frame(probe.length=probe.length,sapwood.thickness=sapwood.thickness),
              data.frame(
                timestamp = as.character(zoo::index(input$k.pd)),
                value = as.numeric(as.character(input$k.pd))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$k.mw)),
                value = as.numeric(as.character(input$k.mw))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$k.dr)),
                value = as.numeric(as.character(input$k.dr))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$k.ed)),
                value = as.numeric(as.character(input$k.ed))
              ),
              data.frame(
                timestamp = as.character(zoo::index(sfd.pd)),
                value = as.numeric(as.character(sfd.pd))
              ),
              data.frame(
                timestamp = as.character(zoo::index(sfd.mw)),
                value = as.numeric(as.character(sfd.mw))
              ),
              data.frame(
                timestamp = as.character(zoo::index(sfd.dr)),
                value = as.numeric(as.character(sfd.dr))
              ),
              data.frame(
                timestamp = as.character(zoo::index(sfd.ed)),
                value = as.numeric(as.character(sfd.ed))
              ),
              model.ens,
              out.param
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
            "k.ed",
            "sfd.pd",
            "sfd.mw",
            "sfd.dr",
            "sfd.ed",
            "model.ens",
            "out.param"
          )
        }

        return(output.data)
      }

      #if a and b are provided
      #a=42.84
      #b=1.231

      if (is.na(a) == F & is.na(b) == F) {
        #e=
        if (is.numeric(a) == F)
          stop("Invalid input data, a is not numeric.")
        if (is.numeric(b) == F)
          stop("Invalid input data, b is not numeric.")

        #o=
        output.data <-
          list(
            input$max.pd,
            input$max.mw,
            input$max.dr,
            input$max.ed,
            input$daily_max.pd,
            input$daily_max.mw,
            input$daily_max.dr,
            input$daily_max.ed,
            input$all.pd,
            input$all.ed,
            input$input,
            input$ed.criteria,
            input$methods,
            input$k.pd,
            input$k.mw,
            input$k.dr,
            input$k.ed,
            sfd.pd <-
              NA,
            sfd.mw <- NA,
            sfd.pd <- NA,
            sfd.ed <- NA,
            sfd.dr <- NA,
            model.ens <- NA,
            out.param <- NA
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
          "k.ed",
          "sfd.pd",
          "sfd.mw",
          "sfd.dr",
          "sfd.ed",
          "model.ens",
          "out.param"
        )


        for (i in c(1:length(methods))) {
          k <- input[[paste0("k.", methods[i])]]

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

          model.means <- a * seq(0, ceiling(max(k, na.rm = TRUE)), 0.001) ^ b
          quant.0.025 <- rep(NA, length(model.means))
          quant.0.975 <- rep(NA, length(model.means))

          if (i == 1) {
            if (make.plot == TRUE) {
              graphics::plot(
                1,
                1,
                ylim = c(0, 400),
                xlim = c(0, ceiling(max(
                  k, na.rm = TRUE
                ))),
                pch = 16,
                xlab = expression(italic("K") * " (-)"),
                ylab = "",
                yaxt = "n",
                col = "white"
              )
              graphics::mtext(
                side = 3,
                "Calibration curve",
                font = 3,
                cex = 1.2,
                padj = -1
              )
              graphics::mtext(
                side = 2,
                expression(italic("SFD") * " (" * cm ^ 3 * cm ^ -2 * h ^ -1 * ")"),
                padj = -2.5
              )
              graphics::axis(side = 2, las = 2)
              graphics::polygon(
                c(
                  min(k, na.rm = TRUE),
                  min(k, na.rm = TRUE),
                  max(k, na.rm = TRUE),
                  max(k, na.rm = TRUE)
                ),
                c(-1000, 1000, 1000, -1000),
                col = "darkgrey",
                border = "white"
              )
              graphics::lines(seq(0, ceiling(max(
                k, na.rm = TRUE
              )), 0.001),
              model.means,
              col = "white",
              lwd = 5)
              graphics::lines(seq(0, ceiling(max(
                k, na.rm = TRUE
              )), 0.001),
              model.means,
              col = "black",
              lwd = 4)
              graphics::lines(seq(0, ceiling(max(
                k, na.rm = TRUE
              )), 0.001),
              model.means,
              col = "orange",
              lwd = 2)
              graphics::legend(
                "topright",
                c("Individual fit"),
                border = "white",
                bg = "white",
                pch = c(NA),
                lty = c(1),
                lwd = c(2),
                col = c("orange"),
                box.col = "white"
              )
              graphics::text(
                min(k, na.rm = TRUE),
                340,
                expression(italic("K") * " Range"),
                col = "white",
                pos = 4,
                cex = 1.5
              )
              graphics::text(
                min(k, na.rm = TRUE),
                380,
                paste0("Method: ", methods[i]),
                col = "white",
                pos = 4,
                cex = 1.5
              )
              graphics::box()
            }
          }
          model.ens <-
            data.frame(
              k = seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
              sfd = model.means,
              q025 = quant.0.025,
              q975 = quant.0.975
            )
          model.ens <-
            merge(data.frame(k = seq(
              0, ceiling(max(k, na.rm = TRUE)), 1 / (10 ^ decimals)
            )),
            model.ens,
            by = "k",
            all = T)
          model.ens$sfd <- zoo::na.approx(model.ens$sfd)
          if (i == 1) {
            output.data[["model.ens"]] <- model.ens
          }
          k.round <- round(k, decimals)
          model.zoo <-
            zoo::zoo(model.ens[match(as.character(k.round), as.character(model.ens$k)), ], order.by =
                       zoo::index(k))
          output.data[[paste0("sfd.", methods[i])]] <- model.zoo
        }
        if (df == T) {
          output.data <-
            list(
              data.frame(
                timestamp = as.character(zoo::index(input$max.pd)),
                value = as.numeric(as.character(input$max.pd))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$max.mw)),
                value = as.numeric(as.character(input$max.mw))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$max.dr)),
                value = as.numeric(as.character(input$max.dr))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$max.ed)),
                value = as.numeric(as.character(input$max.ed))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$daily_max.pd)),
                value = as.numeric(as.character(input$daily_max.pd))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$daily_max.mw)),
                value = as.numeric(as.character(input$daily_max.mw))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$daily_max.dr)),
                value = as.numeric(as.character(input$daily_max.dr))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$daily_max.ed)),
                value = as.numeric(as.character(input$daily_max.ed))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$all.pd)),
                value = as.numeric(as.character(input$all.pd))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$all.ed)),
                value = as.numeric(as.character(input$all.ed))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$input)),
                value = as.numeric(as.character(input$input))
              ),
              input$ed.criteria,
              input$methods,
              #data.frame(probe.length=probe.length,sapwood.thickness=sapwood.thickness),
              data.frame(
                timestamp = as.character(zoo::index(input$k.pd)),
                value = as.numeric(as.character(input$k.pd))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$k.mw)),
                value = as.numeric(as.character(input$k.mw))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$k.dr)),
                value = as.numeric(as.character(input$k.dr))
              ),
              data.frame(
                timestamp = as.character(zoo::index(input$k.ed)),
                value = as.numeric(as.character(input$k.ed))
              ),
              data.frame(
                timestamp = as.character(zoo::index(sfd.pd)),
                value = as.numeric(as.character(sfd.pd))
              ),
              data.frame(
                timestamp = as.character(zoo::index(sfd.mw)),
                value = as.numeric(as.character(sfd.mw))
              ),
              data.frame(
                timestamp = as.character(zoo::index(sfd.dr)),
                value = as.numeric(as.character(sfd.dr))
              ),
              data.frame(
                timestamp = as.character(zoo::index(sfd.ed)),
                value = as.numeric(as.character(sfd.ed))
              ),
              model.ens,
              out.param <- NA
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
            "k.ed",
            "sfd.pd",
            "sfd.mw",
            "sfd.dr",
            "sfd.ed",
            "model.ens",
            "out.param"
          )
        }
        return(output.data)
      }
    } else{
      ######
      #p= process (when no a, b or calib are provided)
      if (is.na(a) == TRUE & is.na(b) == TRUE & ncol(calib) == 10) {
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

        #o = output
        output.data <- list(k, sfd.input <- NA, model.ens <- NA, out.param)

        names(output.data) <- c("input",
                                "sfd.input",
                                "model.ens", "out.param")

        #p= process
        sel.calib <-
          calib[which(
            as.character(calib$Genus) %in% as.character(genus) &
              as.character(calib$Species) %in% as.character(species) &
              as.character(calib$Wood.porosity) %in% as.character(wood)
          ), ]

        output.sub <- data.frame(a = NA, b = NA, sd = NA)
        for (m in c(1:length(unique(paste0((sel.calib$Study), (sel.calib$Species)
        ))))) {
          sel.sub <-
            sel.calib[which(as.character(paste0(
              sel.calib$Study, sel.calib$Species
            )) == as.character(unique(paste0((sel.calib$Study), (sel.calib$Species)
            )))[m]), ]
          if (as.character(unique(paste0((sel.calib$Study), (sel.calib$Species)
          )))[m] == "Paudel et al. 2013Prunus persica") {
            model.calib <- stats::nls(SFD ~ a * k ^ b,
                                      start = list(a = 100, b = 1.231),
                                      data = sel.sub)
          } else{
            model.calib <- stats::nls(SFD ~ a * k ^ b,
                                      start = list(a = 42.84, b = 1.231),
                                      data = sel.sub)
          }
          output.sub[m, "a"] <- stats::coef(model.calib)[1]
          output.sub[m, "b"] <- stats::coef(model.calib)[2]
          output.sub[m, "sd"] <- stats::sd(stats::resid(model.calib))
        }

        #o= output
        output.data[["out.param"]] <-
          data.frame(
            n = nrow(output.sub),
            a_mu = base::mean(output.sub$a),
            a_sd = stats::sd(output.sub$a),
            log.a_mu = base::mean(log(output.sub$a)),
            log.a_sd = stats::sd(log(output.sub$a)),
            b_mu = base::mean(output.sub$b),
            b_sd = stats::sd(output.sub$b)
          )

        output.model <-
          data.frame(k = seq(0, ceiling(max(k, na.rm = TRUE)), 0.001), SFD = NA)
        for (m in c(1:length(unique(
          paste0(sel.calib$Study, sel.calib$Species)
        )))) {
          output.model[, m + 1] <-
            output.sub[m, "a"] * seq(0, ceiling(max(k, na.rm = TRUE)), 0.001) ^ output.sub[m, "b"]
        }

        if (length(paste0(unique(
          paste0(sel.calib$Study, sel.calib$Species)
        ))) == 1) {
          model.means <- output.model[, -1]
        } else{
          model.means <- rowMeans(output.model[, -1])
        }
        model.quant.0.025 <- NA
        model.quant.0.975 <- NA
        model.sd <- NA

        for (r in c(1:nrow(output.model))) {
          model.quant.0.025[r] <-
            as.numeric(stats::quantile(output.model[r, -1], probs = c(0.025))[1])
          model.quant.0.975[r] <-
            as.numeric(stats::quantile(output.model[r, -1], probs = c(0.975))[1])
          model.sd[r] <- as.numeric(as.numeric(stats::sd(output.model[r, -1])))
        }
        quant.0.025 <- model.quant.0.025
        quant.0.975 <- model.quant.0.975

        if (length(unique(paste0(
          sel.calib$Study, sel.calib$Species
        ))) == 1) {
          sel.sub <-
            sel.calib[which(as.character(paste0(
              sel.calib$Study, sel.calib$Species
            )) == as.character(unique(paste0((sel.calib$Study), (sel.calib$Species)
            )))[1]), ]

          output.sub <- data.frame(a = NA, b = NA, sd = NA)
          for (n in c(1:100)) {
            sel.n  <-
              sel.sub[sample(c(1:nrow(sel.sub)), nrow(sel.sub), replace = T), ]
            model.n <-
              stats::nls(SFD ~ a * k ^ b,
                         start = list(a = 100, b = 1.231),
                         data = sel.n)
            output.sub[n, "a"] <- stats::coef(model.n)[1]
            output.sub[n, "b"] <- stats::coef(model.n)[2]
            output.sub[n, "sd"] <- stats::sd(stats::resid(model.n))
            if (n == 1) {
              n.mod <-
                as.numeric(stats::predict(model.n, newdat = data.frame(k = seq(
                  0, ceiling(max(k, na.rm = TRUE)), 0.001
                ))))
            } else{
              n.mod <-
                cbind(n.mod, as.numeric(stats::predict(model.n, newdat = data.frame(
                  k = seq(0, ceiling(max(
                    k, na.rm = TRUE
                  )), 0.001)
                ))))
            }
          }
          #o= output
          output.data[["out.param"]] <-
            data.frame(
              n = nrow(output.sub),
              a_mu = base::mean(output.sub$a),
              a_sd = stats::sd(output.sub$a),
              log.a_mu = base::mean(log(output.sub$a)),
              log.a_sd = stats::sd(log(output.sub$a)),
              b_mu = base::mean(output.sub$b),
              b_sd = stats::sd(output.sub$b)
            )


          model.n.0.025 <- NA
          model.n.0.975 <- NA
          for (r in c(1:nrow(n.mod))) {
            model.n.0.025[r] <-
              as.numeric(stats::quantile(as.numeric(n.mod[r, ]), probs = c(0.025))[1])
            model.n.0.975[r] <-
              as.numeric(stats::quantile(as.numeric(n.mod[r, ]), probs = c(0.975))[1])
          }
          quant.0.025 <- model.n.0.025
          quant.0.975 <- model.n.0.975
        }
        if (length(unique(paste0(
          sel.calib$Study, sel.calib$Species
        ))) != 1) {
          #g= graphics
          if (make.plot == TRUE) {
            graphics::plot(
              sel.calib$k,
              sel.calib$SFD,
              ylim = c(0, 400),
              xlim = c(0, ceiling(max(k, na.rm = TRUE))),
              pch = 16,
              xlab = expression(italic("K") * " (-)"),
              ylab = "",
              yaxt = "n",
              col = "lightgrey"
            )
            graphics::mtext(
              side = 2,
              expression(italic("SFD") * " (" * cm ^ 3 * cm ^ -2 * h ^ -1 * ")"),
              padj = -2.5
            )
            graphics::mtext(
              side = 3,
              paste(as.character(wood), collapse = " | "),
              font = 3,
              cex = 1.2,
              padj = -1
            )
            graphics::axis(side = 2, las = 2)
            graphics::polygon(
              c(
                min(k, na.rm = TRUE),
                min(k, na.rm = TRUE),
                max(k, na.rm = TRUE),
                max(k, na.rm = TRUE)
              ),
              c(-1000, 1000, 1000, -1000),
              col = "darkgrey",
              border = "white"
            )
            graphics::points(
              sel.calib$k,
              sel.calib$SFD,
              pch = 16,
              col = "white",
              cex = 1.5
            )
            graphics::points(
              sel.calib$k,
              sel.calib$SFD,
              pch = 16,
              col = "lightgrey",
              cex = 1.2
            )

            for (m in c(1:nrow(output.sub))) {
              graphics::lines(
                seq(0, ceiling(max(
                  k, na.rm = TRUE
                )), 0.001),
                output.sub[m, "a"] * seq(0, ceiling(max(
                  k, na.rm = TRUE
                )), 0.001) ^ output.sub[m, "b"],
                col = "grey",
                lwd = 1,
                lty = 1
              )
            }

            graphics::lines(seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
                            model.means,
                            col = "white",
                            lwd = 5)
            graphics::lines(seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
                            model.means,
                            col = "black",
                            lwd = 4)
            graphics::lines(seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
                            model.means,
                            col = "orange",
                            lwd = 2)
            graphics::lines(
              seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
              quant.0.025,
              col = "black",
              lwd = 2,
              lty = 2
            )
            graphics::lines(
              seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
              quant.0.975,
              col = "black",
              lwd = 2,
              lty = 2
            )
            graphics::legend(
              "topright",
              c(
                "Raw data",
                "Individual fit",
                expression(mu * " fit"),
                "95% CI"
              ),
              border = "white",
              bg = "white",
              pch = c(16, NA, NA, NA),
              lty = c(NA, 1, 1, 2),
              lwd = c(NA, 1, 3, 2),
              col = c("lightgrey", "grey", "orange", "black"),
              box.col = "white"
            )
            graphics::text(
              min(k, na.rm = TRUE),
              340,
              expression(italic("K") * " Range"),
              col = "white",
              pos = 4,
              cex = 1.5
            )
            graphics::box()
          }
        } else{
          if (make.plot == TRUE) {
            graphics::plot(
              sel.calib$k,
              sel.calib$SFD,
              ylim = c(0, 400),
              xlim = c(0, ceiling(max(k, na.rm = TRUE))),
              pch = 16,
              xlab = expression(italic("K") * " (-)"),
              ylab = "",
              yaxt = "n",
              col = "lightgrey"
            )
            graphics::mtext(
              side = 3,
              unique(paste0(
                sel.calib$Study, " | ", sel.calib$Species
              ))[m],
              font = 3,
              cex = 1.2,
              padj = -1
            )
            graphics::mtext(
              side = 2,
              expression(italic("SFD") * " (" * cm ^ 3 * cm ^ -2 * h ^ -1 * ")"),
              padj = -2.5
            )
            graphics::axis(side = 2, las = 2)
            graphics::polygon(
              c(
                min(k, na.rm = TRUE),
                min(k, na.rm = TRUE),
                max(k, na.rm = TRUE),
                max(k, na.rm = TRUE)
              ),
              c(-1000, 1000, 1000, -1000),
              col = "darkgrey",
              border = "white"
            )
            graphics::points(
              sel.calib$k,
              sel.calib$SFD,
              pch = 16,
              col = "white",
              cex = 1.8
            )
            graphics::points(
              sel.calib$k,
              sel.calib$SFD,
              pch = 16,
              col = "lightgrey",
              cex = 1.2
            )
            for (m in c(1:nrow(output.sub))) {
              graphics::lines(
                seq(0, ceiling(max(
                  k, na.rm = TRUE
                )), 0.001),
                output.sub[m, "a"] * seq(0, ceiling(max(
                  k, na.rm = TRUE
                )), 0.001) ^ output.sub[m, "b"],
                col = "grey",
                lwd = 1,
                lty = 1
              )
            }
            graphics::lines(
              seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
              quant.0.025,
              col = "black",
              lwd = 2,
              lty = 2
            )
            graphics::lines(
              seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
              quant.0.975,
              col = "black",
              lwd = 2,
              lty = 2
            )
            graphics::lines(seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
                            model.means,
                            col = "white",
                            lwd = 5)
            graphics::lines(seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
                            model.means,
                            col = "black",
                            lwd = 4)
            graphics::lines(seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
                            model.means,
                            col = "orange",
                            lwd = 2)
            graphics::legend(
              "topright",
              c(
                "Raw data",
                "Individual fit",
                expression("Fit"),
                "95% CI"
              ),
              border = "white",
              bg = "white",
              pch = c(16, NA, NA, NA),
              lty = c(NA, 1, 1, 2),
              lwd = c(NA, 1, 3, 2),
              col = c("lightgrey", "grey", "orange", "black"),
              box.col = "white"
            )
            graphics::text(
              min(k, na.rm = TRUE),
              340,
              expression(italic("K") * " Range"),
              col = "white",
              pos = 4,
              cex = 1.5
            )
            graphics::box()
          }
        }

        #o= output
        model.ens <-
          data.frame(
            k = seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
            sfd = model.means,
            q025 = quant.0.025,
            q975 = quant.0.975
          )
        model.ens <-
          merge(data.frame(k = seq(0, ceiling(
            max(k, na.rm = TRUE)
          ), 1 / (10 ^ decimals))),
          model.ens,
          by = "k",
          all = T)
        model.ens$sfd <- zoo::na.approx(model.ens$sfd)
        model.ens$q025 <- zoo::na.approx(model.ens$q025)
        model.ens$q975 <- zoo::na.approx(model.ens$q975)
        if (i == 1) {
          output.data[["model.ens"]] <- model.ens
        }
        k.round <- round(k, decimals)
        model.zoo <-
          zoo::zoo(model.ens[match(as.character(k.round), as.character(model.ens$k)), ], order.by =
                     zoo::index(k))
        output.data[[paste0("sfd.input")]] <- model.zoo

        if (df == T) {
          output.data <-
            list(
              data.frame(
                timestamp = as.character(zoo::index(k)),
                value = as.numeric(as.character(k))
              ),
              data.frame(
                timestamp = as.character(zoo::index(sfd.input)),
                value = as.numeric(as.character(sfd.input))
              ),
              model.ens,
              out.param
            )
          names(output.data) <- c("input", "sfd.input",
                                  "model.ens", "out.param")
        }
        return(output.data)
      }

      if (length(which(colnames(calib)[c(1, 2)] %in% c("K", "SFD")))) {
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

        #e=
        if (is.numeric(calib$K) == F)
          stop("Invalid input data, K in calib is not numeric.")
        if (is.numeric(calib$SFD) == F)
          stop("Invalid input data, SFD in calib is not numeric.")

        #o=
        output.data <- list(k, sfd.input <- NA, model.ens <- NA)

        names(output.data) <- c("input", "sfd.input",
                                "model.ens")

        #p=
        sel.sub <- calib

        model.calib <-
          stats::nls(SFD ~ a * K ^ b,
                     start = list(a = 100, b = 1.231),
                     data = sel.sub)
        model.means <-
          stats::predict(model.calib, newdata = data.frame(K = data.frame(K = seq(
            0, ceiling(max(k, na.rm = TRUE)), 0.001
          ))))

        output.sub <- data.frame(a = NA, b = NA, sd = NA)
        for (n in c(1:100)) {
          sel.n  <-
            sel.sub[sample(c(1:nrow(sel.sub)), nrow(sel.sub), replace = T), ]
          model.n <- stats::nls(SFD ~ a * k ^ b,
                                start = list(a = 100, b = 1.231),
                                data = sel.n)
          output.sub[n, "a"] <- stats::coef(model.n)[1]
          output.sub[n, "b"] <- stats::coef(model.n)[2]
          output.sub[n, "sd"] <- stats::sd(stats::resid(model.n))
          if (n == 1) {
            n.mod <-
              as.numeric(stats::predict(model.n, newdat = data.frame(k = seq(
                0, ceiling(max(k, na.rm = TRUE)), 0.001
              ))))
          } else{
            n.mod <-
              cbind(n.mod, as.numeric(stats::predict(model.n, newdat = data.frame(
                k = seq(0, ceiling(max(k, na.rm = TRUE)), 0.001)
              ))))
          }
        }
        #o= output
        output.data[["out.param"]] <-
          data.frame(
            n = nrow(output.sub),
            a_mu = base::mean(output.sub$a),
            a_sd = stats::sd(output.sub$a),
            log.a_mu = base::mean(log(output.sub$a)),
            log.a_sd = stats::sd(log(output.sub$a)),
            b_mu = base::mean(output.sub$b),
            b_sd = stats::sd(output.sub$b)
          )

        model.n.0.025 <- NA
        model.n.0.975 <- NA
        for (r in c(1:nrow(n.mod))) {
          model.n.0.025[r] <-
            as.numeric(stats::quantile(as.numeric(n.mod[r, ]), probs = c(0.025))[1])
          model.n.0.975[r] <-
            as.numeric(stats::quantile(as.numeric(n.mod[r, ]), probs = c(0.975))[1])
        }
        quant.0.025 <- model.n.0.025
        quant.0.975 <- model.n.0.975

        if (make.plot == TRUE) {
          graphics::plot(
            calib$K,
            calib$SFD,
            ylim = c(0, 400),
            xlim = c(0, ceiling(max(k, na.rm = TRUE))),
            pch = 16,
            xlab = expression(italic("K") * " (-)"),
            ylab = "",
            yaxt = "n",
            col = "lightgrey"
          )
          graphics::mtext(
            side = 3,
            "Calibration data",
            font = 3,
            cex = 1.2,
            padj = -1
          )
          graphics::mtext(
            side = 2,
            expression(italic("SFD") * " (" * cm ^ 3 * cm ^ -2 * h ^ -1 * ")"),
            padj = -2.5
          )
          graphics::axis(side = 2, las = 2)
          graphics::polygon(
            c(
              min(k, na.rm = TRUE),
              min(k, na.rm = TRUE),
              max(k, na.rm = TRUE),
              max(k, na.rm = TRUE)
            ),
            c(-1000, 1000, 1000, -1000),
            col = "darkgrey",
            border = "white"
          )
          graphics::points(
            calib$K,
            calib$SFD,
            pch = 16,
            col = "white",
            cex = 1.5
          )
          graphics::points(
            calib$K,
            calib$SFD,
            pch = 16,
            col = "lightgrey",
            cex = 1.2
          )
          graphics::lines(
            seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
            quant.0.025,
            col = "black",
            lwd = 2,
            lty = 2
          )
          graphics::lines(
            seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
            quant.0.975,
            col = "black",
            lwd = 2,
            lty = 2
          )
          graphics::lines(seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
                          model.means,
                          col = "white",
                          lwd = 5)
          graphics::lines(seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
                          model.means,
                          col = "black",
                          lwd = 4)
          graphics::lines(seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
                          model.means,
                          col = "orange",
                          lwd = 2)
          graphics::legend(
            "topright",
            c(
              "Raw data",
              "Individual fit",
              expression("Fit"),
              "95% CI"
            ),
            border = "white",
            bg = "white",
            pch = c(16, NA, NA, NA),
            lty = c(NA, 1, 1, 2),
            lwd = c(NA, 1, 3, 2),
            col = c("lightgrey", "grey", "orange", "black"),
            box.col = "white"
          )
          graphics::text(
            min(k, na.rm = TRUE),
            340,
            expression(italic("K") * " Range"),
            col = "white",
            pos = 4,
            cex = 1.5
          )
          graphics::box()
        }
        #o= output
        model.ens <-
          data.frame(
            k = seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
            sfd = model.means,
            q025 = quant.0.025,
            q975 = quant.0.975
          )
        model.ens <-
          merge(data.frame(k = seq(0, ceiling(
            max(k, na.rm = TRUE)
          ), 1 / (10 ^ decimals))),
          model.ens,
          by = "k",
          all = T)
        model.ens$sfd <- zoo::na.approx(model.ens$sfd)
        model.ens$q025 <- zoo::na.approx(model.ens$q025)
        model.ens$q975 <- zoo::na.approx(model.ens$q975)
        if (i == 1) {
          output.data[["model.ens"]] <- model.ens
        }
        k.round <- round(k, decimals)
        model.zoo <-
          zoo::zoo(model.ens[match(as.character(k.round), as.character(model.ens$k)), ], order.by =
                     zoo::index(k))
        output.data[[paste0("sfd.input")]] <- model.zoo


        if (df == T) {
          output.data <-
            list(
              data.frame(
                timestamp = as.character(zoo::index(k)),
                value = as.numeric(as.character(k))
              ),
              data.frame(
                timestamp = as.character(zoo::index(sfd.input)),
                value = as.numeric(as.character(sfd.input))
              ),
              model.ens,
              out.param
            )
          names(output.data) <- c("input", "sfd.input",
                                  "model.ens", "out.param")
        }

        return(output.data)
      }

      if (is.na(a) == F & is.na(b) == F) {
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

        #e=
        if (is.numeric(a) == F)
          stop("Invalid input data, a is not numeric.")
        if (is.numeric(b) == F)
          stop("Invalid input data, b is not numeric.")

        #o=
        output.data <- list(k, sfd.input <-
                              NA, model.ens <- NA, out.param <- NA)
        names(output.data) <- c("input", "sfd.input",
                                "model.ens", "out.param")

        model.means <- a * seq(0, ceiling(max(k, na.rm = TRUE)), 0.001) ^
          b
        quant.0.025 <- rep(NA, length(model.means))
        quant.0.975 <- rep(NA, length(model.means))


        if (make.plot == TRUE) {
          graphics::plot(
            1,
            1,
            ylim = c(0, 400),
            xlim = c(0, ceiling(max(k, na.rm = TRUE))),
            pch = 16,
            xlab = expression(italic("K") * " (-)"),
            ylab = "",
            yaxt = "n",
            col = "white"
          )
          graphics::mtext(
            side = 3,
            "Calibration curve",
            font = 3,
            cex = 1.2,
            padj = -1
          )
          graphics::mtext(
            side = 2,
            expression(italic("SFD") * " (" * cm ^ 3 * cm ^ -2 * h ^ -1 * ")"),
            padj = -2.5
          )
          graphics::axis(side = 2, las = 2)
          graphics::polygon(
            c(
              min(k, na.rm = TRUE),
              min(k, na.rm = TRUE),
              max(k, na.rm = TRUE),
              max(k, na.rm = TRUE)
            ),
            c(-1000, 1000, 1000, -1000),
            col = "darkgrey",
            border = "white"
          )
          graphics::lines(seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
                          model.means,
                          col = "white",
                          lwd = 5)
          graphics::lines(seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
                          model.means,
                          col = "black",
                          lwd = 4)
          graphics::lines(seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
                          model.means,
                          col = "orange",
                          lwd = 2)
          graphics::legend(
            "topright",
            c("Individual fit"),
            border = "white",
            bg = "white",
            pch = c(NA),
            lty = c(1),
            lwd = c(2),
            col = c("orange"),
            box.col = "white"
          )
          graphics::text(
            min(k, na.rm = TRUE),
            340,
            expression(italic("K") * " Range"),
            col = "white",
            pos = 4,
            cex = 1.5
          )
          graphics::box()
        }
        model.ens <-
          data.frame(
            k = seq(0, ceiling(max(k, na.rm = TRUE)), 0.001),
            sfd = model.means,
            q025 = quant.0.025,
            q975 = quant.0.975
          )
        model.ens <-
          merge(data.frame(k = seq(0, ceiling(
            max(k, na.rm = TRUE)
          ), 1 / (10 ^ decimals))),
          model.ens,
          by = "k",
          all = T)
        model.ens$sfd <- zoo::na.approx(model.ens$sfd)
        if (i == 1) {
          output.data[["model.ens"]] <- model.ens
        }
        k.round <- round(k, decimals)
        model.zoo <-
          zoo::zoo(model.ens[match(as.character(k.round), as.character(model.ens$k)), ], order.by =
                     zoo::index(k))
        output.data[[paste0("sfd.input")]] <- model.zoo

        if (df == T) {
          output.data <-
            list(
              data.frame(
                timestamp = as.character(zoo::index(k)),
                value = as.numeric(as.character(k))
              ),
              data.frame(
                timestamp = as.character(zoo::index(sfd.input)),
                value = as.numeric(as.character(sfd.input))
              ),
              model.ens,
              out.param <- NA
            )
          names(output.data) <- c("input", "sfd.input",
                                  "model.ens", "out.param")
        }
        return(output.data)
      }
    }
  }
