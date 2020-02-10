
#' Gap filling by linear interpolation
#'
#' @description Fills gaps by performing a linear interpolation between observations.
#' This function provides the option to define the minimum size under which gaps should
#' be filled, using linear interpolation.
#'
#' @usage gap.fill(input, max.gap = 60, decimals = 10, df = FALSE)
#'
#' @param input an \code{\link{is.trex}}-compliant object.
#' @param max.gap Numeric value providing the maximum size of a gap in minutes,
#' which can be filled by performing a linear interpolation.
#' @param decimals Integer value defining the number of decimals of the output (default = 10).
#' @param df Logical; if \code{TRUE}, output is provided in a \code{data.frame}
#' format with a timestamp and a value (\eqn{\Delta T} or \eqn{\Delta V}) column.
#' If \code{FALSE}, output is provided as a \code{zoo} object (default = FALSE).
#'
#' @return A \code{zoo} object or \code{data.frame} in the appropriate format for further processing.
#' @export
#'
#' @examples
#' # fill two hour gaps
#' raw   <- example.data(type = "doy")
#' input <-
#'   is.trex(
#'     raw,
#'     tz = "GMT",
#'     time.format = "%H:%M",
#'     solar.time = TRUE,
#'     long.deg = 7.7459,
#'     ref.add = FALSE,
#'     df = FALSE)
#'
#' # create gaps in data
#' input[which(input < 0.4 | input > 0.82)] <- NA
#' fill_120 <- gap.fill(
#'   input = input,
#'   max.gap = 120,
#'   decimals = 10,
#'   df = FALSE)
#' fill_15 <- gap.fill(
#'   input = input,
#'   max.gap = 15,
#'   decimals = 10,
#'   df = FALSE)
gap.fill <- function(input,
                     max.gap = 60,
                     decimals = 10,
                     df = FALSE) {
  #t= test



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

  #d= default conditions
  if (missing(max.gap)) {
    max.gap <- 60
  }
  if (missing(decimals)) {
    decimals <- 10
  }
  if (missing(df)) {
    df = F
  }
  if (df != T &
      df != F)
    stop("Unused argument, df needs to be TRUE|FALSE.")

  #e= errors
  if (zoo::is.zoo(input) == F)
    stop(
      "Invalid input data, use a zoo file from is.trex or a zoo vector containing numeric values (tz= UTC)."
    )
  if (is.numeric(input) == F)
    stop("Invalid input data, values within the vector are not numeric.")
  if (is.numeric(max.gap) == F)
    stop("Unused argument, max.gap is not numeric.")
  if (is.numeric(decimals) == F)
    stop("Unused argument, decimals is not numeric.")
  if (decimals < 3 |
      decimals > 15)
    stop("Unused argument, decimals can only fall between 3-15.")

  #p
  ts.start <-
    as.POSIXct(as.character(zoo::index(input)[1]),
               format = "%Y-%m-%d %H:%M:%S",
               tz = "UTC") - 1
  ts.end <-
    as.POSIXct(as.character(zoo::index(input)[length(input)]),
               format = "%Y-%m-%d %H:%M:%S",
               tz = "UTC") + 1

  #p
  value <-
    stats::na.omit(stats::window(input, start = ts.start, end = ts.end))
  raw.gap <-
    as.numeric(difftime(
      zoo::index(value)[-1],
      zoo::index(value[-length(value)]),
      tz = "UTC",
      units = c("mins")
    ))
  gap <- c(raw.gap, NA) #minimum gap in minutes

  #d
  if (missing(max.gap)) {
    max.gap <- stats::median(raw.gap)
  }

  #e
  if (min(gap, na.rm = TRUE) > max.gap)
    stop("Unused argument, min.gap is smaller the minimum timestep.")

  #p
  gap <- zoo::zoo(gap, order.by = zoo::index(value))
  dummy <-
    zoo::zoo(NA, order.by = seq(
      from = ts.start + 1,
      to = ts.end - 1,
      by = (60 * stats::median(raw.gap))
    )) #*time.int
  proc.1 <- zoo::cbind.zoo(value, gap, dummy)
  proc.1$value[3] <- NA
  proc.1$gap[3] <- 30
  proc.1[which(is.na(proc.1$value) == F), "dummy"] <- 0
  proc.1$value <- zoo::na.approx(proc.1$value, na.rm = F)
  proc.1$gap <- zoo::na.locf(proc.1$gap, na.rm = F)
  proc.1[which(is.na(proc.1$value) == TRUE), "gap"] <- NA
  proc.1[which(proc.1$dummy == 0), "gap"] <- 0

  #p
  proc.1$value <-
    zoo::na.locf(zoo::na.locf(proc.1$value, na.rm = F), fromLast = TRUE)
  proc.1[which(proc.1$gap > max.gap), "value"] <- NA
  proc.1$value <- round(proc.1$value, decimals)

  #o= output
  time.int <- stats::median(raw.gap)
  output.data <- proc.1[which(as.character(zoo::index(proc.1)) %in% as.character(seq(
    from = ts.start + 1,
    to = ts.end - 1,
    by = (60 * time.int)
  ))), "value"]

  if (df == T) {
    output.data <-
      data.frame(timestamp = as.character(zoo::index(output.data)),
                 value = as.numeric(as.character(output.data)))
    output.data$timestamp <- as.character(output.data$timestamp)
    output.data$value <- as.numeric(output.data$value)
  }
  return(output.data)
}
