#' Testing and preparing input data
#'
#' @param data A \code{data.frame} in either timestamp format or doy format.
#' @param tz Character string, indicates the time zone in which the measurements have been recorded.
#' @param time.format Character string, indicates the format of the timestamp.
#' @param solar.time Logical; if \code{TRUE}, time is converted to solar time, depending upon the location where the measurements have been taken. If \code{FALSE}, the output is provided in "UTC" (default = \code{FALSE}).
#' @param long.deg Numeric, longitude in decimal degrees East to perform the solar time conversion. Only required when \code{solar.time=TRUE}.
#' @param ref.add Logical; if \code{TRUE}, additional probes provided within data are considered. The \eqn{\Delta T} values are then corrected by subtracting the \eqn{\Delta T} measured between the reference probes from the \eqn{\Delta} T measured between the heated and unheated probe (default = \code{FALSE}).
#' @param df Logical; if \code{TRUE}, output is provided in a data.frame format with a timestamp and a value (\eqn{\Delta T} or \eqn{\Delta V}) column. If \code{FALSE}, output is provided as a zoo vector object (default = \code{FALSE}).
#'
#'
#' @description Testing if the structure of the input matches the requirements of the TREX functions
#'  and specify the time zone. The input has to be presented in one of two different \code{data.frame} formats.
#'  i) Timestamp format: including a 1) timestamp of the measurements column <as.character>, and
#'  2) value of \eqn{\Delta}V (or \eqn{\Delta}T) \code{as.numeric}. ii) Doy format: including a 1) year of measurements column \code{as.integer},
#'  2) day of the year (doy) of measurement \emph{as.integer}, 3) hour of the measurement \emph{as.character}, and
#'  4) value of \eqn{\Delta}V (or \eqn{\Delta}T) \code{as.numeric}. TREX functions are applied on time series obtained from a set of
#'  thermal dissipation probes. This includes the option where the thermal dissipation method (TDM) is
#'  used with only a reference and heating probe, or when including addition reference probes (see \code{ref.add}).
#'  These reference probe measurements can be added to the doy or timestamp format in \eqn{\Delta}V (or \eqn{\Delta}T) \code{as.numeric}
#'  labelled \code{ref1}, \code{ref2}, etc. (depending on the number of reference probes). For this function the following
#'  column names have to be present within the \code{data.frame}: "timestamp” or "year” & "doy” & "hour” = indicators
#'  of time and "value” = TDM measurements (option "ref1”, "ref2, …, refn = reference probes).
#'  After specifying the time zone (\code{tz}), one can select whether to standardize the temporal series to solar
#'  time (see \code{solar.time}) by providing the longitude in decimal degrees at which the measurements were
#'  obtained (in \code{long.deg}). All timestamps within the function are rounded to minute resolution and output can
#'  be either provided in a \code{zoo} format (df = \code{FALSE}) or \code{data.frame} (\code{df = TRUE}; default is \code{FALSE}).
#'
#'  @usage is.trex(data, tz = “UTC”, format = “%m/%d/%y %H:%M:%S”,
#'   solar.time = FALSE, long.deg = 7.7459,
#'    ref.add = FALSE, df = FALSE)
#'
#'
#'
#' @details To prevent errors occurring in subsequent \code{TREX} functions, it is advised to run this function
#'  for checking the data structure and preparing it for further analyses. For the specific time zone see
#'  \url{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones} or for formatting see \code{\link{OlsonNames}()}.
#'  The format of the timestamp has to be provided according to \url{https://www.stat.berkeley.edu/~s133/dates.html}.
#'  For the method behind the solar time conversion, see the solar package (\url{https://cran.r-project.org/web/packages/solaR/}).
#'  The longitude has to be provided in positive decimal degrees for study sites East from the Greenwich meridian and negative for sites to the West.
#'
#' @return A zoo object or data.frame in the appropriate format for other functionalities.
#' @export
#'
#' @examples
#' #validating and structuring example data
#' raw   <- example.data(type="doy")
#' input <- is.trex(raw,tz="GMT",time.format="%H:%M",
#'     solar.time=TRUE,long.deg=7.7459,
#'     ref.add=FALSE,df=FALSE)
#' head(raw)
#' str(input)
#' head(input)
#' plot(input)
is.trex <-
  function(data,
           tz = "UTC",
           time.format = "%m/%d/%y %H:%M:%S",
           solar.time = TRUE,
           long.deg = 7.7459,
           ref.add = FALSE,
           df = FALSE) {


    # helpers
    left <-  function(string, char){
      substr(string, 1,char)}

    right <-  function (string, char){
      substr(string,nchar(string)-(char-1),nchar(string))
    }


    #t= test
    #data= example.data(type="timestamp")
    #tz= "GMT"
    #time.format="%H:%M"
    #solar.time=TRUE
    #ref.add=FALSE
    #long.deg=7.7459
    #df= FALSE

    #d= default conditions
    if (missing(tz)) {
      tz = "GMT"
      warning("No timezone specified : Using default setting (= GMT/UTC)")
    }
    if (missing(ref.add)) {
      ref.add = F
    }
    if (missing(time.format)) {
      time.format = "%d-%m-%Y %H:%M"
      warning("No time.format specified : Using default setting (= %d-%m-%Y %H:%M)")
    }
    if (missing(solar.time)) {
      solar.time = F
    }
    if (missing(df)) {
      df = F
    }

    #e= errors
    if (length(which(tz %in% base::OlsonNames())) == 0)
      stop("Unused argument, please use a valid time zone.")
    if (solar.time != T &
        solar.time != F)
      stop("Unused argument, solar.time needs to be TRUE|FALSE.")
    if (ref.add != T &
        ref.add != F)
      stop("Unused argument, ref.add needs to be TRUE|FALSE.")
    if (df != T &
        df != F)
      stop("Unused argument, df needs to be TRUE|FALSE.")
    if (length(which(c("timestamp", "doy") %in% colnames(data))) == 0)
      stop("Incorrect data format, no doy|timestamp column present.")
    type = NA
    if (length(which("timestamp" %in% colnames(data))) == 0) {
      type = "doy"
      if (length(which("year" %in% colnames(data))) == 0)
        stop("Incorrect data format, no year column present.")
      if (length(which("hour" %in% colnames(data))) == 0)
        stop("Incorrect data format, no hour column present.")
      if (length(which("value" %in% colnames(data))) == 0)
        stop("Incorrect data format, no value column present.")
    } else{
      type = "timestamp"
      if (length(which("value" %in% colnames(data))) == 0)
        stop("Incorrect data format, no value column present.")
      if (length(which(nchar(as.character(data$timestamp)) > 21)) != 0) {
        print(which(nchar(as.character(
          data$timestamp
        )) > 21))
        stop("Incorrect data format, timestamp has too many characters in the above given lines.")
      }
    }
    if (ref.add == T) {
      if (length(which("ref" %in% substr(colnames(data), 1, 3))) == 0)
        stop("Incorrect input data, no additional reference probes present.")
    }
    if (is.na(base::suppressWarnings(base::mean(c(data$value), na.rm = TRUE))) ==
        T)
      stop("Incorrect data format, value is not numeric.")
    if (solar.time == T &
        missing(long.deg) == T)
      stop("Missing argument, solar.time needs long.deg in decimal degrees E.")
    if (solar.time == T) {
      if (is.numeric(long.deg) == FALSE |
          long.deg > 180 |
          long.deg < -180)
        stop("Unused argument, long.deg needs to be numeric and between -180:180.")
    }

    #c= conversions
    time.format.orig <- NA
    if (type == "timestamp") {
      data$timestamp <- as.character(data$timestamp)
      time.format.orig <- time.format
    }
    if (type == "doy") {
      #e
      if (is.integer(data$doy) == F)
        stop("Incorrect data format, doy is not integer.")
      if (is.integer(data$year) == F)
        stop("Incorrect data format, year is not integer.")
      data$hour <- as.character(data$hour)
      if ((min(nchar(data$hour)) > 3 &
           max(nchar(data$hour)) < 6) == F) {
        "Incorrect data format, invalid hour column."
      }
      if (min(nchar(data$hour)) == 4) {
        data[which(nchar(data$hour) == 4), "hour"] <-
          base::paste0("0", data[which(nchar(data$hour) == 4), "hour"])
      }
      timestamp <-
        paste(base::as.Date(data$doy - 1, origin = paste0(data$year, "-01-01")),
              data$hour,
              sep = " ")
      time.format.orig <- paste0("%Y-%m-%d ", time.format)
      data$timestamp <- timestamp
      data <- data[, -which(colnames(data) %in% c("year", "doy", "hour"))]
    }

    #w= warnings
    if (length(which(base::duplicated(data$timestamp) == T)) != 0) {
      warning("Double timestamp present, daylight saving could be present within the timestamp.")
    }

    #p= process
    if (solar.time == T) {
      timestamp <-
        suppressWarnings(solaR::local2Solar(
          base::as.POSIXct(
            as.character(data$timestamp),
            format = time.format.orig,
            tz = tz
          ),
          lon = long.deg
        ))
    } else{
      timestamp <-
        chron::as.chron(base::as.POSIXct(
          as.character(data$timestamp),
          format = time.format.orig,
          tz = tz
        ))
    }

    #e
    if (as.character(timestamp[1]) == "(NA NA)" |
        is.na(timestamp[1]) == T)
      stop("No timestamp present, time.format is likely incorrect.")
    if (length(which(base::duplicated(timestamp) == T)) != 0) {
      print(data[which(base::duplicated(timestamp) == T), ])
      stop(
        "Double timestamp present, either due to errors in the timestamp or issues with daylight saving (change tz)."
      )
    }

    #p
    if (ref.add == T) {
      if (length(which(left(colnames(data), 3) == "ref")) == 1) {
        value <- data$value - (data[, which(left(colnames(data), 3) == "ref")])
      }
      if (length(which(left(colnames(data), 3) == "ref")) > 1) {
        value <-
          data$value - (base::rowMeans(data[, which(left(colnames(data), 3) == "ref")]))
      }
      if (length(which(left(colnames(data), 3) == "ref")) == 0) {
        stop("No reference probe measurements, missing ref1, ref2, refn columns.")
      }
      data <- data.frame(timestamp = data$timestamp, value = value)
    }

    #p
    if (length(unique(timestamp)) == length(unique(format(timestamp, "%Y-%m-%d %H:%M")))) {
      output.data <-
        zoo::zoo(data$value, order.by = base::as.POSIXct(format(timestamp, "%Y-%m-%d %H:%M")))
    } else{
      agg <-
        stats::aggregate(output.data, by = format(timestamp, "%Y-%m-%d %H:%M"), mean)
      output.data <- zoo::zoo(agg, order.by = base::as.POSIXct(zoo::index(agg)))
    }

    #o= output
    if (df == T) {
      output.data <-
        data.frame(timestamp = as.character(zoo::index(output.data)),
                   value = as.numeric(as.character(output.data)))
      output.data$timestamp <- as.character(output.data$timestamp)
      output.data$value <- as.numeric(output.data$value)
    }
    return(output.data)
  }
