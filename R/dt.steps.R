#' Determine temporal resolution
#'
#' @description Performs minimum time step standardization,
#' gap filling and start/end time selection. This function
#' provides the option to select the minimum temporal step size of an
#' \code{\link{is.trex}} object. Additionally, the user can define the
#' start and end time of the series and select the minimum size under
#' which gaps should be filled, using linear interpolation.
#'
#' @usage dt.steps(input, start,
#'        end, time.int = 10, max.gap = 60,
#'        decimals = 10, df = FALSE)
#'
#' @param input An \code{\link{is.trex}}-compliant (output) object
#' @param start Character string providing the start time for the series.
#'  Format has to be provided in "UTC" (e.g., "2012-05-28 00:00" or
#'  Year-Month-Day Hour:Minute). Starting time should not be earlier
#'  than the start of the series.
#' @param end Character string providing the start time for the series.
#'  Format has to be provided in "UTC" (e.g., "2012-06-28 00:50" or
#'  Year-Month-Day Hour:Minute). End time should be earlier than
#'  the end time and not later than that of the series.
#' @param time.int Numeric value providing the number of minutes for the
#'  minimum time step. When \code{time.int} is smaller than the minimum time step
#'  of the series, a linear interpolation is applied. If \code{time.int} is
#'  larger than the minimum time step of the series values are averaged to the chosen
#'  value of \code{time.int} (after performing a linear interpolation
#'   to obtain a one-minute resolution).
#' @param max.gap Numeric value providing the maximum size of a gap in minutes,
#'  which can be filled by performing a linear interpolation.
#' @param decimals Integer value defining the number of decimals of the output
#'  (default = 10).
#' @param df Logical; if \code{TRUE}, output is provided in a \code{data.frame}
#'  format with a timestamp and a value (\eqn{\Delta T} or \eqn{\Delta V}) column.
#'  If \code{FALSE}, output is provided as a \code{zoo} object (default = FALSE).
#'
#' @description Time series have different temporal resolutions.
#' This function provides the option to standardize the minimum time step by
#' either performing a linear interpolation when the requested time step
#' is smaller than the minimum time step of the series or average values when
#' the requested time step is larger than the minimum time step of the series.
#' Before this process, the entire time series is converted to a one-minute time
#' step by applying a linear interpolation (excluding gap \eqn{periods > \code{max.gap}}).
#'
#' @return A \code{zoo} object or \code{data.frame} in the appropriate
#' format for further processing.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' input <- is.trex(example.data(type="doy"),
#'            tz="GMT",time.format="%H:%M", solar.time=TRUE,
#'            long.deg=7.7459,ref.add=FALSE)
#' in.ts <- dt.steps(input=input,start='2012-06-28 00:00',end='2012-07-28 00:00',
#'                    time.int=60,max.gap=120,decimals=6,df=FALSE)
#' plot(in.ts)
#' head(in.ts)
#' }
dt.steps <-
    function(input,
             start,
             end,
             time.int = 10,
             max.gap = 60,
             decimals = 10,
             df = FALSE) {

        #test
        #input=add1
        #time.int=60
        #max.gap=120
        #decimals=10
        #df=FALSE
        #length(input)
        #tail(input)
        #start=as.character(zoo::index(head(t, 1)))
        #end=as.character(zoo::index(tail(t, 1)))
        #time.int=15
        #max.gap=15
        #decimals=10
        #df=FALSE
        #tz="UTC"

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
        if (missing(start)) {
            start = as.character(zoo::index(input))[1]
        }
        if (missing(end)) {
            end = as.character(zoo::index(input))[length(input)]
        }
        if (missing(time.int)) {
            time.int <- 15
        }
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
        if (is.character(start) == F)
            stop("Unused argument, start is not a character (format= %Y-%m-%d %H:%M:%S).")
        if (is.character(end) == F)
            stop("Unused argument, end is not a character (format= %Y-%m-%d %H:%M:%S).")
        if (is.numeric(max.gap) == F)
            stop("Unused argument, max.gap is not numeric.")
        if (is.numeric(time.int) == F)
            stop("Unused argument, time.int is not numeric.")
        if (is.numeric(decimals) == F)
            stop("Unused argument, decimals is not numeric.")
        if (decimals < 3 |
            decimals > 15)
            stop("Unused argument, decimals can only fall between 3-15.")

        if (nchar(start)==16){start<-base::paste0(start,":00")}
        if (nchar(end)==16){end<-base::paste0(end,":00")}

        #p
        ts.start <-
            as.POSIXct(as.character(base::paste0(start)),
                       format = "%Y-%m-%d %H:%M:%S",
                       tz = "UTC") - 1
        ts.end <-
            as.POSIXct(as.character(base::paste0(end)),
                       format = "%Y-%m-%d %H:%M:%S",
                       tz = "UTC") + 1

        #e
        if (is.na(ts.start) == TRUE)
            stop("Unused argument, start is not in the correct format (%Y-%m-%d %H:%M:%S).")
        if (is.na(ts.end) == TRUE)
            stop("Unused argument, end is not in the correct format (%Y-%m-%d %H:%M:%S).")
        if (round(as.numeric(ts.start - zoo::index(input[1]))) < -1)
            stop("Unused argument, start is earlier than start of the timestamp.")
        if (round(as.numeric(zoo::index(input[length(input)]) - ts.end)) < -1)
            stop("Unused argument, end is later than end of the timestamp.")

        #p
        value <-
            stats::na.omit(stats::window(input, start = ts.start, end = ts.end))
        value <- (stats::na.omit(value))
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
            max.gap <- min(raw.gap)
        }

        #e
        if (min(gap, na.rm = TRUE) > max.gap)
            stop("Unused argument, min.gap is smaller the minimum timestep.")

        #w= warnings
        if (time.int > (60 * 24)) {
            warning("Selected time.int is larger than a day.")
        }

        #c
        if (time.int > min(gap, na.rm = TRUE)) {
            #p
            gap <- zoo::zoo(gap, order.by = zoo::index(value))
            dummy <-
                zoo::zoo(NA, order.by = seq(
                    from = ts.start + 1,
                    to = ts.end - 1,
                    by = (60 * 1)
                )) #*time.int
            proc.1 <- zoo::cbind.zoo(value, gap, dummy)
            proc.1[which(is.na(proc.1$value) == F), "dummy"] <- 0
            proc.1$value <- zoo::na.approx(proc.1$value, na.rm = F)
            proc.1$gap <- zoo::na.locf(proc.1$gap, na.rm = F)
            proc.1[which(is.na(proc.1$value) == TRUE), "gap"] <- NA
            proc.1[which(proc.1$dummy == 0), "gap"] <- 0
            add <-
                zoo::rollapply(
                    proc.1$value,
                    time.int,
                    align = "center",
                    FUN = mean,
                    na.rm = TRUE,
                    partial = TRUE
                )
            add[which(as.character(add) == "NaN")] <- NA
            proc.1$value <- add
        }else{
            #p
            gap <- zoo::zoo(gap, order.by = zoo::index(value))
            dummy <-
                zoo::zoo(NA, order.by = seq(
                    from = ts.start + 1,
                    to = ts.end - 1,
                    by = (60 * time.int)
                ))
            proc.1 <- zoo::cbind.zoo(value, gap, dummy)

            proc.1[which(is.na(proc.1$value) == F), "dummy"] <- 0
            proc.1$value <- zoo::na.approx(proc.1$value, na.rm = F)
            proc.1$gap <- zoo::na.locf(proc.1$gap, na.rm = F)
            proc.1[which(is.na(proc.1$value) == TRUE), "gap"] <- NA
            proc.1[which(proc.1$dummy == 0), "gap"] <- 0
        }

        #p
        proc.1$value <-
            zoo::na.locf(zoo::na.locf(proc.1$value, na.rm = F), fromLast = TRUE)
        proc.1[which(proc.1$gap > max.gap), "value"] <- NA
        proc.1$value <- round(proc.1$value, decimals)

        #o= output
        output.data <- proc.1[which(as.character(zoo::index(proc.1)) %in% as.character(seq(
            from = ts.start + 1,
            to = ts.end - 1,
            by = (60 * time.int)
        ))), "value"]
        length(output.data)
        #remove values outside of the range
        start.input<-zoo::index(na.omit(input))[1]-1
        start.output<-zoo::index(na.omit(output.data))[1]
        if(start.input!=start.output){
            window(output.data,start=start.output,end=start.input)<-NA
        }

        end.input<-zoo::index(na.omit(input))[length(zoo::index(na.omit(input)))]+1
        end.output<-zoo::index(na.omit(output.data))[length(zoo::index(na.omit(output.data)))]
        if(end.input!=end.output){
            window(output.data,start=end.input,end=end.output)<-NA
        }

        if (df == T) {
            output.data <-
                data.frame(timestamp = as.character(zoo::index(output.data)),
                           value = as.numeric(as.character(output.data)))
            output.data$timestamp <- as.character(output.data$timestamp)
            output.data$value <- as.numeric(output.data$value)
        }
        return(output.data)
    }
