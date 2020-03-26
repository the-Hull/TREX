#' Data cleaning and outlier detection
#'
#' @description This function launches a \code{Shiny} application that
#' (1) visualizes raw and outlier-free time series interactively
#' (using \code{plotly}),
#' (2) highlights automatically detected outliers,
#' (3) allows the user to revise the automatically detected outliers
#'  and manually include data points, and
#'  (4) exports the original data, the automatically selected outliers,
#'   the manually selected outliers, and the outlier-free time series
#'    in an \code{\link{is.trex}}-compliant object that can be further processed.
#'
#'
#' @details
#' \strong{Note, that due to the interactive nature of the application, the reactive graphs can become
#' rather slow in updating. We hence suggest breaking long-time series into smaller chunks
#' that do not strain the available memory too much. Trial and error is useful here, but we
#' generally suggest working on a maximum of up to one year at a time.}
#' Once the application is launched,
#'  the user can load an \code{.RData} file where a \code{data.frame}
#'   with a imestamp and sensor data (multiple sensor columns are supported).
#'   The timestamp in this \code{data.frame} should be of class \code{POSIXct}.
#'   Users can select the x and y axes of the interactive time series plots.
#'   In addition, the user can provide the units of the imported data
#'   (e.g., degrees \eqn{C} or \eqn{mV} for \eqn{\Delta T} or \eqn{\Delta V}, respectively).
#'   A parameter (alpha) for automatic outlier detection can be supplied.
#'   More specifically, the automatic identification of outliers is based on a
#'   two-step procedure:
#'     i) the Tukey’s method (Tukey, 1977) is applied to detect statistical outliers
#'     as values falling outside the range
#'     \eqn{[q_{0.25} - alpha * IQR, q_{0.75} + alpha * IQR]}{[q0.25 - alpha * IQR, q0.75 + alpha * IQR]},
#'     where \eqn{IQR} is the interquartile range
#'     (\eqn{q_{0.75} - q_{0.25}}{q0.75 - q0.25})
#'     with \eqn{q_{0.25}}{q0.25} denoting the 25\% lower quartile and \eqn{q_{0.75}}{q0.25} the
#'     75\% upper quartile, and alpha is a user-defined parameter
#'     (default value \code{alpha = 3};
#'     although visual inspection through the interactive plots allows for adjusting
#'     alpha and optimizing the automatic detection of outliers),
#'     and ii) the lag-1 differences of the raw data are calculated
#'     and data points with lag-1 differences greater
#'     than the mean of the raw input time series, are excluded.
#'  The raw input data from the provided \code{.RData} file are depicted with
#'  black points in the first plot titled ‘Raw and automatic detection’
#'  while the automatically detected outliers are also highlighted in this plot in red.
#'  The user can adjust the parameter \code{alpha} and visually inspect the
#'  automatically detected outliers in order to achieve the optimal automatic outlier selection.
#'  This plot allows also interactivity (by hovering the mouse in the upper right corner
#'  the available interactive tools appear, e.g., zoom in/out).
#'  Also, the lower subpanel of this plot provides a better overview of the temporal extent
#'  of the data and allows the user to select narrower time window for a more thorough data inspection.
#'
#'  Once the user is satisfied with the automatically selected data points,
#'  one can proceed to the manual outlier selection.
#'  The second interactive plot (titled ‘Filtered and manual selection’)
#'  presents the raw data after removing the automatically detected outliers of the previous step,
#'   and allows the user to manually select (point, rectangular, and lasso selections are allowed)
#'    data points. The first selection identifies points to be removed (outliers),
#'    and their color changes to red. If a point is selected for a second time,
#'    this will undo its classification as outlier and its color is set back to black (i.e., not an outlier).
#'   The red-color data points correspond to the selected outliers to be removed from the data,
#'   in addition to those identified in the automated detection.
#'
#'
#' @return The function does not return a value,
#' but allows the user to save a \code{list} containing the raw and outlier-free data,
#'  as well as the automatically and manually selected outliers in separate items.
#'   Once the user is satisfied with the selected outliers,
#'    the ‘Download Cleaned Time Series’ button will allow to export this \code{list} as a "\code{.Rds}"
#'    file. This file can be subsequently assigned to an object using \code{\link{readRDS}}.
#'  The list contained in this file is called \code{trex_outlier_output} and has four \code{data.frames},
#'   namely \code{series_input} with the raw data, \code{select_auto} with
#'   the automatically selected outliers, \code{select_manual} with the manually selected outliers,
#'    and \code{series_cleaned} with the outlier-free time series.
#'   Each of these data frames has a column with the timestamp and a column for the sensor values.

#' @export
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{
#' # find example file path
#' system.file("exdata", "example.RData", package = "TREX", mustWork = TRUE)
#' # either copy-paste this into the navigation bar of the file selection window
#' # or navigate here manually for selection
#'
#' # launch shiny application
#' outlier()
#'
#' # after saving the output, run e.g.:
#'
#' my_cleaned_data <- readRDS("./cleaned_file.Rds")
#'
#' ## With full workflow:
#'
#' # get an example time series
#' raw   <- example.data(type="doy")
#' input <- is.trex(raw, tz="GMT", time.format="%H:%M",
#'                  solar.time=TRUE, long.deg=7.7459, ref.add=FALSE, df=FALSE)
#'
#' # clip a period of interest
#' input<-dt.steps(input,time.int=60,start="2014-02-01 00:00",
#'                 end="2014-05-01 00:00",max.gap=180,decimals=15)
#'
#' # organise a data.frame
#' input_df  = data.frame(date = zoo::index(input), data = zoo::coredata(input))
#'
#' # save the RData file to e.g. a temp file, or your project root directory
#'
#' #temp_file_path <- tempfile()
#' # save(input_df, file=temp_file_path)
#'
#' # project_root_path <- "."
#' # save(input_df, file=project_root_path)
#'
#'
#' # call the oulier function and navigate to where the "test.RData" is stored
#' outlier()
#'
#'
#' }
#'
outlier <- function(){



    if (!requireNamespace("shiny", quietly = TRUE)) {
        stop("Package \"shiny\" needed for this function to work. Please install it.",
             call. = FALSE)
    }




    if (!requireNamespace("plotly", quietly = TRUE)) {
        stop("Package \"plotly\" needed for this function to work. Please install it.",
             call. = FALSE)
    }

    # helper
    `%!in%` = Negate(`%in%`)

    # Define UI --------------------------------------------------------
    ui <- shiny::fluidPage(

        # Title
        shiny::titlePanel("TREX: sap flow data cleaning"),

        shiny::flowLayout(
            # Data Input
            shiny::fileInput("file", label = "Input data:"),
            # Select variables/columns from data frame
            shiny::selectInput('timestamp', 'Timestamp', ""),
            shiny::selectInput('sensor_value', 'Sensor Value', ""),
            # Y axis units (deg C or mV)
            shiny::textInput("yunits", label = "Units:", placeholder = "e.g., mV"),
            shiny::textInput("alpha", label = "alpha:", placeholder = "e.g., 3")
        ),

        # Time series plots
        shiny::actionButton(inputId="goButton", label="Plot Time Series"),
        # Data download
        shiny::downloadButton("downloadData", "Download Cleaned Time Series"),

        shiny::actionButton("done", "Done"),


        shiny::br(),

        shiny::h4("Raw and automatic detection"),

        # first plot object
        plotly::plotlyOutput("plotA"),

        shiny::h4("Filtered and manual selection"),

        # second plot object
        plotly::plotlyOutput("plotB"),



        # Data table with selected outliers
        shiny::verbatimTextOutput("click"),
        shiny::verbatimTextOutput("brush"),
        shiny::dataTableOutput('myTable')

    )


    ################################################################################
    ################################################################################
    ################################################################################
    ################################################################################



    # Define server ---------------------------------------------------------
    server <- function(input, output, session){#server

        res_env <- new.env()

        # helper functions --------------------------------------------

        # stat filter
        filter1 = function(data, mult){
            inp=stats::na.omit(data)
            q=stats::quantile(inp, na.rm=T, names = F)
            q_25=q[2]
            q_75=q[4]
            iqr=stats::IQR(inp, na.rm = T)
            low=q_25-(iqr * mult)
            high=(iqr * mult) + q_75
            out = data
            out[out<low | out>high] = NA
            return(out)
        }

        # first diff filter
        filter2 = function(data, lag=1){
            org=data
            #differences
            tt = c(0, diff(org, lag= lag))
            threshold = mean(org, na.rm=T)
            rt= org
            rt[abs(tt) > threshold] = NA
            return(rt)
        }


        # Load data --------------------------------------------------------------------
        dataInput = shiny::reactive({
            if (is.null(input$file)) return(NULL)
            inFile = input$file
            file = inFile$datapath
            # load the file into new environment and get it from there
            e = new.env()
            name = load(file, envir = e)
            data = e[[name]]
        })

        outVar = shiny::reactive({names(dataInput())})
        shiny::observe({shiny::updateSelectInput(session, "timestamp", choices = outVar())})
        shiny::observe({shiny::updateSelectInput(session, "sensor_value", choices = outVar())})
        shiny::observe({shiny::updateTextInput(session, "yunits")})
        shiny::observe({shiny::updateTextInput(session, "alpha")})




        # initiate plotting / filtering action (MAIN ACTION HERE)
        #########################################################
        shiny::observeEvent(input$goButton,{#goButton






            # Define data for plotting  -------------------------------------------------------

            plot_df <- shiny::reactive({



                df = data.frame(x=dataInput()[[input$timestamp]],
                                y=dataInput()[[input$sensor_value]],
                                key=row.names(dataInput()))
                aa = input$alpha
                cln1 = filter1(df$y, mult = as.numeric(aa))
                cln2 = filter2(cln1)
                df$y1 = cln2
                # assign(x = "df", value = df, envir = res_env)

                detected = df[complete.cases(df$y),]
                detected$x = as.character(detected$x)
                # assign(x = "AutoDetect", value = detected[is.na(detected$y1), c("x","y")] , envir = res_env)




                return(list(df = df,
                            AutoDetect = detected[is.na(detected$y1), c("x","y")]))


            })

            # # set overall y-axis range for relayout events
            # not used, but should be implemented for preventing resetting zoom after selection events
            # yRange <- range(plot_df()$df$y1, na.rm = TRUE)






            # Top plot (auto selection) -----------------------------------------------------------------------
            output$plotA = plotly::renderPlotly({





                p = plotly::plot_ly(data=plot_df()$df, x=~x, y=~y) %>%
                    plotly::add_markers(key=~key, color = I("red"), type="scatter",
                                        mode='lines+markers', showlegend = FALSE)

                p <- plotly::add_markers(p, y=~y1, color = I("black"), showlegend = FALSE)

                plotly::layout(p, xaxis = list(title = "",
                                               rangeslider = list(plot_df()$df$x[1],
                                                                  utils::tail(plot_df()$df$x,1))),
                               yaxis = list(title = input$yunits),
                               dragmode = "select",
                               selectdirection = "h",
                               title = "Raw data (detected outliers in red)")

            })




            ##########################################################################
            # point selection events handlers --------------------------------------------------
            # Deals with collecting and handling all events from clicking and dragging



            # monitor values
            was_clicked <- shiny::reactiveVal()
            was_clicked(NULL)

            was_boxed <- shiny::reactiveVal()
            was_boxed(NULL)

            was_dbl_clicked <- shiny::reactiveVal()
            was_dbl_clicked(NULL)





            # BOX / LASSO
            shiny::observeEvent(plotly::event_data("plotly_selected", source = "plotB" ,
                                                   priority = "event"),

                                {
                                    print("triggered area select")

                                    was_boxed(TRUE)
                                    was_clicked(FALSE)
                                    was_dbl_clicked(FALSE)



                                })



            # CLICK
            shiny::observeEvent(plotly::event_data("plotly_click", source = "plotB" ,
                                                   priority = "event"),

                                {
                                    print("triggered click select")

                                    was_boxed(FALSE)
                                    was_clicked(TRUE)
                                    was_dbl_clicked(FALSE)


                                })

            # DBL CLICK for resetting values
            shiny::observeEvent(plotly::event_data("plotly_doubleclick", source = "plotB" ,
                                                   priority = "event"),

                                {
                                    print("triggered dbl click")

                                    was_boxed(NULL)
                                    was_clicked(NULL)
                                    was_dbl_clicked(TRUE)



                                    # Not implemented yet: zoom reset
                                    # also handle re-layout (prevent reset of zoom)
                                    # plotly::plotlyProxy("plotB", session) %>%
                                    #     plotly::plotlyProxyInvoke("relayout", list(yaxis = list(range = yRange)))




                                })




            # Not implemented yet: zoom reset
            # prevent zoom from resetting on all types of events
            # shiny::observeEvent(plotly::event_data("plotly_doubleclick", source = "plotB" ,
            #                    priority = "input"),
            #
            # {
            #
            #     plotly::plotlyProxy("plotB", session) %>%
            #         plotly::plotlyProxyInvoke("relayout", list(yaxis = list(range = yRange)))
            #
            #
            #
            #
            # })





            # Not implemented yet: zoom reset
            # shiny::observeEvent(plotly::event_data("plotly_relayout", source = "plotB"), {
            #
            #     print("triggered re-layout!")
            #     d <- plotly::event_data("plotly_relayout", source = "plotB")
            #
            #
            #
            #     # unfortunately, the data structure emitted is different depending on
            #     # whether the relayout is triggered from the rangeslider or the plot
            #     xmin <- if (length(d[["xaxis.range[0]"]])) d[["xaxis.range[0]"]] else d[["xaxis.range"]][1]
            #     xmax <- if (length(d[["xaxis.range[1]"]])) d[["xaxis.range[1]"]] else d[["xaxis.range"]][2]
            #     if (is.null(xmin) || is.null(xmax)) return(NULL)
            #
            #
            #
            #     # compute the y-range based on the new x-range
            #     # idx <- xmin <= plot_df()$df$x & plot_df()$df$x <= xmax
            #     # yrng <- grDevices::extendrange(plot_df()$df$y1[idx])
            #
            #     tz_data <- lubridate::tz(plot_df()$df$x)
            #
            #
            #
            #     xrng <- grDevices::extendrange(lubridate::ymd_hms(c(xmin,xmax), tz = tz_data))
            #
            #     print(xrng)
            #
            #
            #     plotly::plotlyProxy("plotB", session) %>%
            #         plotly::plotlyProxyInvoke("relayout", list(xaxis = list(range = xrng)))
            # })



            ##########################################################################
            # collect selected values -----------------------------
            # Gathers all selected points from all event types and sets handlers appropriately

            selected_points <- shiny::reactive({







                events <- list(click_select = plotly::event_data("plotly_click", source = "plotB" , priority = "event"),
                               box_select = plotly::event_data("plotly_selected", source = "plotB", priority = "event" ))




                if(!is.null(was_dbl_clicked()) && was_dbl_clicked()) {

                    print("Returned empty event df due to dbl click.\n
                          Reset the selected df to zero-nrows")

                    selected_data_df <<- data.frame()


                    return(NULL)
                }



                # other selection events
                if(!is.null(was_clicked()) && !is.null(was_boxed())){



                    if(was_clicked()){

                        events[["box_select"]] <- NULL

                    } else if(was_boxed()){

                        events[["click_select"]] <- NULL


                    }
                }





                event_data <- do.call(rbind, events)


                # handle output for selection and initialization
                if(!is.null(event_data)) {



                    print("The following data was selected:")
                    print(event_data)


                    return(event_data)
                } else {

                    print("No event data yet.")

                    return(NULL)
                }





            })




            ##########################################################################
            # handle selected data --------------------------------------------------
            # Adjusts selected points (removes duplicates, highlights new points, returns empty frame when
            # all values deselected)


            selected_data_df <- data.frame()



            selected_data_df_reactive <- shiny::reactive({



                selected_points_local <- selected_points()

                if (!is.null(selected_points_local)){


                    # check which points may be duplicates in selection
                    if(nrow(selected_data_df) > 0 &&
                       utils::tail(duplicated(rbind(selected_data_df[ , c("x","y")],
                                             selected_points_local[ ,c("x","y")] ))
                            , 1) > 0){

                        print("Identified duplicates!")

                        selected_data_df_return <- rbind(selected_data_df, selected_points_local)
                        # selected_data_df <<- rbind(selected_data_df, selected_points_local)


                        dup_idcs_lgl <- duplicated(selected_data_df_return[ ,c("x", "y")]) |
                            duplicated(selected_data_df_return[ ,c("x", "y")], fromLast = TRUE)



                        selected_data_df_return <- selected_data_df_return[!dup_idcs_lgl, ]


                        selected_data_df <<- selected_data_df_return





                        # handle when df is reduced to zero
                        if(nrow(selected_data_df) == 0 ){

                            print("Selection df reduced to 0!")
                            return(NULL)
                        }

                        return(selected_data_df_return[ , c("x", "y")])


                        # if no duplicates
                    } else {




                        selected_data_df_return <- rbind(selected_data_df, selected_points_local)
                        selected_data_df <<- rbind(selected_data_df, selected_points_local)
                        print("no duplicates in selection.")

                    }




                    return(selected_data_df_return[ , c("x", "y")])


                } else {

                    print("No selection data yet")

                    return(NULL)
                }

            })

            # reset selected points







            # Plot Manual Select -----------------------------------------------------------------------
            # always plots base filtered data from plot A
            # and overlays red dots when selection has been made
            output$plotB = plotly::renderPlotly({






                p = plotly::plot_ly(data=plot_df()$df,
                                    x = ~x,
                                    y = ~y1,
                                    source = "plotB") %>%
                    plotly::add_markers(color = I("black"),
                                        mode='lines+markers',
                                        showlegend = F) %>%
                    plotly::event_register("plotly_doubleclick") %>%
                    plotly::event_register("plotly_selected") %>%
                    plotly::event_register("plotly_brushed") %>%
                    plotly::event_register("plotly_click")  %>%
                    plotly::layout(xaxis = list(title = ""),
                                   yaxis = list(title = input$yunits),
                                   dragmode = F,
                                   title = "Filtered data")


                # on selection add red dots
                if (!is.null(selected_data_df_reactive())) {



                    p_red <- plotly::add_markers(p,
                                                 data = selected_data_df_reactive(),
                                                 x = ~x,
                                                 y = ~y,
                                                 color = I("red"),
                                                 showlegend = FALSE) %>%
                        plotly::event_register("plotly_doubleclick") %>%
                        plotly::event_register("plotly_selected") %>%
                        plotly::event_register("plotly_brushed") %>%
                        plotly::event_register("plotly_click")


                    return(p_red)




                } else {

                    return(p)

                }


            })












            # Table --------------------------
            # currently not in use
            # output$myTable  <-  DT::renderDataTable({
            #
            #     if(is.null(selected_data_df_reactive())) {
            #
            #         return(NULL)
            #
            #     } else {
            #
            #
            #         return(selected_data_df_reactive())
            #
            #     }
            # })




            # provide filtered data for saving
            cleaned_data <- shiny::reactive({

                time_stamp_auto <- plot_df()$AutoDetect$x



                # need to handle time stamps better
                time_stamp_manual <- paste0(selected_data_df$x, ":00")
                # this fix is not ideal


                all_stamps_to_remove <- c(time_stamp_auto,
                                          time_stamp_manual)







                OriginalData  <-  data.frame(x=dataInput()[[input$timestamp]],
                                             y=dataInput()[[input$sensor_value]])





                filtered_data <- OriginalData[!as.character(OriginalData$x) %in% all_stamps_to_remove, ]
                names(filtered_data) <- c(input$timestamp, input$sensor_value)

                return(filtered_data)


            })



            # deal with download
            output$downloadData <- shiny::downloadHandler(
                filename = paste(strsplit(as.character(input$file),
                                          paste0(".",
                                                 tools::file_ext(as.character(input$file))))[[1]],
                                 "_Cleaned", ".Rds", sep = ""),

                content = function(file){
                    OriginalData = data.frame(timestamp=dataInput()[[input$timestamp]],
                                              sensor_value=dataInput()[[input$sensor_value]])[, 1:2]

                    names(OriginalData) <- c(input$timestamp, input$sensor_value)



                    trex_outlier_output <- list(series_input = OriginalData,
                                                select_auto = plot_df()$AutoDetect,
                                                select_manual = selected_data_df,
                                                series_cleaned = cleaned_data())


                    saveRDS(trex_outlier_output, file = file)
                }

            )

        })#goButton




        shiny::observeEvent(input$done, {

            shiny::stopApp()
        })

    }#server





    ################################################################################
    # Run the application
    shiny::shinyApp(ui = ui, server = server )
    ################################################################################





}
