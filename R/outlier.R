#' Data cleaning and outlier detection
#'
#' @description This function launches a Shiny application that
#'  (1) visualizes raw and outlier-free time series interactively
#'  (using {plotly}),
#'  (2) highlights automatically detected outliers,
#'  (3) allows the user to revise the automatically detected outliers
#'     and manually include data points, and
#'  (4) exports the original data and the outlier-free time series in
#'     an \code{\link{is.trex}}-compliant object that can be further processed.
#'
#'
#' @return Once the application is launched,
#'  the user can load an \code{.RData} file where a \code{data.frame}
#'   with the timestamp and sensor data are stored.
#'   The timestamp in this \code{data.frame} should be of class \code{Date}.
#'   Many columns can be included (corresponding, for example, to different sensors)
#'   and the user can select through the application the x and y axes of the interactive time series plots.
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
#'     and ii) the first difference (or lag-1 differences) of the raw data are calculated
#'     and data points with lag-1 differences greater
#'     than the mean of the raw input time series, are excluded.
#'
#'
#' @return The function does not return a value, but allows the user
#'  to write the raw and outlier-free data in a \code{.RData} file to disk, for
#'  subsequent import via \code{load()}.

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
#' }
#'
outlier <- function(){



    if (!requireNamespace("shiny", quietly = TRUE)) {
        stop("Package \"shiny\" needed for this function to work. Please install it.",
             call. = FALSE)
    }

    if (!requireNamespace("DT", quietly = TRUE)) {
        stop("Package \"DT\" needed for this function to work. Please install it.",
             call. = FALSE)
    }


    if (!requireNamespace("plotly", quietly = TRUE)) {
        stop("Package \"plotly\" needed for this function to work. Please install it.",
             call. = FALSE)
    }

    # helper
    `%!in%` = Negate(`%in%`)

    ################################################################################
    # Define UI
    ui <- shiny::fluidPage(

        # Title
        shiny::titlePanel("TREX: sap flow data cleaning"),

        shiny::flowLayout(
            # Data Input
            shiny::fileInput("file", label = "Input data:"),
            # Select variables/columns from data frame
            shiny::selectInput('xcol', 'X axis', ""),
            shiny::selectInput('ycol', 'Y axis', ""),
            # Y axis units (deg C or mV)
            shiny::textInput("yunits", label = "Units:", placeholder = "e.g., mV"),
            shiny::textInput("alpha", label = "alpha:", placeholder = "e.g., 3")
        ),

        # Time series plots
        shiny::actionButton(inputId="goButton", label="Plot Time Series"),
        # Data download
        shiny::downloadButton("downloadData", "Download Cleaned Time Series"),

        shiny::actionButton("done", "Done"),


        # first plot object
        plotly::plotlyOutput("plotA"),
        # second plot object
        plotly::plotlyOutput("plotB"),

        # Data table with selected outliers
        shiny::verbatimTextOutput("click"),
        shiny::verbatimTextOutput("brush"),
        shiny::dataTableOutput('myTable')

    )
    ################################################################################


    ################################################################################
    # Define server
    server <- function(input, output, session){#server

        res_env <- new.env()




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
        shiny::observe({shiny::updateSelectInput(session, "xcol", choices = outVar())})
        shiny::observe({shiny::updateSelectInput(session, "ycol", choices = outVar())})
        shiny::observe({shiny::updateTextInput(session, "yunits")})
        shiny::observe({shiny::updateTextInput(session, "alpha")})

        shiny::observeEvent(input$goButton,{#goButton
            frame1 <- data.frame()
            frame2 <- data.frame()




# Define data frame -------------------------------------------------------



            # Plot A -----------------------------------------------------------------------
            output$plotA = plotly::renderPlotly({

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

                filter2 = function(data, lag=1){
                    org=data
                    #differences
                    tt = c(0, diff(org, lag= lag))
                    threshold = mean(org, na.rm=T)
                    rt= org
                    rt[abs(tt) > threshold] = NA
                    return(rt)
                }

                df = data.frame(x=dataInput()[[input$xcol]],
                                y=dataInput()[[input$ycol]],
                                key=row.names(dataInput()))
                aa = input$alpha
                cln1 = filter1(df$y, mult = as.numeric(aa))
                cln2 = filter2(cln1)
                df$y1 = cln2
                assign(x = "df", value = df, envir = res_env)

                t = df[complete.cases(df$y),]
                t$x = as.character(t$x)
                assign(x = "AutoDetect", value = t[is.na(t$y1), c("x","y")] , envir = res_env)

                p = plotly::plot_ly(data=df, x=~x, y=~y) %>%
                    plotly::add_markers(key=~key, color = I("red"), type="scatter",
                                mode='lines+markers', showlegend = FALSE)

                p <- plotly::add_markers(p, data = df, y=~y1, color = I("black"), showlegend = FALSE)

                plotly::layout(p, xaxis = list(title = "",
                                       rangeslider = list(df$x[1], utils::tail(df$x,1))),
                       yaxis = list(title = input$yunits),
                       dragmode = "select",
                       selectdirection = "h",
                       title = "Raw data (detected outliers in red)")

            })


            # Plot B -----------------------------------------------------------------------
            output$plotB = plotly::renderPlotly({


                c <- plotly::event_data("plotly_click", source = "plotB" )
                d <- plotly::event_data("plotly_selected", source = "plotB" )

                p = plotly::plot_ly(data=df, x = ~x, y = ~y1, source = "plotB") %>%
                    plotly::add_markers(color = I("black"), mode='lines+markers', showlegend = F)

                if (!is.null(c)) {
                    cc <- df[df$key %in% c[["key"]], ]

                    p <- plotly::add_markers(p, data = frame1_rc()[,c("x","y")], x = ~x, y = ~y, color = I("red"), showlegend = FALSE)

                }

                if (!is.null(d)) {
                    dd <- df[df$key %in% d[["key"]], ]
                    p <- plotly::add_markers(p, data = frame2_rc()[,c("x","y")], x = ~x, y = ~y, color = I("red"), showlegend = FALSE)
                }


                plotly::layout(p, xaxis = list(title = ""),
                       yaxis = list(title = input$yunits), dragmode = F,
                       title = "Filtered data")

            })

            # click selection
            frame1_rc <- shiny::reactive({
                d <- plotly::event_data("plotly_click", source = "plotB" )
                if (!is.null(d)){
                    frame1 <<- rbind(frame1, d)
                }else(frame1)
            })

            output$click  <- shiny::renderPrint({
                frame1_rc()
                xx = frame1_rc()[,c("x","y")]
                xx = xx[!duplicated(xx$x),]
                assign(x = "Clicked", value = xx, envir = res_env)
            })

            frame2_rc <- shiny::reactive({
                d <- plotly::event_data("plotly_selected", source = "plotB" )
                if (!is.null(d)){
                    frame2 <<- rbind(frame2, d)
                }else(frame2)
            })

            # brush selection
            output$brush <- shiny::renderPrint({
                frame2_rc()
                yy = frame2_rc()[,c("x","y")]
                yy = yy[!duplicated(yy$x),]
                assign(x = "Brushed", value = yy, envir = res_env)
            })


            output$myTable  <-  DT::renderDataTable({
                xx = frame1_rc()[,c("x","y")]
                xx = xx[!duplicated(xx$x),]
                yy = frame2_rc()[,c("x","y")]
                yy = yy[!duplicated(yy$x),]
                data.frame(rbind(xx, yy))

            })

            output$downloadData <- shiny::downloadHandler(
                filename = function(){
                    paste(strsplit(as.character(input$file), ".RData")[[1]],
                          "_Cleaned", ".RData", sep = "")},

                content = function(file){
                    OriginalData = data.frame(x=dataInput()[[input$xcol]],
                                              y=dataInput()[[input$ycol]])

                    #subst = rbind(frame1_rc()[,c("x","y")], frame2_rc()[,c("x","y")])
                    #CleanedData = subset(OriginalData, x %!in% subst$x)[,1:2]

                    OriginalData = OriginalData[,1:2]

                    save(OriginalData, res_env$AutoDetect, res_env$Clicked, res_env$Brushed, file=file)
                }

            )

        })#goButton


        shiny::observeEvent(input$done, {
            # timeText <- paste0("\"", as.character(Sys.time()), "\"")
            # rstudioapi::insertText(timeText)
            shiny::stopApp()
        })

    }#server
    ################################################################################


    ################################################################################
    # Run the application
    shiny::shinyApp(ui = ui, server = server)
    ################################################################################





}
