################################################################################
# TREX: Sap flow data cleaning
# Christoforos Pappas | 22.10.2019
################################################################################

# libraries
library(shiny)  # app
library(plotly) # interactive plotting
library(DT)     # nice output tables

# functions
`%!in%` = Negate(`%in%`)


################################################################################
# Define UI
ui <- fluidPage(

    # Title
    titlePanel("TREX: sap flow data cleaning"),

    flowLayout(
        # Data Input
        fileInput("file", label = "Input data:"),
        # Select variables/columns from data frame
        selectInput('xcol', 'X axis', ""),
        selectInput('ycol', 'Y axis', ""),
        # Y axis units (deg C or mV)
        textInput("yunits", label = "Units:", placeholder = "e.g., mV"),
        textInput("alpha", label = "alpha:", placeholder = "e.g., 3")
    ),

    # Time series plots
    actionButton(inputId="goButton", label="Plot Time Series"),
    # Data download
    downloadButton("downloadData", "Download Cleaned Time Series"),

    # first plot object
    plotlyOutput("plotA"),
    # second plot object
    plotlyOutput("plotB"),

    # Data table with selected outliers
    verbatimTextOutput("click"),
    verbatimTextOutput("brush"),
    dataTableOutput('myTable')

)
################################################################################


################################################################################
# Define server
server <- function(input, output, session){#server

    # Load data --------------------------------------------------------------------
    dataInput = reactive({
        if (is.null(input$file)) return(NULL)
        inFile = input$file
        file = inFile$datapath
        # load the file into new environment and get it from there
        e = new.env()
        name = load(file, envir = e)
        data = e[[name]]
    })

    outVar = reactive({names(dataInput())})
    observe({updateSelectInput(session, "xcol", choices = outVar())})
    observe({updateSelectInput(session, "ycol", choices = outVar())})
    observe({updateTextInput(session, "yunits")})
    observe({updateTextInput(session, "alpha")})

    observeEvent(input$goButton,{#goButton
        frame1 <- data.frame()
        frame2 <- data.frame()


        # Plot A -----------------------------------------------------------------------
        output$plotA = renderPlotly({

            filter1 = function(data, mult){
                inp=na.omit(data)
                q=quantile(inp, na.rm=T, names = F)
                q_25=q[2]
                q_75=q[4]
                iqr=IQR(inp, na.rm = T)
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
            assign(x = "df", value = df, envir = .GlobalEnv)

            t = df[complete.cases(df$y),]
            t$x = as.character(t$x)
            assign(x = "AutoDetect", value = t[is.na(t$y1), c("x","y")] , envir = .GlobalEnv)

            p = plot_ly(data=df, x=~x, y=~y) %>%
                add_markers(key=~key, color = I("red"), type="scatter",
                            mode='lines+markers', showlegend = FALSE)

            p <- add_markers(p, data = df, y=~y1, color = I("black"), showlegend = FALSE)

            layout(p, xaxis = list(title = "",
                                   rangeslider = list(df$x[1], tail(df$x,1))),
                   yaxis = list(title = input$yunits),
                   dragmode = "select",
                   selectdirection = "h",
                   title = "Raw data (detected outliers in red)")

        })


        # Plot B -----------------------------------------------------------------------
        output$plotB = renderPlotly({


            c <- event_data("plotly_click", source = "plotB" )
            d <- event_data("plotly_selected", source = "plotB" )

            p = plot_ly(data=df, x = ~x, y = ~y1, source = "plotB") %>%
                add_markers(color = I("black"), mode='lines+markers', showlegend = F)

            if (!is.null(c)) {
                cc <- df[df$key %in% c[["key"]], ]

                p <- add_markers(p, data = frame1_rc()[,c("x","y")], x = ~x, y = ~y, color = I("red"), showlegend = FALSE)

            }

            if (!is.null(d)) {
                dd <- df[df$key %in% d[["key"]], ]
                p <- add_markers(p, data = frame2_rc()[,c("x","y")], x = ~x, y = ~y, color = I("red"), showlegend = FALSE)
            }


            layout(p, xaxis = list(title = ""),
                   yaxis = list(title = input$yunits), dragmode = F,
                   title = "Filtered data")

        })

        # click selection
        frame1_rc <- reactive({
            d <- event_data("plotly_click", source = "plotB" )
            if (!is.null(d)){
                frame1 <<- rbind(frame1, d)
            }else(frame1)
        })

        output$click  <- renderPrint({
            frame1_rc()
            xx = frame1_rc()[,c("x","y")]
            xx = xx[!duplicated(xx$x),]
            assign(x = "Clicked", value = xx, envir = .GlobalEnv)
        })

        frame2_rc <- reactive({
            d <- event_data("plotly_selected", source = "plotB" )
            if (!is.null(d)){
                frame2 <<- rbind(frame2, d)
            }else(frame2)
        })

        # brush selection
        output$brush <- renderPrint({
            frame2_rc()
            yy = frame2_rc()[,c("x","y")]
            yy = yy[!duplicated(yy$x),]
            assign(x = "Brushed", value = yy, envir = .GlobalEnv)
        })


        output$myTable  <-  renderDataTable({
            xx = frame1_rc()[,c("x","y")]
            xx = xx[!duplicated(xx$x),]
            yy = frame2_rc()[,c("x","y")]
            yy = yy[!duplicated(yy$x),]
            data.frame(rbind(xx, yy))

        })

        output$downloadData <- downloadHandler(
            filename = function(){
                paste(strsplit(as.character(input$file), ".RData")[[1]],
                      "_Cleaned", ".RData", sep = "")},

            content = function(file){
                OriginalData = data.frame(x=dataInput()[[input$xcol]],
                                          y=dataInput()[[input$ycol]])

                #subst = rbind(frame1_rc()[,c("x","y")], frame2_rc()[,c("x","y")])
                #CleanedData = subset(OriginalData, x %!in% subst$x)[,1:2]

                OriginalData = OriginalData[,1:2]

                save(OriginalData, AutoDetect, Clicked, Brushed, file=file)
            }

        )

    })#goButton

}#server
################################################################################


################################################################################
# Run the application
shinyApp(ui = ui, server = server)
################################################################################
