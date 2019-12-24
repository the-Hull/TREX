#outlier
################################################################################
# TREX: Sap flow data cleaning
# Christoforos Pappas | 22.10.2019
################################################################################

# install
install.packages("shiny")
install.packages("plotly")
install.packages("DT")

# libraries
library(shiny)  # app
library(plotly) # interactive plotting
library(DT)     # nice output tables



# t= test
raw   <- example.data(type="doy", species="PCAB")
input <- is.trex(raw,tz="GMT",time.format="%H:%M",solar.time=TRUE,long.deg=7.7459,ref.add=FALSE,df=FALSE)
input<-time.step(input,time.int=60,max.gap=180,decimals=10)
input<-window(input,start=as.POSIXct(as.character("(01/01/12 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"),
       end=as.POSIXct(as.character("(01/01/13 00:00:00)"),format="(%m/%d/%y %H:%M:%S)",tz="GMT"))
test<-data.frame(time=as.character(index(input)),value=as.numeric(as.character(input)))
save(test,file = "D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX/R_package/test.RData")
write.RDS(data.frame(time=as.character(index(input)),value=as.numeric(as.character(input))),"D:/Documents/GU - POSTDOC/07_work_document/T1 - TREX/R_package/TREX - Construction/test.txt",row.names=F,col.names=T)

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
    textInput("yunits", label = "Units:", placeholder = "e.g., mV")
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
  
  # Load data
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
  
  observeEvent(input$goButton,{#goButton
    frame1 <- data.frame()
    frame2 <- data.frame()
    
    # Plot A -----------------------------------------------------------------------
    output$plotA = renderPlotly({
      
      df = data.frame(x=dataInput()[[input$xcol]], 
                      y=dataInput()[[input$ycol]],
                      key=row.names(dataInput()))
      
      c <- event_data("plotly_click")
      d <- event_data("plotly_selected") 
      
      p = plot_ly(data=df, x=~x, y=~y) %>%
        add_markers(key=~key, color = I("black"), type="scatter", 
                    mode='lines+markers', showlegend = FALSE)
      
      if (!is.null(c)) {
        cc <- df[df$key %in% c[["key"]], ] 
        p <- add_markers(p, data = cc, color = I("red"), showlegend = FALSE)
      }
      
      if (!is.null(d)) {
        dd <- df[df$key %in% d[["key"]], ]
        p <- add_markers(p, data = dd, color = I("red"), showlegend = FALSE)
      }
      
      layout(p, xaxis = list(title = "", 
                             rangeslider = list(df$x[1], tail(df$x,1))),
             yaxis = list(title = input$yunits),
             dragmode = "select",
             selectdirection = "h",
             title = "Raw data for outlier selection")    
    })
    
    # click selection
    frame1_rc <- reactive({
      d <- event_data("plotly_click") 
      if (!is.null(d)){  
        frame1 <<- rbind(frame1, d)
      }else(frame1)
    })
    
    output$click  <- renderPrint({
      frame1_rc()
      assign(x = "clicked", value = frame1, envir = .GlobalEnv)    
    })
    
    frame2_rc <- reactive({       
      d <- event_data("plotly_selected") 
      if (!is.null(d)){
        frame2 <<- rbind(frame2, d) 
      }else(frame2)
    })
    
    # brush selection
    output$brush <- renderPrint({
      frame2_rc()
      assign(x = "brushed", value = frame2_rc(), envir = .GlobalEnv)
    })
    
    # Plot B -----------------------------------------------------------------------
    output$plotB = renderPlotly({
      
      df = data.frame(x=dataInput()[[input$xcol]], 
                      y=dataInput()[[input$ycol]],
                      key=row.names(dataInput()))
      
      subst = rbind(frame1_rc(), frame2_rc())
      
      myData = subset(df, key %!in% subst$key )
      
      p = plot_ly(data=myData, x = ~x, y = ~y) %>%
        add_markers(color = I("black"), mode='lines+markers', showlegend = F) %>% 
        config(displayModeBar = F)
      
      layout(p, xaxis = list(title = ""), 
             yaxis = list(title = input$yunits), dragmode = F,
             title = "Filterd data to export")
    })
    
    output$myTable  <-  renderDataTable({
      data.frame(rbind(frame1_rc(), frame2_rc()))[,3:4]
    })
    
    output$downloadData <- downloadHandler(
      filename = function(){
        paste(strsplit(as.character(input$file), ".RData")[[1]],
              "_Cleaned", ".RData", sep = "")},   
      
      content = function(file){
        OriginalData = data.frame(x=dataInput()[[input$xcol]], 
                                  y=dataInput()[[input$ycol]],
                                  key=row.names(dataInput()))
        
        subst = rbind(frame1_rc(), frame2_rc())
        
        CleanedData = subset(OriginalData, key %!in% subst$key)[,1:2]
        
        OriginalData = OriginalData[,1:2]
        
        save(OriginalData, CleanedData, file=file)  
      }
      
    )  
    
  })#goButton
  
}#server
################################################################################


################################################################################
# Run the application 
shinyApp(ui = ui, server = server)
################################################################################