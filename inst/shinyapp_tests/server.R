# Shiny server for plottting test results

# load library for generation interactive web pages
library(shiny)

# load library for reading Excel files
library(gdata)

# main function 
shinyServer(function(input, output) {
  
  # load data from Excel
  data <- reactive({
    data <- read.xls(input$dataFile$datapath, sheet="DATA")
    # change Excel numeric date format into R dates 
    data$DateIn <- 
      as.POSIXct(as.Date(data$DateIn, origin="1899-12-30"))
    data$DateOut <- 
      as.POSIXct(as.Date(data$DateOut, origin="1899-12-30"))
    return(data)
  })
  
  # load parameters from Excel
  parameters <- reactive({
    parameters <- read.xls(input$dataFile$datapath, sheet="PARAMETERS")
    return(parameters)
  })
  
  output$dataTable <- renderTable({
    data <- data()
    data$DateIn <- 
      as.character(as.Date(data$DateIn, origin="1899-12-30"),format="%d.%m.%Y")
    data$DateOut <- 
      as.character(as.Date(data$DateOut, origin="1899-12-30"),format="%d.%m.%Y")
    return(data)
    # NOTE: if we want to render the table of data, we have to convert the dates into 
    # characters, since renderTable seems to use xtable, which seems to not handle
    # dates very well (http://stackoverflow.com/questions/8652674/r-xtable-and-dates)
  })
  
  output$parametersTable <- renderTable(parameters())
  
  # generate plot
  output$dataPlot <- renderImage({
    # create name of temporary file
    outfile <- tempfile(fileext='.svg')
    # workatround
    # outfile <- "C:/Users/Crt Ahlin/Desktop/medplot/example_tooltips.svg"
    # generate the plot
    plotTests(data=data(), figureParameters=parameters(), fileName=outfile, generateTooltips=input$generateTooltips, sortMethod=input$sortingMethod)
    
    # return a list with generated plot file location
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
})