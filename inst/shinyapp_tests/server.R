# Shiny server for plottting test results

# load library for generation interactive web pages
library(shiny)

# load library for reading Excel files
library(gdata)

# load medplot library
library(medplot)

# save the location of template data file
templateLocation <- paste0(path.package("medplot"),"/extdata/PlotTests_shiny.xlsx")


# main function 
shinyServer(function(input, output, session) {
  
  # load data from Excel
  data <- reactive({
    if (!is.null(input$dataFile)) {
    data <- read.xls(input$dataFile$datapath, sheet="DATA")
    # change Excel numeric date format into R dates 
    data$DateIn <- 
      as.POSIXct(as.Date(data$DateIn, origin="1899-12-30"))
    data$DateOut <- 
      as.POSIXct(as.Date(data$DateOut, origin="1899-12-30"))
    return(data)} else {
    ### load default data
      # DEMO SCENARIO
      data <- read.xls(templateLocation, sheet="DATA")
      # change Excel numeric date format into R dates 
      data$DateIn <- 
        as.POSIXct(as.Date(data$DateIn, origin="1899-12-30"))
      data$DateOut <- 
        as.POSIXct(as.Date(data$DateOut, origin="1899-12-30"))
      return(data)
    }
  })
  
  # load parameters from Excel
  parameters <- reactive({
    if (!is.null(input$dataFile)) {
    parameters <- read.xls(input$dataFile$datapath, sheet="PARAMETERS")
    return(parameters)} else {
      # loading default parameters
      # DEMO PARAMETERS
      parameters <- read.xls(templateLocation, sheet="PARAMETERS")
      return(parameters)
    
    }
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
  
  # generate reactive plot and error messages
  generatePlot <- reactive({
    outfile <- tempfile(fileext='.svg')
    plotTests(data=data(),
              figureParameters=parameters(),
              fileName=outfile,
              generateTooltips=input$generateTooltips,
              sortMethod=input$sortingMethod)
    list(errorMessages=errorMessages, outfile=outfile)
  }
  )
  # assign the plot to an output slot
  output$dataPlot <- renderImage(
    list(
      src=generatePlot()$outfile,
      alt = "This is alternate text",
      contentType = 'image/svg+xml')
  )
  
  # assign error messages to an output slot
  output$errors <- renderPrint({unlist(generatePlot()$errorMessages)})
  
  # generate debug info and assign it to an output slot
  clientData <- session$clientData
  output$debug <- renderText({
    clientDataNames <- names(clientData)
    
    allValues <- lapply(clientDataNames, function(name) {
      paste(name, clientData[[name]], sep=" = ")
    })
    paste(allValues, collapse = "\n")
  })
  
  # meassage to display over the graph
  output$message <- renderText(
    if (is.null(input$dataFile)) {paste("WORKING WITH DEMO DATA!")} 
    )
})