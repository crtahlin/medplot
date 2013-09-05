# Shiny server for plottting test results

# load library for generation interactive web pages
library(shiny)

# load library for reading Excel files
library(gdata)



# main function 
shinyServer(function(input, output) {

  # load data into dataframe
  data <- reactive({
    data <- read.xls(input$dataFile$datapath)
    data$DateIn <- 
      as.POSIXct(as.Date(data$DateIn, origin="1899-12-30"))
    data$DateOut <- 
      as.POSIXct(as.Date(data$DateOut, origin="1899-12-30"))
#     data$DateOut <- 
#       as.character(as.Date(data$DateOut, origin="1899-12-30"),format="%d.%m.%Y")
    # NOTE: if we want to render the table of data, we have to convert the dates into 
    # characters, since renderTable seems to use xtable, which seems to not handle
    # dates very well (http://stackoverflow.com/questions/8652674/r-xtable-and-dates)
    return(data)
    })
  
  output$dataTable <- renderTable(data())
      
  # create dataframe for testing
  # it contains figureParameters to determine how to plot points
  figureParameters <- data.frame(
    rbind(
  c("neg",  "green",	1),
  c("CoV",	"blue",	2),
  c("INF A",	"red",	1),
  c("Other",	"black",	1),
  c("RSV A",	"purple",	2),
  c("RSV C",	"yellow",	1)
    )
  )
  colnames(figureParameters) <- c("Result","Color","Size")

  # generate plot
  output$dataPlot <- renderImage({
    # create name of temporary file
    # outfile <- tempfile(fileext='.svg')
    # workatround
    outfile <- "C:/Users/Crt Ahlin/Desktop/medplot/example_tooltips.svg"
    # generate the plot
    plotTests(data=data(), figureParameters=figureParameters, graphsDir="C:/Users/Crt Ahlin/Desktop/medplot")
#     png(outfile, width=400, height=400)
#     hist(rnorm(input$n))
#     dev.off()
    
    # return a list with generated plot file location
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  })