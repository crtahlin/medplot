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
      as.character(as.Date(data$DateIn, origin="1899-12-30"),format="%d.%m.%Y")
    data$DateOut <- 
      as.character(as.Date(data$DateOut, origin="1899-12-30"),format="%d.%m.%Y")
    return(data)
    })
  
  output$dataTable <- renderTable(data())
  
  output$test <- renderText("blablabla")
    
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
    outfile <- tempfile(fileext='.svg')
    
    # generate the plot
    plotTests(data=read.xls(input$dataFile$datapath), figureParameters)
#     png(outfile, width=400, height=400)
#     hist(rnorm(input$n))
#     dev.off()
    
    # return a list with generated plot file location
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  })