# This is the server part of the shiny script to plot
# graph for displaying presence of symptoms. 

# load library for generation interactive web pages
library(shiny)
# load library for generating graph scales
library(scales)
# load library for melting data
library(reshape2)
# library for plotting data
library(ggplot2)
# library for reading Excel files
library(gdata)

# Main function
shinyServer(function(input, output, session) {
  
  # load data from Excel
  symptomsData <- reactive(function() {
    if (!is.null(input$dataFile)) {
    # read the data into R
    data <- read.xls(input$dataFile$datapath, sheet="DATA")
    
    # transform date information into R compliant dates
    data["Date"] <- as.Date(data[,"Date"], "%d.%m.%Y")
    
    # transform data into ggplot compliant format
    data <- melt(data, id.vars = c("Patient", "Date"))
    return(data)
    } else {
      #### load default data
      # DEMO SCENARIO
      data <- read.xls("../extdata/PlotSymptoms_shiny.xlsx", sheet="DATA")
      
      # transform date information into R compliant dates
      data["Date"] <- as.Date(data[,"Date"], "%d.%m.%Y")
      
      # transform data into ggplot compliant format
      data <- melt(data, id.vars = c("Patient", "Date"))
      return(data)
      #####
    
    }
  })
  

  
  # subset the data with the selected symptoms
  data <- reactive( function() {
    symptomsData()[symptomsData()$variable %in% input$selectedSymptoms,]
  })
  
  # list the subseted data in an output slot
  output$data <- renderTable({
    data <- data()
    data$Date <- as.character(as.Date(data$Date, origin="1899-12-30"),format="%d.%m.%Y")
    return(data)
                             # NOTE: if we want to render the table of data, we have to convert the dates into 
                             # characters, since renderTable seems to use xtable, which seems to not handle
                             # dates very well (http://stackoverflow.com/questions/8652674/r-xtable-and-dates)
    })
  
  # list all available symptoms in an output slot
  output$levels <- renderUI( {
    symptoms <- unlist(levels(symptomsData()[,"variable"]))
    checkboxGroupInput(inputId="selectedSymptoms",
                       label="Choose symptoms", 
                       choices=symptoms)
  } 
                             )
  
  
  # plot the graph, but only for selected symptoms
  output$plot <- renderPlot(function() {
    # if no symbols are selected, do not plot
    if (dim(data())[1]>0) {
    print(
      ggplot(data(), aes(x = Date, y = Patient, size = value, colour = variable)) +
        geom_point(shape = 1) + theme_bw() +
        # old scale - scale according to diameter
        #scale_size(range= c(1,20)) +
        # scale according to area
        scale_size_area(breaks=c(1:10),minor_breaks=c(1:10),
                        guide="legend",
                        limits=c(1,10),
                        max_size=10) +
        scale_x_date(breaks = date_breaks("1 week"),
                     labels = date_format("%d %b"),
                     minor_breaks = date_breaks("1 day"))
            
    )}
  })
  
  output$message <- renderText(
    if (is.null(input$dataFile)) {paste("WORKING WITH DEMO DATA!")} else {
    if (dim(data())[1]==0){paste("Please select one or more symptoms.")}
    })
  output$debug <- renderPrint(symptomsData())
    
  
})

