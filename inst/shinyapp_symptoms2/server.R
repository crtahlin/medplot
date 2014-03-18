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
# library for manipulating with data
library(dplyr)
# load medplot library
library(medplot)
# save the location of template data file
templateLocation <- paste0(path.package("medplot"),"/extdata/PlotSymptoms_shiny.xlsx")

# variables for melting data into ggplot compliant format
meltBy <- c("PersonID", "Date", "Measurement")

# Main function
shinyServer(function(input, output, session) {
  
  # load data from Excel
  symptomsData <- reactive(function() { # returns the DATA sheet
    if (!is.null(input$dataFile)) {
    # read the data into R
    data <- read.xls(input$dataFile$datapath, sheet="DATA")
    
    # transform date information into R compliant dates
    data["Date"] <- as.Date(data[,"Date"], "%d.%m.%Y")
    return(data)
    } else { #### load default data
      
      # DEMO SCENARIO
      data <- read.xls(templateLocation, sheet="DATA")
      
      # transform date information into R compliant dates
      data["Date"] <- as.Date(data[,"Date"], "%d.%m.%Y")
      return(data)
      #####
    
    }
  })
  
  symptomsPatients <- reactive(function() { # returns the PATIENTS sheet
    if (!is.null(input$dataFile)) {
      # read the data into R
      data <- read.xls(input$dataFile$datapath, sheet="PATIENTS")
      return(data)
    } else { #### load default data
      
      # DEMO SCENARIO
      data <- read.xls(templateLocation, sheet="PATIENTS")
      return(data)
      #####
      
    }
  })
  
  # subset the data with the selected symptoms
  data <- reactive( function() {
    data <- melt(symptomsData(), id.vars = meltBy)
    data[data$variable %in% input$selectedSymptoms,]
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
    ### TODO HERE
    # transform data into ggplot compliant format
    data <- melt(symptomsData(), id.vars = meltBy)
    symptoms <- unlist(levels(data[,"variable"]))
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
      ggplot(data(), aes(x = Date, y = PersonID, size = value, colour = variable)) +
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
  
  
  # build extended data set for additional graphs
  dataExtended <- reactive( function() {
    data <- inner_join(x = symptomsData(), y = symptomsPatients(), by="PersonID")
    return(data)
  })
  
  output$plotPyramid <- renderPlot (
    plotPropWithSymptoms(data=AAA,
                         grouping="Sex",
                         measurements="Measurement",
                         symptomsNames=unlist(levels(data()[,"variable"])))
    )
  
  output$debug <- renderTable(dataExtended())
    
  
})

