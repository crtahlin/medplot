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
# library for manipulating with data (does not work with R>3.0.2)
# library(dplyr)
# library for manipulating data
library(plyr)
# library for clustering
library(pheatmap)
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
    })
  
  # plot the graph, but only for selected symptoms
  output$plot <- renderPlot(function() {
    # if no symbols are selected, do not plot
    if (dim(data())[1]>0) {
    print(plotSymptomsTimeline(data()))}
  })
  
  # message for working with DEMO data
  output$message <- renderText(
    if (is.null(input$dataFile)) {paste("WORKING WITH DEMO DATA!")} else {
    if (dim(data())[1]==0){paste("Please select one or more symptoms.")}
    })
  
  # build extended data set for additional graphs
  dataExtended <- reactive( function() {
    # ddplyer command below - does not work with R<3.0.2
    # data <- inner_join(x = symptomsData(), y = symptomsPatients(), by="PersonID")
    # using plyr instead
    data <- join(x=symptomsData(), y=symptomsPatients(), by="PersonID", type="inner")
    return(data)
  })
  
  # build the second graph
  output$plotPyramid <- renderPlot (
    plotPropWithSymptoms(data=dataExtended(),
                         grouping=input$groupingVar,
                         measurements="Measurement",
                         symptomsNames=unlist(levels(data()[,"variable"])))
    )
  
  # debuging information
  output$debug <- renderTable(dataExtended())
    
  
  ########### clustering of the symptoms ###################
  
  ########## user interface to select which measurments to cluster
  output$clusteringUI = renderUI({
    #levels of the measurement variable, save as third variable in the dataset symptomsData
    # TODO: make selection of levels dependent on Sidebar, not fixed to "Measurement"
    myLevels=levels(as.factor(symptomsData()[,"Measurement"]))
    #select the measurement
    selectInput(inputId="selectedMeasurementValue",
                label="Select the measurement occasion (time):", 
                choices=myLevels, selected=myLevels[1])
    })
  
  
  output$plotClusterDendrogram=renderPlot({
    plotClusterDendrogram(data=symptomsData(),
                          variableName="Measurement",
                          variableValue=input$selectedMeasurementValue)
    })
  
  output$plotClusterHeatmap=renderPlot({
    plotClusterHeatmap(data=symptomsData(),
                       #TODO: make dependent on selection
                       variableName="Measurement",
                       variableValue=input$selectedMeasurementValue) 
    })
  
  
  if (input$dataFileType=="TSV") { output$sidebar <- renderUI(source("SidebarTSV.R")) }
  if (input$dataFileType=="Excel") { output$sidebar <- renderUI(source("SidebarExcel.R")) }
  if (input$dataFileType=="Demo") { output$sidebar <- renderUI(source("SidebarExcel.R")) }
  
})

