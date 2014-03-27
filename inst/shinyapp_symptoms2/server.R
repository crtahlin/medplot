# This is the server part of the shiny script to plot
# graph for displaying presence of symptoms. 

# Load libraries ----------------------------------------------------------
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
# library for manipulating with data (does not work with R<3.0.2)
# library(dplyr)
# library for manipulating data
library(plyr)
# library for clustering
library(pheatmap)
# load medplot library
library(medplot)

# Global variables ---------------------------------------------------------------------
# variables for melting data into ggplot compliant format for Timeline graph
meltBy <- c("PersonID", "Date", "Measurement")


# Main function -----------------------------------------------------------

shinyServer(function(input, output, session) {
  ### Import and prepare data ####
  # load data from Excel
  symptomsData <- reactive(function() { # returns the DATA sheet
    if (input$dataFileType=="Demo") {
      # load default data for Demo scenario
      # the location of template data file
      templateLocation <- paste0(path.package("medplot"),"/extdata/PlotSymptoms_shiny.xlsx")
      importSymptomsData(datafile=templateLocation,
                         format="Excel")
    } else { # load data for non-demo scenario
    if (!is.null(input$dataFile) && !(input$dataFileType=="Demo")) {
      importSymptomsData(datafile=input$dataFile$datapath,
                         format=input$dataFileType) }
    } 
  })
  
  symptomsPatients <- reactive(function() { # returns the PATIENTS sheet
    if (input$dataFileType=="Demo") {
      # load default data for Demo scenario
      # the location of template data file
      templateLocation <- paste0(path.package("medplot"),"/extdata/PlotSymptoms_shiny.xlsx")
      importSymptomsPatients(datafile=templateLocation)    } #else {}
    # TODO: load patients for "Excel" scenario
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
  
#   # list all available symptoms in an output slot
#   output$levels <- renderUI( {
#     ### TODO HERE
#     # transform data into ggplot compliant format
#     data <- melt(symptomsData(), id.vars = meltBy)
#     symptoms <- unlist(levels(data[,"variable"]))
#     checkboxGroupInput(inputId="selectedSymptoms",
#                        label="Choose symptoms", 
#                        choices=symptoms)
#   })
  
  
  
  # build extended data set for additional graphs
  dataExtended <- reactive( function() {
    # ddplyer command below - does not work with R<3.0.2
    # data <- inner_join(x = symptomsData(), y = symptomsPatients(), by="PersonID")
    # using plyr instead
    data <- join(x=symptomsData(), y=symptomsPatients(), by="PersonID", type="inner")
    return(data)
  })
  
  ## Mainpanel dynamic output 
  # message for working with DEMO data
  output$message <- renderText(
    if (is.null(input$dataFile)) {paste("WORKING WITH DEMO DATA!")} else {
      if (dim(data())[1]==0){paste("Please select one or more symptoms.")}
    })
  
  # debuging information
  output$debug <- renderTable(dataExtended())
  
  output$selectSymptoms <- renderUI({
    # Sidebar for Excel and demo files ####
    # TODO: Try to redo this part with switch() statement as
    # it seems only the last evaluated block is returned
    if (!is.null(symptomsData())) {
    data <- melt(symptomsData(), id.vars = meltBy)
    symptoms <- unlist(levels(data[,"variable"]))
    checkboxGroupInput(inputId="selectedSymptoms",
                       label="Choose symptoms:", 
                       choices=symptoms)}
      })
  
output$selectDateVar <- renderUI({
  allSymptoms <- unlist(names(dataExtended()))
  selectInput(inputId="dateVar",
                     label="Choose date variable:", 
                     choices=allSymptoms,
              selected="Date")
})

output$selectGroupingVar <- renderUI({
  allSymptoms <- unlist(names(dataExtended()))
  selectInput(inputId="groupingVar",
              label="Choose grouping variable:", 
              choices=allSymptoms,
              selected="Sex")
})

  ## Plots for different tabs ####
  
  # Timeline tab plots and output ####
  # plot the graph, but only for selected symptoms
  output$plot <- renderPlot(function() {
    # if no symbols are selected, do not plot
    if (dim(data())[1]>0) {
      print(plotSymptomsTimeline(data()))}
  })
  
  # Proportions tab plots and output ####
  # pyramid plot of proportions
  output$plotPyramid <- renderPlot (
    plotPropWithSymptoms(data=dataExtended(),
                         grouping=input$groupingVar,
                         measurements="Measurement",
                         symptomsNames=unlist(levels(data()[,"variable"])))
  )
  
  # Clustering tab plots and output ####
  # selection of measurement occasions  
  output$clusteringUI = renderUI({
    #levels of the measurement variable, save as third variable in the dataset symptomsData
    # TODO: make selection of levels dependent on Sidebar, not fixed to "Measurement"
    myLevels=levels(as.factor(symptomsData()[,"Measurement"]))
    #select the measurement
    selectInput(inputId="selectedMeasurementValue",
                label="Select the measurement occasion (time):", 
                choices=myLevels, selected=myLevels[1])
  })
  
  # dendrogram plot on the Clustering tab
  output$plotClusterDendrogram=renderPlot({
    plotClusterDendrogram(data=symptomsData(),
                          variableName="Measurement",
                          variableValue=input$selectedMeasurementValue)
  })
  
  # heatmap plot on the Clustering tab
  output$plotClusterHeatmap=renderPlot({
    plotClusterHeatmap(data=symptomsData(),
                       #TODO: make dependent on selection
                       variableName="Measurement",
                       variableValue=input$selectedMeasurementValue) 
  })
  
output$symptomsData <- renderText(names(dataExtended()))
  
})









