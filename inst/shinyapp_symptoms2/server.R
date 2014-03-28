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
#library for date management
library(lubridate)

# Global variables ---------------------------------------------------------------------
# variables for melting data into ggplot compliant format for Timeline graph
meltBy <- c("PersonID", "Date", "Measurement")


# Main function -----------------------------------------------------------

shinyServer(function(input, output, session) {
  ### Import and prepare data ####
  # load data from Excel
  # TODO: are these functions below needed anymore?
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
      #if(is.null(input$dataFile)) return()
      importSymptomsPatients(datafile=templateLocation)    } #else {}
    # TODO: load patients for "Excel" scenario
  })
  
  # subset the data with the selected symptoms - for 1st graph
  # TODO: is this function still needed?
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
  
  
  # build extended data set for additional graphs
  dataExtended <- reactive( function() {
    observe(input$dataFile)
    # TODO: napiši kodo za scenarij, ko input$datafile ni null, ampak
    # samo ni v pravem formatu - tudi takrat naj vrne NULL
    if(is.null(input$dataFile)) return()
    if (input$dataFileType=="Excel") {
      patients <- importSymptomsPatients(datafile=input$dataFile$datapath)
      symptoms <- importSymptomsData(datafile=input$dataFile$datapath,
                                     format="Excel")
      data <- join(x=symptoms, y=patients, by="PersonID", type="inner")
    }
    
    if (input$dataFileType=="Demo") {
      templateLocation <- paste0(path.package("medplot"),"/extdata/PlotSymptoms_shiny.xlsx")
      patients <- importSymptomsPatients(datafile=templateLocation)
      symptoms <- importSymptomsData(datafile=templateLocation,
                                     format="Excel")
      data <- join(x=symptoms, y=patients, by="PersonID", type="inner")
    }
    
    if (input$dataFileType=="TSV") {
      data <- importSymptomsData(datafile=input$dataFile$datapath,
                                 format="TSV")
    }
    return(data)
  })
  
  dataFiltered <- reactive(function(){
    data <- dataExtended()[ , 
                           c(input$patientIDVar,
                             input$groupingVar,
                             input$dateVar,
                             input$measurementVar,
                             input$selectedSymptoms
                           )]
    # try to convert dates into R format
    data[input$dateVar] <- as.Date(data[,input$dateVar], format="%d.%m.%Y")
    return(data)
  })
  
  ## Mainpanel dynamic output 
  # message for working with DEMO data
  output$message <- renderText(
    if (is.null(input$dataFile)) {paste("WORKING WITH DEMO DATA!")} else {
      if (dim(data())[1]==0){paste("Please select one or more symptoms.")}
    })
  
  # debuging information
  output$debug <- renderTable(dataFiltered.yn())
  
  output$selectSymptoms <- renderUI({
    # Sidebar for Excel and demo files ####
    # TODO: Try to redo this part with switch() statement as
    # it seems only the last evaluated block is returned
    if (!is.null(symptomsData())) {
      data <- melt(symptomsData(), id.vars = meltBy)
      symptoms <- unlist(levels(data[,"variable"]))
      checkboxGroupInput(inputId="selectedSymptoms",
                         label="Choose symptoms:", 
                         #choices=symptoms
                         choices=dataVariableNames())}
  })
  
  dataVariableNames <- reactive(function(){
    unlist(names(dataExtended()))
  })
  
  output$selectDateVar <- renderUI({
    #allSymptoms <- unlist(names(dataExtended()))
    selectInput(inputId="dateVar",
                label="Choose date variable:", 
                choices=dataVariableNames(),
                selected="Date")
  })
  
  output$selectGroupingVar <- renderUI({
    #allSymptoms <- unlist(names(dataExtended()))
    selectInput(inputId="groupingVar",
                label="Choose grouping variable:", 
                choices=dataVariableNames(),
                selected="Sex")
  })
  
  output$selectPatientIDVar <- renderUI({
    # allSymptoms <- unlist(names(dataExtended()))
    selectInput(inputId="patientIDVar",
                label="Choose patient ID variable:", 
                choices=dataVariableNames(),
                selected="PersonID")
  })
  
  output$selectMeasurementVar <- renderUI({
    #allSymptoms <- unlist(names(dataExtended()))
    selectInput(inputId="measurementVar",
                label="Choose measurument occasion variable:", 
                choices=dataVariableNames(),
                selected="Measurement")
  })
  
  ## Plots for different tabs ####
  
  # Timeline tab plots and output ####
  # TODO: make data structure data() for TSV import
  # plot the graph, but only for selected symptoms
  output$plotTimeline <- renderPlot(function() {
    data=dataFiltered()
    # observe({dataFiltered()})
    # if no symbols are selected, do not plot
    #if (dim(dataFiltered())[1]>0) {
    print(plotSymptomsTimeline(data=data,
                               date=input$dateVar,
                               personID=input$patientIDVar,
                               measurement=input$measurementVar,
                               symptoms=input$selectedSymptoms)
    )#}
    
  })
  
  # Proportions tab plots and output ####
  # pyramid plot of proportions
  output$plotPyramid <- renderPlot (
    plotPropWithSymptoms(data=dataFiltered(),
                         grouping=input$groupingVar,
                         measurements=input$measurementVar,
                         symptomsNames=input$selectedSymptoms) #unlist(levels(data()[,"variable"]))
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
    plotClusterDendrogram(data=dataFiltered(),
                          variableName=input$measurementVar,
                          variableValue=input$selectedMeasurementValue,
                          selectedSymptoms=input$selectedSymptoms)
  })
  
  # heatmap plot on the Clustering tab
  output$plotClusterHeatmap=renderPlot({
    plotClusterHeatmap(data=dataExtended(),
                       #TODO: make dependent on selection
                       variableName=input$measurementVar,
                       variableValue=input$selectedMeasurementValue,
                       selectedSymptoms=input$selectedSymptoms) 
  })
  
  output$symptomsData <- renderTable((dataExtended()))
  
  ### TODO: organize, make independent of column order
  # TODO: rename into dataExtended.yn() ?; comment on data structure
  #dataset with the positive/negative values for the selected symptoms
  dataFiltered.yn=reactive({
    #apply(symptomsData()[, -c(1:3)], 1, function(x) ifelse(x>input$threshold, 1, 0))
    data=ifelse(dataFiltered()[, input$selectedSymptoms]>input$thresholdValue, 1, 0)
    return(data)
  })
  
  #saving Measurment in a reactive object - useful?
  Measurement=reactive({
    dataFiltered()[,input$measurementVar]
  })
  
  output$proportionUI = renderUI({
    
    #levels of the measurement variable, save as third variable in the dataset symptomsData
    # TODO: reference to measurementVar instead of 3rd column
    my.levels=levels(as.factor(dataFiltered()[,input$measurementVar]))
    
    #select the measurement
    selectInput(inputId="measurementSelectedProportion",
                label="Select the measurment (time)", 
                choices=my.levels, selected=my.levels[1])
  })
  
  
  output$plotProportion=renderPlot({
    plotDistribution(data=dataFiltered.yn(),
                     selectedSymptoms=input$selectedSymptoms,
                     selectedProportion=input$measurementSelectedProportion,
                     measurements=Measurement())
  })
  
  
  output$plotBoxplot=renderPlot({
    plotDistributionBoxplot(data=dataFiltered(),
                            data.yn=dataFiltered.yn(),
                            selectedSymptoms=input$selectedSymptoms,
                            selectedProportion=input$measurementSelectedProportion,
                            measurements=Measurement(),
                            posOnly=input$posOnly,
                            threshold=input$thresholdValue)
    
  })
  
  output$plotCI <- renderPlot({
    
    plotCI(data.yn=dataFiltered.yn(),
            measurements=Measurement(),
            selectedSymptoms=input$selectedSymptoms,
            selectedProportion=input$measurementSelectedProportion)
    
    
    })
  

  ########## user interface to select a numerical variable to associate with the presence of symptom
  output$rcsUI= renderUI({
    print("UI for rcs variable selection")
    selectInput(inputId="rcsIDVar",
                label="Numerical variable", 
                choices=dataVariableNames(),#[-c(input$selectedSymptoms)],
                selected=NULL, multiple=FALSE)   
  })
  
  ########## user interface to select which measurments to cluster
  output$rcsUI2 = renderUI({
    
    #levels of the measurement variable, save as third variable in the dataset symptomsData
    my.levels=levels(as.factor(dataFiltered()[,input$measurementVar]))
    
    #select the measurement
    selectInput(inputId="measurementSelectedrcs",
                label="Select the measurement (time)", 
                choices=my.levels, selected=my.levels[1])
  })
  
  
  output$plotRCS=renderPlot({
    plotRCS(data.all=dataExtended(),
                         data.yn=dataFiltered.yn(),
                         measurement=Measurement(),
                         selectedSymptoms=input$selectedSymptoms,
                         measurementSelectedrcs=input$measurementSelectedrcs,
                         rcsIDVar=input$rcsIDVar)   
  }, height=1000)
  
  
  
})









