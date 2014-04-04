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
# library for manipulating data
library(plyr)
# library for clustering
library(pheatmap)
# load medplot library
library(medplot)
#library for date management
library(lubridate)
# library for regression modeling 
library(rms)
# library for logistic regression modeling
library(logistf)

# TEMP for debuging
# source("C:/Users/Crt Ahlin/Documents/Dropbox/medplot_package/R/TablePropWithSymptoms.r")

# Main function -----------------------------------------------------------

shinyServer(function(input, output, session) {
  
  # FUNCTIONS ####
  #how much space should be used for the graphical output of the Rcs estimates and others?  
  numRowsTimeline <- function(){max(ceiling(length(input$selectedSymptoms))*40, # so that legend is visible
                                    (dim(dataFiltered()[1])*0.75), # to not compress patient axis too much
                                    400 # at least this size
  )}
  
  NumRows <- function(){ceiling(length(input$selectedSymptoms)/3)*300  }
  
  # REACTIVE FUNCTIONS ####
  # reactive - values of variable selected as measurement occasion variable ####
  Measurement <- reactive({
    dataFiltered()[,input$measurementVar]
  })
  
  # reactive - the measurement levels available
  measurementLevels <- reactive ({
    sort(unique(Measurement()))
    })
  
  # reactive - data set with all the imported data ####
  dataExtended <- reactive( function() {
    observe(input$dataFile)
    # TODO: napiši kodo za scenarij, ko input$datafile ni null, ampak
    # samo ni v pravem formatu - tudi takrat naj vrne NULL
    if (input$dataFileType=="Demo") {
      # commented code for importing Excel DEMO file
      #       templateLocation <- paste0(path.package("medplot"),"/extdata/PlotSymptoms_shiny.xlsx")
      #       patients <- importSymptomsPatients(datafile=templateLocation)
      #       symptoms <- importSymptomsData(datafile=templateLocation,
      #                                      format="Excel")
      #       data <- join(x=symptoms, y=patients, by="PersonID", type="inner")
      templateLocation <- paste0(path.package("medplot"),"/extdata/DataEM.txt")
      data <- importSymptomsData(datafile=templateLocation,
                                 format="TSV")
      return(data)
    }
    
    if(is.null(input$dataFile)) return()
    if (input$dataFileType=="Excel") {
      patients <- importSymptomsPatients(datafile=input$dataFile$datapath)
      symptoms <- importSymptomsData(datafile=input$dataFile$datapath,
                                     format="Excel")
      data <- join(x=symptoms, y=patients, by="PersonID", type="inner")
    }
    
    if (input$dataFileType=="TSV") {
      data <- importSymptomsData(datafile=input$dataFile$datapath,
                                 format="TSV")
    }
    return(data)
  })
  
  # reactive - data set with data only for selected variables ####
  dataFiltered <- reactive(function(){
    data <- dataExtended()[ , 
                           c(input$patientIDVar,
                             input$groupingVar,
                             input$dateVar,
                             input$measurementVar,
                             input$selectedSymptoms
                           )]
    # try to convert dates into R format
    try(expr={data[input$dateVar] <- as.Date(data[,input$dateVar], format="%d.%m.%Y")}, silent=TRUE)
    return(data)
  })
  
  # reactive - returns the names of all column of imported data ####
  dataVariableNames <- reactive(function(){
    unlist(names(dataExtended()))
  })
  
  # reactive - dataset with the positive/negative values for the selected symptoms ####
  dataFiltered.yn=reactive({
    #apply(symptomsData()[, -c(1:3)], 1, function(x) ifelse(x>input$threshold, 1, 0))
    data=ifelse(dataFiltered()[, input$selectedSymptoms]>input$thresholdValue, 1, 0)
    return(data)
  })
  
  
  # MAINPANEL ####
  # message for working with DEMO data
  output$message <- renderText(
    if (is.null(input$dataFile)) {paste("WORKING WITH DEMO DATA!")} else {
      if (dim(dataExtended())[1]==0){paste("Please select one or more symptoms.")}
    })
  
  # SIDEBAR ####
  # GUI - selecting symptoms ####
  output$selectSymptoms <- renderUI({
    if (!is.null(dataVariableNames())) {
      selectInput(inputId="selectedSymptoms",
                  label="Choose symptoms:", 
                  choices=dataVariableNames(),
                  multiple=TRUE,
                  if (input$dataFileType=="Demo"){selected=c("Fatigue","Malaise",
                                                             "Arthralgia","Headache",
                                                             "Myalgia","Back.C",
                                                             "Dizziness", "Nausea",
                                                             "Sleepiness", "Forgetfulness",
                                                             "Concentration", "Paresthesias",
                                                             "Irritability","Back.L",
                                                             "Back.Th", "Insomnia")})}
    })
  
  # GUI - selecting Date variable ####
  output$selectDateVar <- renderUI({
    selectInput(inputId="dateVar",
                label="Choose date variable:", 
                choices=dataVariableNames(),
                selected="Date")
  })
  
  # GUI - selecting grouping variable ####
  output$selectGroupingVar <- renderUI({
    selectInput(inputId="groupingVar",
                label="Choose grouping variable:", 
                choices=dataVariableNames(),
                selected="Sex")
  })
  
  # GUI - selecting person ID variable ####
  output$selectPatientIDVar <- renderUI({
    selectInput(inputId="patientIDVar",
                label="Choose patient ID variable:", 
                choices=dataVariableNames(),
                selected="PersonID")
  })
  
  # GUI - selecting measurements variable
  output$selectMeasurementVar <- renderUI({
    selectInput(inputId="measurementVar",
                label="Choose measurument occasion variable:", 
                choices=dataVariableNames(),
                selected="Measurement")
  })
  
  # TAB - Timeline ####
  output$plotTimeline <- renderPlot(function() {
    data=dataFiltered()
    # observe({dataFiltered()})
    # if no symbols are selected, do not plot
    #if (dim(dataFiltered())[1]>0) {
    print(plotSymptomsTimeline(data=data,
                               date=input$dateVar,
                               personID=input$patientIDVar,
                               measurement=input$measurementVar,
                               symptoms=input$selectedSymptoms,
                               displaySinceInclusion = input$displaySinceInclusion)
    )#}    
  }, height=numRowsTimeline)
  
  # TAB - Proportions ####
  # plot - pyramid plot of proportions ###
  output$plotPyramid <- renderPlot ({
    plotPropWithSymptoms(data=dataFiltered(),
                         grouping=input$groupingVar,
                         measurements=input$measurementVar,
                         symptomsNames=input$selectedSymptoms) #unlist(levels(data()[,"variable"]))
    # TODO: adapt the height of the figure based on the number of symptoms: does not work now>>>>
    # CRT: it seems that heaight will also have to be set in plotOutput, otherwise the graph might not fit
    # and window becomes scrollable (anoying)
  } ,height="auto")  #height=NumRows)
  
  # ui - user interface to select which measurements to draw tables for ###
  output$UIpropTable = renderUI({
    #select the measurement
    selectInput(inputId="measurementSelectedprop",
                label="Select the measurement (time)", 
                choices=Measurement(), selected=NULL)
  })
  
  # table - of proportions of patients in a group with a symptom
  output$tablePyramid <- renderTable ({
    tablePropWithSymptoms(data=dataFiltered(),
                          groupingVar=input$groupingVar,
                          measurementVar=input$measurementVar,
                          forMeasurement=input$measurementSelectedprop,
                          symptomsNames=input$selectedSymptoms,
                          thresholdValue=input$thresholdValue)
  })
  
  # table - with medians of symptoms values in a group
  output$tablePyramid2 <- renderTable ({
    tableMediansWithSymptoms(data=dataFiltered(),
                             groupingVar=input$groupingVar,
                             measurementVar=input$measurementVar,
                             forMeasurement=input$measurementSelectedprop,
                             symptomsNames=input$selectedSymptoms,
                             thresholdValue=input$thresholdValue)
  })
  
  # table - for all patients - proportions and medians
  output$tablePyramid3 <- renderTable({ 
    tableAllWithSymptoms(data=dataFiltered(),
                         measurementVar=input$measurementVar,
                         forMeasurement=input$measurementSelectedprop,
                         symptomsNames=input$selectedSymptoms,
                         thresholdValue=input$thresholdValue)
  })
  
  
  # TAB - Clustering ####
  # ui - selection of measurement occasions  ###
  output$clusteringUI = renderUI({
    #levels of the measurement variable, save as third variable in the dataset symptomsData
    # TODO: make selection of levels dependent on Sidebar, not fixed to "Measurement"
    myLevels=levels(as.factor(dataFiltered()[,"Measurement"]))
    #select the measurement
    selectInput(inputId="selectedMeasurementValue",
                label="Select the measurement occasion (time):", 
                choices=myLevels, selected=myLevels[1])
  })
  
  # plot - dendrogram plot on the Clustering tab ###
  output$plotClusterDendrogram=renderPlot({
    plotClusterDendrogram(data=dataFiltered(),
                          variableName=input$measurementVar,
                          variableValue=input$selectedMeasurementValue,
                          selectedSymptoms=input$selectedSymptoms)
  })
  
  
  # ui - selection of annotation variables
  output$selectClusterAnnotations <- renderUI({
    selectedSymptoms <- which(dataVariableNames() %in% input$selectedSymptoms)
    selectInput(inputId="selectedClusterAnnotations",
                label="Select variables for annotating graph:",
                # TODO: remove some variables from selection
                choices=dataVariableNames()[-selectedSymptoms],
                selected=c(input$groupingVar),
                multiple=TRUE)
  })
  
  
  
  # plot - heatmap plot on the Clustering tab ###
  output$plotClusterHeatmap=renderPlot({
    plotClusterHeatmap(data=dataExtended(),
                       #TODO: make dependent on selection
                       variableName=input$measurementVar,
                       variableValue=input$selectedMeasurementValue,
                       selectedSymptoms=input$selectedSymptoms,
                       annotationVars=input$selectedClusterAnnotations) 
  })
  
  
  # TAB - Distributions of symptoms ####
  # ui - select measurement occasion ###
  output$proportionUI = renderUI({
    selectInput(inputId="measurementSelectedProportion",
                label="Select the measurement (time)", 
                choices=measurementLevels(), selected=measurementLevels()[1])
  })
  
  # plot - proportions ###
  output$plotProportion=renderPlot({
    plotDistribution(data=dataFiltered.yn(),
                     selectedSymptoms=input$selectedSymptoms,
                     selectedProportion=input$measurementSelectedProportion,
                     measurements=Measurement())
  })
  
  # plot - boxplots ###
  output$plotBoxplot=renderPlot({
    plotDistributionBoxplot(data=dataFiltered(),
                            data.yn=dataFiltered.yn(),
                            selectedSymptoms=input$selectedSymptoms,
                            selectedProportion=input$measurementSelectedProportion,
                            measurements=Measurement(),
                            posOnly=input$posOnly,
                            threshold=input$thresholdValue)
  })
  
  # plot - CI ###
  output$plotCI <- renderPlot({
    plotCI(data.yn=dataFiltered.yn(),
           measurements=Measurement(),
           selectedSymptoms=input$selectedSymptoms,
           selectedProportion=input$measurementSelectedProportion)
  })
  
  # TAB - RCS ####
  # ui - user interface to select a numerical variable to associate with the presence of symptom ###
  output$rcsUI= renderUI({
    print("UI for rcs variable selection")
    selectInput(inputId="rcsIDVar",
                label="Numerical variable", 
                choices=dataVariableNames(),
                multiple=FALSE,
                if (input$dataFileType=="Demo"){selected=c("Age")})   
  })
  
  # ui - user interface to select which measurments to cluster ###
  output$rcsUI2 = renderUI({
    #select the measurement
    selectInput(inputId="measurementSelectedrcs",
                label="Select the measurement (time)", 
                choices=measurementLevels(), selected=measurementLevels()[1])
  })
  
  # plot - RCS plot ###
  output$plotRCS=renderPlot({
    plotRCS(data.all=dataExtended(),
            data.yn=dataFiltered.yn(),
            measurement=Measurement(),
            selectedSymptoms=input$selectedSymptoms,
            measurementSelectedrcs=input$measurementSelectedrcs,
            rcsIDVar=input$rcsIDVar)   
  }, height=NumRows)
  
  ################ association of variables with the outcome using logistic regression with Firth correction
  
  # TAB - Logistf ####
  # ui - user interface to select which measurments to cluster ###
  output$logistfUI = renderUI({
    #select the measurement
    selectInput(inputId="measurementSelectedlogistf",
                label="Select the measurement (time)", 
                choices=measurementLevels(), selected=measurementLevels()[1])
  })
  
  
  # ui - user interface to select a numerical variable to associate with the presence of symptom ###
  output$logistfUI2= renderUI({
    selectInput(inputId="logistfIDVar",
                label="Select a numeric variable to associate with presence of symptoms:", 
                choices=dataVariableNames(),
                if (input$dataFileType=="Demo"){selected=c("Age")})
  })
  
  # plot - logistf ###
  output$plotLogistf <- renderPlot({
    plotLogistf(data=dataExtended(),
                data.yn=dataFiltered.yn(),
                measurement=Measurement(),
                measurementSelectedlogistf=input$measurementSelectedlogistf,
                logistfIDVar=input$logistfIDVar,
                selectedSymptoms=input$selectedSymptoms,
                numSymptoms=length(input$selectedSymptoms))
  })
  
  # TAB - Selected transformed data ####
  # table - list the subseted data in an output slot ###
  output$data <- renderTable({
    data <- dataExtended()
    data$Date <- as.character(as.Date(data$Date, origin="1899-12-30"),format="%d.%m.%Y")
    return(data)
    # NOTE: if we want to render the table of data, we have to convert the dates into 
    # characters, since renderTable seems to use xtable, which seems to not handle
    # dates very well (http://stackoverflow.com/questions/8652674/r-xtable-and-dates)
  })
  
  # TAB - Debug ####
  # table - debuging information ###
  output$debug <- renderTable(dataFiltered.yn())
  
  
  ### TEMP CODE ####
  #  
  #
  
  
  
  
  
})









