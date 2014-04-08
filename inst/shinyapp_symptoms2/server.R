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
  # TODO: could drawing graphs be done if(no graph){height=0)?
  numRowsTimeline <- function(){if(!is.null(dataFiltered())){
    max(ceiling(length(input$selectedSymptoms))*40, # so that legend is visible
        (dim(dataFiltered()[1])*0.75), # to not compress patient axis too much
        400) # at least this size
  } else {return(0)} # if there is no data, height of plot should be zero
  }
  
  
  numRowsProportions <- function(){if(!is.null(dataFilteredwithThreshold())){
    max(ceiling(length(input$selectedSymptoms))*40,
        # if there are less than cca. 4 measurement occasions,
        # each symptom should get aprox. 40 lines
        length(input$selectedSymptoms)*length(measurementLevels())*15,
        # for more than 4 measurement occasions,
        # this should give extra vertical space for 
        # measurements to be visible
        300 # minumum reserved space
    )}else{return(0)} # height of plot when no data available
  }
  
  numRowsClustering <- function() {if(!is.null(dataFiltered())){
    max(ceiling(length(input$selectedSymptoms))*40,
        300)}else{return(0)}}
  numRowsClustering2 <- function() {if(!is.null(dataFiltered())){
    max(ceiling(length(input$selectedSymptoms))*40,
        300)}else{return(0)}}
  
  numRowsDistributions <- function() {if(!is.null(dataFiltered())){
    max(ceiling(length(input$selectedSymptoms))*30,
        300)}else{return(0)}}
  
  numRowsLogistf <- function() {if(!is.null(dataFiltered())){
    max(ceiling(length(input$selectedSymptoms))*30,
        300)}else{return(0)}}
  
  NumRows <- function(){ceiling(length(input$selectedSymptoms)/3)*300  }
  
  # REACTIVE FUNCTIONS ####
  # Measurement() - values of variable selected as measurement occasion variable ####
  Measurement <- reactive({
    if(!is.null(dataFiltered())){
      dataFiltered()[,input$measurementVar]
    }
  })
  
  # measurementLevels() - the measurement levels available
  measurementLevels <- reactive ({
    if(!is.null(Measurement())){
      sort(unique(Measurement()))
    }
  })
  
  # dataExtended() - data set with all the imported data ####
  dataExtended <- reactive({
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
  
  # dataFiltered() - data set with data only for selected variables ####
  dataFiltered <- reactive({
    if(! (is.null(dataExtended()) || is.null(input$selectedSymptoms) )){
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
    }
  })
  
  # dataFilteredwithThreshold() - filtered data set with threshold value honored #### 
  # sets all symptom values below threshold value to zero
  dataFilteredwithThreshold <- reactive ({
    if(!is.null(dataFiltered())){
      data <- dataFiltered()
      data[,input$selectedSymptoms] <- 
        ifelse(data[, input$selectedSymptoms]>input$thresholdValue, 1, 0)
      return(data)
    }
  })
  
  # dataVariableNames() - returns the names of all column of imported data ####
  dataVariableNames <- reactive({
    if(!is.null(dataExtended())){
      unlist(names(dataExtended()))
    }
  })
  
  # dataFiltered.yn() - dataset with the positive/negative values for the selected symptoms ####
  dataFiltered.yn=reactive({
    if(!is.null(dataFiltered())){
      #apply(symptomsData()[, -c(1:3)], 1, function(x) ifelse(x>input$threshold, 1, 0))
      data=ifelse(dataFiltered()[, input$selectedSymptoms]>input$thresholdValue, 1, 0)
      return(data)
    }
  })
  
  
  # MAINPANEL ####
  # message for working with DEMO data
  #   output$message <- renderText(
  #     if (is.null(input$dataFile)) {paste("WORKING WITH DEMO DATA!")} else {
  #       if (dim(dataExtended())[1]==0){paste("Please select one or more symptoms.")}
  #     })
  
  # SIDEBAR ####
  # GUI - selecting symptoms ####
  output$selectSymptoms <- renderUI({
    if (!is.null(dataVariableNames())) {
      selectInput(inputId="selectedSymptoms",
                  label="Choose variables to analyse:", 
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
    if (!is.null(dataVariableNames())){
      selectInput(inputId="dateVar",
                  label="Choose date variable:", 
                  choices=dataVariableNames(),
                  selected="Date")
    }
  })
  
  # GUI - selecting grouping variable ####
  output$selectGroupingVar <- renderUI({
    if (!is.null(dataVariableNames())){
      selectInput(inputId="groupingVar",
                  label="Choose grouping variable:", 
                  choices=dataVariableNames(),
                  selected="Sex")
    }
  })
  
  # GUI - selecting person ID variable ####
  output$selectPatientIDVar <- renderUI({
    if (!is.null(dataVariableNames())) {
      selectInput(inputId="patientIDVar",
                  label="Choose patient ID variable:", 
                  choices=dataVariableNames(),
                  selected="PersonID")
    }
  })
  
  # GUI - selecting measurements variable
  output$selectMeasurementVar <- renderUI({
    if (!is.null(dataVariableNames())) {
      selectInput(inputId="measurementVar",
                  label="Choose measurement occasion variable:", 
                  choices=dataVariableNames(),
                  selected="Measurement")
    }
  })
  
  # GUI - selecting treshold value
  output$selectThresholdValue <- renderUI({
    if (!is.null(dataVariableNames())){
      numericInput(inputId="thresholdValue",
                   "Threshold for positivity of the variables:",
                   value=0,
                   min=0,
                   max=10)
    }
  })
  
  # TABS ####
  # TAB - Timeline ####
  
#   output$messageSelectVars <- renderText(
#     {if(is.null(dataFiltered())) {
#       print("Please use the sidebar menus on the left to upload data, select parameters and one or more variables to analyse.")}
#   })
#   
  
  output$selectDisplayFormat <- renderUI({
    #if(!is.null(dataFiltered())){
      checkboxInput(inputId="displaySinceInclusion",
                    label="Display time from inclusion in the study on the horizontal axis?",
                    value= FALSE)
    #}
  })
  
  
  output$plotTimeline <- renderPlot({
    if(!is.null(dataFiltered())){
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
      )} else {print("Please select some variables to plot.")}    
  }, height=numRowsTimeline)
  
  # TAB - Distributions of variables ####
  # ui - select measurement occasion ###
  output$proportionUI = renderUI({
    if(!is.null(measurementLevels())){ 
      selectInput(inputId="measurementSelectedProportion",
                  label="Select the measurement occasion (time):", 
                  choices=measurementLevels(), selected=measurementLevels()[1])
    }
  })
  
  # ui - select distribution only for patients above a threshold
  output$selectPosOnly <- renderUI({
    if(!is.null(dataFiltered())){
      tagList(
      checkboxInput(inputId="posOnly",
                    "Display the distribution only for patients with variable values
                  above the selected threshold of positivity?",
                    value = FALSE),
      br(), br()
      )
    }
  })
  
  # plot - proportions ###
  output$plotProportion=renderPlot({
    if(!is.null(dataFiltered.yn())){
      plotDistribution(data=dataFiltered.yn(),
                       selectedSymptoms=input$selectedSymptoms,
                       selectedProportion=input$measurementSelectedProportion,
                       measurements=Measurement())
    }
  }, height=numRowsDistributions)
  
  # plot - boxplots ###
  output$plotBoxplot=renderPlot({
    if(!is.null(dataFiltered())){
      plotDistributionBoxplot(data=dataFiltered(),
                              data.yn=dataFiltered.yn(),
                              selectedSymptoms=input$selectedSymptoms,
                              selectedProportion=input$measurementSelectedProportion,
                              measurements=Measurement(),
                              posOnly=input$posOnly,
                              threshold=input$thresholdValue)
    }
  }, height=numRowsDistributions)
  
  # plot - CI ###
  output$plotCI <- renderPlot({
    if(!is.null(dataFiltered.yn())){
      plotCI(data.yn=dataFiltered.yn(),
             measurements=Measurement(),
             selectedSymptoms=input$selectedSymptoms,
             selectedProportion=input$measurementSelectedProportion)
    }
  }, height=numRowsDistributions)
  
  # table - for all patients - proportions and medians
  output$tablePropMedian <- renderTable({ 
    if(!is.null(dataFiltered())){
      tableAllWithSymptoms(data=dataFiltered(),
                           measurementVar=input$measurementVar,
                           forMeasurement=input$measurementSelectedProportion,
                           symptomsNames=input$selectedSymptoms,
                           thresholdValue=input$thresholdValue)
    }
  })
  
  # text - explainig tableMedianGroups
  output$textTablePropMedian <- renderUI({
    if(!is.null(dataFiltered())){
    tagList(p("Table displays for each variable the proportion of subject with
            positive values of a variable,  median value and interquantile
            range for of the variable (25th to 75th percentile).", br(), br() ))
    }
  })
  
  
  
  # TAB - Distribution of the variables: by grouping variable ####
  # plot - pyramid plot of proportions ###
  output$plotPyramid <- renderPlot ({
    if(!is.null(dataFilteredwithThreshold())){
      plotPropWithSymptoms(data=dataFilteredwithThreshold(),
                           grouping=input$groupingVar,
                           measurements=input$measurementVar,
                           symptomsNames=input$selectedSymptoms)
    }
  } ,height=numRowsProportions)
  
  # ui - user interface to select which measurements to draw tables for ###
  output$UIpropTable = renderUI({
    if(!is.null(measurementLevels())){
      #select the measurement
      selectInput(inputId="measurementSelectedprop",
                  label="Select the measurement occasion (time):", 
                  choices=measurementLevels(), selected=measurementLevels()[1])
    }
  })
  
  # table - of proportions of patients in a group with a symptom
  output$tablePropGroups <- renderTable ({
    if(!is.null(dataFiltered())){
    tablePropWithSymptoms(data=dataFiltered(),
                          groupingVar=input$groupingVar,
                          measurementVar=input$measurementVar,
                          forMeasurement=input$measurementSelectedprop,
                          symptomsNames=input$selectedSymptoms,
                          thresholdValue=input$thresholdValue)
    }
  })
  
  # text - explainig tablePropGroups
  output$textTablePropGroups <- renderUI({
    if(!is.null(dataFiltered())){
    tagList(p("Table displays for each variable the proportion of subjects in a
  certain group, P value for the difference of proportions and the 
  95% confidence interval for the difference of proportions.", br(), br()))
    }
  })
  
  # table - with medians of symptoms values in a group
  output$tableMedianGroups <- renderTable ({
    if(!is.null(dataFiltered())){
      tableMediansWithSymptoms(data=dataFiltered(),
                               groupingVar=input$groupingVar,
                               measurementVar=input$measurementVar,
                               forMeasurement=input$measurementSelectedprop,
                               symptomsNames=input$selectedSymptoms,
                               thresholdValue=input$thresholdValue)
    }
  })
  
  # text - explainig tableMedianGroups
  output$textTableMedianGroups <- renderUI({
    if(!is.null(dataFiltered())){
    tagList(p("Table displays for each variable the median value for subjects in a
certain group, interquantile range for of the variable 
(25th to 75th percentile) and P value for the difference of medians.", br(), br() ))
    }
  })
  
  
  
  # TAB - Clustering ####
  # ui - selection of measurement occasions  ###
  output$clusteringUI = renderUI({
    if(!is.null(measurementLevels())){
      #select the measurement
      selectInput(inputId="selectedMeasurementValue",
                  label="Select the measurement occasion (time):", 
                  choices=measurementLevels(), selected=measurementLevels()[1])
    }
  })
  
  # plot - dendrogram plot on the Clustering tab ###
  output$plotClusterDendrogram=renderPlot({
    if(!is.null(dataFiltered())){
      plotClusterDendrogram(data=dataFiltered(),
                            variableName=input$measurementVar,
                            variableValue=input$selectedMeasurementValue,
                            selectedSymptoms=input$selectedSymptoms)
    }
  },height=numRowsClustering)
  
  
  # ui - selection of annotation variables
  output$selectClusterAnnotations <- renderUI({
    if(!is.null(dataFiltered())){
      selectedSymptoms <- which(dataVariableNames() %in% input$selectedSymptoms)
      selectInput(inputId="selectedClusterAnnotations",
                  label="Select variables for annotating graph:",
                  # TODO: remove some variables from selection
                  choices=dataVariableNames()[-selectedSymptoms],
                  selected=c(input$groupingVar),
                  multiple=TRUE)
    }
  })
  
  
  
  # plot - heatmap plot on the Clustering tab ###
  output$plotClusterHeatmap=renderPlot({
    if(!is.null(dataExtended())){
      plotClusterHeatmap(data=dataExtended(),
                         #TODO: make dependent on selection
                         variableName=input$measurementVar,
                         variableValue=input$selectedMeasurementValue,
                         selectedSymptoms=input$selectedSymptoms,
                         annotationVars=input$selectedClusterAnnotations) 
    }
  },height=numRowsClustering2)
  
  
  
  # TAB - RCS ####
  # ui - user interface to select a numerical variable to associate with the presence of symptom ###
  output$rcsUI= renderUI({
    if(!is.null(dataFiltered())){
      selectInput(inputId="rcsIDVar",
                  label="Numerical variable:", 
                  choices=dataVariableNames(),
                  multiple=FALSE,
                  if (input$dataFileType=="Demo"){selected=c("Age")})   
    }
  })
  
  # ui - user interface to select which measurments to cluster ###
  output$rcsUI2 = renderUI({
    if(!is.null(measurementLevels())){
      #select the measurement
      selectInput(inputId="measurementSelectedrcs",
                  label="Select the measurement occasion (time):", 
                  choices=measurementLevels(), selected=measurementLevels()[1])
    }
  })
  
  # plot - RCS plot ###
  output$plotRCS=renderPlot({
    if(!is.null(dataFiltered())){
    plotRCS(data.all=dataExtended(),
            data.yn=dataFiltered.yn(),
            measurement=Measurement(),
            selectedSymptoms=input$selectedSymptoms,
            measurementSelectedrcs=input$measurementSelectedrcs,
            rcsIDVar=input$rcsIDVar)   
    }
  }, height=NumRows)
  
  ################ association of variables with the outcome using logistic regression with Firth correction
  
  # TAB - Logistf ####
  # ui - user interface to select which measurments to cluster ###
  output$logistfUI = renderUI({
    if(!is.null(measurementLevels())){
      #select the measurement
      selectInput(inputId="measurementSelectedlogistf",
                  label="Select the measurement occasion (time):", 
                  choices=measurementLevels(), selected=measurementLevels()[1])
    }
  })
  
  
  # ui - user interface to select a numerical variable to associate with the presence of symptom ###
  output$logistfUI2= renderUI({
    if(!is.null(dataFiltered())){
      selectInput(inputId="logistfIDVar",
                  label="Select a numeric variable to associate with presence of symptoms:", 
                  choices=dataVariableNames(),
                  if (input$dataFileType=="Demo"){selected=c("Age")})
    }
  })
  
  # plot - logistf ###
  output$plotLogistf <- renderPlot({
    if(!is.null(Measurement())){
      plotLogistf(data=dataExtended(),
                  data.yn=dataFiltered.yn(),
                  measurement=Measurement(),
                  measurementSelectedlogistf=input$measurementSelectedlogistf,
                  logistfIDVar=input$logistfIDVar,
                  selectedSymptoms=input$selectedSymptoms,
                  numSymptoms=length(input$selectedSymptoms))
    }
  }, height=numRowsLogistf)
  
  # TAB - Selected transformed data ####
  # table - list the subseted data in an output slot ###
  output$data <- renderTable({
    if(!is.null(dataFiltered())){
      data <- dataFiltered()
      # TODO: We could render a renderDataTable(), but how to display dates in 
      # format 1.12.2014 and still sort them correctly?
      # Sys.setlocale("LC_TIME", "Slovenian")
      #data[,input$dateVar] <- as.Date(data[,input$dateVar], format="%d.%m.%Y")
      data[,input$dateVar] <- as.character(as.Date(data[,input$dateVar], format="%d.%m.%Y"),
                                           format="%d.%m.%Y")
      
      #data$Date <- as.character(as.Date(data$Date, origin="1899-12-30"),format="%d.%m.%Y")
      
      return(data)
      # NOTE: if we want to render the table of data, we have to convert the dates into 
      # characters, since renderTable seems to use xtable, which seems to not handle
      # dates very well (http://stackoverflow.com/questions/8652674/r-xtable-and-dates)
    }
  })
  
  # TAB - Debug ####
  # table - debuging information ###
  output$debug <- renderPrint(paste(str(dataExtended())))
  
  
  ### TEMP CODE ####
  #  
  #
  
 
  
  
  
})









