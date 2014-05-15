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
# library for permutation tests
library(permute)
# libraries for mixed models
library(lme4)
library(lmerTest)

# TEMP for debuging
# source("C:/Users/Crt Ahlin/Documents/Dropbox/medplot_package/R/TablePropWithSymptoms.r")

# Main function -----------------------------------------------------------

shinyServer(function(input, output, session) {
  
  # FUNCTIONS ####
  #how much space should be used for the graphical output of the Rcs estimates and others?  
  # TODO: could drawing graphs be done if(no graph){height=0)?
  numRowsTimeline <- function(){if(!is.null(dataFiltered())){
    if(input$treatasBinary==TRUE) {return(0)} # no graph if not possible to draw
    max(ceiling(length(input$selectedSymptoms))*40, # so that legend is visible
        (dim(dataFiltered()[1])*0.75), # to not compress patient axis too much
        400) # at least this size
  } else {return(0)} # if there is no data, height of plot should be zero
  }
  
  numRowsTimelineProfile <- function(){if(!is.null(input$selectedGraphType)){
    if(input$treatasBinary==TRUE) {return(0)} # no graph if not possible to draw
    if(input$selectedGraphType=="multipleGraphs") { # case of multiple graphs per one variable
      uniqueSubjects <- unique(dataFiltered()[input$patientIDVar])
      numSubjects <- dim(uniqueSubjects)[1]
      numGroups <- ceiling(numSubjects/input$groupSize)
      size <- (numGroups*length(input$selectedSymptoms)*300)
      # Cairo graphics has some limit on the max. num of lines,
      # apparently it is somewhere below 38000?)
      return(min(size, 30000)) 
    }
    if(input$selectedGraphType=="oneGraph" || input$selectedGraphType=="randomSample") {
      size <- max(ceiling(length(input$selectedSymptoms))*300, # so that legend is visible
                  (dim(dataFiltered()[1])*0.75), # to not compress patient axis too much
                  400) # at least this size
      return(size)
    }
  } else {return(0)} # if there is no data, height of plot should be zero
  }
  
  numRowsTimelineBoxplots <- function(){if(!is.null(dataFiltered())){
    if(input$treatasBinary==TRUE) {return(0)} 
    tmp <- max(ceiling(length(input$selectedSymptoms))*200,
               300) # minumum reserved space
    return(tmp)
    }else{return(0)} # height of plot when no data available
  }
  
  
  numRowsProportions <- function(){
    if(!is.null(dataFilteredwithThreshold())){
    if(input$treatasBinary==FALSE) {return(0)} 
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
  
  numRowsProportionsCI<- function(){
    try({
    if(!is.null(dataFilteredwithThreshold())){
    if(input$treatasBinary==FALSE) {return(0)} 
    max(ceiling(length(input$selectedSymptoms))*80,
        # if there are less than cca. 4 measurement occasions,
        # each symptom should get aprox. 40 lines
        length(input$selectedSymptoms)*length(measurementLevels())*20,
        # for more than 4 measurement occasions,
        # this should give extra vertical space for 
        # measurements to be visible
        300 # minumum reserved space
    )}else{return(0)} # height of plot when no data available
    }, silent=TRUE)
  }
  
  numRowsClustering <- function() {if(!is.null(dataFiltered())){
    if(input$treatasBinary==TRUE) {return(0)}
    max(ceiling(length(input$selectedSymptoms))*40,
        300)}else{return(0)}}
  
  numRowsClustering2 <- function() {if(!is.null(dataFiltered())){
    if(input$treatasBinary==TRUE) {return(0)}
    max(ceiling(length(input$selectedSymptoms))*40,
        300)}else{return(0)}}
  
  numRowsDistributions <- function() {if(!(is.null(dataFiltered()) || is.null(input$posOnly)) ){
    if(input$treatasBinary==FALSE){return(0)}
    max(ceiling(length(input$selectedSymptoms))*30,
        300)}else{return(0)}}
  
  numRowsLogistf <- function() {if(!is.null(dataFiltered())){
    if(input$treatasBinary==FALSE){return(0)}
    max(ceiling(length(input$selectedSymptoms))*30,
        300)}else{return(0)}}
  
  NumRows <- function(){if(!is.null(dataFiltered())){
    #if(input$treatasBinary==FALSE){return(0)}
    ceiling(length(input$selectedSymptoms)/3)*300  }else{return(0)} }
  
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
    if(!(is.null(dataFiltered()) || is.null(input$thresholdValue)  )){
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
    if( !( is.null(dataFiltered()) | is.null(input$thresholdValue) )) {
      #apply(symptomsData()[, -c(1:3)], 1, function(x) ifelse(x>input$threshold, 1, 0))
      data=ifelse(dataFiltered()[, input$selectedSymptoms, drop=FALSE]>input$thresholdValue, 1, 0)
      return(data)
    } else {return(NULL)}
  })
  
  
  # MAINPANEL ####
  # message for working with DEMO data
  #   output$message <- renderText(
  #     if (is.null(input$dataFile)) {paste("WORKING WITH DEMO DATA!")} else {
  #       if (dim(dataExtended())[1]==0){paste("Please select one or more symptoms.")}
  #     })
  
  # SIDEBAR ####
  
  # GUI - printing medpot package version
  output$medplotVersion <- renderText({
    paste("Version:",packageVersion(pkg="medplot"))
  })
  
  
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
                  label="Choose subject ID variable:", 
                  choices=dataVariableNames(),
                  selected="PersonID")
    }
  })
  
  # GUI - selecting measurements variable ####
  output$selectMeasurementVar <- renderUI({
    if (!is.null(dataVariableNames())) {
      selectInput(inputId="measurementVar",
                  label="Choose measurement occasion variable:", 
                  choices=dataVariableNames(),
                  selected="Measurement")
    }
  })
  
  # GUI - selecting use of thresholding ####
  output$selectTreatasBinary <- renderUI({
    if (!is.null(dataVariableNames())){
      checkboxInput(inputId="treatasBinary",
                    label="Treat and analyse variables as binary?",
                    value=FALSE)
    }  
    
  })
  
  # GUI - selecting treshold value ####
  output$selectThresholdValue <- renderUI({
    if (!is.null(dataVariableNames()) & !is.null(input$treatasBinary)){
      if(input$treatasBinary==TRUE) {
        numericInput(inputId="thresholdValue",
                     "Threshold for positivity of the variables:",
                     value=0,
                     min=0,
                     max=10)
      }
    }
  })
  
  # GUI - reseting threshold value
  observe({
    input$treatasBinary
    updateNumericInput(session, inputId="thresholdValue", value=0)
  })
  
  # TABS ####
  # message - used on all tabs
  output$messageSelectVars <- renderUI(
{if(is.null(dataFiltered())) {h4("Please use the menus below to upload data, select parameters and one or more variables to analyse.")}
})

# TAB - Data summary ####
output$dataSummary <- renderPrint({
  if(!is.null(dataFiltered())) {
    summarizeData(data=dataFiltered(),
                  personIDVar=input$patientIDVar,
                  measurementVar=input$measurementVar,
                  selectedSymptoms=input$selectedSymptoms,
                  groupingVar=input$groupingVar
    )
  }
})

# TAB - Timeline ####
output$selectDisplayFormat <- renderUI({
  if(!is.null(dataFiltered())){
    if(input$treatasBinary==FALSE){
      selectInput(inputId="displayFormat",
                  label="Choose what to display on the horizontal axis:",
                  choices=c("Dates" = "dates",
                            "Time from inclusion" ="timeFromInclusion",
                            "Measurement occasions" = "measurementOccasions"),
                  selected="dates",
                  multiple=FALSE)
    }
  }
})


output$plotTimeline <- renderPlot({
  if(!(is.null(dataFiltered()) || is.null(input$displayFormat))){
    if (input$treatasBinary==FALSE) { 
      data=dataFiltered()
      # observe({dataFiltered()})
      # if no symbols are selected, do not plot
      #if (dim(dataFiltered())[1]>0) {
      print(plotSymptomsTimeline(data=data,
                                 date=input$dateVar,
                                 personID=input$patientIDVar,
                                 measurement=input$measurementVar,
                                 symptoms=input$selectedSymptoms,
                                 displayFormat = input$displayFormat)
      )}}   
}, height=numRowsTimeline)


output$messageNotAppropriate <- renderText({
  if(!is.null(input$treatasBinary)){
  if (input$treatasBinary==TRUE) {
    "This type of analysis is not appropriate for binary responses."
  }}
})

# TAB - Distribution of the variables: over time ####
# ui - select type of graph
    output$selectGraphType <- renderUI({
      if(!is.null(dataFiltered())) {
        if(input$treatasBinary==FALSE){
        selectInput(inputId="selectedGraphType",
                    label="Select type of graphs to plot:",
                    choices=c("All subjects on one graph"="oneGraph",
                              "Random selection of subjects on one graph"="randomSample",
                              "Multiple graphs per variable"="multipleGraphs"),
                    selected="randomSample",
                    multiple=FALSE)
        }
      }
      })
    
    output$selectRandomSampleSize <- renderUI({
      if(!is.null(input$selectedGraphType)) {
        if (input$selectedGraphType=="randomSample") {
          if(input$treatasBinary==FALSE){
          numericInput(inputId="sampleSize",
                       label="Select number of randomly selected subjects:",
                       value=10,
                       min=1,
                       max=100,
                       step=5)
          }
        }}
    })
    
    
    output$selectMaxGroupSize <- renderUI({
      if(!is.null(input$selectedGraphType)) {
        if (input$selectedGraphType=="multipleGraphs") {
          if(input$treatasBinary==FALSE){
          numericInput(inputId="groupSize",
                       label="Select the maximum number of subjects on one graph:",
                       value=25,
                       min=10,
                       max=100,
                       step=5)
          }
        }}
      })
    output$plotTimelineProfiles <- renderPlot({
      if(!is.null(input$selectedGraphType)) {
        if ( (input$selectedGraphType=="oneGraph") ||
               (input$selectedGraphType=="randomSample" && !is.null(input$sampleSize)) ||
               (input$selectedGraphType=="multipleGraphs" && !is.null(input$groupSize))) {
          if(input$treatasBinary==FALSE){
          print(plotTimelineProfiles(data=dataFiltered(),
                                     plotType=input$selectedGraphType,
                                     personIDVar=input$patientIDVar,
                                     measurementVar=input$measurementVar,
                                     selectedSymptoms=input$selectedSymptoms,
                                     sizeofRandomSample=input$sampleSize,
                                     sizeofGroup=input$groupSize))
          }
        }}
      }, height=numRowsTimelineProfile)

output$messageNotAppropriate2 <- renderText({
  if(!is.null(input$treatasBinary)){
    if (input$treatasBinary==TRUE) {
      "This type of analysis is not appropriate for binary responses."
    }}
})

# TAB - Distr. of the vars.: over time - boxplots ####
output$plotTimelineBoxplots <- renderPlot({
  if(!is.null(dataFiltered())) {
    if(input$treatasBinary==FALSE){
    print(plotTimelineBoxplots(data=dataFiltered(),
                               personIDVar=input$patientIDVar,
                               measurementVar=input$measurementVar,
                               selectedSymptoms=input$selectedSymptoms)
    )
    }
  } else {return()}
},height=numRowsTimelineBoxplots)

output$tableforBoxplots <- renderUI({
  if(!is.null(dataFiltered())) {
    if(input$treatasBinary==FALSE){
  
  out <- tabelizeBoxplots(measurements=Measurement(),
                          measurementVar=input$measurementVar,
                          data=dataFiltered(),
                          selectedSymptoms=input$selectedSymptoms) 
  
  return(div(HTML(out),class="shiny-html-output"))
}}
  })

output$messageNotAppropriate3 <- renderText({
  if(!is.null(input$treatasBinary)){
    if (input$treatasBinary==TRUE) {
      "This type of analysis is not appropriate for binary responses."
    }}
})

# TAB - Distributions of variables ####
# ui - select measurement occasion ###
output$proportionUI = renderUI({
  if(!(is.null(measurementLevels()) || is.null(measurementLevels())  )){ 
    if(input$treatasBinary==TRUE){
    selectInput(inputId="measurementSelectedProportion",
                label="Select the measurement occasion (time):", 
                choices=measurementLevels(), selected=measurementLevels()[1])
    }
  }
})

# ui - select distribution only for patients above a threshold
output$selectPosOnly <- renderUI({
  if(!is.null(dataFiltered())){
    if(input$treatasBinary==TRUE){
    tagList(
      checkboxInput(inputId="posOnly",
                    "Display the distribution only for patients with variable values
                  above the selected threshold of positivity?",
                    value = FALSE),
      br(), br()
    )
    }
  }
})

# plot - proportions ###
output$plotProportion=renderPlot({
  if(!(is.null(dataFiltered.yn()) || is.null(input$measurementSelectedProportion) )){
    if(input$treatasBinary==TRUE){
          plotDistribution(data=dataFiltered.yn(),
                     selectedSymptoms=input$selectedSymptoms,
                     selectedProportion=input$measurementSelectedProportion,
                     measurements=Measurement())
    }
  }
}, height=numRowsDistributions)

# plot - boxplots ###
output$plotBoxplot=renderPlot({
  if(!(is.null(dataFiltered()) || is.null(input$posOnly)  )){
    if(input$treatasBinary==TRUE){
      
    plotDistributionBoxplot(data=dataFiltered(),
                            data.yn=dataFiltered.yn(),
                            selectedSymptoms=input$selectedSymptoms,
                            selectedProportion=input$measurementSelectedProportion,
                            measurements=Measurement(),
                            posOnly=input$posOnly,
                            threshold=input$thresholdValue)
    }
  }
}, height=numRowsDistributions)

# plot - CI ###
output$plotCI <- renderPlot({
  if(!(is.null(dataFiltered.yn()) ||
         is.null(input$measurementSelectedProportion) ||
         is.null(Measurement()) ||
         is.null(input$selectedSymptoms) )){
    if(input$treatasBinary==TRUE){
      
    plotCI(data.yn=dataFiltered.yn(),
           measurements=Measurement(),
           selectedSymptoms=input$selectedSymptoms,
           selectedProportion=input$measurementSelectedProportion)
    }
  }
}, height=numRowsDistributions)

# table - for all patients - proportions and medians
output$tablePropMedian <- renderTable({ 
  if(!( is.null(dataFiltered()) || is.null(input$measurementSelectedProportion) )){
    if(input$treatasBinary==TRUE){
      
    tableAllWithSymptoms(data=dataFiltered(),
                         measurementVar=input$measurementVar,
                         forMeasurement=input$measurementSelectedProportion,
                         symptomsNames=input$selectedSymptoms,
                         thresholdValue=input$thresholdValue)
    }
  }
})

# text - explainig tableMedianGroups
output$textTablePropMedian <- renderUI({
  if(!is.null(dataFiltered())){
    if(input$treatasBinary==TRUE){
      
    tagList(p("The table displays for each variable the proportion and number of subject with
            positive values for each variable, and the 95% confidence interval for the proportion (based on binomial distribution).  
            For numerical variables: the table displays the median value and interquantile
            range for of the variable (25th to 75th percentile) and the 95% confidence interval for the median (evaluated using 
              the bootstrap percentile method based on 2000 bootstrap iterations).", br(), br() ))
    }
  }
})

output$messageNotAppropriate4 <- renderText({
  if(!is.null(input$treatasBinary)){
    if (input$treatasBinary==FALSE) {
      "This type of analysis is not appropriate for numerical responses."
    }}
})


# TAB - Distribution of the variables: by grouping variable ####
# plot - pyramid plot of proportions ###
output$plotPyramid <- renderPlot ({
  try({
  if(!(is.null(dataFilteredwithThreshold()) || is.null(input$treatasBinary) )){
    if(input$treatasBinary==TRUE){
    
      plotPropWithSymptoms(data=dataFilteredwithThreshold(),
                         grouping=input$groupingVar,
                         measurements=input$measurementVar,
                         symptomsNames=input$selectedSymptoms)
    
    }
  }
  }, silent=TRUE)
} ,height=numRowsProportions)

# plot - plot of proportions with conf. intervals
output$plotPropCIs <- renderPlot ({
  try({
  if(!is.null(dataFilteredwithThreshold())){
    if(input$treatasBinary==TRUE){
    print(
      plotPropWithSymptomsCI(data=dataFilteredwithThreshold(),
                             groupingVar=input$groupingVar,
                             measurementVar=input$measurementVar,
                             selectedSymptoms=input$selectedSymptoms)
    )
    }
  }
  }, silent=TRUE)
} ,height=numRowsProportionsCI)


# ui - user interface to select which measurements to draw tables for ###
output$UIpropTable = renderUI({
  if(!is.null(measurementLevels())){
    if(input$treatasBinary==TRUE){
    #select the measurement
    selectInput(inputId="measurementSelectedprop",
                label="Select the measurement occasion (time):", 
                choices=measurementLevels(), selected=measurementLevels()[1])
    }
  }
})

# table - of proportions of patients in a group with a symptom
output$tablePropGroups <- renderTable ({
  
  if(!(is.null(dataFiltered()) || is.null(input$thresholdValue)  )){
    if(input$treatasBinary==TRUE){
    tablePropWithSymptoms(data=dataFiltered(),
                          groupingVar=input$groupingVar,
                          measurementVar=input$measurementVar,
                          forMeasurement=input$measurementSelectedprop,
                          symptomsNames=input$selectedSymptoms,
                          thresholdValue=input$thresholdValue)
    }
  }
  
})

# text - explainig tablePropGroups
output$textTablePropGroups <- renderUI({
  if(!is.null(dataFiltered())){
    if(input$treatasBinary==TRUE){
    tagList(p("Table displays for each variable the proportion of subjects in a
  certain group, P value for the difference of proportions and the 
  95% confidence interval for the difference of proportions. 
              Data with missing values for grouping variable 
              are removed from analysis.", br(), br()))
    }
  }
})

# table - with medians of symptoms values in a group
output$tableMedianGroups <- renderTable ({
  if(!(is.null(dataFiltered()) || is.null(input$measurementSelectedprop) )){
    if(input$treatasBinary==TRUE){
    tableMediansWithSymptoms(data=dataFiltered(),
                             groupingVar=input$groupingVar,
                             measurementVar=input$measurementVar,
                             forMeasurement=input$measurementSelectedprop,
                             symptomsNames=input$selectedSymptoms,
                             thresholdValue=input$thresholdValue)
    }
  }
})

# text - explainig tableMedianGroups
output$textTableMedianGroups <- renderUI({
  if(!is.null(dataFiltered())){
    if(input$treatasBinary==TRUE){
    tagList(p("Table displays for each variable the median value for subjects in a
certain group, interquantile range for of the variable 
(25th to 75th percentile)and P value for the difference of samples (Mann-Whitney test). 
              Data with missing values for grouping variable 
              are removed from analysis. Threshold for positivity of variables is not taken into account.", br(), br() ))
    }
  }
})

output$messageNotAppropriate5 <- renderText({
  if(!is.null(input$treatasBinary)){
    if (input$treatasBinary==FALSE) {
      "This type of analysis is not appropriate for numerical responses."
    }}
})

# TAB - Clustering ####
# ui - selection of measurement occasions  ###
output$clusteringUI = renderUI({
  if(!(is.null(measurementLevels()) || is.null(measurementLevels()) )){
    if(input$treatasBinary==FALSE){
    #select the measurement
    selectInput(inputId="selectedMeasurementValue",
                label="Select the measurement occasion (time):", 
                choices=measurementLevels(), selected=measurementLevels()[1])
    }
  }
})

# plot - dendrogram plot on the Clustering tab ###
output$plotClusterDendrogram=renderPlot({
  if(!(is.null(dataFiltered()) || is.null(input$selectedMeasurementValue) )){
    if(input$treatasBinary==FALSE){
    plotClusterDendrogram(data=dataFiltered(),
                          variableName=input$measurementVar,
                          variableValue=input$selectedMeasurementValue,
                          selectedSymptoms=input$selectedSymptoms)
    }
  }
},height=numRowsClustering)


# ui - selection of annotation variables
output$selectClusterAnnotations <- renderUI({
  if(!is.null(dataFiltered())){
    if(input$treatasBinary==FALSE){
    selectedSymptoms <- which(dataVariableNames() %in% input$selectedSymptoms)
    selectInput(inputId="selectedClusterAnnotations",
                label="Select variables for annotating graph:",
                # TODO: remove some variables from selection
                choices=dataVariableNames()[-selectedSymptoms],
                selected=c(input$groupingVar),
                multiple=TRUE)
    }
  }
})



# plot - heatmap plot on the Clustering tab ###
output$plotClusterHeatmap=renderPlot({
  if(!is.null(dataExtended())){
    if(input$treatasBinary==FALSE){
    plotClusterHeatmap(data=dataExtended(),
                       #TODO: make dependent on selection
                       variableName=input$measurementVar,
                       variableValue=input$selectedMeasurementValue,
                       selectedSymptoms=input$selectedSymptoms,
                       annotationVars=input$selectedClusterAnnotations) 
    }
  }
},height=numRowsClustering2)

output$messageNotAppropriate6 <- renderText({
  if(!is.null(input$treatasBinary)){
    if (input$treatasBinary==TRUE) {
      "This type of analysis is not appropriate for binary responses."
    }}
})

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
 
  if(!(is.null(dataFiltered()) || is.null(input$measurementSelectedrcs) )){
    plotRCS(data.all=dataExtended(),
            data.yn=dataFiltered.yn(),
            measurement=Measurement(),
            selectedSymptoms=input$selectedSymptoms,
            measurementSelectedrcs=input$measurementSelectedrcs,
            rcsIDVar=input$rcsIDVar,
            binaryVar=input$treatasBinary)   
   }
}, height=NumRows)

output$messageNotAppropriate7 <- renderText({
  if(!is.null(input$treatasBinary)){
    if (input$treatasBinary==FALSE) {
      "This type of analysis is not appropriate for numerical responses."
    }}
})

# table - RCS table ###
output$tableRCS <- renderTable({
  
  if(!(is.null(dataFiltered()) || is.null(input$measurementSelectedrcs) )){
    #if(input$treatasBinary==TRUE){
      tabelizeRCS(data.all=dataExtended(),
                  data.yn=dataFiltered.yn(),
                  measurement=Measurement(),
                  selectedSymptoms=input$selectedSymptoms,
                  measurementSelectedrcs=input$measurementSelectedrcs,
                  rcsIDVar=input$rcsIDVar, 
				  binaryVar=input$treatasBinary
				  )
    #}
	}
})

################ association of variables with the outcome using logistic regression with Firth correction

# TAB - Logistf ####
# ui - user interface to select which measurments to cluster ###
output$logistfUI = renderUI({
  if(!is.null(measurementLevels())){
    if (input$treatasBinary==TRUE) {
    #select the measurement
    selectInput(inputId="measurementSelectedlogistf",
                label="Select the measurement occasion (time):", 
                choices=measurementLevels(), selected=measurementLevels()[1])
    }
  }
})


# ui - user interface to select a numerical variable to associate with the presence of symptom ###
output$logistfUI2= renderUI({
  if(!is.null(dataFiltered())){
    if (input$treatasBinary==TRUE) {
    selectInput(inputId="logistfIDVar",
                label="Select a variable to associate with presence of symptoms:", 
                choices=dataVariableNames(),
                if (input$dataFileType=="Demo"){selected=c("Sex")})
    }
  }
})

# plot - logistf ###
output$plotLogistf <- renderPlot({
  if(!(is.null(Measurement()) || is.null(input$measurementSelectedlogistf) )){
    if (input$treatasBinary==TRUE) {
    plotLogistf(data=dataExtended(),
                data.yn=dataFiltered.yn(),
                measurement=Measurement(),
                measurementSelectedlogistf=input$measurementSelectedlogistf,
                logistfIDVar=input$logistfIDVar,
                selectedSymptoms=input$selectedSymptoms,
                numSymptoms=length(input$selectedSymptoms))
    }
  }
}, height=numRowsLogistf)


# table - logistf ###
output$tableLogistf <- renderTable({
  if(!(is.null(Measurement()) || is.null(input$measurementSelectedlogistf) )){
    if (input$treatasBinary==TRUE) {
  tabelizeLogistf(data=dataExtended(),
                  data.yn=dataFiltered.yn(),
                  measurement=Measurement(),
                  measurementSelectedlogistf=input$measurementSelectedlogistf,
                  logistfIDVar=input$logistfIDVar,
                  selectedSymptoms=input$selectedSymptoms)
    }}
  })

output$messageNotAppropriate8 <- renderText({
  if(!is.null(input$treatasBinary)){
    if (input$treatasBinary==FALSE) {
      "This type of analysis is not appropriate for numerical responses."
    }}
})


# TAB - Mixed model ####
output$selectMixedModelType <- renderUI({
  selectInput(inputId="selectedMixedModelType",
              label="Select a mixed model type:",
choices=c("Model response with fixed effect of grouping variable and
          random intercept for every subject"="MMsimple",
          "Model response with fixed effects of grouping variable,
          measurement occasion and random intercept for every subject"="MMmeasurement",
          "Model response with fixed effect of grouping variable,
          time from inclusion in study and 
          random intercept for every subject"="MMtimeSinceInclusion"),
              selected="MMsimple")
  
  })

output$mixedModelTables <- renderTable({
  .mixedModel(data=dataFiltered(),
              selectedSymptoms=input$selectedSymptoms,
              groupingVar=input$groupingVar,
              subjectIDVar=input$patientIDVar,
              measurementVar=input$measurementVar,
              dateVar=input$dateVar,
              thresholdValue=input$thresholdValue,
              treatasBinary=input$treatasBinary,
              selectedModel=input$selectedMixedModelType)
  
  })


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
    
    save(data, file="dataFiltered.Rdata")
    
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









