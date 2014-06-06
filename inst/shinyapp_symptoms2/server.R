# This is the server part of the shiny script to plot
# graph for displaying presence of symptoms. 

# Load libraries ----------------------------------------------------------

# load library for generation interactive web pages
library(shiny)
if(!require(shinyIncubator)) {
  devtools::install_github("shiny-incubator", "rstudio")
  library(shinyIncubator)
}
# library(shinyIncubator) # NOTE!!!: this is not available on CRAN, might not be best to include it?
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
# bootstrap library
library(boot)
# libraries for mixed models
library(lme4)
library(lmerTest)

# Global activity ####
# purge temporary files
file.remove(list.files(paste0(x=getwd(), "/www/temp"), full.names=TRUE))

# set number of processors for boot package
options(boot.ncpus=Sys.getenv('NUMBER_OF_PROCESSORS'))

# Main function -----------------------------------------------------------

shinyServer(function(input, output, session) {
  
  # VARIABLES ####
  # the working directory
  workingDir <- paste0(gsub(pattern="/", replacement="\\\\", x=getwd()))
  
  # FUNCTIONS ####
  #how much space should be used for the graphical output of the Rcs estimates and others?  
  # TODO: could drawing graphs be done if(no graph){height=0)?
  numRowsTimeline <- function(){if(!is.null(dataFiltered())){
    if(input$selectedGraphOverTime!="timelinePlot") {return(0)} # no space reserved
    if(input$treatasBinary==TRUE) {return(0)} # no graph if not possible to draw
    max(ceiling(length(input$selectedSymptoms))*40, # so that legend is visible
        (dim(dataFiltered()[1])*0.75), # to not compress patient axis too much
        400) # at least this size
  } else {return(0)} # if there is no data, height of plot should be zero
  }
  
  numRowsTimelineProfile <- function(){if(!is.null(input$selectedGraphType)){
    if(input$selectedGraphOverTime!="profilePlot") {return(0)}
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
  
  
#   selectInput(inputId="selectedFacetingType",
#               label="Select faceting type:",
#               choices=c("Variables ~ Measurement occasions"="variablesOnYaxis",
#                         "Measurement occasions ~ Variables"="variablesOnXaxis")
#   
  numRowsTimelineBoxplots <- function(){if(!is.null(dataFiltered())){
    if(input$selectedGraphOverTime!="boxPlot") {return(0)}
    if (input$selectedFacetingType=="variablesOnYaxis") {
    if(input$treatasBinary==TRUE) {return(0)} 
    tmp <- max(ceiling(length(input$selectedSymptoms))*200,
               300) # minumum reserved space
    return(tmp)}
    
    if (input$selectedFacetingType=="variablesOnXaxis") {
      if(input$treatasBinary==TRUE) {return(0)} 
      tmp <- max(ceiling(length(unique(na.omit(Measurement()))))*200,
                 300) # minumum reserved space
      return(tmp)}
    
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
  
  numRowsPresencePlot <- function(){
    try({
      if(!is.null(input$selectedMeasurementForPresencePlot)){
        if(input$selectedGraphOverTime=="presencePlot") {
        max(ceiling(length(input$selectedSymptoms))*30,
            300 # minumum reserved space
        )}}else{return(0)} # height of plot when no data available
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
  
  numRowsLogistf <- function() {if(!is.null(regressionScenario())){
    if(regressionScenario()!="scenarioLogistf") {return(0)}
    max(ceiling(length(input$selectedSymptoms))*30,
        300)}else{return(0)}}
  
numRowsLogist <- function() {if(!is.null(regressionScenario())){
  if(regressionScenario()!="scenarioLogist") {return(0)}
  max(ceiling(length(input$selectedSymptoms))*30,
      300)}else{return(0)}}

numRowsLinear <- function() {if(!is.null(regressionScenario())){
  if(regressionScenario()!="scenarioLinearModel") {return(0)}
  max(ceiling(length(input$selectedSymptoms))*30,
      300)}else{return(0)}}

numRowsRCSModel <- function() {if(!is.null(regressionScenario())){
  if(regressionScenario()!="scenarioRCSModel") {return(0)}
  max(ceiling(length(input$selectedSymptoms))*100,
      300)}else{return(0)}}

  numRowsMixedModels1 <- function(){if(!is.null(dataFiltered()) &
                                         !is.null(input$selectedMixedModelType)){
    ceiling(length(input$selectedSymptoms))*30  }else{return(0)} }
  
  numRowsMixedModels2 <- function(){if(!is.null(dataFiltered()) &
                                                  !is.null(input$selectedMixedModelType)){
    nFacets <- length(unique(Measurement()))-1
    (input$selectedMixedModelType=="MMmeasurement")*
      ceiling(length(input$selectedSymptoms))*30*nFacets  }else{return(0)} }
  
  numRowsMixedModels3 <- function(){if(!is.null(dataFiltered()) &
                                                  !is.null(input$selectedMixedModelType)){
    (input$selectedMixedModelType=="MMtimeSinceInclusion")*
      ceiling(length(input$selectedSymptoms))*30  }else{return(0)} }
  
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
                  label="Choose outcome variables to analyse:", 
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
                  label="Choose evaluation occasion variable:", 
                  choices=dataVariableNames(),
                  selected="Measurement")
    }
  })
  
  # GUI - selecting use of thresholding ####
  output$selectTreatasBinary <- renderUI({
    if (!is.null(dataVariableNames())){
      checkboxInput(inputId="treatasBinary",
                    label="Treat and analyse outcome variables as binary?",
                    value=FALSE)
    }  
    
  })
  
  # GUI - selecting treshold value ####
  output$selectThresholdValue <- renderUI({
    if (!is.null(dataVariableNames()) & !is.null(input$treatasBinary)){
      if(input$treatasBinary==TRUE) {
        numericInput(inputId="thresholdValue",
                     "Threshold for positivity of the outcome variables:",
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

# TAB - Data overview ####
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

# TAB - Graphical exploration over time ####
output$selectGraphOverTime <- renderUI({
  if (!is.null(dataFiltered())) {
  selectInput(inputId="selectedGraphOverTime",
              label="Select type of graph:",
              choices= if (input$treatasBinary==TRUE) {
                c(#"Profile plots"="profilePlot",
                  "Lasagna plots"="lasagnaPlot",
                  #"Boxplots"="boxPlot",
                  #"Timeline"="timelinePlot",
                  "Presence of symptoms"="presencePlot"
                  )} else {
                    c("Profile plots"="profilePlot",
                      "Lasagna plots"="lasagnaPlot",
                      "Boxplots"="boxPlot",
                      "Timeline"="timelinePlot" #,
                      # "Presence of symptoms"="presencePlot"
                        )},
              selected= if (input$treatasBinary==TRUE) {"presencePlot"} else {"timelinePlot"},
              multiple=FALSE)
  }  
})

# Profile plots ####
# Menus
# ui - select type of graph
output$selectGraphType <- renderUI({
  if(!is.null(input$selectedGraphOverTime)) {
    if (input$selectedGraphOverTime=="profilePlot") {
    if(input$treatasBinary==FALSE){
      selectInput(inputId="selectedGraphType",
                  label="Select type of graphs to plot:",
                  choices=c("All subjects on one graph"="oneGraph",
                            "Random selection of subjects on one graph"="randomSample",
                            "Multiple graphs per outcome variable"="multipleGraphs"),
                  selected="randomSample",
                  multiple=FALSE)
    }}}
})

output$selectRandomSampleSize <- renderUI({
  if(!is.null(input$selectedGraphType)) {
    if (input$selectedGraphOverTime=="profilePlot") {
    if (input$selectedGraphType=="randomSample") {
      if(input$treatasBinary==FALSE){
        numericInput(inputId="sampleSize",
                     label="Select number of randomly selected subjects:",
                     value=10,
                     min=1,
                     max=100,
                     step=5)
      }}}}
})


output$selectMaxGroupSize <- renderUI({
  if(!is.null(input$selectedGraphType)) {
    if (input$selectedGraphOverTime=="profilePlot") {
    if (input$selectedGraphType=="multipleGraphs") {
      if(input$treatasBinary==FALSE){
        numericInput(inputId="groupSize",
                     label="Select the maximum number of subjects on one graph:",
                     value=25,
                     min=10,
                     max=100,
                     step=5)
      }}}}
})

# Graph
output$plotTimelineProfiles <- renderPlot({
  if(!is.null(input$selectedGraphType)) {
    if ( (input$selectedGraphType=="oneGraph") ||
           (input$selectedGraphType=="randomSample" && !is.null(input$sampleSize)) ||
           (input$selectedGraphType=="multipleGraphs" && !is.null(input$groupSize))) {
      if(input$treatasBinary==FALSE){
        
        progress <- Progress$new(session, min=1, max=100)
        on.exit(progress$close())
        
        progress$set(message = 'Calculation in progress',
                     detail = 'This may take a while...', 
                     value=NULL)
        
        print(plotTimelineProfiles(data=dataFiltered(),
                                   plotType=input$selectedGraphType,
                                   personIDVar=input$patientIDVar,
                                   measurementVar=input$measurementVar,
                                   selectedSymptoms=input$selectedSymptoms,
                                   sizeofRandomSample=input$sampleSize,
                                   sizeofGroup=input$groupSize))
      }}}
}, height=numRowsTimelineProfile)

# Lasagna plots ####
# Graph
output$plotLasagna <- renderUI({
  if (!is.null(input$selectedGraphOverTime)) {
  if (input$selectedGraphOverTime=="lasagnaPlot") {
  
    progress <- Progress$new(session, min=1, max=100)
    on.exit(progress$close())
    
    progress$set(message = 'Calculation in progress',
                 detail = 'This may take a while...', 
                 value=NULL)
    
    
  filenames <- vector()
  
  # generate as many files as there are plots
  for (symptom in input$selectedSymptoms) {
    filenames[symptom] <- tempfile(pattern="symptom", tmpdir=paste0(workingDir,"\\www\\temp"), fileext=".png")
   
  # plot graph for each symptom
  #for(symptom in input$selectedSymptoms) {
        
    png(paste0(filenames[symptom]))
    plotLasagna(if (input$treatasBinary==FALSE) {dataFiltered()}else{dataFilteredwithThreshold()}, 
                  treatasBinary=input$treatasBinary, 
                  symptom=symptom,
                  dateVar=input$dateVar, 
                  personIDVar=input$patientIDVar, 
                  measurementVar=input$measurementVar,
                  groupingVar=input$groupingVar,  
                  thresholdValue=input$thresholdValue) 
    dev.off()
  }
  

out <- pastePlotFilenames(filenames)
  
return(div(HTML(out),class="shiny-plot-output shiny-bound-output"))
}}
})


# Boxplots ####
# Menu
output$selectFacetingType <- renderUI({
  if(!is.null(input$selectedGraphType)) {
    if (input$selectedGraphOverTime=="boxPlot") {
  selectInput(inputId="selectedFacetingType",
              label="Select faceting type:",
              choices=c("Variables ~ Evaluation occasions"="variablesOnYaxis",
                        "Evaluation occasions ~ Variables"="variablesOnXaxis")
              )
    }}
  })
   

# Graph
output$plotTimelineBoxplots <- renderPlot({
  if(!is.null(dataFiltered())) {
    if(input$selectedGraphOverTime=="boxPlot") {
      
    if(input$treatasBinary==FALSE){
      
      progress <- Progress$new(session, min=1, max=100)
      on.exit(progress$close())
      
      progress$set(message = 'Calculation in progress',
                   detail = 'This may take a while...', 
                   value=NULL)
      
      print(plotTimelineBoxplots(data=dataFiltered(),
                                 personIDVar=input$patientIDVar,
                                 measurementVar=input$measurementVar,
                                 selectedSymptoms=input$selectedSymptoms,
                                 faceting=input$selectedFacetingType)
      )
    }}
  } else {return()}
},height=numRowsTimelineBoxplots)

# Timeline graph ####
# Menu
output$selectDisplayFormat <- renderUI({
  if(!is.null(dataFiltered())){
    if(input$selectedGraphOverTime=="timelinePlot") {
      if(input$treatasBinary==FALSE){
        selectInput(inputId="displayFormat",
                    label="Choose what to display on the horizontal axis:",
                    choices=c("Dates" = "dates",
                              "Time from inclusion" ="timeFromInclusion",
                              "Evaluation occasions" = "measurementOccasions"),
                    selected="dates",
                    multiple=FALSE)
      }
    }
  }
})

# Graph
output$plotTimeline <- renderPlot({
  if(!(is.null(dataFiltered()) || is.null(input$displayFormat))){
    if(input$selectedGraphOverTime=="timelinePlot") {
    if (input$treatasBinary==FALSE) {
      
      progress <- Progress$new(session, min=1, max=100)
      on.exit(progress$close())
      
      progress$set(message = 'Calculation in progress',
                   detail = 'This may take a while...', 
                   value=NULL)
      
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
      )}} }  
}, height=numRowsTimeline)

# Presence of symptoms graph ####
# Menu
output$selectMeasurementForPresencePlot <- renderUI({
  if(!is.null(input$selectedGraphOverTime)) {
  if(input$selectedGraphOverTime=="presencePlot") {
    selectInput(inputId="selectedMeasurementForPresencePlot",
                label="Select evaluation occasion:",
                choices=measurementLevels(), selected=measurementLevels()[1])
  }}
})

# Graph
output$plotPresence <- renderPlot({
  if(!is.null(input$selectedMeasurementForPresencePlot)) {
    if(input$selectedGraphOverTime=="presencePlot") {
 
      progress <- Progress$new(session, min=1, max=100)
      on.exit(progress$close())
      
      progress$set(message = 'Calculation in progress',
                   detail = 'This may take a while...', 
                   value=NULL)
      
      plot <- plotPresenceofSymptoms(data=dataFiltered(),
                         selectedSymptoms=input$selectedSymptoms,
                         measurementVar=input$measurementVar,
                         measurement=input$selectedMeasurementForPresencePlot,
                         thresholdValue=ifelse(!is.null(input$thresholdValue),input$thresholdValue ,0))
      print(plot)
  }}
  }, height=numRowsPresencePlot)


# TAB - Summary tables : time ####
# Boxplot tables - all tables at once
# output$tableforBoxplots <- renderUI({
#   if(!is.null(dataFiltered())) {
#     #if(input$treatasBinary==FALSE){
#       progress <- Progress$new(session, min=1, max=100)
#       on.exit(progress$close())
#       
#       progress$set(message = 'Calculation in progress',
#                    detail = 'This may take a while...', 
#                    value=NULL)
#       

# SERIOUS BUG HERE - the measurements passed to this function is a vector of 
# all measurements, when it should be only of unique measurement; 
#       out <- tabelizeBoxplots(measurements=Measurement(),
#                               measurementVar=input$measurementVar,
#                               data=dataFiltered(),
#                               selectedSymptoms=input$selectedSymptoms) 
#       
#       return(div(HTML(out),class="shiny-html-output"))
#     } #}
# })

# Menu
output$selectEvaluationTime2 <- renderUI({
  selectInput(inputId="selectedEvaluationTime2",
              label="Select evaluation occasion:",
              choices=if(!is.null(measurementLevels())) {measurementLevels()},
              selected=if(!is.null(measurementLevels())) {measurementLevels()[1]})
  
})

# Boxplot tables
output$tableforBoxplots <- renderTable({
  if(!is.null(dataFiltered())) {
    if(input$treatasBinary==FALSE){
      progress <- Progress$new(session, min=1, max=100)
      on.exit(progress$close())
      
      progress$set(message = 'Calculation in progress',
                   detail = 'This may take a while...', 
                   value=NULL)
      
      out <- tabelizeBoxplotsforMeasurement(measurement=input$selectedEvaluationTime2,
                              measurementVar=input$measurementVar,
                              data=dataFiltered(),
                              selectedSymptoms=input$selectedSymptoms) 
      
      return(out)
    } }
})

# Proportions tables
output$tableforProportions <- renderTable({
  if(!is.null(dataFilteredwithThreshold())) {
    if(input$treatasBinary==TRUE){
    
      out <- tabelizeProportionsforMeasurement(measurement=input$selectedEvaluationTime2,
                                            measurementVar=input$measurementVar,
                                            data=dataFilteredwithThreshold(),
                                            selectedSymptoms=input$selectedSymptoms) 
      
      return(out)
      
      
    }
  }
  
  })


# TAB - Graphical exploration : by grouping variable ####
# Proportion by groups (aka Pyramid plot) ####
# Message
output$messageNotAppropriate5 <- renderText({
  if(!is.null(input$treatasBinary)){
    if (input$treatasBinary==FALSE) {
      "This type of analysis is not appropriate for numerical responses."
    }}
})

# Graph
output$plotPyramid <- renderPlot ({
  try({
    if(!(is.null(dataFilteredwithThreshold()) || is.null(input$treatasBinary) )){
      if(input$treatasBinary==TRUE){
        
        progress <- Progress$new(session, min=1, max=100)
        on.exit(progress$close())
        
        progress$set(message = 'Calculation in progress',
                     detail = 'This may take a while...', 
                     value=NULL)
        
        plotPropWithSymptoms(data=dataFilteredwithThreshold(),
                             grouping=input$groupingVar,
                             measurements=input$measurementVar,
                             symptomsNames=input$selectedSymptoms)
        
      }
    }
  }, silent=TRUE)
} ,height=numRowsProportions)

# Proportions by groups with confidence intervals ####
# Graph
output$plotPropCIs <- renderPlot ({
  try({
    if(!is.null(dataFilteredwithThreshold())){
      if(input$treatasBinary==TRUE){
        
        progress <- Progress$new(session, min=1, max=100)
        on.exit(progress$close())
        
        progress$set(message = 'Calculation in progress',
                     detail = 'This may take a while...', 
                     value=NULL)
        
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


# TAB - Summary tables : grouping variable ####
# Menu
output$UIpropTable = renderUI({
  if(!is.null(measurementLevels())){
    if(input$treatasBinary==TRUE){
      #select the measurement
      selectInput(inputId="measurementSelectedprop",
                  label="Select evaluation occasion:", 
                  choices=measurementLevels(), selected=measurementLevels()[1])
    }
  }
})

# Tables
# Table of proportions of patients in a group with a symptom ####
output$tablePropGroups <- renderTable ({
  
  
  
  if(!(is.null(dataFiltered()) || is.null(input$thresholdValue)  )){
    if(input$treatasBinary==TRUE){
      
      progress <- Progress$new(session, min=1, max=100)
      
      progress$set(message = 'Calculation in progress',
                   detail = 'This will take a while...', 
                   value=NULL)
      
      out <- tablePropWithSymptoms(data=dataFiltered(),
                            groupingVar=input$groupingVar,
                            measurementVar=input$measurementVar,
                            forMeasurement=input$measurementSelectedprop,
                            symptomsNames=input$selectedSymptoms,
                            thresholdValue=input$thresholdValue)
      on.exit(progress$close())
      return(out)
    }
  }
  
})

# text - explaining tablePropGroups
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

# Table with medians of symptoms values in a group ####
output$tableMedianGroups <- renderTable ({
  if(!(is.null(dataFiltered()) || is.null(input$measurementSelectedprop) )){
    if(input$treatasBinary==TRUE){
      
      progress <- Progress$new(session, min=1, max=100)
      
      progress$set(message = 'Calculation in progress',
                   detail = 'This will take a while...', 
                   value=NULL)
      
      tableMediansWithSymptoms(data=dataFiltered(),
                               groupingVar=input$groupingVar,
                               measurementVar=input$measurementVar,
                               forMeasurement=input$measurementSelectedprop,
                               symptomsNames=input$selectedSymptoms,
                               thresholdValue=input$thresholdValue)
      on.exit(progress$close())
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


output$messageNotAppropriate10 <- renderText({
  if(!is.null(input$treatasBinary)){
    if (input$treatasBinary==FALSE) {
      "This type of analysis is not appropriate for numerical responses."
    }}
})


# TAB - Clustering ####
# Menu
output$clusteringUI = renderUI({
  if(!(is.null(measurementLevels()) || is.null(measurementLevels()) )){
    if(input$treatasBinary==FALSE){
      #select the measurement
      selectInput(inputId="selectedMeasurementValue",
                  label="Select evaluation occasion:", 
                  choices=measurementLevels(), selected=measurementLevels()[1])
    }
  }
})

# Graphs
# Dendrogram plot ####
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


# Heatmap - Selection of annotation variables
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



# Heatmap plot ####
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


# TAB - Regression model : one evaluation ####
# Menu
output$debug10 <- renderText({paste(regressionScenario())})

output$debug9 <- renderText({
  paste("selectedEvaluationTime:", ifelse(is.null(input$selectedEvaluationTime), "NULL", input$selectedEvaluationTime),
        "selectedCovariate:", ifelse(is.null(input$selectedCovariate), "NULL", input$selectedCovariate) ,
        "treatasBinary:",ifelse(is.null(input$treatasBinary), "NULL", input$treatasBinary),
        "useFirthCorrection:", ifelse(is.null(input$useFirthCorrection), "NULL", input$useFirthCorrection),
        "useRCSModel:", ifelse(is.null(input$useRCSModel), "NULL", input$useRCSModel))
  })

output$selectEvaluationTime <- renderUI({
  selectInput(inputId="selectedEvaluationTime",
              label="Select evaluation occasion:",
              choices=if(!is.null(measurementLevels())) {measurementLevels()},
              selected=if(!is.null(measurementLevels())) {measurementLevels()[1]})
  
  })

output$selectCovariate <- renderUI({
  selectInput(inputId="selectedCovariate",
              label="Select covariate for analysis:",
              choices=dataVariableNames(),
              selected=input$groupingVar)
  })

output$checkUseFirthCorrection <- renderUI({
  if (!is.null(input$treatasBinary)) {
    if (input$treatasBinary==TRUE) {
    #if (determineTypeofVariable(dataExtended()[,input$selectedCovariate])[["nLevels"]]=="binary") {
      checkboxInput(inputId="useFirthCorrection",
                    label="Use Firth correction?",
                    value=FALSE)
  # }
  }}
  })

output$checkUseRCSModel <- renderUI({
  if (!is.null(input$treatasBinary) & !is.null(input$selectedCovariate)) {
    if (input$treatasBinary==FALSE) {
      if (determineTypeofVariable(dataExtended()[,input$selectedCovariate])[["nLevels"]]=="multilevel" &
            (determineTypeofVariable(dataExtended()[,input$selectedCovariate])[["type"]]=="integer" |
               determineTypeofVariable(dataExtended()[,input$selectedCovariate])[["type"]]=="numeric")
          ) {
        checkboxInput(inputId="useRCSModel", label="Use flexible model of the association of the selected
                variables with the numerical covariate?",
                value=FALSE)
      }}}
})

# Determine scenario ####
regressionScenario <- reactive({
  if (!is.null(input$treatasBinary) &
        !is.null(input$selectedEvaluationTime) &
        !is.null(input$selectedCovariate) &
        !is.null(dataFiltered()) &
        !is.null(input$measurementVar) &
        !is.null(input$selectedSymptoms)
      ) {

  if (input$treatasBinary==TRUE) {
    if (is.null(input$useFirthCorrection)) {return("scenarioLogist")}
    if (input$useFirthCorrection==FALSE) {return("scenarioLogist")}
    if (input$useFirthCorrection==TRUE) {return("scenarioLogistf")}
  }
  
  if (input$treatasBinary==FALSE) {
    if (is.null(input$useRCSModel) ) {return("scenarioLinearModel")}
    if (input$useRCSModel==FALSE) {return("scenarioLinearModel")}
    if (input$useRCSModel==TRUE) {return("scenarioRCSModel")}
  }
  }
})


# reset checkboxes on parameter change
# observe({input$selectedCovariate
#          print("Observer triggered")
#          updateCheckboxInput(session, inputId="useRCSModel",value=FALSE)
#          updateCheckboxInput(session, inputId="useFirthCorrection",value=FALSE)
#          print("Observer triggered2")
#          print(input$useFirthCorrection)
#          })
# observe({input$treatasBinary
#          print("Observer triggered")
#          updateCheckboxInput(session, inputId="useRCSModel",value=FALSE)
#          updateCheckboxInput(session, inputId="useFirthCorrection",value=FALSE)
#          print("Observer triggered2")
#          print(input$useFirthCorrection)
# })

# Graph(s)

# Scenario - Logistic regression with Firth correction
# Create results of Logistic regression with Firth correction
resultsLogistf <- reactive({
  if(!is.null(regressionScenario())) {
  if(regressionScenario()=="scenarioLogistf") {
    out <- tabelizeLogistf(data=dataExtended(),
                    measurementVar=input$measurementVar,
                    selectedMeasurement=input$selectedEvaluationTime,
                    covariate=input$selectedCovariate,
                    selectedSymptoms=input$selectedSymptoms,
                    thresholdValue=input$thresholdValue)
    return(out)
  }}
  
  })

# plot - logistf ### OBSOLETE
# output$plotLogistf <- renderPlot({
#   if(!(is.null(Measurement()) || is.null(input$selectedEvaluationTime) )){
#     if(input$useFirthCorrection==TRUE) {
#       if (input$treatasBinary==TRUE) {
#         plotLogistf(data=dataExtended(),
#                     data.yn=dataFiltered.yn(),
#                     measurement=Measurement(),
#                     measurementSelectedlogistf=input$selectedEvaluationTime,
#                     logistfIDVar=input$selectedCovariate,
#                     selectedSymptoms=input$selectedSymptoms,
#                     numSymptoms=length(input$selectedSymptoms))
#       }
#     }}
# }, height=numRowsLogistf)

# plot - logistf ####
output$plotLogistf2 <- renderPlot({
  if(!is.null(resultsLogistf()) ){
    if(regressionScenario()=="scenarioLogistf") {
       out <- plotValueswithCIs(data=resultsLogistf()[["rawResultsTable"]],
                          variableName="Variable",
                          valueName="OR",
                          CILowerName="CILower",
                          CIUpperName="CIUpper",
                          xLabel="Odds ratios",
                          yLabel="Variables",
                          graphTitle=paste("Odds ratios and confidence intervals for",
                                           resultsLogistf()[["referenceValue"]], 
                                           "at evaluation T=",
                                           input$selectedEvaluationTime,
                                           "(using Firth correction)"),
                          vLine=1)    
       
       print(out)
      }
    }
}, height=numRowsLogistf)

# table - logistf ####
output$tableLogistf <- renderTable({
  if(!is.null(resultsLogistf()) ){
    if(regressionScenario()=="scenarioLogistf") {
      resultsLogistf()[["printableResultsTable"]]
    }}
  })


# Scenario - logistic regression (without Firth correction)
resultsLogist <- reactive({
  if(!is.null(regressionScenario()) ){
    if(regressionScenario()=="scenarioLogist") {
      out <- tabelizeLogist(data=dataExtended(),
                            measurementVar=input$measurementVar,
                            selectedMeasurement=input$selectedEvaluationTime,
                            covariate=input$selectedCovariate,
                            selectedSymptoms=input$selectedSymptoms,
                            thresholdValue=input$thresholdValue)
      return(out)
    }}
  })


# table - logist ####
output$tableLogist <- renderTable({
  if(!is.null(resultsLogist()) ){
    if(regressionScenario()=="scenarioLogist") {
      resultsLogist()[["printableResultsTable"]]
    }}
  })


# plot - logist ####
output$plotLogist <- renderPlot({
  if(!(is.null(resultsLogist()))){
    if(regressionScenario()=="scenarioLogist") {
        out <- plotValueswithCIs(data=resultsLogist()[["rawResultsTable"]],
                                 variableName="Variable",
                                 valueName="OR",
                                 CILowerName="CILower",
                                 CIUpperName="CIUpper",
                                 xLabel="Odds ratios",
                                 yLabel="Variables",
                                 graphTitle=paste("Odds ratios and confidence intervals for",
                                                  resultsLogist()[["referenceValue"]], 
                                                  "at evaluation T=",
                                                  input$selectedEvaluationTime),
                                 vLine=1)    
        
        print(out)
    }}
  }, height=numRowsLogist)


# Scenario - linear regression
resultsLinear <- reactive({
  if (!is.null(regressionScenario())) {
    if (regressionScenario()=="scenarioLinearModel") {
      out <- tabelizeLinear(data=dataExtended(),
                            measurementVar=input$measurementVar,
                            selectedMeasurement=input$selectedEvaluationTime,
                            covariate=input$selectedCovariate,
                            selectedSymptoms=input$selectedSymptoms)
      return(out)
    }}
})

# table - linear ####
output$tableLinear <- renderTable({
  if(!is.null(resultsLinear())){
    if (regressionScenario()=="scenarioLinearModel") {
      resultsLinear()[["printableResultsTable"]]
    }}
  })

# plot - linear ####
output$plotLinear <- renderPlot({
  if(!is.null(resultsLinear())){
    if (regressionScenario()=="scenarioLinearModel") {
        out <- plotValueswithCIs(data=resultsLinear()[["rawResultsTable"]],
                                 variableName="Variable",
                                 valueName="beta",
                                 CILowerName="CILower",
                                 CIUpperName="CIUpper",
                                 xLabel="Beta (slope) coefficient",
                                 yLabel="Variables",
                                 graphTitle=paste("Beta coefficients and confidence intervals for effects of",
                                                  input$selectedCovariate, 
                                                  "on selected variables at evaluation T=",
                                                  input$selectedEvaluationTime),
                                 vLine=0)  
        print(out)
    }}
  }, height=numRowsLinear) 

# Scenario - modeling with Restricted Cubic Splines

# plot - RCS plot ####
output$plotRCS=renderPlot({
  
  if(!is.null(regressionScenario())){
    if (regressionScenario()=="scenarioRCSModel") {
      
    plotRCS(data.all=dataExtended(),
            data.yn=dataFiltered.yn(),
            measurement=Measurement(),
            selectedSymptoms=input$selectedSymptoms,
            measurementSelectedrcs=input$selectedEvaluationTime,
            rcsIDVar=input$selectedCovariate,
            binaryVar=input$treatasBinary)   
  }}
}, height=numRowsRCSModel)

# table - RCS table ####
output$tableRCS <- renderTable({
  
  if(!is.null(regressionScenario())){
    if (regressionScenario()=="scenarioRCSModel") {
    tabelizeRCS(data.all=dataExtended(),
                data.yn=dataFiltered.yn(),
                measurement=Measurement(),
                selectedSymptoms=input$selectedSymptoms,
                measurementSelectedrcs=input$selectedEvaluationTime,
                rcsIDVar=input$selectedCovariate, 
                binaryVar=input$treatasBinary
    )
  }}
})

# TAB - Regression model : all evaluations ####
# Menu
output$selectMixedModelType <- renderUI({
  selectInput(inputId="selectedMixedModelType",
              label="Select a mixed model type:",
              choices=c("Model response with fixed effect of grouping variable and
                        random intercept for every subject"="MMsimple",
                        "Model response with fixed effects of grouping variable,
                        evaluation occasion and random intercept for every subject"="MMmeasurement",
                        "Model response with fixed effect of grouping variable,
                        time from inclusion in study and 
                        random intercept for every subject"="MMtimeSinceInclusion"),
              selected="MMsimple")
})

# Results
mixedModelResults <- reactive({
  
  progress <- Progress$new(session, min=1, max=100)
  
  progress$set(message = 'Calculation in progress',
               detail = 'This will take a while...', 
               value=NULL)
  on.exit(progress$close())
  
  mixedModel(data=dataFiltered(),
             selectedSymptoms=input$selectedSymptoms,
             groupingVar=input$groupingVar,
             subjectIDVar=input$patientIDVar,
             measurementVar=input$measurementVar,
             dateVar=input$dateVar,
             thresholdValue=input$thresholdValue,
             treatasBinary=input$treatasBinary,
             selectedModel=input$selectedMixedModelType)
})

# Table 1 ####
output$mixedModelTable1 <- renderUI({
  if(!is.null(input$selectedMixedModelType)) {
    results <- mixedModelResults()[["groupingVar"]] 
    
    out <- print(xtable(results, caption=paste("Fixed effects of",
                                               input$groupingVar,
                                               "for", 
                                               mixedModelResults()[["groupingVarComparison"]])),
                 type="html",
                 html.table.attributes='class="data table table-bordered table-condensed"',
                 caption.placement="top")
    return(div(HTML(out),class="shiny-html-output"))
  }
  
})

# Graph 1 ####
output$mixedModelGraph1 <- renderPlot({
  if(!is.null(input$selectedMixedModelType)) {
    print(plotFixedEffectsofGroupingVar(calculatedStatistics=mixedModelResults()[["groupingVar"]],
                                        groupingVar=input$groupingVar,
                                        groupingVarReferenceValue=mixedModelResults()[["groupingVarReferenceValue"]],
                                        treatasBinary=input$treatasBinary) 
    )
  }
}, height=numRowsMixedModels1)

# Table 2 ####
output$mixedModelTable2 <- renderUI({
  if(!is.null(input$selectedMixedModelType)) {
    if (input$selectedMixedModelType=="MMmeasurement") {
      results <- mixedModelResults()[["measurementVar"]] 
      
      out <- print(xtable(results, caption=paste("Fixed effects of",
                                                 input$measurementVar,
                                                 "for T=",
                                                 mixedModelResults()[["measurementVarComparison"]],
                                                 "used as reference")),
                   type="html",
                   html.table.attributes='class="data table table-bordered table-condensed"',
                   caption.placement="top")
      return(div(HTML(out),class="shiny-html-output"))
    }
  }
})

# Graph 2 ####
output$mixedModelGraph2 <- renderPlot({
  if(!is.null(input$selectedMixedModelType)) {
    if (input$selectedMixedModelType=="MMmeasurement") {
      print(plotFixedEffectsofMeasurementVar(calculatedStatistics=mixedModelResults()[["measurementVar"]],
                                             measurementVar=input$measurementVar,
                                             treatasBinary=input$treatasBinary) 
      )
    }}
}, height=numRowsMixedModels2)

# Table 3 ####
output$mixedModelTable3 <- renderUI({
  if(!is.null(input$selectedMixedModelType)) {
    if (input$selectedMixedModelType=="MMtimeSinceInclusion") {
      results <- mixedModelResults()[["daysSinceInclusion"]] 
      
      out <- print(xtable(results, caption=paste("Fixed effects of time since inclusion in the study")),
                   type="html",
                   html.table.attributes='class="data table table-bordered table-condensed"',
                   caption.placement="top")
      return(div(HTML(out),class="shiny-html-output"))
    }
  }
})

# Graph 3 ####
output$mixedModelGraph3 <- renderPlot({
  if(!is.null(input$selectedMixedModelType)) {
    if (input$selectedMixedModelType=="MMtimeSinceInclusion") {
      print(plotFixedEffectsofDaysSinceInclusion(calculatedStatistics=mixedModelResults()[["daysSinceInclusion"]],
                                                 treatasBinary=input$treatasBinary) 
      )
    }}
}, height=numRowsMixedModels3)

# TAB - Uploaded data ####
# Table - list the subseted data in an output slot ####
output$data <- renderDataTable({
  if(!is.null(dataFiltered())){
    data <- dataFiltered()
    # TODO: We could render a renderDataTable(), but how to display dates in 
    # format 1.12.2014 and still sort them correctly?
    # Sys.setlocale("LC_TIME", "Slovenian")
    data[,input$dateVar] <- as.Date(data[,input$dateVar], format="%d.%m.%Y")
    #data[,input$dateVar] <- as.character(as.Date(data[,input$dateVar], format="%d.%m.%Y"),
    #                                    format="%d.%m.%Y")
    
    #data$Date <- as.character(as.Date(data$Date, origin="1899-12-30"),format="%d.%m.%Y")
    
    # save(data, file="dataFiltered.Rdata")
    
    return(data)
    # NOTE: if we want to render the table of data, we have to convert the dates into 
    # characters, since renderTable seems to use xtable, which seems to not handle
    # dates very well (http://stackoverflow.com/questions/8652674/r-xtable-and-dates)
  }
})

# TAB - Timeline ####
# output$selectDisplayFormat <- renderUI({
#   if(!is.null(dataFiltered())){
#     if(input$selectedGraphOverTime=="timelinePlot") {
#     if(input$treatasBinary==FALSE){
#       selectInput(inputId="displayFormat",
#                   label="Choose what to display on the horizontal axis:",
#                   choices=c("Dates" = "dates",
#                             "Time from inclusion" ="timeFromInclusion",
#                             "Measurement occasions" = "measurementOccasions"),
#                   selected="dates",
#                   multiple=FALSE)
#     }
#     }
#   }
# })
# 
# 
# output$plotTimeline <- renderPlot({
#   if(!(is.null(dataFiltered()) || is.null(input$displayFormat))){
#     if (input$treatasBinary==FALSE) { 
#       data=dataFiltered()
#       # observe({dataFiltered()})
#       # if no symbols are selected, do not plot
#       #if (dim(dataFiltered())[1]>0) {
#       print(plotSymptomsTimeline(data=data,
#                                  date=input$dateVar,
#                                  personID=input$patientIDVar,
#                                  measurement=input$measurementVar,
#                                  symptoms=input$selectedSymptoms,
#                                  displayFormat = input$displayFormat)
#       )}}   
# }, height=numRowsTimeline)
# 
# 
# output$messageNotAppropriate <- renderText({
#   if(!is.null(input$treatasBinary)){
#   if (input$treatasBinary==TRUE) {
#     "This type of analysis is not appropriate for binary responses."
#   }}
# })

# TAB - Distribution of the variables: over time ####
# # ui - select type of graph
#     output$selectGraphType <- renderUI({
#       if(!is.null(dataFiltered())) {
#         if(input$treatasBinary==FALSE){
#         selectInput(inputId="selectedGraphType",
#                     label="Select type of graphs to plot:",
#                     choices=c("All subjects on one graph"="oneGraph",
#                               "Random selection of subjects on one graph"="randomSample",
#                               "Multiple graphs per variable"="multipleGraphs"),
#                     selected="randomSample",
#                     multiple=FALSE)
#         }
#       }
#       })
#     
#     output$selectRandomSampleSize <- renderUI({
#       if(!is.null(input$selectedGraphType)) {
#         if (input$selectedGraphType=="randomSample") {
#           if(input$treatasBinary==FALSE){
#           numericInput(inputId="sampleSize",
#                        label="Select number of randomly selected subjects:",
#                        value=10,
#                        min=1,
#                        max=100,
#                        step=5)
#           }
#         }}
#     })
#     
#     
#     output$selectMaxGroupSize <- renderUI({
#       if(!is.null(input$selectedGraphType)) {
#         if (input$selectedGraphType=="multipleGraphs") {
#           if(input$treatasBinary==FALSE){
#           numericInput(inputId="groupSize",
#                        label="Select the maximum number of subjects on one graph:",
#                        value=25,
#                        min=10,
#                        max=100,
#                        step=5)
#           }
#         }}
#       })
#     output$plotTimelineProfiles <- renderPlot({
#       if(!is.null(input$selectedGraphType)) {
#         if ( (input$selectedGraphType=="oneGraph") ||
#                (input$selectedGraphType=="randomSample" && !is.null(input$sampleSize)) ||
#                (input$selectedGraphType=="multipleGraphs" && !is.null(input$groupSize))) {
#           if(input$treatasBinary==FALSE){
#           print(plotTimelineProfiles(data=dataFiltered(),
#                                      plotType=input$selectedGraphType,
#                                      personIDVar=input$patientIDVar,
#                                      measurementVar=input$measurementVar,
#                                      selectedSymptoms=input$selectedSymptoms,
#                                      sizeofRandomSample=input$sampleSize,
#                                      sizeofGroup=input$groupSize))
#           }
#         }}
#       }, height=numRowsTimelineProfile)
# 
# output$messageNotAppropriate2 <- renderText({
#   if(!is.null(input$treatasBinary)){
#     if (input$treatasBinary==TRUE) {
#       "This type of analysis is not appropriate for binary responses."
#     }}
# })

# TAB - Distr. of the vars.: over time - boxplots ####
# output$plotTimelineBoxplots <- renderPlot({
#   if(!is.null(dataFiltered())) {
#     if(input$treatasBinary==FALSE){
#     print(plotTimelineBoxplots(data=dataFiltered(),
#                                personIDVar=input$patientIDVar,
#                                measurementVar=input$measurementVar,
#                                selectedSymptoms=input$selectedSymptoms)
#     )
#     }
#   } else {return()}
# },height=numRowsTimelineBoxplots)
#
# output$tableforBoxplots <- renderUI({
#   if(!is.null(dataFiltered())) {
#     if(input$treatasBinary==FALSE){
#   
#   out <- tabelizeBoxplots(measurements=Measurement(),
#                           measurementVar=input$measurementVar,
#                           data=dataFiltered(),
#                           selectedSymptoms=input$selectedSymptoms) 
#   
#   return(div(HTML(out),class="shiny-html-output"))
# }}
#   })
# 
# output$messageNotAppropriate3 <- renderText({
#   if(!is.null(input$treatasBinary)){
#     if (input$treatasBinary==TRUE) {
#       "This type of analysis is not appropriate for binary responses."
#     }}
# })

# TAB - Distributions of variables ####
# ui - select measurement occasion ###
output$proportionUI = renderUI({
  if(!(is.null(measurementLevels()) || is.null(measurementLevels())  )){ 
    if(input$treatasBinary==TRUE){
    selectInput(inputId="measurementSelectedProportion",
                label="Select evaluation occasion:", 
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
# # plot - pyramid plot of proportions ###
# output$plotPyramid <- renderPlot ({
#   try({
#   if(!(is.null(dataFilteredwithThreshold()) || is.null(input$treatasBinary) )){
#     if(input$treatasBinary==TRUE){
#     
#       plotPropWithSymptoms(data=dataFilteredwithThreshold(),
#                          grouping=input$groupingVar,
#                          measurements=input$measurementVar,
#                          symptomsNames=input$selectedSymptoms)
#     
#     }
#   }
#   }, silent=TRUE)
# } ,height=numRowsProportions)
# 
# # plot - plot of proportions with conf. intervals
# output$plotPropCIs <- renderPlot ({
#   try({
#   if(!is.null(dataFilteredwithThreshold())){
#     if(input$treatasBinary==TRUE){
#     print(
#       plotPropWithSymptomsCI(data=dataFilteredwithThreshold(),
#                              groupingVar=input$groupingVar,
#                              measurementVar=input$measurementVar,
#                              selectedSymptoms=input$selectedSymptoms)
#     )
#     }
#   }
#   }, silent=TRUE)
# } ,height=numRowsProportionsCI)


# # ui - user interface to select which measurements to draw tables for ###
# output$UIpropTable = renderUI({
#   if(!is.null(measurementLevels())){
#     if(input$treatasBinary==TRUE){
#     #select the measurement
#     selectInput(inputId="measurementSelectedprop",
#                 label="Select the measurement occasion (time):", 
#                 choices=measurementLevels(), selected=measurementLevels()[1])
#     }
#   }
# })
# 
# # table - of proportions of patients in a group with a symptom
# output$tablePropGroups <- renderTable ({
#   
#   if(!(is.null(dataFiltered()) || is.null(input$thresholdValue)  )){
#     if(input$treatasBinary==TRUE){
#     tablePropWithSymptoms(data=dataFiltered(),
#                           groupingVar=input$groupingVar,
#                           measurementVar=input$measurementVar,
#                           forMeasurement=input$measurementSelectedprop,
#                           symptomsNames=input$selectedSymptoms,
#                           thresholdValue=input$thresholdValue)
#     }
#   }
#   
# })
# 
# # text - explainig tablePropGroups
# output$textTablePropGroups <- renderUI({
#   if(!is.null(dataFiltered())){
#     if(input$treatasBinary==TRUE){
#     tagList(p("Table displays for each variable the proportion of subjects in a
#   certain group, P value for the difference of proportions and the 
#   95% confidence interval for the difference of proportions. 
#               Data with missing values for grouping variable 
#               are removed from analysis.", br(), br()))
#     }
#   }
# })
# 
# # table - with medians of symptoms values in a group
# output$tableMedianGroups <- renderTable ({
#   if(!(is.null(dataFiltered()) || is.null(input$measurementSelectedprop) )){
#     if(input$treatasBinary==TRUE){
#     tableMediansWithSymptoms(data=dataFiltered(),
#                              groupingVar=input$groupingVar,
#                              measurementVar=input$measurementVar,
#                              forMeasurement=input$measurementSelectedprop,
#                              symptomsNames=input$selectedSymptoms,
#                              thresholdValue=input$thresholdValue)
#     }
#   }
# })
# 
# # text - explainig tableMedianGroups
# output$textTableMedianGroups <- renderUI({
#   if(!is.null(dataFiltered())){
#     if(input$treatasBinary==TRUE){
#     tagList(p("Table displays for each variable the median value for subjects in a
# certain group, interquantile range for of the variable 
# (25th to 75th percentile)and P value for the difference of samples (Mann-Whitney test). 
#               Data with missing values for grouping variable 
#               are removed from analysis. Threshold for positivity of variables is not taken into account.", br(), br() ))
#     }
#   }
# })

# output$messageNotAppropriate5 <- renderText({
#   if(!is.null(input$treatasBinary)){
#     if (input$treatasBinary==FALSE) {
#       "This type of analysis is not appropriate for numerical responses."
#     }}
# })


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
                label="Select evaluation occasion:", 
                choices=measurementLevels(), selected=measurementLevels()[1])
   
  }
})

# # plot - RCS plot ###
# output$plotRCS=renderPlot({
#  
#   if(!(is.null(dataFiltered()) || is.null(input$measurementSelectedrcs) )){
#     plotRCS(data.all=dataExtended(),
#             data.yn=dataFiltered.yn(),
#             measurement=Measurement(),
#             selectedSymptoms=input$selectedSymptoms,
#             measurementSelectedrcs=input$measurementSelectedrcs,
#             rcsIDVar=input$rcsIDVar,
#             binaryVar=input$treatasBinary)   
#    }
# }, height=NumRows)

output$messageNotAppropriate7 <- renderText({
  if(!is.null(input$treatasBinary)){
    if (input$treatasBinary==FALSE) {
      "This type of analysis is not appropriate for numerical responses."
    }}
})

# # table - RCS table ###
# output$tableRCS <- renderTable({
#   
#   if(!(is.null(dataFiltered()) || is.null(input$measurementSelectedrcs) )){
#     #if(input$treatasBinary==TRUE){
#       tabelizeRCS(data.all=dataExtended(),
#                   data.yn=dataFiltered.yn(),
#                   measurement=Measurement(),
#                   selectedSymptoms=input$selectedSymptoms,
#                   measurementSelectedrcs=input$measurementSelectedrcs,
#                   rcsIDVar=input$rcsIDVar, 
# 				  binaryVar=input$treatasBinary
# 				  )
#     #}
# 	}
# })

################ association of variables with the outcome using logistic regression with Firth correction

# TAB - Logistf ####
# ui - user interface to select which measurments to cluster ###
output$logistfUI = renderUI({
  if(!is.null(measurementLevels())){
    if (input$treatasBinary==TRUE) {
    #select the measurement
    selectInput(inputId="measurementSelectedlogistf",
                label="Select evaluation occasion:", 
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

# # plot - logistf ###
# output$plotLogistf <- renderPlot({
#   if(!(is.null(Measurement()) || is.null(input$measurementSelectedlogistf) )){
#     if (input$treatasBinary==TRUE) {
#     plotLogistf(data=dataExtended(),
#                 data.yn=dataFiltered.yn(),
#                 measurement=Measurement(),
#                 measurementSelectedlogistf=input$measurementSelectedlogistf,
#                 logistfIDVar=input$logistfIDVar,
#                 selectedSymptoms=input$selectedSymptoms,
#                 numSymptoms=length(input$selectedSymptoms))
#     }
#   }
# }, height=numRowsLogistf)
# 
# 
# # table - logistf ###
# output$tableLogistf <- renderTable({
#   if(!(is.null(Measurement()) || is.null(input$measurementSelectedlogistf) )){
#     if (input$treatasBinary==TRUE) {
#   tabelizeLogistf(data=dataExtended(),
#                   data.yn=dataFiltered.yn(),
#                   measurement=Measurement(),
#                   measurementSelectedlogistf=input$measurementSelectedlogistf,
#                   logistfIDVar=input$logistfIDVar,
#                   selectedSymptoms=input$selectedSymptoms)
#     }}
#   })

output$messageNotAppropriate8 <- renderText({
  if(!is.null(input$treatasBinary)){
    if (input$treatasBinary==FALSE) {
      "This type of analysis is not appropriate for numerical responses."
    }}
})


# # TAB - Mixed model ####
# output$selectMixedModelType <- renderUI({
#   selectInput(inputId="selectedMixedModelType",
#               label="Select a mixed model type:",
# choices=c("Model response with fixed effect of grouping variable and
#           random intercept for every subject"="MMsimple",
#           "Model response with fixed effects of grouping variable,
#           measurement occasion and random intercept for every subject"="MMmeasurement",
#           "Model response with fixed effect of grouping variable,
#           time from inclusion in study and 
#           random intercept for every subject"="MMtimeSinceInclusion"),
#               selected="MMsimple")
# })
# 
# 
# mixedModelResults <- reactive({
#   mixedModel(data=dataFiltered(),
#               selectedSymptoms=input$selectedSymptoms,
#               groupingVar=input$groupingVar,
#               subjectIDVar=input$patientIDVar,
#               measurementVar=input$measurementVar,
#               dateVar=input$dateVar,
#               thresholdValue=input$thresholdValue,
#               treatasBinary=input$treatasBinary,
#               selectedModel=input$selectedMixedModelType)
#   })
# 
# output$mixedModelTable1 <- renderUI({
#   if(!is.null(input$selectedMixedModelType)) {
#   results <- mixedModelResults()[["groupingVar"]] 
#   
#   out <- print(xtable(results, caption=paste("Fixed effects of", input$groupingVar)),
#         type="html",
#         html.table.attributes='class="data table table-bordered table-condensed"',
#         caption.placement="top")
#   return(div(HTML(out),class="shiny-html-output"))
#   }
#   
#   })
# 
# output$mixedModelGraph1 <- renderPlot({
#   if(!is.null(input$selectedMixedModelType)) {
#   print(plotFixedEffectsofGroupingVar(calculatedStatistics=mixedModelResults()[["groupingVar"]],
#                                        groupingVar=input$groupingVar,
#                                        groupingVarReferenceValue=mixedModelResults()[["groupingVarReferenceValue"]],
#                                        treatasBinary=input$treatasBinary) 
#   )
#   }
# }, height=numRowsMixedModels1)
# 
# 
# output$mixedModelTable2 <- renderUI({
#   if(!is.null(input$selectedMixedModelType)) {
#   if (input$selectedMixedModelType=="MMmeasurement") {
#   results <- mixedModelResults()[["measurementVar"]] 
#   
#   out <- print(xtable(results, caption=paste("Fixed effects of", input$measurementVar)),
#                type="html",
#                html.table.attributes='class="data table table-bordered table-condensed"',
#                caption.placement="top")
#   return(div(HTML(out),class="shiny-html-output"))
#   }
#   }
# })
# 
# output$mixedModelGraph2 <- renderPlot({
#   if(!is.null(input$selectedMixedModelType)) {
#     if (input$selectedMixedModelType=="MMmeasurement") {
#   print(plotFixedEffectsofMeasurementVar(calculatedStatistics=mixedModelResults()[["measurementVar"]],
#                                        measurementVar=input$measurementVar,
#                                        treatasBinary=input$treatasBinary) 
#   )
#   }}
# }, height=numRowsMixedModels2)
# 
# output$mixedModelTable3 <- renderUI({
#   if(!is.null(input$selectedMixedModelType)) {
#   if (input$selectedMixedModelType=="MMtimeSinceInclusion") {
#   results <- mixedModelResults()[["daysSinceInclusion"]] 
#   
#   out <- print(xtable(results, caption=paste("Fixed effects of time since inclusion in the study")),
#                type="html",
#                html.table.attributes='class="data table table-bordered table-condensed"',
#                caption.placement="top")
#   return(div(HTML(out),class="shiny-html-output"))
#   }
#   }
# })
# 
# output$mixedModelGraph3 <- renderPlot({
#   if(!is.null(input$selectedMixedModelType)) {
#     if (input$selectedMixedModelType=="MMtimeSinceInclusion") {
#   print(plotFixedEffectsofDaysSinceInclusion(calculatedStatistics=mixedModelResults()[["daysSinceInclusion"]],
#                                        treatasBinary=input$treatasBinary) 
#   )
#   }}
# }, height=numRowsMixedModels3)



# TAB - Debug ####
# table - debuging information ###
#output$debug <- renderPrint(paste(str(dataExtended())))
})









