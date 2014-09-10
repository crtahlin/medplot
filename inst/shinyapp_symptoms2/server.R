# This is the server part of the shiny script to plot
# graph for displaying presence of symptoms. 

# Load libraries ----------------------------------------------------------

# load library for generation interactive web pages
library(shiny)
if(!require(shinyIncubator)) { # NOTE!!!: this is not available on CRAN, might not be best to include it?
  devtools::install_github("shiny-incubator", "rstudio")
  library(shinyIncubator)
}
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

shinyServer(function(input, output, session, clientData) {
  
  # VARIABLES ####
  # the working directory
  # workingDir <- paste0(gsub(pattern="/", replacement="\\\\", x=getwd()))
  workingDir <- getwd()
  
  # FUNCTIONS ####
  #how much space should be used for the graphical output of different graphs?  
  numRowsTimeline <- function(){if(!is.null(dataFiltered())){
    if(input$selectedGraphOverTime!="timelinePlot") {return(0)} # no space reserved
    # if(input$treatasBinary==TRUE) {return(0)} # no graph if not possible to draw
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
      if(!is.null(input$selectedEvaluationTime2)) {
        if (input$treatasBinary==FALSE) {return(0)}
        if(input$treatasBinary==TRUE) {
          max(ceiling(length(input$selectedSymptoms))*30,
              300 # minumum reserved space
          )
        }}else{return(0)} # height of plot when no data available    
  }
  
  numRowsMedianPlot <- function(){    
      if(!is.null(input$selectedEvaluationTime2)) {
        if (input$treatasBinary==TRUE) {return(0)}
        if(input$treatasBinary==FALSE) {
          max(ceiling(length(input$selectedSymptoms))*30,
              300 # minumum reserved space
          )
        }}else{return(0)} # height of plot when no data available    
  }
  
  numRowsClustering <- function() {if(!is.null(dataFiltered())){
    #if(input$treatasBinary==TRUE) {return(0)}
    max(ceiling(length(input$selectedSymptoms))*40,
        300)}else{return(0)}}
  
  numRowsClustering2 <- function() {if(!is.null(dataFiltered())){
    #if(input$treatasBinary==TRUE) {return(0)}
    max(ceiling(length(input$selectedSymptoms))*40,
        300)}else{return(0)}}
  
  numRowsClustering3 <- function() {if(!is.null(dataFiltered())){
    #if(input$treatasBinary==TRUE) {return(0)}
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
  
  numRowsProportion <- function(){
    if(!(is.null(dataFiltered.yn()) || is.null(input$selectedMeasurementForPresencePlot) )){
      if(input$selectedGraphOverTime!="presencePlot") {return(0)}
      if(input$treatasBinary==TRUE){
        max(ceiling(length(input$selectedSymptoms))*30,
            300)
      }} else {return(0)}
  }
  
  numRowsRCSModel <- function() {if(!is.null(regressionScenario())){
    if(regressionScenario()!="scenarioRCSModel") {return(0)}
    max(ceiling(length(input$selectedSymptoms))*100,
        300)}else{return(0)}}
  
  numRowsMixedModels1 <- function(){if(!is.null(dataFiltered()) &
                                         !is.null(input$selectedMixedModelType)){
    max(ceiling(length(input$selectedSymptoms))*30*(length(unique(dataFiltered()[,input$selectedCovariate1st]))-1),
        300)  }else{return(0)} }
  
  numRowsMixedModels2 <- function(){if(!is.null(dataFiltered()) &
                                         !is.null(input$selectedMixedModelType)){
    nFacets <- length(unique(Measurement()))-1
    (input$selectedMixedModelType=="MMmeasurement")*
      max(ceiling(length(input$selectedSymptoms))*30*nFacets,
          300 ) }else{return(0)} }
  
  numRowsMixedModels3 <- function(){if(!is.null(dataFiltered()) &
                                         !is.null(input$selectedMixedModelType)){
    (input$selectedMixedModelType=="MMtimeSinceInclusion")*
      max( ceiling(length(input$selectedSymptoms))*30,
           300  )}else{return(0)} }
  
  NumRows <- function(){if(!is.null(dataFiltered())){
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
  
  # dataExtendedwithThreshold() - all data with threshold value honored ####
  # sets all symptom values below threshold value to zero
  dataExtendedwithThreshold <- reactive ({
    if(!(is.null(dataExtended()) || is.null(input$thresholdValue)  )){
      data <- dataExtended()
      data[,input$selectedSymptoms] <- 
        ifelse(data[, input$selectedSymptoms]>input$thresholdValue, 1, 0)
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

  
  # SIDEBAR ####
  
  # GUI - printing medpot package version
  output$medplotVersion <- renderText({
    paste("Version:",packageVersion(pkg="medplot"))
  })
  
  
  # GUI - selecting symptoms ####
  output$selectSymptoms <- renderUI({
    if (!is.null(dataVariableNames())) {
      selectInput(inputId="selectedSymptoms",
                  label="Choose outcome variables to analyse (the order used is also used on most graphs):", 
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
                     max=9)
      }}
  })
  
  # GUI - reseting threshold value
  observe({
    input$treatasBinary
    updateNumericInput(session, inputId="thresholdValue", value=0)
  })
  
  # TABS ####
  # message - used on all tabs
  output$messageSelectVars <- renderUI(
    {if(is.null(dataFiltered())) 
{h4("Please use the menus below to upload data,
    select parameters and one or more variables to analyse.")}
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

# TAB - Graphical exploration ####
output$selectGraphOverTime <- renderUI({
  if (!is.null(dataFiltered())) {
    selectInput(inputId="selectedGraphOverTime",
                label="Select type of graph:",
                choices= if (input$treatasBinary==TRUE) {
                  c("Lasagna plots"="lasagnaPlot",
                    "Barplots with proportions"="presencePlot",
                    "Timeline"="timelinePlot"
                  )} else {
                    c("Profile plots"="profilePlot",
                      "Lasagna plots"="lasagnaPlot",
                      "Boxplots"="boxPlot",
                      "Timeline"="timelinePlot" 
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
# reactive code for ploting
plotTimelineProfilesReactive <- reactive({
  plotTimelineProfiles(data=dataFiltered(),
                             plotType=input$selectedGraphType,
                             personIDVar=input$patientIDVar,
                             measurementVar=input$measurementVar,
                             selectedSymptoms=input$selectedSymptoms,
                             sizeofRandomSample=input$sampleSize,
                             sizeofGroup=input$groupSize) 
    })
# the plot rendered in browser
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

        plotTimelineProfilesReactive()

      }}}
  }, height=numRowsTimelineProfile
)

# the plot to be downloaded - high quality plot
output$downLoadplotTimelineProfiles <-  downloadPlot(
  plotFunction=plotTimelineProfilesReactive,
  width=clientData$output_plotTimelineProfiles_width,
  height=clientData$output_plotTimelineProfiles_height,
  print=TRUE
  )
      
output$plotTimelineProfilesDescr <- reactive({
  if(!is.null(input$selectedGraphType)) {
    if(input$selectedGraphType=="profilePlot") {
  description <- switch(input$selectedGraphType,
         "oneGraph" = generateDescription("GraphExpl_ProfilePlots_AllSubjects"),
         "randomSample" = generateDescription("GraphExpl_ProfilePlots_RandomSubjects"),
         "multipleGraphs" = generateDescription("GraphExpl_ProfilePlots_MultipleGraphs")
    )
  return(description())
    } else {return()}
  }})


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
      filenamesEPS <- vector()
      
      # generate as many files as there are plots
      for (symptom in input$selectedSymptoms) {
        #filenames[symptom] <- tempfile(pattern="symptom", tmpdir=paste0(workingDir,"\\www\\temp"), fileext=".png")
        filenames[symptom] <- tempfile(pattern="symptom", tmpdir=paste0(workingDir,"/www/temp"), fileext=".png")
        filenamesEPS[symptom] <- tempfile(pattern=symptom, fileext = ".eps")
        
        # plot PNG graph for each symptom
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
        
        # prepare EPS graphs
        width = 5 # in inches
        height = 5 # in inches
        postscript(filenamesEPS[symptom], paper="special", width=width, height = height)
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

      # create a ZIP file of EPS plots
      zipFile <<- tempfile(pattern="zip", tmpdir=paste0(workingDir,"/www/temp/"), fileext=".zip")
      zip(zipfile=zipFile, files=filenamesEPS, flags="-Dj")
      
      
      out <- pastePlotFilenames(filenames)
      
      return(div(HTML(out),class="shiny-plot-output shiny-bound-output"))
    }}
})

# prepare ZIP for downloading EPS files
output$lasagnaDownload <- downloadHandler(
  filename="Lasagna.zip",
  content= function(file) {
    file.copy(from=zipFile, to=file)
  }, contentType="application/octet-stream")
  


# prepare download button
output$downloadLasagna <- renderUI({
  out <- downloadButton(outputId = "lasagnaDownload", label = "Download")
  #         # delete EPS files
  #         unlink(filenamesEPS)
  return(out)
})

output$plotLasagnaDesc <- reactive({
  if (!is.null(input$selectedGraphOverTime)) {
    if (input$selectedGraphOverTime=="lasagnaPlot") {
    description <- generateDescription("GraphExpl_LasagnaPlots")
    
    return(description())
  }}})


# Boxplots ####
# Menu
output$selectFacetingType <- renderUI({
  if(!is.null(input$selectedGraphOverTime)) {
    if (input$selectedGraphOverTime=="boxPlot") {
      selectInput(inputId="selectedFacetingType",
                  label="Select faceting type:",
                  choices=c("Variables ~ Evaluation occasions"="variablesOnYaxis",
                            "Evaluation occasions ~ Variables"="variablesOnXaxis")
                  )
    }}
  })


# Graph
plotTimelineBoxplotsReactive <- reactive({
  #print(
    plotTimelineBoxplots(data=dataFiltered(),
                             personIDVar=input$patientIDVar,
                             measurementVar=input$measurementVar,
                             selectedSymptoms=input$selectedSymptoms,
                             faceting=input$selectedFacetingType)
    #)
  
})

output$plotTimelineBoxplots <- renderPlot({
  if(!is.null(dataFiltered())) {
    if(input$selectedGraphOverTime=="boxPlot") {
      if(input$treatasBinary==FALSE){
        
        progress <- Progress$new(session, min=1, max=100)
        on.exit(progress$close())
        
        progress$set(message = 'Calculation in progress',
                     detail = 'This may take a while...', 
                     value=NULL)

        plotTimelineBoxplotsReactive()
        }
    }
  } else {return()}
},height=numRowsTimelineBoxplots)

output$downLoadplotTimelineBoxplot <- downloadPlot(
  plotFunction = plotTimelineBoxplotsReactive,
  width = clientData$output_plotTimelineBoxplots_width,
  height = clientData$output_plotTimelineBoxplots_height,
  print = TRUE)

output$plotTimelineBoxplotsDesc <- reactive({
  if (!is.null(input$selectedGraphOverTime)) {
    if (input$selectedGraphOverTime=="boxPlot") {
      description <- generateDescription("GraphExpl_BoxPlots")
      
      return(description())
    }}})


# Timeline graph ####
# Menu
output$selectDisplayFormat <- renderUI({
  if(!is.null(dataFiltered())){
    if(input$selectedGraphOverTime=="timelinePlot") {
        selectInput(inputId="displayFormat",
                    label="Choose what to display on the horizontal axis:",
                    choices=c("Dates" = "dates",
                              "Time from inclusion" ="timeFromInclusion",
                              "Evaluation occasions" = "measurementOccasions"),
                    selected="dates",
                    multiple=FALSE)
      }}
})

# Graph
plotTimelineReactive <- reactive({
  
  if (input$treatasBinary == TRUE) { 
    data=dataFilteredwithThreshold()
  } else { data=dataFiltered() }

  plotTimeline(data=data,
                     date=input$dateVar,
                     personID=input$patientIDVar,
                     measurement=input$measurementVar,
                     symptoms=input$selectedSymptoms,
                     displayFormat = input$displayFormat,
                     treatasBinary=input$treatasBinary)
  })
  

output$plotTimeline <- renderPlot({
  if(!(is.null(dataFiltered()) || is.null(input$displayFormat))){
    if(input$selectedGraphOverTime=="timelinePlot") {
        
        progress <- Progress$new(session, min=1, max=100)
        on.exit(progress$close())
        
        progress$set(message = 'Calculation in progress',
                     detail = 'This may take a while...', 
                     value=NULL)

      plotTimelineReactive()
      }}  
}, height=numRowsTimeline)

output$downLoadplotTimeline <- downloadPlot(plotFunction = plotTimelineReactive,
                                            width = clientData$output_plotTimeline_width,
                                            height = clientData$output_plotTimeline_height,
                                            print = TRUE)

output$plotTimelineDesc <- reactive({
  if (!is.null(input$selectedGraphOverTime)) {
    if (input$selectedGraphOverTime=="timelinePlot") {
      description <- generateDescription("GraphExpl_Timeline")
      
      return(description())
    }}})


#Barplots with proportions ####
#Menu
output$selectMeasurementForPresencePlot <- renderUI({
  if(!is.null(input$selectedGraphOverTime)) {
    if(input$selectedGraphOverTime=="presencePlot") {
      selectInput(inputId="selectedMeasurementForPresencePlot",
                  label="Select evaluation occasion:",
                  choices=measurementLevels(), selected=measurementLevels()[1])
    }}
})

plotProportionReactive <- reactive({
  plotDistribution(data=dataFiltered.yn(),
                   selectedSymptoms=input$selectedSymptoms,
                   selectedProportion=input$selectedMeasurementForPresencePlot,
                   measurements=Measurement())
})

# Plot - Presence (plot - proportions) ###
output$plotProportion=renderPlot({
  if(!(is.null(dataFiltered.yn()) || is.null(input$selectedMeasurementForPresencePlot) )){
    if(input$treatasBinary==TRUE){

      plotProportionReactive()
    }}
  }, height=numRowsProportion) 

output$downLoadplotProportion <- downloadHandler (
  # since this is base graphics (not ggplot) the EPS file generation
  # has to be handled differently - the downloadPlot() function does not work
  # due to "Cairo" graphics device being used instead of "postscipt"
  # maybe has to do with being called from a reactive function and/or plot being 
  # in a different environement?
  filename="plot.eps",
  
  content = function (file) {
    width = clientData$output_plotProportion_width
    height = clientData$output_plotProportion_height
    postscript(file, paper="special", width = width/72, height = height/72)
    
    plotDistribution(data=dataFiltered.yn(),
                     selectedSymptoms=input$selectedSymptoms,
                     selectedProportion=input$selectedMeasurementForPresencePlot,
                     measurements=Measurement())
    
    dev.off()
  }, contentType="application/postscript")

output$plotProportionDesc <- reactive({
  if (!is.null(input$selectedGraphOverTime)) {
    if (input$selectedGraphOverTime=="presencePlot") {
      description <- generateDescription("GraphExpl_Barplots")
      
      return(description())
    }}})



# TAB - Summary ####

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

# Pyramid plot ####
# Graph

plotPyramidReactive <- reactive({
  
  plotPropPositive(data=dataFilteredwithThreshold(),
                   grouping=input$groupingVar,
                   measurements=input$measurementVar,
                   symptomsNames=input$selectedSymptoms)
  
})

output$plotPyramid <- renderPlot ({
  try({
    if(!(is.null(dataFilteredwithThreshold()) || is.null(input$treatasBinary) )){
      if(input$treatasBinary==TRUE){
        
        progress <- Progress$new(session, min=1, max=100)
        on.exit(progress$close())
        
        progress$set(message = 'Calculation in progress',
                     detail = 'This may take a while...', 
                     value=NULL)
        
       plotPyramidReactive()
      }}}, silent=TRUE)
} ,height=numRowsProportions)

output$downLoadplotPyramid <- downloadHandler (
  # since this is base graphics (not ggplot) the EPS file generation
  # has to be handled differently - the downloadPlot() function does not work
  # due to "Cairo" graphics device being used instead of "postscipt"
  # maybe has to do with being called from a reactive function and/or plot being 
  # in a different environement?
  filename="plot.eps",
  
  content = function (file) {
    width = clientData$output_plotPyramid_width
    height = clientData$output_plotPyramid_height
    postscript(file, paper="special", width = width/72, height = height/72)
    
    plotPropPositive(data=dataFilteredwithThreshold(),
                     grouping=input$groupingVar,
                     measurements=input$measurementVar,
                     symptomsNames=input$selectedSymptoms)
    
    dev.off()
  }, contentType="application/postscript")

# calculate data for tables of medians & CI plots ####
dataforSummaryNonBinary <- reactive({
  if(!is.null(dataFiltered())) {
    if(input$treatasBinary==FALSE){
      progress <- Progress$new(session, min=1, max=100)
      on.exit(progress$close())
      
      progress$set(message = 'Calculation in progress',
                   detail = 'This may take a while...', 
                   value=NULL)
      
      tableMedians(measurement=input$selectedEvaluationTime2,
                                     measurementVar=input$measurementVar,
                                     data=dataFiltered(),
                                     selectedSymptoms=input$selectedSymptoms)
    }}
})

# Median tables ####
output$tableforBoxplots <- renderDataTable({
  if(!is.null(dataFiltered())) {
    if(input$treatasBinary==FALSE){
      return(dataforSummaryNonBinary()[["printableTable"]])
    }}
  }, options=list(bFilter=FALSE, bPaginate=FALSE, bInfo=FALSE))

# Median plot
plotMediansReactive <- reactive({
  
  plotValueswithCIs(data=dataforSummaryNonBinary()[["rawTable"]],
                    variableName="Variables",
                    valueName="Median",
                    CILowerName="CILower",
                    CIUpperName="CIUpper",
                    xLabel="Medians",
                    yLabel="Variable",
                    graphTitle="Medians of variables \n(with 95% confidence intervals)",
                    vLine=NULL,
                    variableOrder=input$selectedSymptoms)
  })


output$plotMedians <- renderPlot({
  plotMediansReactive()
}, height=numRowsMedianPlot)

output$downLoadplotMedians <- downloadPlot(
  plotFunction = plotMediansReactive,
  width = clientData$output_plotMedians_width,
  height = clientData$output_plotMedians_height,
  print = TRUE
  )

# Medians description
output$mediansDescr <- reactive({
  if (!is.null(dataFiltered())) {
    if (input$treatasBinary==FALSE) {
      description <- generateDescription("Summary_Medians")
      
      return(description())
    }}})

# Proportions tables
output$tableforProportions <- renderDataTable({
  if(!is.null(dataFilteredwithThreshold())) {
    if(input$treatasBinary==TRUE){
      
      out <- tableProportions(measurement=input$selectedEvaluationTime2,
                                               measurementVar=input$measurementVar,
                                               data=dataFilteredwithThreshold(),
                                               selectedSymptoms=input$selectedSymptoms) 
      return(out)
    }}
}, options=list(bFilter=FALSE, bPaginate=FALSE, bInfo=FALSE))

# Proportions graph
plotPresenceReactive <- reactive({
  plot <- plotPresenceofSymptoms(data=dataFiltered(),
                                 selectedSymptoms=input$selectedSymptoms,
                                 measurementVar=input$measurementVar,
                                 measurement=input$selectedEvaluationTime2,
                                 thresholdValue=ifelse(!is.null(input$thresholdValue),input$thresholdValue ,0))
  })

output$plotPresence <- renderPlot({
  if(!is.null(input$selectedEvaluationTime2)) {
    if(input$treatasBinary==TRUE) {
      
      progress <- Progress$new(session, min=1, max=100)
      on.exit(progress$close())
      
      progress$set(message = 'Calculation in progress',
                   detail = 'This may take a while...', 
                   value=NULL)
      
      print(plotPresenceReactive())
    }}
}, height=numRowsPresencePlot)

output$downLoadplotPresence <- downloadPlot(
  plotFunction = plotPresenceReactive,
  width = clientData$output_plotPresence_width,
  height = clientData$output_plotPresence_height,
  print = TRUE
  )


# Proportions description
output$proportionsDescr <- reactive({
  if (!is.null(dataFiltered())) {
    if (input$treatasBinary==TRUE) {
      description <- generateDescription("Summary_Proportions")
      
      return(description())
    }}})

# TAB - Summary tables : grouping variable ####
# Graph
# Proportions by groups with confidence intervals ####
# Graph
plotPropPositiveCIReactive <- reactive({
 
  plotPropPositiveCI(data=dataFilteredwithThreshold(),
                     groupingVar=input$groupingVar,
                     measurementVar=input$measurementVar,
                     selectedSymptoms=input$selectedSymptoms)
  
})

output$plotPropCIs <- renderPlot ({
  try({
    if(!is.null(dataFilteredwithThreshold())){
      if(input$treatasBinary==TRUE){
        
        progress <- Progress$new(session, min=1, max=100)
        on.exit(progress$close())
        
        progress$set(message = 'Calculation in progress',
                     detail = 'This may take a while...', 
                     value=NULL)

plotPropPositiveCIReactive()
      }}}, silent=TRUE)
} ,height=numRowsProportionsCI)

output$downLoadPropPositiveCI <- downloadPlot(
  plotFunction = plotPropPositiveCIReactive,
  width = clientData$output_plotPropCIs_width,
  height = clientData$output_plotPropCIs_height,
  print = TRUE
  )


# Menu
output$UIpropTable = renderUI({
  if(!is.null(measurementLevels())){
    #select the measurement
    selectInput(inputId="measurementSelectedprop",
                label="Select evaluation occasion:", 
                choices=measurementLevels(), selected=measurementLevels()[1])
    }
  })

output$UIdoPvalueAdjustments <- renderUI({
  if(!is.null(measurementLevels())){
    checkboxInput(inputId="doPValueAdjustments",
                  label="Calculate P value adjustments? (It may take a long time.)",
                  value=FALSE)
  }
  })

# Tables
# Table of proportions of patients in a group with a symptom ####
output$tablePropGroups <- renderDataTable ({  
  if(!(is.null(dataFiltered()) || is.null(input$thresholdValue))){
    if(input$treatasBinary==TRUE){
      
      progress <- Progress$new(session, min=1, max=100)
      
      progress$set(message = 'Calculation in progress',
                   detail = 'This will take a while...', 
                   value=NULL)
      
      on.exit(progress$close())
      
      out <- tablePropPosGroups(data=dataFiltered(),
                                   groupingVar=input$groupingVar,
                                   measurementVar=input$measurementVar,
                                   forMeasurement=input$measurementSelectedprop,
                                   symptomsNames=input$selectedSymptoms,
                                   thresholdValue=input$thresholdValue,
                                   doPValueAdjustments=input$doPValueAdjustments
      )
      return(out)
    }}
  }, options=list(bFilter=FALSE, bPaginate=FALSE, bInfo=FALSE))

# text - explaining tablePropGroups
output$textTablePropGroups <- reactive({
  if(!is.null(dataFiltered())){
    if(input$treatasBinary==TRUE){
#       tagList(p("Table displays for each variable the proportion of subjects in a
#                 certain group, P value for the difference of proportions and the 
#                 95% confidence interval for the difference of proportions. 
#                 Data with missing values for grouping variable 
#                 are removed from analysis.", br(), br()))
      
      description <- generateDescription("SummaryGrouping_Proportions")
      
      return(description())
    }}
})

# Table with medians of symptoms values in a group ####
output$tableMedianGroups <- renderDataTable ({
  if(!(is.null(dataFiltered()) || is.null(input$measurementSelectedprop) )){
    if(input$treatasBinary==FALSE){
      
      progress <- Progress$new(session, min=1, max=100)
      
      progress$set(message = 'Calculation in progress',
                   detail = 'This will take a while...', 
                   value=NULL)
      
      on.exit(progress$close())
      
      tableMeGroups(data=dataFiltered(),
                               groupingVar=input$groupingVar,
                               measurementVar=input$measurementVar,
                               forMeasurement=input$measurementSelectedprop,
                               symptomsNames=input$selectedSymptoms,
                               thresholdValue=input$thresholdValue,
                               doPValueAdjustments=input$doPValueAdjustments)
      }}
  }, options=list(bFilter=FALSE, bPaginate=FALSE, bInfo=FALSE))

# text - explainig tableMedianGroups
output$textTableMedianGroups <- reactive({
  if(!is.null(dataFiltered())){
    if(input$treatasBinary==FALSE){
#       tagList(p("Table displays for each variable the median value for subjects in a
#                 certain group, interquantile range for of the variable 
#                 (25th to 75th percentile)and P value for the difference of samples (Mann-Whitney test). 
#                 Data with missing values for grouping variable 
#                 are removed from analysis. Threshold for positivity of variables is not taken into account.", br(), br() ))
      description <- generateDescription("SummaryGrouping_Medians")
      
      return(description())
      
    }}
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
    #if(input$treatasBinary==FALSE){
      #select the measurement
      selectInput(inputId="selectedMeasurementValue",
                  label="Select evaluation occasion:", 
                  choices=measurementLevels(), selected=measurementLevels()[1])
    }#}
  })

# Graphs
# Dendrogram plot ####
output$plotClusterDendrogram=renderPlot({
  if(!(is.null(dataFiltered()) || is.null(input$selectedMeasurementValue) )){
    #if(input$treatasBinary==FALSE){
    if (input$treatasBinary==TRUE) {data=dataFilteredwithThreshold()} else {data=dataFiltered()}  
    plotDendrogram(data=data,
                          variableName=input$measurementVar,
                          variableValue=input$selectedMeasurementValue,
                          selectedSymptoms=input$selectedSymptoms,
                          treatasBinary=input$treatasBinary)
  }#}
},height=numRowsClustering)


output$downLoadplotClusterDendrogram <- downloadHandler(
  # since this is base graphics (not ggplot) the EPS file generation
  # has to be handled differently - the downloadPlot() function does not work
  # due to "Cairo" graphics device being used instead of "postscipt"
  # maybe has to do with being called from a reactive function and/or plot being 
  # in a different environement?
  filename="plot.eps",
  
  content = function (file) {
    width = clientData$output_plotClusterDendrogram_width
    height = clientData$output_plotClusterDendrogram_height
    postscript(file, paper="special", width = width/72, height = height/72)
    
    if (input$treatasBinary==TRUE) {data=dataFilteredwithThreshold()} else {data=dataFiltered()}
    plotDendrogram(data=data,
                   variableName=input$measurementVar,
                   variableValue=input$selectedMeasurementValue,
                   selectedSymptoms=input$selectedSymptoms,
                   treatasBinary=input$treatasBinary)
    
    dev.off()
  }, contentType="application/postscript"
  )

# Dendrogram description
output$dendrogramDescr <- reactive({
  if (!is.null(dataFiltered())) {
    if (!is.null(input$selectedMeasurementValue)) {
      description <- generateDescription("Clustering_Dendrogram")
      
      return(description())
    }}})

# Heatmap - Selection of annotation variables
output$selectClusterAnnotations <- renderUI({
  if(!is.null(dataFiltered())){
    #if(input$treatasBinary==FALSE){
      selectedSymptoms <- which(dataVariableNames() %in% input$selectedSymptoms)
      selectInput(inputId="selectedClusterAnnotations",
                  label="Select variables for annotating graph:",
                  # TODO: remove some variables from selection
                  choices=dataVariableNames()[-selectedSymptoms],
                  selected=c(input$groupingVar),
                  multiple=TRUE)
    }#}
  })



# Heatmap plot ####
output$plotClusterHeatmap=renderPlot({
  if(!is.null(dataExtended())){
    #if(input$treatasBinary==FALSE){
    if (input$treatasBinary==TRUE) {data=dataExtendedwithThreshold()} else {data=dataExtended()}  
    plotClusterHeatmap(data=data,
                         #TODO: make dependent on selection
                         variableName=input$measurementVar,
                         variableValue=input$selectedMeasurementValue,
                         selectedSymptoms=input$selectedSymptoms,
                         annotationVars=input$selectedClusterAnnotations,
                       treatasBinary=input$treatasBinary) 
    }#}
  },height=numRowsClustering2)


output$downLoadplotClusterHeatmap <- downloadHandler(
  # since this is base graphics (not ggplot) the EPS file generation
  # has to be handled differently - the downloadPlot() function does not work
  # due to "Cairo" graphics device being used instead of "postscipt"
  # maybe has to do with being called from a reactive function and/or plot being 
  # in a different environement?
  filename="plot.eps",
  
  content = function (file) {
    width = clientData$output_plotClusterHeatmap_width
    height = clientData$output_plotClusterHeatmap_height
    postscript(file, paper="special", width = width/72, height = height/72)
    
    if (input$treatasBinary==TRUE) {data=dataExtendedwithThreshold()} else {data=dataExtended()}  
    plotClusterHeatmap(data=data,
                       #TODO: make dependent on selection
                       variableName=input$measurementVar,
                       variableValue=input$selectedMeasurementValue,
                       selectedSymptoms=input$selectedSymptoms,
                       annotationVars=input$selectedClusterAnnotations,
                       treatasBinary=input$treatasBinary)
    
    dev.off()
  }, contentType="application/postscript"
)

# Heat map description
output$heatmapDescr <- reactive({
  if (!is.null(dataFiltered())) {
    if (!is.null(input$selectedMeasurementValue)) {
      description <- generateDescription("Clustering_Heatmap")
      
      return(description())
    }}})


# Correlation plot ####
output$plotClusterCorrelations <- renderPlot({
  if(!is.null(dataExtended())){
    if (input$treatasBinary==TRUE) {data=dataFilteredwithThreshold()} else {data=dataFiltered()}
    
    plotCorrelations(data=data,
                            variableName=input$measurementVar,
                            variableValue=input$selectedMeasurementValue,
                            selectedSymptoms=input$selectedSymptoms,
                            treatasBinary=input$treatasBinary)
    
  }
  },height=numRowsClustering3)


output$downLoadplotClusterCorrelations <- downloadHandler(
  # since this is base graphics (not ggplot) the EPS file generation
  # has to be handled differently - the downloadPlot() function does not work
  # due to "Cairo" graphics device being used instead of "postscipt"
  # maybe has to do with being called from a reactive function and/or plot being 
  # in a different environement?
  filename="plot.eps",
  
  content = function (file) {
    width = clientData$output_plotClusterCorrelations_width
    height = clientData$output_plotClusterCorrelations_height
    postscript(file, paper="special", width = width/72, height = height/72)
    
    if (input$treatasBinary==TRUE) {data=dataFilteredwithThreshold()} else {data=dataFiltered()}
    
    plotCorrelations(data=data,
                     variableName=input$measurementVar,
                     variableValue=input$selectedMeasurementValue,
                     selectedSymptoms=input$selectedSymptoms,
                     treatasBinary=input$treatasBinary)
    
    dev.off()
  }, contentType="application/postscript"
)


# Correlation plot description
output$correlationDescr <- reactive({
  if (!is.null(dataFiltered())) {
    if (!is.null(input$selectedMeasurementValue)) {
      description <- generateDescription("Clustering_Correlations")
      
      return(description())
    }}})


# TAB - Regression model : one evaluation ####
# Menus ####
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
      checkboxInput(inputId="useFirthCorrection",
                    label="Use Firth correction?",
                    value=FALSE)  
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
    }}
  })

# Scenario - Logistic regression with Firth correction
# Create results of Logistic regression with Firth correction
resultsLogistf <- reactive({
  if(!is.null(regressionScenario())) {
    if(regressionScenario()=="scenarioLogistf") {
      out <- tableLogistf(data=dataExtended(),
                             measurementVar=input$measurementVar,
                             selectedMeasurement=input$selectedEvaluationTime,
                             covariate=input$selectedCovariate,
                             selectedSymptoms=input$selectedSymptoms,
                             thresholdValue=input$thresholdValue)
      return(out)
    }}
})


# plot - logistf ####
plotLogistfReactive <- reactive({
  if(regressionScenario()!="scenarioLogistf") {return()}
  plotValueswithCIs(data=resultsLogistf()[["rawResultsTable"]],
                    variableName="Variable",
                    valueName="OR",
                    CILowerName="CILower",
                    CIUpperName="CIUpper",
                    xLabel="Odds ratios",
                    yLabel="Variables",
                    graphTitle=paste("Odds ratios and confidence intervals for",
                                     resultsLogistf()[["referenceValue"]], 
                                     "\n at evaluation T=",
                                     input$selectedEvaluationTime,
                                     "(using Firth correction)"),
                    vLine=1,
                    variableOrder=input$selectedSymptoms) 
  }) 

output$plotLogistf <- renderPlot({
  if(!is.null(resultsLogistf()) ){
    if(regressionScenario()=="scenarioLogistf") {

      print(plotLogistfReactive())
    }}
}, height=numRowsLogistf)

# table - logistf ####
output$tableLogistf <- renderDataTable({
  
  if(!is.null(resultsLogistf()) ){
    if(regressionScenario()=="scenarioLogistf") {
      return(resultsLogistf()[["printableResultsTable"]])
    }}
}, options=list(bFilter=FALSE, bPaginate=FALSE, bInfo=FALSE))

# table - logistf Intercepts
output$tableLogistfIntercept <- renderDataTable({
  if(!is.null(resultsLogistf()) ){
    if(regressionScenario()=="scenarioLogistf") {
      return(resultsLogistf()[["printableInterceptTable"]])
    }}
}, options=list(bFilter=FALSE, bPaginate=FALSE, bInfo=FALSE))

# description logistf
output$logistfDescr <- reactive({
  if (!is.null(resultsLogistf())) {
    if (regressionScenario()=="scenarioLogistf") {
      description <- generateDescription("RegressionOne_Firth")
      
      return(description())
    }}})


# Scenario - logistic regression (without Firth correction) ####
resultsLogist <- reactive({
  if(!is.null(regressionScenario()) ){
    if(regressionScenario()=="scenarioLogist") {
      out <- tableLogist(data=dataExtended(),
                            measurementVar=input$measurementVar,
                            selectedMeasurement=input$selectedEvaluationTime,
                            covariate=input$selectedCovariate,
                            selectedSymptoms=input$selectedSymptoms,
                            thresholdValue=input$thresholdValue)
      return(out)
    }}
})


# table - logist ####
output$tableLogist <- renderDataTable({
  if(!is.null(resultsLogist()) ){
    if(regressionScenario()=="scenarioLogist") {
      resultsLogist()[["printableResultsTable"]]
    }}
}, options=list(bFilter=FALSE, bPaginate=FALSE, bInfo=FALSE))

# table - logist intercept ####
output$tableLogistIntercept <- renderDataTable({
  if(!is.null(resultsLogist()) ){
    if(regressionScenario()=="scenarioLogist") {
      resultsLogist()[["printableInterceptTable"]]
    }}
}, options=list(bFilter=FALSE, bPaginate=FALSE, bInfo=FALSE))

# plot - logist ####
plotLogistReactive <- reactive({
  plotValueswithCIs(data=resultsLogist()[["rawResultsTable"]],
                           variableName="Variable",
                           valueName="OR",
                           CILowerName="CILower",
                           CIUpperName="CIUpper",
                           xLabel="Odds ratios",
                           yLabel="Variables",
                           graphTitle=paste("Odds ratios and confidence intervals for",
                                            resultsLogist()[["referenceValue"]], 
                                            "\n at evaluation T=",
                                            input$selectedEvaluationTime),
                           vLine=1,
                           variableOrder=input$selectedSymptoms) 
  
})

output$plotLogist <- renderPlot({
  if(!is.null(resultsLogist())){
    if(regressionScenario()=="scenarioLogist") {

      plotLogistReactive()
    }}
}, height=numRowsLogist)

# description logist
output$logistDescr <- reactive({
  if (!is.null(resultsLogist())) {
    if (regressionScenario()=="scenarioLogist") {
      description <- generateDescription("RegressionOne_OddsRatio")
      
      return(description())
    }}})

# Scenario - linear regression
resultsLinear <- reactive({
  if (!is.null(regressionScenario())) {
    if (regressionScenario()=="scenarioLinearModel") {
      out <- tableLinear(data=dataExtended(),
                            measurementVar=input$measurementVar,
                            selectedMeasurement=input$selectedEvaluationTime,
                            covariate=input$selectedCovariate,
                            selectedSymptoms=input$selectedSymptoms)
      return(out)
    }}
})

# table - linear ####
output$tableLinear <- renderDataTable({
  if(!is.null(resultsLinear())){
    if (regressionScenario()=="scenarioLinearModel") {
      resultsLinear()[["printableResultsTable"]]
    }}
}, options=list(bFilter=FALSE, bPaginate=FALSE, bInfo=FALSE))

# table - linear Intercepts ####
output$tableLinearIntercept <- renderDataTable({
  if(!is.null(resultsLinear())){
    if (regressionScenario()=="scenarioLinearModel") {
      resultsLinear()[["printableInterceptTable"]]
    }}
}, options=list(bFilter=FALSE, bPaginate=FALSE, bInfo=FALSE))

# plot - linear ####
plotLinearReactive <- reactive({
  plotValueswithCIs(data=resultsLinear()[["rawResultsTable"]],
                           variableName="Variable",
                           valueName="beta",
                           CILowerName="CILower",
                           CIUpperName="CIUpper",
                           xLabel="Beta (slope) coefficient",
                           yLabel="Variables",
                           graphTitle=paste("Beta coefficients and confidence intervals for effects of",
                                            input$selectedCovariate, 
                                            "\n on selected variables at evaluation T=",
                                            input$selectedEvaluationTime),
                           vLine=0,
                           variableOrder=input$selectedSymptoms)  
  })

output$plotLinear <- renderPlot({
  if(!is.null(resultsLinear())){
    if (regressionScenario()=="scenarioLinearModel") {

      plotLinearReactive()
    }}
  }, height=numRowsLinear) 

# description linear
output$linearDescr <- reactive({
  if (!is.null(resultsLinear())) {
    if (regressionScenario()=="scenarioLinearModel") {
      description <- generateDescription("RegressionOne_Linear")
      
      return(description())
    }}})


# Scenario - modeling with Restricted Cubic Splines

# plot - RCS plot ####
plotRCSReactive <- reactive({
  
  plotRCS(data.all=dataExtended(),
          data.yn=dataFiltered.yn(),
          measurement=Measurement(),
          selectedSymptoms=input$selectedSymptoms,
          measurementSelectedrcs=input$selectedEvaluationTime,
          rcsIDVar=input$selectedCovariate,
          binaryVar=input$treatasBinary) 
})

output$plotRCS=renderPlot({
  
  if(!is.null(regressionScenario())){
    if (regressionScenario()=="scenarioRCSModel") {
      
      plotRCSReactive()
    }}
}, height=numRowsRCSModel)

# table - RCS table ####
output$tableRCS <- renderDataTable({
  
  if(!is.null(regressionScenario())){
    if (regressionScenario()=="scenarioRCSModel") {
      tableRCS(data.all=dataExtended(),
                  data.yn=dataFiltered.yn(),
                  measurement=Measurement(),
                  selectedSymptoms=input$selectedSymptoms,
                  measurementSelectedrcs=input$selectedEvaluationTime,
                  rcsIDVar=input$selectedCovariate, 
                  binaryVar=input$treatasBinary
      )
    }}
}, options=list(bFilter=FALSE, bPaginate=FALSE, bInfo=FALSE))

# description RCS
output$RCSDescr <- reactive({
  if (!is.null(regressionScenario())) {
    if (regressionScenario()=="scenarioRCSModel") {
      description <- generateDescription("RegressionOne_RCS")
      
      return(description())
    }}})


# download "buttons" for each of the regression graphs ####
output$linearRegDownload <- renderUI({
  if (regressionScenario()=="scenarioLinearModel") {
    output$downLoadRegressionOneTime1 <- downloadPlot(plotFunction = plotLinearReactive,
                              width = clientData$output_plotLinear_width,
                              height = clientData$output_plotLinear_height,
                              print = TRUE)
    downloadButton("downLoadRegressionOneTime1", label="Download")
  }
  })

output$logistRegDownload <- renderUI({
  if (regressionScenario()=="scenarioLogist") {
    output$downLoadRegressionOneTime2 <- downloadPlot(plotFunction = plotLogistReactive,
                                                     width = clientData$output_plotLogist_width,
                                                     height = clientData$output_plotLogist_height,
                                                     print = TRUE)
    downloadButton("downLoadRegressionOneTime2", label="Download")
  }
})

output$logistfRegDownload <- renderUI({
  if (regressionScenario()=="scenarioLogistf") {
    output$downLoadRegressionOneTime3 <- downloadPlot(plotFunction = plotLogistfReactive,
                                                     width = clientData$output_plotLogistf_width,
                                                     height = clientData$output_plotLogistf_height,
                                                     print = TRUE)
    downloadButton("downLoadRegressionOneTime3", label="Download")
  }
})

output$RCSRegDownload <- renderUI({
  if (regressionScenario()=="scenarioRCSModel") {
    output$downLoadRegressionOneTime4 <- downloadHandler(
      # since this is base graphics (not ggplot) the EPS file generation
      # has to be handled differently - the downloadPlot() function does not work
      # due to "Cairo" graphics device being used instead of "postscipt"
      # maybe has to do with being called from a reactive function and/or plot being 
      # in a different environement?
      filename="plot.eps",
      
      content = function (file) {
        width = clientData$output_plotRCS_width
        height = clientData$output_plotRCS_height
        postscript(file, paper="special", width = width/72, height = height/72)
        
        # if (input$treatasBinary==TRUE) {data=dataFilteredwithThreshold()} else {data=dataFiltered()}
        
        plotRCS(data.all=dataExtended(),
                data.yn=dataFiltered.yn(),
                measurement=Measurement(),
                selectedSymptoms=input$selectedSymptoms,
                measurementSelectedrcs=input$selectedEvaluationTime,
                rcsIDVar=input$selectedCovariate,
                binaryVar=input$treatasBinary)
        
        dev.off()
      }, contentType="application/postscript"
    )
         
    downloadButton("downLoadRegressionOneTime4", label="Download")
  }
})


# TAB - Regression model : all evaluations ####
# Menu
output$selectCovariate1st <- renderUI({
  selectInput(inputId="selectedCovariate1st",
              label="Select covariate for analysis:",
              choices=dataVariableNames(),
              selected=input$groupingVar)
})

output$selectMixedModelType <- renderUI({
  selectInput(inputId="selectedMixedModelType",
              label="Select a mixed model type:",
              choices=c("Outcome ~ Covariate + Subject (random effect)"="MMsimple",
                        "Outcome ~ Covariate + Evaluation occasion + Subject (random effect)"="MMmeasurement",
                        "Outcome ~ Covariate + Time from inclusion + Subject (random effect)"="MMtimeSinceInclusion"),
              selected="MMsimple")
})



# Results
mixedModelResults <- reactive({
  
  progress <- Progress$new(session, min=1, max=100)
  
  progress$set(message = 'Calculation in progress',
               detail = 'This will take a while...', 
               value=NULL)
  on.exit(progress$close())
  
  mixedModel(data=dataExtended(),
             selectedSymptoms=input$selectedSymptoms,
             coVariate1st=input$selectedCovariate1st,
             subjectIDVar=input$patientIDVar,
             measurementVar=input$measurementVar,
             dateVar=input$dateVar,
             thresholdValue=input$thresholdValue,
             treatasBinary=input$treatasBinary,
             selectedModel=input$selectedMixedModelType)
})

# Table 0 ####
output$mixedModelTable0Caption <- renderText(
  if(!is.null(input$selectedMixedModelType)) {
    
    paste("Table: Intercepts for the models")
    
  })

output$mixedModelTable0 <- renderDataTable({
  if(!is.null(input$selectedMixedModelType)) {
    
    results <- mixedModelResults()[["printableIntercept"]] 

    return(results)
  }
}, options=list(bFilter=FALSE, bPaginate=FALSE, bInfo=FALSE))

# Table 1 ####
output$mixedModelTable1Caption <- renderText(
  if(!is.null(input$selectedMixedModelType)) {
    
  paste("Table: Fixed effects of",
        input$selectedCovariate1st,
        "for", 
        mixedModelResults()[["coVariate1stComparison"]])
  })

output$mixedModelTable1 <- renderDataTable({
  if(!is.null(input$selectedMixedModelType)) {
    
    results <- mixedModelResults()[["printablecoVariate1st"]] 

    return(results)
  }
  }, options=list(bFilter=FALSE, bPaginate=FALSE, bInfo=FALSE))

# Graph 1 ####
mixedModelGraph1Reactive <- reactive({
  
  #print(
    plotFixedEffectsofcoVariate1st(calculatedStatistics=mixedModelResults()[["coVariate1st"]],
                                       coVariate1st=input$selectedCovariate1st,
                                       coVariate1stReferenceValue=mixedModelResults()[["coVariate1stReferenceValue"]],
                                       treatasBinary=input$treatasBinary,
                                       variableOrder=input$selectedSymptoms
                                   )
  
})


output$mixedModelGraph1 <- renderPlot({
  if(!is.null(input$selectedMixedModelType)) {
#     print(plotFixedEffectsofcoVariate1st(calculatedStatistics=mixedModelResults()[["coVariate1st"]],
#                                          coVariate1st=input$selectedCovariate1st,
#                                          coVariate1stReferenceValue=mixedModelResults()[["coVariate1stReferenceValue"]],
#                                          treatasBinary=input$treatasBinary,
#                                          variableOrder=input$selectedSymptoms) 
#     )
    mixedModelGraph1Reactive()
    
  }
}, height=numRowsMixedModels1)

# Download Graph 1 ####
output$graph1Download <- renderUI({
  
  output$downLoadGraph1 <- downloadPlot(plotFunction = mixedModelGraph1Reactive,
                                                    width = clientData$output_mixedModelGraph1_width,
                                                    height = clientData$output_mixedModelGraph1_height,
                                                    print = TRUE)
  downloadButton("downLoadGraph1", label="Download")
  
})


# Table 2 ####
output$mixedModelTable2Caption <- renderText(
  if(!is.null(input$selectedMixedModelType)) {
    if (input$selectedMixedModelType=="MMmeasurement") {
      paste("Table: Fixed effects of",
            input$measurementVar,
            "for T=",
            mixedModelResults()[["measurementVarComparison"]],
            "used as reference")
    }}
  )

output$mixedModelTable2 <- renderDataTable({
  #renderUI({
  if(!is.null(input$selectedMixedModelType)) {
    if (input$selectedMixedModelType=="MMmeasurement") {
      results <- mixedModelResults()[["printablemeasurementVar"]] 
 
      return(results)
    }}
  }, options=list(bFilter=FALSE, bPaginate=FALSE, bInfo=FALSE))

# Graph 2 ####
mixedModelGraph2Reactive <- reactive({
  #print(
    plotFixedEffectsofMeasurementVar(calculatedStatistics=mixedModelResults()[["measurementVar"]],
                                         measurementVar=input$measurementVar,
                                         treatasBinary=input$treatasBinary,
                                         variableOrder=input$selectedSymptoms) 
  #)
  
})

output$mixedModelGraph2 <- renderPlot({
  if(!is.null(input$selectedMixedModelType)) {
    if (input$selectedMixedModelType=="MMmeasurement") {
#       print(plotFixedEffectsofMeasurementVar(calculatedStatistics=mixedModelResults()[["measurementVar"]],
#                                              measurementVar=input$measurementVar,
#                                              treatasBinary=input$treatasBinary,
#                                              variableOrder=input$selectedSymptoms) 
#       )
      
      mixedModelGraph2Reactive()
    }}
}, height=numRowsMixedModels2)

# Download Graph 2 ####
output$graph2Download <- renderUI({
  if(!is.null(input$selectedMixedModelType)) {
    if (input$selectedMixedModelType=="MMmeasurement") {
  output$downLoadGraph2 <- downloadPlot(plotFunction = mixedModelGraph2Reactive,
                                        width = clientData$output_mixedModelGraph2_width,
                                        height = clientData$output_mixedModelGraph2_height,
                                        print = TRUE)
  downloadButton("downLoadGraph2", label="Download")
    }}
  
})


# Table 3 ####
output$mixedModelTable3Caption <- renderText(
  if(!is.null(input$selectedMixedModelType)) {
    if (input$selectedMixedModelType=="MMtimeSinceInclusion") {
  paste("Table: Fixed effects of time since inclusion in the study")
    }}
  )

output$mixedModelTable3 <- renderDataTable({
  if(!is.null(input$selectedMixedModelType)) {
    if (input$selectedMixedModelType=="MMtimeSinceInclusion") {
      results <- mixedModelResults()[["printabledaysSinceInclusion"]] 
   
      return(results)
    }}
  }, options=list(bFilter=FALSE, bPaginate=FALSE, bInfo=FALSE))

# Graph 3 ####
mixedModelGraph3Reactive <- reactive({
  #print(
    plotFixedEffectsofDaysSinceInclusion(calculatedStatistics=mixedModelResults()[["daysSinceInclusion"]],
                                             treatasBinary=input$treatasBinary,
                                             variableOrder=input$selectedSymptoms) 
  #)
  
})

output$mixedModelGraph3 <- renderPlot({
  if(!is.null(input$selectedMixedModelType)) {
    if (input$selectedMixedModelType=="MMtimeSinceInclusion") {
#       print(plotFixedEffectsofDaysSinceInclusion(calculatedStatistics=mixedModelResults()[["daysSinceInclusion"]],
#                                                  treatasBinary=input$treatasBinary,
#                                                  variableOrder=input$selectedSymptoms) 
#       )
      
      mixedModelGraph3Reactive()
    }}
}, height=numRowsMixedModels3)

# Download Graph 3 ####
output$graph3Download <- renderUI({
  if(!is.null(input$selectedMixedModelType)) {
    if (input$selectedMixedModelType=="MMtimeSinceInclusion") {
  output$downLoadGraph3 <- downloadPlot(plotFunction = mixedModelGraph3Reactive,
                                        width = clientData$output_mixedModelGraph3_width,
                                        height = clientData$output_mixedModelGraph3_height,
                                        print = TRUE)
  downloadButton("downLoadGraph3", label="Download")
    }}
  
})

# description Regression All
output$regressionAllDescr <- reactive({
  if (!is.null(input$selectedMixedModelType)) {
      description <- generateDescription("RegressionAll")
      
      return(description())
    }})



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

# OBSOLETE TAB - Distributions of variables ####
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

# TAB - Selected variables ####

output$selectedVariables <- renderPrint({
  selectedInputs <- reactiveValuesToList(input)
  print(selectedInputs)
  })

output$debug <- reactive({
  browser()
  
})

})