# This file contains functions that determine the sizes (heights) of the medplot package shiny app

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