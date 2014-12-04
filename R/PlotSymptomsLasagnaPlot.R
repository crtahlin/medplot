plotLasagna <- function(data, # dataFiltered()
                        treatasBinary, # input$treatasBinary
                        symptom, # input$selectedSymptoms
                        dateVar, # input$dateVar
                        personIDVar, # input$patientIDVar
                        measurementVar, # input$measurementVar
                        groupingVar, # input$groupingVar
                        thresholdValue # input$thresholdValue
) {
  # list all subject IDs
  subjects <- as.character(unique(data[, personIDVar]))
  # list all levels of evaluation occasions
  levels <- as.character(unique(data[, measurementVar]))
  # construct empty matrix of results for all possible combinations of subjects X levels
  results <- matrix(nrow=length(subjects), ncol=length(levels), dimnames=list(subjects, levels))
  
  # run a loop filling the data into the matrix
  for (level in levels) {
    # subset data for a particular evaluation accasion
    tempdata <- data[data[, measurementVar]==level, c(personIDVar,symptom)]
    # only look at subjects, that were measured at that evaluation occasion
    subjects <- as.character(unique(tempdata[, personIDVar]))
    for (subject in subjects) {
       results[subject, level] <- tempdata[tempdata[,personIDVar]==subject, symptom]
      }
  }

  # plot a different graph if data is binary (legend is different)
  if (treatasBinary==TRUE) { 
    pheatmap(results, cluster_cols=FALSE, main=symptom, show_rownames=FALSE, 
             legend_breaks=c(0,1),
             legend_labels=c(0,1),
             color=c('blue','red'),
             drop_levels=TRUE)
    } else {
    pheatmap(results, cluster_cols=FALSE, main=symptom, show_rownames=FALSE)
  }
}

########
pastePlotFilenames <- function(filenames) {
  out <- vector()
  for (file in filenames) {
    #data <- readPNG(file, native=TRUE)
    out <- paste(out, img(src=paste0("./temp/",basename(file))))
    #out <- paste(out, (img(src=data)))
  }
  return(out)
}