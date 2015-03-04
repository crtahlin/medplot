#' @title Plot dendrogram of patient groups based on symptoms.
#' 
#' @description Plots a dendrogram of patient groups based on symptom values they
#' have.
#' 
#' @param data Data fram to be passed to the function.
#' @param variableName The column name of the variable used for filtering.
#' @param variableValues The value of the filtering variable used to filter.
#' 
#' @export
plotDendrogram <- function (data,
                            variableName,
                            variableValue,
                            selectedSymptoms,
                            treatasBinary=FALSE) {
  
  dataSubset=data[data[,variableName]==variableValue, selectedSymptoms]
  #distance: 1-Spearman's correlation, agglomeration method='complete',
  if (treatasBinary==FALSE) { # for numerical outcomes
    plot(hclust(as.dist(1-cor(dataSubset, use="c", method="s"))), ann=FALSE)
  } else { # for binary outcomes
    plot(hclust(dist(t(dataSubset))), ann=FALSE)
  }
  
}


#' @title Plot clustered heatmaps.
#' 
#' @description Plots a heatmap for presence of symptoms.
#' 
#' @param data Data fram to be passed to the function.
#' @param variableName The column name of the variable used for filtering.
#' @param variableValues The value of the filtering variable used to filter.
#' @param selectedSymptoms The outcome variables selected.
#' @param treatasBinary Dichotomize?
#' @param annotationVars Which variables to use for annotation?
#' 
#' @export
plotClusterHeatmap <- function (data,
                                variableName,
                                variableValue,
                                selectedSymptoms,
                                annotationVars=NA,
                                treatasBinary=FALSE) {
  dataSubset=t(data[data[,variableName]==variableValue, c(selectedSymptoms)])
  #TODO: remove reference above to the first three columns "-c(1:3)"
  #browser()
  annotation <- data.frame(data[, annotationVars])
  if (treatasBinary==FALSE) {
    pheatmap(dataSubset, annotation=annotation,
             show_colnames=FALSE
             
             #main=FALSE) #"Outcomes and subjects heatmap"
    )
  } else {
    pheatmap(dataSubset,
             legend_breaks=c(0,1),
             legend_labels=c(0,1),
             color=c('blue','red'),
             drop_levels=TRUE,
             annotation=annotation,
             show_colnames=FALSE
             #main=FALSE #"Outcomes and subjects heatmap"
    )
  }
}

#' @title Plot heatmap of correlations.
#' 
#' @description Plots a heatmap for correlation of symptoms.
#' 
#' @param data Data fram to be passed to the function.
#' @param variableName The column name of the variable used for filtering.
#' @param variableValues The value of the filtering variable used to filter.
#' @param selectedSymptoms The outcome variables selected.
#' @param treatasBinary Dichotomize?
#' 
#' @export
plotCorrelations <- function (data,
                              variableName,
                              variableValue,
                              selectedSymptoms,
                              treatasBinary=FALSE) {
  
  dataSubset=data[data[,variableName]==variableValue, selectedSymptoms]  
  pheatmap(cor(dataSubset,method="s", use="c" ), display_numbers=TRUE#,
           #main="Outcome correlations heatmap"
  )
}