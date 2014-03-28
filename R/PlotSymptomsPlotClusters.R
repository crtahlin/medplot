#' @title Plot dendrogram of patient groups based on symptoms.
#' 
#' @description Plots a dendrogram of patient groups based on symptom values they
#' have.
#' 
#' @param data Data fram to be passed to the function.
#' @param variableName The column name of the variable used for filtering.
#' @param variableValues The value of the filtering variable used to filter.
plotClusterDendrogram <- function (data, variableName, variableValue, selectedSymptoms ) {

  dataSubset=data[data[,variableName]==variableValue, selectedSymptoms]
  plot(hclust(as.dist(cor(dataSubset, use="c", method="s"))))

}


# output$plotClusterDendrogram=renderPlot({
#   print("plot the hierarchical clustering")
#   #my.data.for.cluster=symptomsData()[symptomsData()[,3]==input$measurementSelected,-c(1:3)]
#   my.data.for.cluster=symptomsData()[symptomsData()[,3]=="T0",-c(1:3)]
#   plot(hclust(as.dist(cor(my.data.for.cluster, use="c", method="s"))))
#   
#   



#' @title Plot clustered heatmaps.
#' 
#' @description Plots a heatmap for presence of symptoms.
#' 
#' @param data Data fram to be passed to the function.
#' @param variableName The column name of the variable used for filtering.
#' @param variableValues The value of the filtering variable used to filter.
plotClusterHeatmap <- function (data, variableName, variableValue, selectedSymptoms ) {
  dataSubset=t(data[data[,variableName]==variableValue, selectedSymptoms])
  #TODO: remove reference above to the first three columns "-c(1:3)"
  pheatmap(dataSubset)
}
