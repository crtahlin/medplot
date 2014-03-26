#' @title Plot clusters of patients based on symptoms.
#' 
#' @description Visualize clusters of patients based on symptoms and other arguments.
#' 
#' @param ddd dddd
plotClustersofPatients <- function () {



}

#' @title Plot clustered heatmaps.
#' 
#' @description Plots a heatmap for presence of symptoms.
#' 
#' @param data Data fram to be passed to the function.
#' @param variableName The column name of the variable used for filtering.
#' @param variableValues The value of the filtering variable used to filter.
plotClusterHeatmap <- function (data, variableName, variableValue) {
  X=t(data[data[,variableName]==variableValue,-c(1:3)])
  pheatmap(X)
}
