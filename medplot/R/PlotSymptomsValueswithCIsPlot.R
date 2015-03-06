#' @title A generic plot that plots values and their confidence intervals
#' 
#' @param data The data to plot.
#' @param variableName Label for the variables plotted.
#' @param valueName Variable that holds values.
#' @param CILowerName Variable that holds lower CI value.
#' @param CIUpperName Variable that holds upper CI value.
#' @param xLabel Horizontal label.
#' @param yLabel Vertical label.
#' @param graphTitle Graph title.
#' @param vLine Where to plot a vertical line.
#' @param variableOrder The order of the variables on the graph.
#' 
#' @export
plotValueswithCIs <- function (data,
                               variableName="Variable",
                               valueName="OR",
                               CILowerName="CILower",
                               CIUpperName="CIUpper",
                               xLabel,
                               yLabel,
                               graphTitle,
                               vLine=NULL,
                               variableOrder=NULL) {
  
  plot <- ggplot() +
    geom_errorbarh(data=data,
                   mapping=aes_string(y=variableName, x=CIUpperName, xmin=CIUpperName, xmax=CILowerName),
                   height=0.2, size=1) +
    geom_point(data=data, 
               mapping=aes_string(y=variableName, x=valueName),
               size=4, shape=21, fill="white") +
    myTheme() +
    labs(title=graphTitle, x= xLabel, y=yLabel) + 
    geom_vline(xintercept = vLine) +
    scale_y_discrete(limits=rev(variableOrder))
  
  return(plot)  
  
}