#' @title Code to generate a downloadable file containing plot (downloaHandler) to download using a button.
#' 
#' @param filename Suggested file name.
#' @param plotFunction The function that plots the graph (can be reactive).
#' @param width Width of the plot in pixels.
#' @param height Height of plot in pixels.
#' @param print Whether to use print() function or not. Needed with ggplot objects and not with base plots?
#' 
#' @export
downloadPlot <- function(filename="plot.eps",
                         plotFunction,
                         width,
                         height,
                         print=FALSE) {  
  downloadHandler(
    filename = filename,
    content = function(file) {
      postscript(file, paper="special", width = width/72, height = height/72)
      
      if (print==TRUE) { print(match.fun(plotFunction)()) }
      if (print==FALSE) { match.fun(plotFunction)() }
      
      dev.off()
    }, contentType="application/postscript"
  )
}