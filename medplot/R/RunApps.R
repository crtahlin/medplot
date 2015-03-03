#' @title Function to load the medplot plotSymptoms shiny web app
#' 
#' @description Function call loads the shiny app in located the default installation folder.
#' 
#' @import xtable
#' 
#' @export
medplotOnline <- function(){
  runApp(file.path(path.package("medplot"), "shinyapp_symptoms2" ))
}