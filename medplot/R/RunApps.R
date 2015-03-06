#' @title Function to load the medplot plotSymptoms shiny web app
#' 
#' @description Function call loads the shiny app in located the default installation folder.
#' 
#' @import Cairo XML boot ggplot2 logistf lubridate lme4 permute
#' @import plyr reshape2 rms scales shiny xtable 
#' 
#' @export
medplotOnline <- function(){
  runApp(file.path(path.package("medplot"), "shinyapp_symptoms2" ))
}