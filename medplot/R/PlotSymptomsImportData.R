#' @title Function that imports data for the plotSymptoms shiny app
#' 
#' @description Function is called to import the data in appropriate format for the
#' plotSymptoms shiny app.
#' 
#' @param datafile Path to the file containing data.
#' @param format Format of the file containing data ("Excel", "TSV", "Demo").
#' 
#' @export
importSymptomsData <- function (datafile, format) {
  
  if (format=="Excel") {
    # read the data into R
    data <- read.xls(datafile, sheet="DATA")
    # transform date information into R compliant dates
    # TODO: should we remove this line, since transform is done later, when the user select the 
    # date column?
    data["Date"] <- as.Date(data[,"Date"], "%d.%m.%Y")
    return(data)
  }
  
  if (format=="TSV") {
    # read the data into R
    data <- read.csv(datafile, header=TRUE, sep="\t", stringsAsFactors=FALSE)
    return(data)
  }
  if (format=="Demo") {# Same code as for Excel at the moment
  }
  
}

#' @title Function that imports PATIENTS Excel tab for the plotSymptoms plot
#' 
#' @param datafile Path to the file containing data.
#' 
#' @export
importSymptomsPatients <- function (datafile) {
  
  data <- read.xls(datafile, sheet="PATIENTS")
  return(data)
}