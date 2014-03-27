#' @title Function that imports data for the plotSymptoms plot
#' 
#' @description Function is called to import the data in appropriate format for the
#' plotSymptoms plot.
#' 
#' @param datafile Path to the file containing data.
#' @param format Format of the file containing data ("Excel", "TSV", "Demo").
importSymptomsData <- function (datafile, format) {
  
  if (format=="Excel") {
    # read the data into R
    data <- read.xls(datafile, sheet="DATA")
    # transform date information into R compliant dates
    data["Date"] <- as.Date(data[,"Date"], "%d.%m.%Y")
    return(data)
  }
  
  if (format=="TSV") {
    #read the txt data
      data <- read.csv(datafile, header=TRUE, sep="\t")
#       names.data=names(data)
#       
#       #column with the dates
#       which.DateID=which(names.data==input$DateIDVar)
#       
#       #column with the patient ID
#       which.PatientID=which(names.data==input$PatientIDVar)
#       
#       #column with the measurement ID
#       which.MeasurementID=which(names.data==input$MeasurementIDVar)
#       
#       #column with the SymptomsID
#       which.SymptomsID=which(names.data %in%  input$SymptomsIDVar)
#       
#       print(which.DateID)
#       
#       # transform date information into R compliant dates
#       #changed: the user can specify which is the date in the database
#       data[, which.DateID]=as.Date(as.character(data[,which.DateID]), "%d.%m.%Y") 
#             
#       #maintain only the information about the ID, Date and Symptoms
#       
#       data = data[, c(which.PatientID, which.DateID, which.MeasurementID, which.SymptomsID)]
#       
#       # transform data into ggplot compliant format
#       #data <- melt(data, id.vars = c("PersonID", "Date", "Measurement"))
          
      return(data)
      
  }
  if (format=="Demo") {# Same code as for Excel at the moment
  }


}

#' @title Function that imports PATIENTS Excel tab for the plotSymptoms plot
#' 
#' @param datafile Path to the file containing data.
importSymptomsPatients <- function (datafile) {
  data <- read.xls(datafile, sheet="PATIENTS")
  return(data)
}