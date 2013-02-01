# drawing presence of symptoms

### melting data ####
# load library for melting data
library(reshape2)

# transform date information into real dates in R
mySymptomsData["Date"] <- as.Date(mySymptomsData[,"Date"], "%d.%m.%Y")

# transform data into ggplot compliant format
symptomsData <- melt(mySymptomsData, id.vars = c("Patient", "Date"))

# save data to faile to share with the R session that will run shiny
save.image(file="data.Rdata")

# generate Run.R file to start the shiny server

workingDir <- getwd()

script <- paste(
  "
  ### This script is automatically generated and is not meant to be edited
  # set working directory to this directory   
  # load library used for ploting
  library(ggplot2)
  # set working directory
  setwd(\"", workingDir   ,"\")
  # load data saved from Excel
  load(\"data.Rdata\")
  # load library for HTML drawing
  library(shiny)
  runApp(launch.browser=TRUE)
  
  "
  
  
  , sep="")

writeLines(script, con="Run.R")
