# This is the server part of the shiny script to plot
# graph for displaying presence of symptoms. 

# Load libraries ----------------------------------------------------------
# load library for generation interactive web pages
library(shiny)
# load library for generating graph scales
library(scales)
# load library for melting data
library(reshape2)
# library for plotting data
library(ggplot2)
# library for reading Excel files
library(gdata)
# library for manipulating with data (does not work with R<3.0.2)
# library(dplyr)
# library for manipulating data
library(plyr)
# library for clustering
library(pheatmap)
# load medplot library
library(medplot)

# Global variables ---------------------------------------------------------------------
# variables for melting data into ggplot compliant format for Timeline graph
meltBy <- c("PersonID", "Date", "Measurement")


# Main function -----------------------------------------------------------

shinyServer(function(input, output, session) {
  ### Import and prepare data ####
  # load data from Excel
  symptomsData <- reactive(function() { # returns the DATA sheet
    if (input$dataFileType=="Demo") {
      # load default data for Demo scenario
      # the location of template data file
      templateLocation <- paste0(path.package("medplot"),"/extdata/PlotSymptoms_shiny.xlsx")
      importSymptomsData(datafile=templateLocation,
                         format="Excel")
    } else { # load data for non-demo scenario
    if (!is.null(input$dataFile) && !(input$dataFileType=="Demo")) {
      importSymptomsData(datafile=input$dataFile$datapath,
                         format=input$dataFileType) }
    } 
  })
  
  symptomsPatients <- reactive(function() { # returns the PATIENTS sheet
    if (input$dataFileType=="Demo") {
      # load default data for Demo scenario
      # the location of template data file
      templateLocation <- paste0(path.package("medplot"),"/extdata/PlotSymptoms_shiny.xlsx")
      importSymptomsPatients(datafile=templateLocation)    } #else {}
    # TODO: load patients for "Excel" scenario
  })
  
  # subset the data with the selected symptoms - for 1st graph
  data <- reactive( function() {
    data <- melt(symptomsData(), id.vars = meltBy)
    data[data$variable %in% input$selectedSymptoms,]
  })
  
  # list the subseted data in an output slot
  output$data <- renderTable({
    data <- data()
    data$Date <- as.character(as.Date(data$Date, origin="1899-12-30"),format="%d.%m.%Y")
    return(data)
    # NOTE: if we want to render the table of data, we have to convert the dates into 
    # characters, since renderTable seems to use xtable, which seems to not handle
    # dates very well (http://stackoverflow.com/questions/8652674/r-xtable-and-dates)
  })

  
  # build extended data set for additional graphs
  dataExtended <- reactive( function() {
    observe(input$dataFile)
    if (input$dataFileType=="Excel") {
      patients <- importSymptomsPatients(datafile=input$dataFile$datapath)
      symptoms <- importSymptomsData(datafile=input$dataFile$datapath,
                                     format="Excel")
      data <- join(x=symptoms, y=patients, by="PersonID", type="inner")
    return(data)
    }
    
    if (input$dataFileType=="Demo") {
      templateLocation <- paste0(path.package("medplot"),"/extdata/PlotSymptoms_shiny.xlsx")
      patients <- importSymptomsPatients(datafile=templateLocation)
      symptoms <- importSymptomsData(datafile=templateLocation,
                                     format="Excel")
      data <- join(x=symptoms, y=patients, by="PersonID", type="inner")
    }
    
    if (input$dataFileType=="TSV") {
      data <- importSymptomsData(datafile=input$dataFile$datapath,
                                 format="TSV")
      return(data)
      
#             data <- symptomsData()
#             # TODO: make date conversion more universal, depending on user selected Date column
#             # - perhaps implement this in the plotting function?
#            data[, "Date"] <- as.Date(as.character(data[, "Date"]), format="%d.%m.%Y")
#            # data[, "Date2"] <- as.character(data[, "Date"])
#            # data[, "Date"] <- strptime(as.character(data[, "Date"]), format="%e.%m.$Y")
#            
#            names.data=names(data)
#                  
#            #       #column with the dates
#                   which.DateID=which(names.data==input$dateVar)
#            #       
#            #       #column with the patient ID
#                   which.PatientID=which(names.data==input$patientIDVar)
#            #       
#            #       #column with the measurement ID
#                   which.MeasurementID=which(names.data==input$measurementVar)
#            #       
#            #       #column with the SymptomsID
#                   which.SymptomsID=which(names.data %in%  input$selectedSymptoms)
#            #       
#            #       print(which.DateID)
#            #       
#            #       # transform date information into R compliant dates
#            #       #changed: the user can specify which is the date in the database
#            #       data[, which.DateID]=as.Date(as.character(data[,which.DateID]), "%d.%m.%Y") 
#            #             
#            #       #maintain only the information about the ID, Date and Symptoms
#            #       
#                   data = data[, c(which.PatientID, which.DateID, which.MeasurementID, which.SymptomsID)]
#            
           
    }
    return(data)
  })
  
  dataFiltered <- reactive(function(){
    data <- dataExtended()[ , 
                           c(input$patientIDVar,
                             input$groupingVar,
                             input$dateVar,
                             input$measurementVar,
                             input$selectedSymptoms
                             )]
    return(data)
    })
  
  ## Mainpanel dynamic output 
  # message for working with DEMO data
  output$message <- renderText(
    if (is.null(input$dataFile)) {paste("WORKING WITH DEMO DATA!")} else {
      if (dim(data())[1]==0){paste("Please select one or more symptoms.")}
    })
  
  # debuging information
  output$debug <- renderTable(dataFiltered())
  
  output$selectSymptoms <- renderUI({
    # Sidebar for Excel and demo files ####
    # TODO: Try to redo this part with switch() statement as
    # it seems only the last evaluated block is returned
    if (!is.null(symptomsData())) {
    data <- melt(symptomsData(), id.vars = meltBy)
    symptoms <- unlist(levels(data[,"variable"]))
    checkboxGroupInput(inputId="selectedSymptoms",
                       label="Choose symptoms:", 
                       #choices=symptoms
                       choices=dataVariableNames())}
      })
  
dataVariableNames <- reactive(function(){
  unlist(names(dataExtended()))
  })

output$selectDateVar <- renderUI({
  allSymptoms <- unlist(names(dataExtended()))
  selectInput(inputId="dateVar",
                     label="Choose date variable:", 
                     choices=dataVariableNames(),
              selected="Date")
})

output$selectGroupingVar <- renderUI({
  allSymptoms <- unlist(names(dataExtended()))
  selectInput(inputId="groupingVar",
              label="Choose grouping variable:", 
              choices=allSymptoms,
              selected="Sex")
})

output$selectPatientIDVar <- renderUI({
  allSymptoms <- unlist(names(dataExtended()))
  selectInput(inputId="patientIDVar",
              label="Choose patient ID variable:", 
              choices=allSymptoms,
              selected="PersonID")
})

output$selectMeasurementVar <- renderUI({
  allSymptoms <- unlist(names(dataExtended()))
  selectInput(inputId="measurementVar",
              label="Choose measurument occasion variable:", 
              choices=allSymptoms,
              selected="Measurement")
})

  ## Plots for different tabs ####
  
  # Timeline tab plots and output ####
# TODO: make data structure data() for TSV import
  # plot the graph, but only for selected symptoms
  output$plot <- renderPlot(function() {
    # if no symbols are selected, do not plot
    if (dim(data())[1]>0) {
      print(plotSymptomsTimeline(data()))}
  })
  
  # Proportions tab plots and output ####
  # pyramid plot of proportions
  output$plotPyramid <- renderPlot (
    plotPropWithSymptoms(data=dataExtended(),
                         grouping=input$groupingVar,
                         measurements="Measurement",
                         symptomsNames=unlist(levels(data()[,"variable"])))
  )
  
  # Clustering tab plots and output ####
  # selection of measurement occasions  
  output$clusteringUI = renderUI({
    #levels of the measurement variable, save as third variable in the dataset symptomsData
    # TODO: make selection of levels dependent on Sidebar, not fixed to "Measurement"
    myLevels=levels(as.factor(symptomsData()[,"Measurement"]))
    #select the measurement
    selectInput(inputId="selectedMeasurementValue",
                label="Select the measurement occasion (time):", 
                choices=myLevels, selected=myLevels[1])
  })
  
  # dendrogram plot on the Clustering tab
  output$plotClusterDendrogram=renderPlot({
    plotClusterDendrogram(data=dataExtended(),
                          variableName="Measurement",
                          variableValue=input$selectedMeasurementValue)
  })
  
  # heatmap plot on the Clustering tab
  output$plotClusterHeatmap=renderPlot({
    plotClusterHeatmap(data=dataExtended(),
                       #TODO: make dependent on selection
                       variableName="Measurement",
                       variableValue=input$selectedMeasurementValue) 
  })
  
output$symptomsData <- renderTable((dataExtended()))

### TODO: organize, make independent of column order
# TODO: rename into dataExtended.yn() ?; comment on data structure
#dataset with the positive/negative values for the selected symptoms
symptomsData.yn=reactive({
  #apply(symptomsData()[, -c(1:3)], 1, function(x) ifelse(x>input$threshold, 1, 0))
  data=ifelse(dataExtended()[, -c(1:3)]>input$threshold, 1, 0)
  return(data)
})

#saving Measurment in a reactive object - useful?
# TODO: reference via column name
Measurement=reactive({
  dataExtended()[,3]
  
})

output$proportionUI = renderUI({
  
  #levels of the measurement variable, save as third variable in the dataset symptomsData
  # TODO: reference to measurementVar instead of 3rd column
  my.levels=levels(as.factor(symptomsData()[,3]))
  
  #select the measurement
  selectInput(inputId="measurementSelectedProportion",
              label="Select the measurment (time)", 
              choices=my.levels, selected=my.levels[1])
  
  
})


output$plot.proportion=renderPlot({
  plotDistribution(data=symptomsData.yn(),
                   selectedSymptoms=input$selectedSymptoms,
                   selectedProportion=input$measurementSelectedProportion,
                   measurements=Measurement())
})


output$plot.boxplot=renderPlot({
  plotDistributionBoxplot(data=dataExtended(),
                          selectedSymptoms=input$selectedSymptoms,
                          selectedProportion=input$selectedProportion,
                          measurements=Measurement(),
                          posOnly=input$posOnly,
                          threshold=input$thresholdValue)
  
#   
#   print("plotting the distribution of the symptoms - boxplot")
#   #print(input$measurementSelectedProportion)
#   #print(dim(symptomsData.yn()))
#   #adjust the margins for the labels of the boxplot
#   linch <-  max(strwidth(input$SymptomsIDVar, "inch")+0.4, na.rm = TRUE)
#   par(mai=c(1.02,linch,0.82,0.42))
#   #par(mfrow=c(1,2))
#   #calculate the proportion with symptoms and reorder (from the most common to the least common)
#   prop.with.symptoms=apply(symptomsData.yn()[Measurement()==input$measurementSelectedProportion,], 2, function(x) mean(x==TRUE, na.rm=TRUE))
#   my.order.symptoms=order(prop.with.symptoms, decreasing=FALSE)
#   
#   #display all the data 
#   if(!input$posOnly) {
#     boxplot(t(apply(symptomsData()[Measurement()==input$measurementSelectedProportion,-c(1:3)], 1, function(x) x)), 
#             horizontal=TRUE, names=input$SymptomsIDVar[my.order.symptoms], las=1, xlab="Value")
#     
#     title(paste0("T = ", input$measurementSelectedProportion, "; distribution of symptoms"))
#   } else { #display the distribution only for positive patients
#     #remove the non-positive observations
#     
#     tmp=(apply(symptomsData()[Measurement()==input$measurementSelectedProportion,-c(1:3)], 1,  function(x) x))
#     #print(dim(tmp))
#     
#     #remove the non-positive values
#     boxplot(apply(tmp, 1, function(x) x[which(x>input$threshold)]),  
#             horizontal=TRUE, names=input$SymptomsIDVar[my.order.symptoms], las=1, xlab="Value")
#     
#     title(paste0("T = ", input$measurementSelectedProportion, "; distribution of symptoms"))
#     
#     
#   }
  
})


})









