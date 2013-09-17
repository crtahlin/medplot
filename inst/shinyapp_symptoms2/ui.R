# This is the GUI part of the shiny script to plot
# graph for displaying presence of symptoms. 

library(shiny)

# Prepare the list of possible symptoms
reportedSymptoms <- unlist(levels(symptomsData[,"variable"]))

# Define UI for displaying presence of symptoms
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Patients and their symptoms"),
  
  # Define the selection panel
  sidebarPanel(wellPanel(
    checkboxGroupInput(inputId="selectedSymptoms",
                       label=h5("Symptoms:"),
                       choices=reportedSymptoms),
    fileInput(inputId="dataFile",
              label=h5("Upload Excel data file:"),
              multiple=FALSE,
              accept="application/vnd.ms-excel")
  )
  ),
  
  # Define the output panel
  mainPanel(
    tabsetPanel(
      tabPanel(title="Graph",
               plotOutput("plot"),
               h3(textOutput("message"))),
      tabPanel("Selected transformed data", tableOutput("data")),
      tabPanel("Parameters"),
      tabPanel("Debug")
    ) ) 
)
)

