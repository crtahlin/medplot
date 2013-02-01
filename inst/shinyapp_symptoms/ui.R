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
                       choices=reportedSymptoms)
  )
  ),
  
  # Define the output panel
  mainPanel(
    plotOutput("plot")
  )
)
)

