library(shiny)

reportedSymptoms <- unlist(levels(symptomsData[,"variable"]))

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Patients and their symptoms"),
  
  sidebarPanel(wellPanel(
    checkboxGroupInput(inputId="selectedSymptoms",
                       label=h5("Symptoms:"),
                       choices=reportedSymptoms)
  )
    ),
  
  mainPanel(
    plotOutput("plot")
    
    )
))

