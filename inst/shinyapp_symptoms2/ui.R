# This is the GUI part of the shiny script to plot
# graph for displaying presence of symptoms. 

library(shiny)


# Define UI for displaying presence of symptoms
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Patients and their symptoms"),
  
  # Define the selection panel
  sidebarPanel(wellPanel(
    # offer user to upload file
    fileInput(inputId="dataFile",
              label=h5("Upload Excel data file:"),
              multiple=FALSE,
              accept=c("application/vnd.ms-excel",
                       "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
    # output checkbox selection generated on the server side
    uiOutput("levels")
    
  )
  ),
  
  # Define the output panel
  mainPanel(
    tabsetPanel(
      tabPanel(title="Graph",
               h3(textOutput("message")),
               plotOutput("plot")),
      tabPanel(title="Pyramid",
               plotOutput("plotPyramid")),
      tabPanel("Selected transformed data", tableOutput("data")),
      tabPanel("Parameters"),
      tabPanel("Debug", tableOutput("debug"))
    ) ) 
)
)

