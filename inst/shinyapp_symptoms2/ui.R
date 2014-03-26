# This is the GUI part of the shiny script to plot
# graph for displaying presence of symptoms. 

library(shiny)

# Define UI for displaying presence of symptoms
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Patients and their symptoms"),
  
  # Define the selection panel
  sidebarPanel(
    wellPanel(
      # offer selection of type of data file
      selectInput(inputId="dataFileType",
                label="Select type of data file:",
                choices=c(
                  "Excel template"="Excel",
                  "Tab separated values (TSV) file"="TSV",
                  "Demo data"="Demo"
                  )),
      # offer user to upload file
      fileInput(inputId="dataFile",
                label=h5("Upload Excel data file:"),
                multiple=FALSE,
                accept=c("application/vnd.ms-excel",
                         "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
      
      conditionalPanel(
        condition="input.dataFileType =='TSV'",
        #uiOutput("sidebar"),
        # offer selection of patient gruping variable
        selectInput(inputId="groupingVar",
                    label="Grouping variable",
                    choices=c("Sex", "CaseorControl"),
                    selected="Sex"),
        # output checkbox selection generated on the server side
        uiOutput("levels"))
      )),
  
  # Define the output panel
  mainPanel(
    tabsetPanel(
      tabPanel(title="Timeline",
               h3(textOutput("message")),
               plotOutput("plot")),
      tabPanel(title="Proportions",
               plotOutput("plotPyramid")),
      tabPanel("Clustering",
               uiOutput("clusteringUI"),
               plotOutput("plotClusterDendrogram"),
               plotOutput("plotClusterHeatmap")),
      tabPanel("Selected transformed data", tableOutput("data")),
      tabPanel("Debug", tableOutput("debug"))
      ))
  )
  )