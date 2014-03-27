# This is the GUI part of the shiny script to plot
# graph for displaying presence of symptoms. 

library(shiny)

# Define UI for displaying presence of symptoms
shinyUI(pageWithSidebar(
  
  # Application title ####
  headerPanel("Patients and their symptoms"),
  
  # Define the sidebar panel ####
  sidebarPanel(
    wellPanel(
      # selection of type of data file
      selectInput(inputId="dataFileType",
                  label="Select type of data file:",
                  choices=c(
                    " "=NA,
                    "Excel template"="Excel",
                    "Tab separated values (TSV) file"="TSV",
                    "Demo data"="Demo"
                  )),
      # offer user to upload file conditional on previous choice
      conditionalPanel(
        condition="input.dataFileType =='Excel' || input.dataFileType =='TSV'",
        fileInput(inputId="dataFile",
                  label={h5("Upload data file:")},
                  multiple=FALSE,
                  accept=c("application/vnd.ms-excel",
                           "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                           "text/plain"))
      ),
#       selectInput(inputId="groupingVar",
#                   label="Grouping variable:",
#                   choices=c(names),
#                   selected="Sex"),
#       selectInput(inputId="dateVar",
#                   label="Date variable:",
#                   choices=c("Sex", "CaseorControl"),
#                   selected="Sex"),
uiOutput("selectDateVar"),
uiOutput("selectGroupingVar"),
uiOutput("selectSymptoms")
     
      
    )),
  
  # Define the main panel ####
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
      tabPanel("Debug", textOutput("symptomsData"))
    ))
)
)