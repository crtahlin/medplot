# This is the GUI part of the shiny script to plot
# graph for displaying presence of symptoms. 

library(shiny)

# Define UI for displaying presence of symptoms
shinyUI(pageWithSidebar(
  
  # Application title ####
  headerPanel("medplot"),
  
  # Define the sidebar panel ####
  sidebarPanel(
    wellPanel(
      conditionalPanel(
        condition="input.dataFileType =='Demo'",
        h2("Working with DEMO data!")),
      
      # selection of type of data file
      selectInput(inputId="dataFileType",
                  label="Select type of data file:",
                  choices=c(
                    " "=NA,
                    "Excel template"="Excel",
                    "Tab separated values (TSV) file"="TSV",
                    "Demo data"="Demo"
                  )),
      
      conditionalPanel(
        # condition="input.dataFileType =='Excel' || input.dataFileType =='TSV'",
        # Q : make displaying conditional? Looks OK as it is?
        condition="TRUE",
        fileInput(inputId="dataFile",
                  label={h5("Upload data file:")},
                  multiple=FALSE,
                  accept=c("application/vnd.ms-excel",
                           "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                           "text/plain"))
      ),
      
      uiOutput("selectPatientIDVar"),
      
      uiOutput("selectDateVar"),
      
      uiOutput("selectMeasurementVar"),
      
      uiOutput("selectGroupingVar"),
      
      uiOutput("selectSymptoms"),
      
      uiOutput("selectThresholdValue")
    )),
  
  # Define the main panel ####
  mainPanel(
    tabsetPanel(
      # TAB - Timeline ####
      tabPanel(title="Timeline",
               uiOutput("selectDisplayFormat"),
               plotOutput("plotTimeline", height="auto")              
              ),
      
      # TAB - Proportions ####
      tabPanel(title="Proportions",
               plotOutput("plotPyramid", height="auto"),
               uiOutput("UIpropTable"),
               tableOutput("tablePyramid"),
               tableOutput("tablePyramid2"),
               tableOutput("tablePyramid3")
               ),
      
      
      # TAB - Clustering ####
      tabPanel("Clustering",
               uiOutput("clusteringUI"),
               plotOutput("plotClusterDendrogram", height="auto"),
               uiOutput("selectClusterAnnotations"),
               plotOutput("plotClusterHeatmap"), height="auto"),
      
      # TAB - Distributions ####
      tabPanel(title="Distributions of the symptoms",
               uiOutput("proportionUI"),
               plotOutput("plotProportion", height="auto"), 
               plotOutput("plotCI", height="auto"), 
               plotOutput("plotBoxplot", height="auto"),
               uiOutput("selectPosOnly")
               
               ######### TODO: add a table with the number
      ),
      
      # TAB - RCS ####
      tabPanel(title="RCS",
               uiOutput("rcsUI"),
               uiOutput("rcsUI2"),
               plotOutput("plotRCS", height="100%")
      ),
      
      # TAB - Logistf ####
      tabPanel(title="Logistf",
               uiOutput("logistfUI2"),
               uiOutput("logistfUI"),
               plotOutput("plotLogistf", height="auto")
               ),
      
      # TAB - Selected data ####
      tabPanel("Selected data", tableOutput("data")),
      
      # TAB - Debug ####
      tabPanel("Debug", tableOutput("debug"))
    )
  )
)
)