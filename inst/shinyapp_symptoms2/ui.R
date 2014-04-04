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
      
      numericInput("thresholdValue", "Threshold for positivity of the variables", value=0)
    )),
  
  # Define the main panel ####
  mainPanel(
    tabsetPanel(
      tabPanel(title="Timeline",
               h3(textOutput("message")),
               plotOutput("plotTimeline"),
               checkboxInput(inputId="displaySinceInclusion",
                             label="Display the data as time from inclusion in the study?",
                             value= FALSE)),
      
      tabPanel(title="Proportions",
               plotOutput("plotPyramid"),
               uiOutput("UIpropTable"),
               tableOutput("tablePyramid"),
               tableOutput("tablePyramid2"),
               tableOutput("tablePyramid3")
               ),
      
      
      
      tabPanel("Clustering",
               uiOutput("clusteringUI"),
               plotOutput("plotClusterDendrogram"),
               uiOutput("selectClusterAnnotations"),
               plotOutput("plotClusterHeatmap")),
      
      tabPanel(title="Distributions of the symptoms",
               uiOutput("proportionUI"),
               plotOutput("plotProportion"), 
               plotOutput("plotCI"), 
               plotOutput("plotBoxplot"),
               checkboxInput("posOnly",
                             "Display the distribution only for patients with present symptom",
                             value = FALSE)
               ######### TODO: add a table with the number
      ),
      
      tabPanel(title="RCS",
               uiOutput("rcsUI"),
               uiOutput("rcsUI2"),
               plotOutput("plotRCS")
      ),
      
      tabPanel(title="Logistf",
               uiOutput("logistfUI2"),
               uiOutput("logistfUI"),
               plotOutput("plotLogistf")),
      
      tabPanel("Selected transformed data", tableOutput("data")),
      
      tabPanel("Debug", tableOutput("debug"))
    )
  )
)
)