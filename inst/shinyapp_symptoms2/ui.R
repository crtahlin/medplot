# This is the GUI part of the shiny script to plot
# graph for displaying presence of symptoms. 

library(shiny)

# Define UI for displaying presence of symptoms
shinyUI(pageWithSidebar(
  
  # Application title ####
  headerPanel("medplot"),
  
  # Define the sidebar panel ####
  sidebarPanel(
    textOutput("medplotVersion"),
    uiOutput("messageSelectVars"),
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
      # TAB - summary of data ####
      tabPanel(title="Data summary",
               verbatimTextOutput("dataSummary")), 
      
      # TAB - Timeline ####
      tabPanel(title="Timeline",
              # textOutput("messageSelectVars"),
               uiOutput("selectDisplayFormat"),
               plotOutput("plotTimeline", height="auto")              
              ),
   
      # TAB - Distr. of the vars: over time ####
      tabPanel(title="Distribution of the variables: over time - profile plots" ,
               uiOutput("selectGraphType"),
               uiOutput("selectRandomSampleSize"),
               uiOutput("selectMaxGroupSize"),
               plotOutput("plotTimelineProfiles", height="auto")               
      ),
      
      # TAB - Distr. of the vars: over time w boxplots ####
      tabPanel(title="Distribution of the variables: over time - boxplots",
               plotOutput("plotTimelineBoxplots", height="auto")
               ),
      
      # TAB - Distribution of the variables ####
      tabPanel(title="Distribution of the variables: by measurement occasion",
              # textOutput("messageSelectVars"),
               uiOutput("proportionUI"),
               plotOutput("plotProportion", height="auto"), 
               plotOutput("plotCI", height="auto"), 
               plotOutput("plotBoxplot", height="auto"),
               uiOutput("selectPosOnly"),
               
               tableOutput("tablePropMedian"),
               uiOutput("textTablePropMedian")
      ),
      
      # TAB - Distribution of the variables: by grouping variable ####
      tabPanel(title="Distribution of the variables: by grouping variable",
              # textOutput("messageSelectVars"),
               plotOutput("plotPyramid", height="auto"),
               uiOutput("UIpropTable"),
               
               tableOutput("tablePropGroups"),
               uiOutput("textTablePropGroups"),
               
               tableOutput("tableMedianGroups"),
               uiOutput("textTableMedianGroups")
               
               ),
      
      
      # TAB - Clustering ####
      tabPanel("Clustering: by measurement occasion",
              # textOutput("messageSelectVars"),
               uiOutput("clusteringUI"),
               plotOutput("plotClusterDendrogram", height="auto"),
               uiOutput("selectClusterAnnotations"),
               plotOutput("plotClusterHeatmap"), height="auto"),
      
      
      
      # TAB - RCS ####
      tabPanel(title="RCS: by measurement occasion",
               #textOutput("messageSelectVars"),
               uiOutput("rcsUI"),
               uiOutput("rcsUI2"),
               plotOutput("plotRCS", height="100%")
      ),
      
      # TAB - Logistf ####
      tabPanel(title="Logistf: by measurement occasion",
              # textOutput("messageSelectVars"),
               uiOutput("logistfUI2"),
               uiOutput("logistfUI"),
               plotOutput("plotLogistf", height="auto")
               ),
      
      # TAB - Selected data ####
      tabPanel(title="Selected data", 
              # textOutput("messageSelectVars"),
               tableOutput("data")),
      
      # TAB - Debug ####
      tabPanel("Debug", verbatimTextOutput("debug"))
    )
  )
)
)