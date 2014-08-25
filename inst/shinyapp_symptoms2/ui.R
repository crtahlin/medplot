# This is the GUI part of the shiny script to plot
# graph for displaying presence of symptoms. 

library(shiny)
library(shinyIncubator)

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
        h4("Working with DEMO data")),
      
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
        condition="input.dataFileType =='Excel' || input.dataFileType =='TSV'",
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
      
      uiOutput("selectTreatasBinary"),
      
      uiOutput("selectThresholdValue")
    )),
  
  # Define the main panel ####
  mainPanel(
    progressInit(),
    tabsetPanel(
      # TAB - welcome page with copyright info
      tabPanel(title="Welcome",
               includeHTML("www/welcome.html")),
      
      # TAB - summary of data ####
      tabPanel(title="Data overview",
               verbatimTextOutput("dataSummary")), 
      
      # TAB - Graphical exploration over time ####
      tabPanel(title="Graphical exploration",
               uiOutput("selectGraphOverTime"),
               
               # Profile plots
               uiOutput("selectGraphType"),
               uiOutput("selectRandomSampleSize"),
               uiOutput("selectMaxGroupSize"),
               plotOutput("plotTimelineProfiles", height="auto"),
               conditionalPanel(condition="input.selectedGraphOverTime=='profilePlot'",
                                downloadButton("downLoadplotTimelineProfiles", label="Download")),
               uiOutput("plotTimelineProfilesDescr"),
               
               # Lasagna plot
               uiOutput("plotLasagna"), #, height="700px"),
               uiOutput("plotLasagnaDesc"),
               
               # Boxplots
               uiOutput("selectFacetingType"),
               plotOutput("plotTimelineBoxplots", height="auto"),
               conditionalPanel(condition="input.selectedGraphOverTime=='boxPlot'",
                                downloadButton("downLoadplotTimelineBoxplot", label="Download")),
               uiOutput("plotTimelineBoxplotsDesc"),
               
               # Timeline graph
               uiOutput("selectDisplayFormat"),
               plotOutput("plotTimeline", height="auto"),
               conditionalPanel(condition="input.selectedGraphOverTime=='timelinePlot'",
                                downloadButton("downLoadplotTimeline", label="Download")),
               uiOutput("plotTimelineDesc"),
               
               # Presence of symptoms graph
               uiOutput("selectMeasurementForPresencePlot"),
               plotOutput("plotProportion", height="auto"),
               conditionalPanel(condition="input.selectedGraphOverTime=='presencePlot'",
                                downloadButton("downLoadplotProportion", label="Download")),
               uiOutput("plotProportionDesc")
               
      ),
     
      # TAB - Summary ####
      tabPanel(title="Summary",
               plotOutput("plotPyramid", height="auto"),
               conditionalPanel(condition="input.treatasBinary==true",
                                downloadButton("downLoadplotPyramid", label="Download")),
               uiOutput("selectEvaluationTime2"),
               dataTableOutput("tableforBoxplots"),
               dataTableOutput("tableforProportions"),
               plotOutput("plotPresence", height="auto"),
               conditionalPanel(condition="input.treatasBinary==true",
                                downloadButton("downLoadplotPresence", label="Download")),
               plotOutput("plotMedians", height="auto"),
               conditionalPanel(condition="input.treatasBinary==false",
                                downloadButton("downLoadplotMedians", label="Download")),
               uiOutput("mediansDescr"),
               uiOutput("proportionsDescr")
      ),
      
    
      # TAB - Summary tables : grouping variable ####
      tabPanel(title="Summary tables : grouping variable",
               #textOutput("messageNotAppropriate10"),
               plotOutput("plotPropCIs", height="auto"),
               
               uiOutput("UIpropTable"),
               uiOutput("UIdoPvalueAdjustments"),
               
               dataTableOutput("tablePropGroups"),
               uiOutput("textTablePropGroups"),
               
               dataTableOutput("tableMedianGroups"),
               uiOutput("textTableMedianGroups")
               ),
              
      # TAB - Clustering ####
      tabPanel(title="Clustering",
               #textOutput("messageNotAppropriate6"),
               # textOutput("messageSelectVars"),
               uiOutput("clusteringUI"),
               plotOutput("plotClusterDendrogram", height="auto"),
               uiOutput("dendrogramDescr"),
               plotOutput("plotClusterCorrelations", height="auto"),
               uiOutput("correlationDescr"),
               uiOutput("selectClusterAnnotations"),
               plotOutput("plotClusterHeatmap", height="auto"),
               uiOutput("heatmapDescr")
               
               ),
    
      # TAB - Regression model : one evaluation time ####
      tabPanel(title="Regression model : one evaluation time",
               # Menus
               #textOutput("debug10"),
               #textOutput("debug9"),
               uiOutput("selectEvaluationTime"),
               uiOutput("selectCovariate"),
               uiOutput("checkUseFirthCorrection"),
               uiOutput("checkUseRCSModel"),
               
               # Graphs
               # Logistic regression with Firth correction
               # plotOutput("plotLogistf", height="auto"),
               plotOutput("plotLogistf2", height="auto"),
               dataTableOutput("tableLogistf"),
               dataTableOutput("tableLogistfIntercept"),
               uiOutput("logistfDescr"),
               
               # Logistic regression 
               plotOutput("plotLogist", height="auto"),
               dataTableOutput("tableLogist"),
               dataTableOutput("tableLogistIntercept"),
               uiOutput("logistDescr"),
               
               # Linear regression
               plotOutput("plotLinear", height="auto"),
               dataTableOutput("tableLinear"),
               dataTableOutput("tableLinearIntercept"),
               uiOutput("linearDescr"),
               
               # RCS regression
               plotOutput("plotRCS", height="100%"),
               dataTableOutput("tableRCS"),
               uiOutput("RCSDescr")
        ),
      
      # TAB - Regression model : all evaluation times ####
      tabPanel(title="Regression model : all evaluation times",
               uiOutput("selectCovariate1st"),
               uiOutput("selectMixedModelType"),
               textOutput("mixedModelTable0Caption"),
               dataTableOutput("mixedModelTable0"),
               textOutput("mixedModelTable1Caption"),
               dataTableOutput("mixedModelTable1"),
               plotOutput("mixedModelGraph1", height="auto"),               
               textOutput("mixedModelTable2Caption"),
               dataTableOutput("mixedModelTable2"),
               plotOutput("mixedModelGraph2", height="auto"),
               textOutput("mixedModelTable3Caption"),               
               dataTableOutput("mixedModelTable3"),
               plotOutput("mixedModelGraph3", height="auto"),
               uiOutput("regressionAllDescr")
      ),
      
      # TAB - Selected data ####
      tabPanel(title="Uploaded data", 
               # textOutput("messageSelectVars"),
               dataTableOutput("data")) #,
      
      
#       # TAB - Timeline ####
#       tabPanel(title="Timeline",
#                textOutput("messageNotAppropriate"),  
#                # textOutput("messageSelectVars"),
#                  uiOutput("selectDisplayFormat"),
#                  plotOutput("plotTimeline", height="auto")
#                ),
   
#       # TAB - Distr. of the vars: over time ####
#       tabPanel(title="Distribution of the variables: over time - profile plots" ,
#                textOutput("messageNotAppropriate2"),
#                uiOutput("selectGraphType"),
#                uiOutput("selectRandomSampleSize"),
#                uiOutput("selectMaxGroupSize"),
#                plotOutput("plotTimelineProfiles", height="auto")               
#       ),
#       
#       # TAB - Distr. of the vars: over time w boxplots ####
#       tabPanel(title="Distribution of the variables: over time - boxplots",
#                textOutput("messageNotAppropriate3"),
#                #plotOutput("plotTimelineBoxplots", height="auto"),
#                uiOutput("tableforBoxplots")
#                ),
#       
#       # TAB - Distribution of the variables ####
#       tabPanel(title="OBSOLETE - Distribution of the variables: by measurement occasion",
#               textOutput("messageNotAppropriate4"),
#                # textOutput("messageSelectVars"),
#                uiOutput("proportionUI"),
#                #plotOutput("plotProportion", height="auto"), 
#                plotOutput("plotCI", height="auto"), 
#                plotOutput("plotBoxplot", height="auto"),
#                uiOutput("selectPosOnly"),
#                
#                tableOutput("tablePropMedian"),
#                uiOutput("textTablePropMedian")
#       )
#       
#       # TAB - Distribution of the variables: by grouping variable ####
#       tabPanel(title="Distribution of the variables: by grouping variable"#,
# #                textOutput("messageNotAppropriate5"),
#                # textOutput("messageSelectVars"),
# #                plotOutput("plotPyramid", height="auto"),              
# #                plotOutput("plotPropCIs", height="auto"),
#                           
# #                uiOutput("UIpropTable"),
# #               
# #                tableOutput("tablePropGroups"),
# #                uiOutput("textTablePropGroups"),
# #                
# #                tableOutput("tableMedianGroups"),
# #                uiOutput("textTableMedianGroups")
#                
#                ),
#       
      
     
      
      
      
#       # TAB - RCS ####
#       tabPanel(title="RCS: by measurement occasion",
#                #textOutput("messageNotAppropriate7"),
#                #textOutput("messageSelectVars"),
#                uiOutput("rcsUI"),
#                uiOutput("rcsUI2")#,
# #                plotOutput("plotRCS", height="100%"),
# #                tableOutput("tableRCS")
#       ),
#       
#       # TAB - Logistf ####
#       tabPanel(title="Logistf: by measurement occasion",
#                textOutput("messageNotAppropriate8"),
#                # textOutput("messageSelectVars"),
#                uiOutput("logistfUI2"),
#                uiOutput("logistfUI")#,
# #                plotOutput("plotLogistf", height="auto"),
# #                tableOutput("tableLogistf")
#                )#,
      
#       # TAB - Mixed model ####
#       tabPanel(title="Mixed model",
#                uiOutput("selectMixedModelType"),
#                tableOutput("mixedModelTable1"),
#                plotOutput("mixedModelGraph1", height="auto"),               
#                tableOutput("mixedModelTable2"),
#                plotOutput("mixedModelGraph2", height="auto"),
#                tableOutput("mixedModelTable3"),
#                plotOutput("mixedModelGraph3", height="auto")
#                )
#       
      
      # TAB - Debug ####
      # tabPanel("Debug", verbatimTextOutput("debug"))
    )
  )
)
)