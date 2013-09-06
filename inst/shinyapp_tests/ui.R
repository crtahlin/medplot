# Shiny UI for plotting test results

# load library for generation interactive web pages
library(shiny)

# make UI a page with sidebar for options
shinyUI(
  pageWithSidebar(
    
    # create header for the window
    headerPanel(title="Positive test results"),
    
    # create the sidebar
    sidebarPanel(    
      
      # create selection - should tooltips be generated?
      radioButtons(inputId="generateTooltips",
                   label="Generate tooltips?",
                   choices=c("Yes"=TRUE,"No"=FALSE)),
      
      # create selection - which sorting method should be used?
      selectInput(inputId="sortingMethod",
                  label="What kind of sorting should be used?",
                  choices=c("none","DateIn","BEA","BEA_TSP","PCA"),
                  multiple=FALSE),
      
      # create option for uploading file with data
      fileInput(inputId="dataFile",
                label="Upload Excel data file",
                multiple=FALSE,
                accept="application/vnd.ms-excel")
    ),
    
     mainPanel(
       
       tabsetPanel(
         
         tabPanel("Graph", imageOutput("dataPlot", height="100%")),
         tabPanel("Data", tableOutput("dataTable")),
         tabPanel("Parameters", tableOutput("parametersTable"))
       )
    )
  )
)


