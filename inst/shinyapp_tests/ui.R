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
                   choices=c("Yes","No")),
      
      # create selection - which sorting method should be used?
      selectInput(inputId="sortingMethod",
                  label="What kind of sorting should be used?",
                  choices=c("none","Datein","BEA","BEA_TSP","PCA"),
                  multiple=FALSE),
      
      # create option for uploading file with data
      fileInput(inputId="dataFile",
                label="Upload Excel data file",
                multiple=FALSE,
                accept="application/vnd.ms-excel")
    ),
    
     mainPanel(
       textOutput("test"),
       tabsetPanel(
         tabPanel("Data", tableOutput("dataTable")),
         tabPanel("Graph", plotOutput("dataPlot"))
       )
    )
  )
)


