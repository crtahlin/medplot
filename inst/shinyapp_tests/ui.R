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
                accept=c("application/vnd.ms-excel",
                         "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"))  #,
      
#       # path to save the SVG file to
#       textInput(inputId="fileName",
#                 label="Filename of saved SVG file (optional)"
#         ),
#       # make a submit button to update changes
#       submitButton(text = "Apply Changes")
      
    ),
    
     mainPanel(
       
       tabsetPanel(
         # TODO: to display SVG inside HTML and use the "tooltips",
         # it must be properly embedded as described in:
         # http://www.w3schools.com/svg/svg_inhtml.asp
         # Figure out how to create custom shiny code for this.
         tabPanel("Graph", h3(textOutput("message")), imageOutput("dataPlot", height="100%")),
         tabPanel("Data", tableOutput("dataTable")),
         tabPanel("Parameters", tableOutput("parametersTable")),
         tabPanel(title="Errors", verbatimTextOutput("errors")),
         tabPanel(title="Debug",h3("Client Session Parameters"), verbatimTextOutput("debug"))
       )
    )
  )
)


