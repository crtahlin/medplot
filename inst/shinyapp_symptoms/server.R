# load library for generation interactive web pages
library(shiny)
# load library for generating graph scales
library(scales)


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {

  # subset the data with the selected symptoms
  data <- reactive( function() {
    symptomsData[symptomsData$variable %in% input$selectedSymptoms,]
  }
    )
  
  # plot the graph, but only for selected symptoms
  output$plot <- reactivePlot( function() {
    print(
      ggplot(data(), aes(x = Date, y = Patient, size = value, colour = variable)) +
      geom_point(shape = 1) + theme_bw() + scale_size(range= c(1,20)) +
        scale_x_date(breaks = date_breaks("1 week"),
                          labels = date_format("%d %b")) +
        scale_x_date(minor_breaks = date_breaks("1 day"))
      
    )
  }
  )
  
})

#ms[ms$variable %in% c("Symptom1","Symptom2"),]
#x.sub2 <- subset(x.df, y > 2 & V2 > 0.4, select = c(V1, V4))
