# This is the server part of the shiny script to plot
# graph for displaying presence of symptoms. 

# load library for generation interactive web pages
library(shiny)
# load library for generating graph scales
library(scales)

# Main function
shinyServer(function(input, output) {
  
  # subset the data with the selected symptoms
  data <- reactive( function() {
    symptomsData[symptomsData$variable %in% input$selectedSymptoms,]
  })
  
  # plot the graph, but only for selected symptoms
  output$plot <- renderPlot(function() {
    if (dim(data())[1]>0) {
    print(
      ggplot(data(), aes(x = Date, y = Patient, size = value, colour = variable)) +
        geom_point(shape = 1) + theme_bw() + scale_size(range= c(1,20)) +
        scale_x_date(breaks = date_breaks("1 week"),
                     labels = date_format("%d %b")) +
        scale_x_date(minor_breaks = date_breaks("1 day"))
    
    )}
  })
  
  output$message <- renderText(if (dim(data())[1]==0){paste("Please select one or more symptoms.")})
})


# runApp(launch.browser=TRUE)
