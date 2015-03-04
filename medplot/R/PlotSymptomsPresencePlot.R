#' @title Plot the presence of symptoms
#' 
#' @param data The data.
#' @param Which outcome variables were selected.
#' @param measurementVar Which variable holds evaluation occasions.
#' @param measurement Which evaluation occasio was chosen.
#' @param thresholdValue What is the threshold for dichotomizing.
#' 
#' @export
plotPresenceofSymptoms <- function (data, # dataFiltered()
                                    selectedSymptoms, # input$selectedSymptoms
                                    measurementVar, # input$measurementVar
                                    measurement, # input$selectedMeasurementForPresencePlot
                                    thresholdValue=0 # input$thresholdValue
) {

  # filter - use only data for selected measurement occasion
  data <- data[data[measurementVar]==measurement,]
  # binarize the symptoms data
  data[,selectedSymptoms] <- ifelse(data[,selectedSymptoms]>thresholdValue, 1, 0)
  
  calculatedData <- t(apply(data[,selectedSymptoms],
                            2, # by column
                            function(x) {
                              result=prop.test(sum(x==1, na.rm=TRUE), length(x))
                              # browser()
                              return(c( "PresentIn"=sum(x==1, na.rm=T),
                                        "Proportion"=result$estimate[[1]],
                                        "CILower"=result$conf.int[1],
                                        "CIUpper"=result$conf.int[2]))
                              
                            }))
  
  calculatedData <- as.data.frame(calculatedData)
  calculatedData[,"Variable"] <- row.names(calculatedData)
  
  plot <- ggplot() +
    geom_errorbarh(data=calculatedData, 
                   mapping=aes(y=Variable, x=CIUpper, xmin=CILower, xmax=CIUpper),
                   height=0.2, size=1) +
    geom_point(data=calculatedData, 
               mapping=aes(y=Variable, x=Proportion), size=4, shape=21, fill="white")  +
    # facet_grid(Variable~.) + 
    scale_x_continuous(limits=c(0,1),
                       breaks=seq(0,1,by=0.2),
                       labels=abs(seq(0,1,by=0.2)),
                       minor_breaks=((seq(0,1, by=0.1)))) +
    # geom_vline(xintercept=0)+
    myTheme() + labs(title="Proportions of positive variables \n(with 95% confidence intervals)",
                     x= "Proportions") +
    scale_y_discrete(limits=rev(selectedSymptoms))
  
  return(plot)
  }