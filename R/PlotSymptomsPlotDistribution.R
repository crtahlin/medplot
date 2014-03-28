#' @title Plots of distribution of the symptoms
#' 
#' @param data Data frame for ploting.
plotDistribution <- function (data, selectedSymptoms, selectedProportion, measurements ) {
   
  print("plotting the distribution of the symptoms")
  #print(input$measurementSelectedProportion)
  #print(dim(symptomsData.yn()))
  #adjust the margins for the labels of the boxplot
  linch <-  max(strwidth(selectedSymptoms, "inch")+0.4, na.rm = TRUE)
  par(mai=c(1.02,linch,0.82,0.42))
  #par(mfrow=c(1,2))
  #calculate the proportion with symptoms and reorder (from the most common to the least common)
  prop.with.symptoms=apply(data[measurements==selectedProportion,], 2, function(x) mean(x==TRUE, na.rm=TRUE))
  my.order.symptoms=order(prop.with.symptoms, decreasing=FALSE)
  tmp=barplot(prop.with.symptoms[my.order.symptoms],
              hor=TRUE, names.arg=selectedSymptoms[my.order.symptoms],
              las=1, xlim=c(0,1),
              xlab="Proportion of patients")
  abline(v=seq(0, 1, by=.1), col="light gray", lty=3)
  title(paste0("T = ", selectedProportion, "; presence of symptoms"))
  
}