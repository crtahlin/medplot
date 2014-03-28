#' @title Plots of distribution of the symptoms
#' 
#' @param data Data frame for ploting.
plotDistribution <- function (data, selectedSymptoms, selectedProportion, measurements ) {
   
  print("plotting the distribution of the symptoms")
  #print(input$measurementSelectedProportion)
  print(dim(data))
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


#' @title Plots boxplots of symptoms. 
#'
#'@param data Data for the boxplots.
#'@param selectedSymptoms Column names of symptoms in data frame.
#'@param selectedProportion User selected measurement time.
#'@param measurements Vector of measurement values.
#'@param posOnly TRUE/FALSE
#'@param threshold User selected threshold value of symptoms. 
plotDistributionBoxplot <- function (data,
                                     data.yn,
                                     selectedSymptoms,
                                     selectedProportion,
                                     measurements,
                                     posOnly,
                                     threshold  ) {

  #adjust the margins for the labels of the boxplot
  linch <-  max(strwidth(selectedSymptoms, "inch")+0.4, na.rm = TRUE)
  par(mai=c(1.02,linch,0.82,0.42))

  #calculate the proportion with symptoms and reorder (from the most common to the least common)
  prop.with.symptoms=apply(data.yn[measurements==selectedProportion,], 2, function(x) mean(x==TRUE, na.rm=TRUE))
  my.order.symptoms=order(prop.with.symptoms, decreasing=FALSE)
  
  #display all the data 
  if(!posOnly) {
    print("Drawing boxplot")
    boxplot(t(apply(data[measurements==selectedProportion, selectedSymptoms], 1, function(x) x)), 
            horizontal=TRUE, names=selectedSymptoms[my.order.symptoms], las=1, xlab="Value")
    
    title(paste0("T = ", selectedProportion, "; distribution of symptoms"))
  } else { #display the distribution only for positive patients
    #remove the non-positive observations
    tmp=(apply(data[measurements==selectedProportion, selectedSymptoms], 1,  function(x) x))
    #print(dim(tmp))
    
    #remove the non-positive values
    boxplot(apply(tmp, 1, function(x) x[which(x>threshold)]),  
            horizontal=TRUE, names=selectedSymptoms[my.order.symptoms], las=1, xlab="Value")
    
    title(paste0("T = ", selectedProportion, "; distribution of symptoms"))
      }
}



