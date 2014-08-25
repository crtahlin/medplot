#' @title Plots of distribution of the symptoms
#' 
#' @param data Data frame for ploting.
plotDistribution <- function (data, selectedSymptoms, selectedProportion, measurements ) {
  
  #adjust the margins for the labels of the boxplot
  linch <-  max(strwidth(selectedSymptoms, "inch")+0.4, na.rm = TRUE)
  
  par(mai=c(1.02,linch,0.82,0.42))
  
  #par(mfrow=c(1,2))
  #calculate the proportion with symptoms and reorder (from the most common to the least common)

  prop.with.symptoms=apply(data[measurements==selectedProportion, ,drop=FALSE], 2, function(x) mean(x==TRUE, na.rm=TRUE))
  my.order.symptoms=order(prop.with.symptoms, decreasing=FALSE)
  tmp=barplot(prop.with.symptoms[my.order.symptoms],
              hor=TRUE, names.arg=selectedSymptoms[my.order.symptoms],
              las=1, xlim=c(0,1),
              xlab="Proportion of subjects")
  abline(v=seq(0, 1, by=.1), col="light gray", lty=3)
  title(paste0("Presence of outcome variables\n",
               "at evaluation occasion T = ",
               selectedProportion
               ))
  # TODO Remove
  # TODO Remove
  dev.list()
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
                                     posOnly=FALSE,
                                     threshold  ) {
  
  #adjust the margins for the labels of the boxplot
  linch <-  max(strwidth(selectedSymptoms, "inch")+0.4, na.rm = TRUE)
  par(mai=c(1.02,linch,0.82,0.42))
  
  #calculate the proportion with symptoms and reorder (from the most common to the least common)
  prop.with.symptoms=apply(data.yn[measurements==selectedProportion,], 2, function(x) mean(x==TRUE, na.rm=TRUE))
  my.order.symptoms=order(prop.with.symptoms, decreasing=FALSE)
  
  #display all the data 
  if(!posOnly) {
    boxplot(t(apply(data[measurements==selectedProportion, selectedSymptoms[my.order.symptoms]], 1, function(x) x)), 
            horizontal=TRUE, names=selectedSymptoms[my.order.symptoms], las=1, xlab="Value")
    
    title(paste0("T = ", selectedProportion, "; distribution of symptoms"))
  } else { #display the distribution only for positive patients
    #remove the non-positive observations
    tmp=(apply(data[measurements==selectedProportion, selectedSymptoms[my.order.symptoms]], 1,  function(x) x))
    #print(dim(tmp))
    
    #remove the non-positive values
    boxplot(apply(tmp, 1, function(x) x[which(x>threshold)]),  
            horizontal=TRUE, names=selectedSymptoms[my.order.symptoms], las=1, xlab="Value")
    
    title(paste0("T = ", selectedProportion, "; distribution of symptoms"))
  }
}

#' @title Ploting confidence intervals
#' 
#' @param ddd dddd
plotCI <- function (data.yn,
                    measurements,
                    selectedSymptoms,
                    selectedProportion) {
  
  for.CI=t(apply(data.yn[measurements==selectedProportion,], 2, function(x) {a=prop.test(sum(x==1, na.rm=T), length(x))
                              return( c(sum(x==1, na.rm=T), length(x), a$estimate, a$conf.int ))
                 }
  )
  )
  
  prop.with.symptoms=apply(data.yn[measurements==selectedProportion,], 2,
                           function(x) mean(x==TRUE, na.rm=TRUE))
  my.order.symptoms=order(prop.with.symptoms, decreasing=FALSE)
  
  #reorder the symptoms
  for.CI=for.CI[my.order.symptoms, ]
  
  num.symptoms=length(selectedSymptoms)
  
  linch <-  max(strwidth(selectedSymptoms, "inch")+0.4, na.rm = TRUE)
  par(mai=c(1.02,linch,0.82,0.42))
  
  for(i in 1:num.symptoms){
    plot( for.CI[,3], 1:num.symptoms, cex=2, axes=FALSE, xlab="Proportion of patients", ylab="", xlim=c(0,1))
    segments( for.CI[,4], 1:num.symptoms, for.CI[,5], 1:num.symptoms)#, lty=2 )
    axis(2, at=1:num.symptoms, labels=selectedSymptoms[my.order.symptoms], las=1)
    axis(1)
    abline(v=seq(0, 1, by=.2), lty=2, col="light gray")
    box()
  }
  
  title(paste0("T = ", selectedProportion, ";\n  95% confidence intervals for the presence of symptom"))
}

#' @title Table for both groups 
#' 
#' @description TODO Thresholds take into account for proportions, not for medians.
#' 
#' @param TODO
tableAllWithSymptoms <- function (data,
                                  measurementVar,
                                  forMeasurement,
                                  symptomsNames,
                                  thresholdValue=0) {
  
  # TODO: create a Groupinga() reactive vector to pass to this and other functions?
  #groupingLevels <- as.character(unlist(unique(data[groupingVar])))
  
  data <- data[data[measurementVar]==forMeasurement,]
  
  tableData <- data.frame("Variable"=symptomsNames)
  
  column1Name <- paste("Prop. with positive ")
  column2Name <- paste("Positive/all")
  column3Name <- paste("Median")
  column4Name <- paste("IQR")
  column5Name <- paste("Median w threshold")
  column6Name <- paste("IQR w threshold")
  
  aboveThresholdData <- data
  aboveThresholdData[, symptomsNames] <- (data[,symptomsNames]>thresholdValue)
  #group2Data <- data[data[groupingVar]==groupingLevels[2],]
  #group2Data[, symptomsNames] <- (group2Data[,symptomsNames]>thresholdValue)
  
  for (symptom in symptomsNames) {
    patientsPositive <- sum(aboveThresholdData[,symptom], na.rm=TRUE)
    patientsNegative <- sum(!aboveThresholdData[,symptom], na.rm=TRUE)
    #     group2Positive <- sum(group2Data[,symptom])
    #     group2Negative <- sum(!group2Data[,symptom])
    #     testMatrix <- matrix(c(group1Positive, group1Negative,
    #                            group2Positive, group2Negative),
    #                          byrow=TRUE, ncol=2)
    #     results <- prop.test(testMatrix)
    
    tableData[tableData["Variable"]==symptom, column1Name ] <- 
      patientsPositive / (patientsPositive + patientsNegative)
    tableData[tableData["Variable"]==symptom, column2Name ] <- 
      paste(patientsPositive, "/", patientsPositive+patientsNegative)
    tableData[tableData["Variable"]==symptom, column3Name ] <- median(data[,symptom], na.rm=TRUE)
    
    tableData[tableData["Variable"]==symptom, column4Name ] <- 
      paste(quantile(data[,symptom], c(0.25, 0.75), na.rm=TRUE)[1], " to ",
            quantile(data[,symptom], c(0.25, 0.75), na.rm=TRUE)[2])
    
    tableData[tableData["Variable"]==symptom, column5Name ] <- 
      median(data[data[symptom]>thresholdValue, symptom], na.rm=TRUE)
    
    tableData[tableData["Variable"]==symptom, column6Name ] <-
      paste(quantile(data[data[symptom]>thresholdValue, symptom], c(0.25, 0.75), na.rm=TRUE)[1], " to ",
            quantile(data[data[symptom]>thresholdValue, symptom], c(0.25, 0.75), na.rm=TRUE)[2])
    
    
    #     tableData[tableData["Variable"]==symptom, column5Name ] <- 
    #       format(results$p.value, digits=2)
    #     tableData[tableData["Variable"]==symptom, column6Name ] <- 
    #       paste(format(results$conf.int[[1]], digits=2),
    #             format(results$conf.int[[2]], digits=2), sep=";")
  }
  
  return(tableData)
}