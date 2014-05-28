mixedModel <- function(data,               # dataFiltered()
                       selectedSymptoms,    # input$selectedSymptoms
                       groupingVar,         # input$groupingVar
                       subjectIDVar,        # input$patientIDVar
                       measurementVar,      # input$measurementVar
                       dateVar,             # input$dateVar
                       thresholdValue,      # input$thresholdValue
                       treatasBinary,       # input$treatasBinary
                       selectedModel){      # input$selectedMixedModelType
  
  # make groupingVar bniary variable
  data[, groupingVar] <- as.factor(data[, groupingVar])
  
  # name of the binary grouping variable coeficient assigned by R
  groupingVarCoefName <- paste0(groupingVar,levels(data[,groupingVar])[2])
  # reference value is?
  referenceValue <- levels(data[,groupingVar])[2]
  # if response variable is binary, do a data transformation based on the thresholdvalue
  if (treatasBinary==TRUE) {data[, selectedSymptoms] <- 
                              ifelse(data[,selectedSymptoms]>thresholdValue, 1, 0)                            
  }
  
  
  if (selectedModel=="MMtimeSinceInclusion") {
    # if the model includes days since inclusion, add this info to the data (column "daysSinceInclusion")
    data <- calculateDaysSinceInclusion(data, subjectIDVar, dateVar)
    time <- "daysSinceInclusion"
  }
  
  # prepare data frame for the results, depending on what kind of response variable we have
  if (treatasBinary==TRUE) {
    resultsGroupingVar <- 
      data.frame(expand.grid(Variable=selectedSymptoms), OR=NA, ORCILower=NA, ORCIUpper=NA, ORPValue=NA)
    resultsMeasurementVar <- 
      data.frame(expand.grid(Variable=selectedSymptoms), OR=NA, ORCILower=NA, ORCIUpper=NA, ORPValue=NA)
    resultsDaysSinceInclusion <- 
      data.frame(expand.grid(Variable=selectedSymptoms), OR=NA, ORCILower=NA, ORCIUpper=NA, ORPValue=NA)
  }
  if (treatasBinary==FALSE) {
    resultsGroupingVar <- 
      data.frame(expand.grid(Variable=selectedSymptoms), beta=NA, betaCILower=NA, betaCIUpper=NA, betaPValue=NA)
    resultsMeasurementVar <- 
      data.frame(expand.grid(Variable=selectedSymptoms), beta=NA, betaCILower=NA, betaCIUpper=NA, betaPValue=NA)
    resultsDaysSinceInclusion <- 
      data.frame(expand.grid(Variable=selectedSymptoms), beta=NA, betaCILower=NA, betaCIUpper=NA, betaPValue=NA)
  }
  
  # cycle through response variables
  for (symptom in selectedSymptoms) {
    
    # choose the right model depending on user selected option
    # construct the right formula based on selected model
    if (selectedModel=="MMsimple") {
      formula <- as.formula(paste(symptom, "~", groupingVar, "+(1|", subjectIDVar, ")"))
    }
    if (selectedModel=="MMmeasurement") {
      formula <- as.formula(paste(symptom, "~", as.factor(measurementVar),"+", groupingVar, "+(1|", subjectIDVar, ")"))
    }
    if (selectedModel=="MMtimeSinceInclusion") {
      formula <- as.formula(paste(symptom, "~", time,"+", groupingVar, "+(1|", subjectIDVar, ")"))
    }
    
    
    # build the model depending on whether the response is binary or not
    if (treatasBinary==TRUE) {
      model <- glmer(formula, family=binomial,  na.action=na.omit, data=data)
      
      # results for the grouping variable
      resultsGroupingVar[resultsGroupingVar["Variable"]==symptom, "OR"] <- 
        exp(summary(model)$coef[groupingVarCoefName, "Estimate"])
      resultsGroupingVar[resultsGroupingVar["Variable"]==symptom, "ORCILower"] <- 
        exp(summary(model)$coef[groupingVarCoefName, "Estimate"] - 
              qnorm(.975)*summary(model)$coef[groupingVarCoefName, "Std. Error"])
      resultsGroupingVar[resultsGroupingVar["Variable"]==symptom, "ORCIUpper"] <- 
        exp(summary(model)$coef[groupingVarCoefName, "Estimate"] +
              qnorm(.975)*summary(model)$coef[groupingVarCoefName, "Std. Error"])
      resultsGroupingVar[resultsGroupingVar["Variable"]==symptom, "ORPValue"] <- 
        summary(model)$coef[groupingVarCoefName, "Pr(>|z|)"]
      
      if (selectedModel=="MMmeasurement") {
        # results for the measurement variable
        resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom, "OR"] <- 
          exp(summary(model)$coef[measurementVar, "Estimate"])
        resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom, "ORCILower"] <- 
          exp(summary(model)$coef[measurementVar, "Estimate"] - 
                qnorm(.975)*summary(model)$coef[groupingVarCoefName, "Std. Error"])
        resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom, "ORCIUpper"] <- 
          exp(summary(model)$coef[measurementVar, "Estimate"] +
                qnorm(.975)*summary(model)$coef[groupingVarCoefName, "Std. Error"])
        resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom, "ORPValue"] <- 
          summary(model)$coef[measurementVar, "Pr(>|z|)"]
      }
      
      if (selectedModel=="MMtimeSinceInclusion") {
        # results for the days since inclusion variable
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "OR"] <- 
          exp(summary(model)$coef[time, "Estimate"])
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "ORCILower"] <- 
          exp(summary(model)$coef[time, "Estimate"] - 
                qnorm(.975)*summary(model)$coef[groupingVarCoefName, "Std. Error"])
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "ORCIUpper"] <- 
          exp(summary(model)$coef[time, "Estimate"] +
                qnorm(.975)*summary(model)$coef[groupingVarCoefName, "Std. Error"])
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "ORPValue"] <- 
          summary(model)$coef[time, "Pr(>|z|)"]
      }      
    }
    
    
    if(treatasBinary==FALSE) {
      model <- lmerTest::lmer(formula, na.action=na.omit, data=data)
      
      # results for the grouping variable
      resultsGroupingVar[resultsGroupingVar["Variable"]==symptom, "beta"] <-
        summary(model)$coef[groupingVarCoefName, "Estimate"]
      resultsGroupingVar[resultsGroupingVar["Variable"]==symptom, "betaCILower"] <-
        confint(model)[groupingVarCoefName, "2.5 %"]
      resultsGroupingVar[resultsGroupingVar["Variable"]==symptom, "betaCIUpper"] <-
        confint(model)[groupingVarCoefName, "97.5 %"]
      resultsGroupingVar[resultsGroupingVar["Variable"]==symptom, "betaPValue"] <-
        lmerTest::summary(model)$coef[groupingVarCoefName, "Pr(>|t|)"]
      
      if (selectedModel=="MMmeasurement") {
        # results for the measurement variable
        resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom, "beta"] <-
          summary(model)$coef[measurementVar, "Estimate"]
        resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom, "betaCILower"] <-
          confint(model)[measurementVar, "2.5 %"]
        resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom, "betaCIUpper"] <-
          confint(model)[measurementVar, "97.5 %"]
        resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom, "betaPValue"] <-
          lmerTest::summary(model)$coef[measurementVar, "Pr(>|t|)"]
      }
      
      if (selectedModel=="MMtimeSinceInclusion") {
        # results for the days since inclusion variable
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "beta"] <-
          summary(model)$coef[time, "Estimate"]
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "betaCILower"] <-
          confint(model)[time, "2.5 %"]
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "betaCIUpper"] <-
          confint(model)[time, "97.5 %"]
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "betaPValue"] <-
          lmerTest::summary(model)$coef[time, "Pr(>|t|)"]
      }
    }
  }
  return(list(groupingVar=resultsGroupingVar,
              groupingVarReferenceValue=referenceValue,
              measurementVar=resultsMeasurementVar,
              daysSinceInclusion=resultsDaysSinceInclusion ))
}

calculateDaysSinceInclusion <- function (data,
                                         subjectIDVar,
                                         dateVar) {
  
  # find the day of inclusion in the study for each person
  uniquePeople <- as.data.frame(unique(data[subjectIDVar]))
  colnames(uniquePeople) <- subjectIDVar
  
  for (person in uniquePeople[,1]) {
    subset <- data[which(data[subjectIDVar]==person), dateVar]
    uniquePeople[which(uniquePeople[subjectIDVar]==person), "minDate"] <- as.character(min(subset))
    data[which(data[subjectIDVar]==person), "minDate"] <- as.character(min(subset))
  }
  data$minDate <- as.Date(data$minDate, format="%Y-%m-%d")
  data$daysSinceInclusion <- as.numeric(data[,dateVar] - data$minDate) # save as numeric for melt()to work
  
  return(data)
}

plotFixedEffectsofGroupingVar <- function (calculatedStatistics,
                                           groupingVar,
                                           groupingVarReferenceValue,
                                           treatasBinary) {
  graphTitle <- paste("Fixed effects of", groupingVar)
  
  # for binary response variable
  if (treatasBinary==TRUE) {
    xlabLabel <- "Odds ratios"
    plot <- ggplot() +
      geom_errorbarh(data=calculatedStatistics, 
                     mapping=aes(y=Variable, x=ORCIUpper, xmin=ORCIUpper, xmax=ORCILower),
                     height=0.2, size=1) +
      geom_point(data=calculatedStatistics, 
                 mapping=aes(y=Variable, x=OR), size=4, shape=21, fill="white")  
  }
  
  # for continious response variable
  if (treatasBinary==FALSE) {
    xlabLabel <- "Slope coefficients"
    plot <- ggplot() +
      geom_errorbarh(data=calculatedStatistics, 
                     mapping=aes(y=Variable, x=betaCIUpper, xmin=betaCIUpper, xmax=betaCILower),
                     height=0.2, size=1) +
      geom_point(data=calculatedStatistics, 
                 mapping=aes(y=Variable, x=beta), size=4, shape=21, fill="white")  
  }
  
  plot <- plot + theme_bw() + labs(title=graphTitle,
                                   x= xlabLabel)
  return(plot)  
}


plotFixedEffectsofMeasurementVar <- function (calculatedStatistics,
                                              measurementVar,
                                              treatasBinary) {
  
  graphTitle <- paste("Fixed effects of", measurementVar)
  
  # for binary response variable
  if (treatasBinary==TRUE) {
    xlabLabel <- "Odds ratios"
    plot <- ggplot() +
      geom_errorbarh(data=calculatedStatistics, 
                     mapping=aes(y=Variable, x=ORCIUpper, xmin=ORCIUpper, xmax=ORCILower),
                     height=0.2, size=1) +
      geom_point(data=calculatedStatistics, 
                 mapping=aes(y=Variable, x=OR), size=4, shape=21, fill="white")  
  }
  
  # for continious response variable
  if (treatasBinary==FALSE) {
    xlabLabel <- "Slope coefficients"
    plot <- ggplot() +
      geom_errorbarh(data=calculatedStatistics, 
                     mapping=aes(y=Variable, x=betaCIUpper, xmin=betaCIUpper, xmax=betaCILower),
                     height=0.2, size=1) +
      geom_point(data=calculatedStatistics, 
                 mapping=aes(y=Variable, x=beta), size=4, shape=21, fill="white")  
  }
  
  plot <- plot + theme_bw() + labs(title=graphTitle,
                                   x= xlabLabel)
  return(plot)  
}


plotFixedEffectsofDaysSinceInclusion <- function (calculatedStatistics,
                                                  treatasBinary) {
  
  graphTitle <- paste("Fixed effects of days since inclusion in the study")
  
  # for binary response variable
  if (treatasBinary==TRUE) {
    xlabLabel <- "Odds ratios"
    plot <- ggplot() +
      geom_errorbarh(data=calculatedStatistics, 
                     mapping=aes(y=Variable, x=ORCIUpper, xmin=ORCIUpper, xmax=ORCILower),
                     height=0.2, size=1) +
      geom_point(data=calculatedStatistics, 
                 mapping=aes(y=Variable, x=OR), size=4, shape=21, fill="white")  
  }
  
  # for continious response variable
  if (treatasBinary==FALSE) {
    xlabLabel <- "Slope coefficients"
    plot <- ggplot() +
      geom_errorbarh(data=calculatedStatistics, 
                     mapping=aes(y=Variable, x=betaCIUpper, xmin=betaCIUpper, xmax=betaCILower),
                     height=0.2, size=1) +
      geom_point(data=calculatedStatistics, 
                 mapping=aes(y=Variable, x=beta), size=4, shape=21, fill="white")  
  }
  
  plot <- plot + theme_bw() + labs(title=graphTitle,
                                   x= xlabLabel)
  return(plot)  
}