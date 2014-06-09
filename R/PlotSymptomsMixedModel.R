mixedModel <- function(data,               # dataFiltered()
                       selectedSymptoms,    # input$selectedSymptoms
                       coVariate1st,         # input$groupingVar
                       subjectIDVar,        # input$patientIDVar
                       measurementVar,      # input$measurementVar
                       dateVar,             # input$dateVar
                       thresholdValue,      # input$thresholdValue
                       treatasBinary,       # input$treatasBinary
                       selectedModel){      # input$selectedMixedModelType
  
  # make coVariate1st binary variable
  data[, coVariate1st] <- as.factor(data[, coVariate1st])
  # make measurementVar a factor variable
  data[, measurementVar] <- as.factor(data[, measurementVar])
  
  
  # name of the binary grouping variable coeficient assigned by R
  coVariate1stCoefName <- paste0(coVariate1st,levels(data[,coVariate1st])[2])
  # reference value is? the second level - the one that gets compared to the first level
  referenceValue <- levels(data[,coVariate1st])[2]
  # what are we comparing?
  coVariate1stComparison <- paste(levels(data[,coVariate1st])[2], "vs", levels(data[,coVariate1st])[1])
  # list measurement occasion levels
  measurementLevels <- levels(data[, measurementVar])
  # the measurement level that other are compared to is?
  measurementVarComparison <- measurementLevels[1]
  # the other measurement levels
  nonReferenceMeasurements <- measurementLevels[-1]
  
  
  # if response variable is binary, do a data transformation based on the thresholdvalue
  if (treatasBinary==TRUE) {data[, selectedSymptoms] <- 
                              ifelse(data[,selectedSymptoms]>thresholdValue, 1, 0)                            
  }
  
  # if the model includes days since inclusion, add this info to the data (column "daysSinceInclusion")
  if (selectedModel=="MMtimeSinceInclusion") {
    data <- calculateDaysSinceInclusion(data, subjectIDVar, dateVar)
    time <- "daysSinceInclusion"
  }
  
  # prepare data frame for the results, depending on what kind of response variable we have
  resultscoVariate1st <- data.frame(Variable=selectedSymptoms) 
  
  resultsMeasurementVar <- data.frame(expand.grid(Variable=selectedSymptoms,
                                                    Measurement=nonReferenceMeasurements))
  resultsDaysSinceInclusion <- data.frame(Variable=selectedSymptoms)

  
  # cycle through response variables
  for (symptom in selectedSymptoms) {
    
    # choose the right model formula depending on user selected option of model
    # construct the right formula based on selected model
    if (selectedModel=="MMsimple") {
      formula <- as.formula(paste(symptom, "~", coVariate1st, "+(1|", subjectIDVar, ")"))
    }
    if (selectedModel=="MMmeasurement") {
      formula <- as.formula(paste(symptom, "~", as.factor(measurementVar),"+", coVariate1st, "+(1|", subjectIDVar, ")"))
    }
    if (selectedModel=="MMtimeSinceInclusion") {
      formula <- as.formula(paste(symptom, "~", time,"+", coVariate1st, "+(1|", subjectIDVar, ")"))
    }
    
    
    # build the model depending on whether the response is binary or not
    if (treatasBinary==TRUE) {
      model <- glmer(formula, family=binomial,  na.action=na.omit, data=data)
   
      #### Returns results from glmer ####
      returnResultsGlmer <- function (model, variateName) {
        Estimate <- exp(summary(model)$coef[variateName, "Estimate"])
        CILower <- exp(summary(model)$coef[variateName, "Estimate"] - 
                         qnorm(.975)*summary(model)$coef[variateName, "Std. Error"])
        CIUpper <- exp(summary(model)$coef[variateName, "Estimate"] + 
                         qnorm(.975)*summary(model)$coef[variateName, "Std. Error"])
        PValue <- summary(model)$coef[variateName, "Pr(>|z|)"]
        return(list(Estimate=Estimate, CILower=CILower, CIUpper=CIUpper, PValue=PValue))
      }
      ####
      
      # results for the grouping variable
      tempResult <- returnResultsGlmer(model, coVariate1stCoefName)
        
      resultscoVariate1st[resultscoVariate1st["Variable"]==symptom, "OR"] <- 
        tempResult[["Estimate"]] #exp(summary(model)$coef[coVariate1stCoefName, "Estimate"])
      
      resultscoVariate1st[resultscoVariate1st["Variable"]==symptom, "ORCILower"] <- 
        tempResult[["CILower"]] # exp(summary(model)$coef[coVariate1stCoefName, "Estimate"] - 
              #qnorm(.975)*summary(model)$coef[coVariate1stCoefName, "Std. Error"])
      
      resultscoVariate1st[resultscoVariate1st["Variable"]==symptom, "ORCIUpper"] <- 
        tempResult[["CIUpper"]] # exp(summary(model)$coef[coVariate1stCoefName, "Estimate"] +
              # qnorm(.975)*summary(model)$coef[coVariate1stCoefName, "Std. Error"])
      
      resultscoVariate1st[resultscoVariate1st["Variable"]==symptom, "ORPValue"] <- 
        tempResult[["PValue"]] #summary(model)$coef[coVariate1stCoefName, "Pr(>|z|)"]
      rm(tempResult)
      
      # results for the measurement variable
      if (selectedModel=="MMmeasurement") {
                
        for (measurement in nonReferenceMeasurements) {
          tempResult <- returnResultsGlmer(model, paste0(measurementVar,measurement))
          
          resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom &
                                resultsMeasurementVar["Measurement"]==measurement,
                              "OR"] <- 
            tempResult[["Estimate"]] # exp(summary(model)$coef[paste0(measurementVar,measurement), "Estimate"])
        
        resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom &
                                resultsMeasurementVar["Measurement"]==measurement,
                              "ORCILower"] <- 
          tempResult[["CILower"]] # exp(summary(model)$coef[paste0(measurementVar,measurement), "Estimate"] - 
               # qnorm(.975)*summary(model)$coef[paste0(measurementVar,measurement), "Std. Error"])
        
        resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom &
                                resultsMeasurementVar["Measurement"]==measurement
                              , "ORCIUpper"] <- 
          tempResult[["CIUpper"]] #exp(summary(model)$coef[paste0(measurementVar,measurement), "Estimate"] +
                # qnorm(.975)*summary(model)$coef[paste0(measurementVar,measurement), "Std. Error"])
        
        resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom &
                                resultsMeasurementVar["Measurement"]==measurement,
                              "ORPValue"] <- 
          tempResult[["PValue"]] # summary(model)$coef[paste0(measurementVar,measurement), "Pr(>|z|)"]
        rm(tempResult)
      }}
      
      # results for the days since inclusion variable
      if (selectedModel=="MMtimeSinceInclusion") {
        tempResult <- returnResultsGlmer(model, time)
                
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "OR"] <- 
          tempResult[["Estimate"]] #exp(summary(model)$coef[time, "Estimate"])
        
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "ORCILower"] <- 
          tempResult[["CILower"]] # exp(summary(model)$coef[time, "Estimate"] - 
                #qnorm(.975)*summary(model)$coef[time, "Std. Error"])
        
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "ORCIUpper"] <- 
          tempResult[["CIUpper"]] # exp(summary(model)$coef[time, "Estimate"] +
               # qnorm(.975)*summary(model)$coef[time, "Std. Error"])
        
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "ORPValue"] <- 
          tempResult[["PValue"]] #  summary(model)$coef[time, "Pr(>|z|)"]
      }      
    }
    
    
    if(treatasBinary==FALSE) {
      model <- lmerTest::lmer(formula, na.action=na.omit, data=data)
    
      
      ##### Returns results form lmer() ####
      returnResultsLmer <- function(model,
                                    variateName) {
      Estimate <- summary(model)$coef[variateName, "Estimate"]
      confIntervals <- confint(model)[variateName, c("2.5 %", "97.5 %")]
      CILower <- confIntervals[1]
      CIUpper <- confIntervals[2]
      PValue <- lmerTest::summary(model)$coef[variateName, "Pr(>|t|)"]
      return(list(Estimate=Estimate, CILower=CILower, CIUpper=CIUpper, PValue=PValue))
      }
      #####
      # results for the coVariate1st variable
      tempResult <- returnResultsLmer(model, coVariate1stCoefName)
      resultscoVariate1st[resultscoVariate1st["Variable"]==symptom, "beta"] <-
        tempResult[["Estimate"]]
        #summary(model)$coef[coVariate1stCoefName, "Estimate"]
      
      resultscoVariate1st[resultscoVariate1st["Variable"]==symptom, "betaCILower"] <-
        tempResult[["CILower"]]
        #confint(model)[coVariate1stCoefName, "2.5 %"]
      
      resultscoVariate1st[resultscoVariate1st["Variable"]==symptom, "betaCIUpper"] <-
        tempResult[["CIUpper"]]
        #confint(model)[coVariate1stCoefName, "97.5 %"]
      
      resultscoVariate1st[resultscoVariate1st["Variable"]==symptom, "betaPValue"] <-
        tempResult[["PValue"]]
        #lmerTest::summary(model)$coef[coVariate1stCoefName, "Pr(>|t|)"]
      rm(tempResult)
      
      if (selectedModel=="MMmeasurement") {
        # results for the measurement variable
        for (measurement in nonReferenceMeasurements) {
        tempResult <- returnResultsLmer(model, paste0(measurementVar,measurement))
          
          resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom &
                                resultsMeasurementVar["Measurement"]==measurement,
                              "beta"] <-
          tempResult[["Estimate"]]
         # summary(model)$coef[paste0(measurementVar,measurement), "Estimate"]
        
        resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom &
                                resultsMeasurementVar["Measurement"]==measurement,
                              "betaCILower"] <-
          tempResult[["CILower"]]
        
          # confint(model)[paste0(measurementVar,measurement), "2.5 %"]
        
        resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom &
                                resultsMeasurementVar["Measurement"]==measurement,
                              "betaCIUpper"] <-
          tempResult[["CIUpper"]]
          #confint(model)[paste0(measurementVar,measurement), "97.5 %"]
        
        resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom &
                                resultsMeasurementVar["Measurement"]==measurement, "betaPValue"] <-
          tempResult[["PValue"]]
          #lmerTest::summary(model)$coef[paste0(measurementVar,measurement), "Pr(>|t|)"]
        rm(tempResult)
      }}
      
      if (selectedModel=="MMtimeSinceInclusion") {
        # results for the days since inclusion variable
        tempResult <- returnResultsLmer(model, time)
        
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "beta"] <-
          tempResult[["Estimate"]]
                  #summary(model)$coef[time, "Estimate"]
        
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "betaCILower"] <-
          tempResult[["CILower"]]
                  #confint(model)[time, "2.5 %"]
        
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "betaCIUpper"] <-
          tempResult[["CIUpper"]]
        #          confint(model)[time, "97.5 %"]
       
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "betaPValue"] <-
          tempResult[["PValue"]]
                  #lmerTest::summary(model)$coef[time, "Pr(>|t|)"]
        rm(tempResult)
      }
    }
  }
  return(list(coVariate1st=resultscoVariate1st,
              coVariate1stComparison=coVariate1stComparison,
              measurementVar=resultsMeasurementVar,
              measurementVarComparison=measurementVarComparison,
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

plotFixedEffectsofcoVariate1st <- function (calculatedStatistics,
                                           coVariate1st,
                                           coVariate1stReferenceValue,
                                           treatasBinary) {
  graphTitle <- paste("Fixed effects of", coVariate1st)
  
  # for binary response variable
  if (treatasBinary==TRUE) {
    xlabLabel <- "Odds ratios"
    plot <- ggplot() +
      geom_errorbarh(data=calculatedStatistics, 
                     mapping=aes(y=Variable, x=ORCIUpper, xmin=ORCIUpper, xmax=ORCILower),
                     height=0.2, size=1) +
      geom_point(data=calculatedStatistics, 
                 mapping=aes(y=Variable, x=OR), size=4, shape=21, fill="white") +
      geom_vline(xintercept=1)
  }
  
  # for continious response variable
  if (treatasBinary==FALSE) {
    xlabLabel <- "Slope coefficients"
    plot <- ggplot() +
      geom_errorbarh(data=calculatedStatistics, 
                     mapping=aes(y=Variable, x=betaCIUpper, xmin=betaCIUpper, xmax=betaCILower),
                     height=0.2, size=1) +
      geom_point(data=calculatedStatistics, 
                 mapping=aes(y=Variable, x=beta), size=4, shape=21, fill="white") +
      geom_vline(xintercept=0)
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
    plot <- ggplot(data=calculatedStatistics) +
      geom_errorbarh(data=calculatedStatistics, 
                     mapping=aes(y=Variable, x=ORCIUpper, xmin=ORCIUpper, xmax=ORCILower),
                     height=0.2, size=1) +
      geom_point(data=calculatedStatistics, 
                 mapping=aes(y=Variable, x=OR), size=4, shape=21, fill="white") +
      geom_vline(xintercept=1) +
      facet_grid(Measurement ~ .)
  }
  
  # for continious response variable
  if (treatasBinary==FALSE) {
    xlabLabel <- "Slope coefficients"
    plot <- ggplot(data=calculatedStatistics) +
      geom_errorbarh(data=calculatedStatistics, 
                     mapping=aes(y=Variable, x=betaCIUpper, xmin=betaCIUpper, xmax=betaCILower),
                     height=0.2, size=1) +
      geom_point(data=calculatedStatistics, 
                 mapping=aes(y=Variable, x=beta), size=4, shape=21, fill="white") +
      geom_vline(xintercept=0) +
      facet_grid(Measurement ~ .)  
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
                 mapping=aes(y=Variable, x=OR), size=4, shape=21, fill="white") +
      geom_vline(xintercept=1)
  }
  
  # for continious response variable
  if (treatasBinary==FALSE) {
    xlabLabel <- "Slope coefficients"
    plot <- ggplot() +
      geom_errorbarh(data=calculatedStatistics, 
                     mapping=aes(y=Variable, x=betaCIUpper, xmin=betaCIUpper, xmax=betaCILower),
                     height=0.2, size=1) +
      geom_point(data=calculatedStatistics, 
                 mapping=aes(y=Variable, x=beta), size=4, shape=21, fill="white") +
      geom_vline(xintercept=0)
  }
  
  plot <- plot + theme_bw() + labs(title=graphTitle,
                                   x= xlabLabel)
  return(plot)  
}