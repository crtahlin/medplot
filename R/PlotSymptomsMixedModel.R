mixedModel <- function(data,               
                       selectedSymptoms,   
                       coVariate1st,        
                       subjectIDVar,        
                       measurementVar,      
                       dateVar,             
                       thresholdValue,      
                       treatasBinary,       
                       selectedModel){      
  # if response variable is binary, do a data transformation based on the thresholdvalue
  if (treatasBinary==TRUE) {data[, selectedSymptoms] <- 
                              ifelse(data[,selectedSymptoms]>thresholdValue, 1, 0)                            
  }
  
  # transform the 1st covariate into appropriate form ####
  coVariate1stType <- determineTypeofVariable(data[,coVariate1st])
  if (coVariate1stType["type"]=="character") {
  # make coVariate1st factor variable
  data[, coVariate1st] <- as.factor(data[, coVariate1st])
  }
  if (coVariate1stType["type"]=="numeric" | coVariate1stType["type"]=="integer") {
    # make coVariate1st numeric variable
    data[, coVariate1st] <- as.numeric(data[, coVariate1st])
  }
  
  # make measurementVar a factor variable ####
  data[, measurementVar] <- as.factor(data[, measurementVar])
  
  # if the model includes days since inclusion, add this info to the data (column "daysSinceInclusion")
  if (selectedModel=="MMtimeSinceInclusion") {
    data <- calculateDaysSinceInclusion(data, subjectIDVar, dateVar)
    time <- "daysSinceInclusion"
  }
  
  # function to generate text for describing what is compared (reference level)####
  describeComparison <- function (data, variable) {
    if(!is.factor(data[,variable])) {return(print("Function takes a factor variable!"))}
    levels <- levels(data[, variable])
    referenceLevel <- levels[1]
    nonReferenceLevels <- levels[-1]
    comparisonString <- paste0(variable, nonReferenceLevels)
    comparisonText <- paste ("reference level:", referenceLevel)
    
    return(list(levels=levels,
                referenceLevel=referenceLevel,
                nonReferenceLevels=nonReferenceLevels,
                comparisonString=comparisonString,
                comparisonText=comparisonText))
  }
  
  if (is.factor(data[,coVariate1st])) {
  # name of the binary grouping variable coeficient assigned by R
  coVariate1stCoefName <- describeComparison(data, coVariate1st)[["comparisonString"]]
  # reference value is? the second level - the one that gets compared to the first level
  referenceValue <- describeComparison(data, coVariate1st)[["nonReferenceLevels"]]
  # what are we comparing?
  coVariate1stComparison <- describeComparison(data, coVariate1st)[["comparisonText"]]
  } else {
    coVariate1stCoefName <- coVariate1st
    coVariate1stComparison <- "continious variable"
  }
  
  # list measurement occasion levels
  measurementLevels <- describeComparison(data, measurementVar)[["levels"]]
  # the measurement level that other are compared to is?
  measurementVarComparison <- describeComparison(data, measurementVar)[["referenceLevel"]]
  # the other measurement levels
  nonReferenceMeasurements <- describeComparison(data, measurementVar)[["nonReferenceLevels"]]
   
  # prepare data frame for the results, depending on what kind of response variable we have
  if ( is.factor(data[,coVariate1st]) & (length(levels(data[,coVariate1st]))>2) ) { 
    # if 1st covariate is multilevel factor
    resultscoVariate1st <- data.frame(expand.grid(Variable=selectedSymptoms,
                                                  CovariateLevel=referenceValue))
    
    } else { # if the 1st covariate is binary factor or numerical
      resultscoVariate1st <- data.frame(Variable=selectedSymptoms) 
      
    }
  resultsMeasurementVar <- data.frame(expand.grid(Variable=selectedSymptoms,
                                                  Measurement=nonReferenceMeasurements))

  resultsDaysSinceInclusion <- data.frame(Variable=selectedSymptoms)
  

  # The logic ####
  # cycle through response variables
  for (symptom in selectedSymptoms) {
    
    # choose the right model formula depending on user selected option of model
    # construct the right formula based on selected model
    if (selectedModel=="MMsimple") {
      formula <- as.formula(paste(symptom, "~", coVariate1st,
                                  "+(1|", subjectIDVar, ")"))
    }
    if (selectedModel=="MMmeasurement") {
      formula <- as.formula(paste(symptom, "~",
                                  as.factor(measurementVar),
                                  "+",
                                  coVariate1st,
                                  "+(1|", subjectIDVar, ")"))
    }
    if (selectedModel=="MMtimeSinceInclusion") {
      formula <- as.formula(paste(symptom, "~", time,"+", coVariate1st,
                                  "+(1|", subjectIDVar, ")"))
    }
    
    
    # build the model depending on whether the response is binary or not ####
    if (treatasBinary==TRUE) { # treatasBinary = TRUE ####
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
      
      # results for the grouping variable if it is multilevel factor ####
      if ( is.factor(data[,coVariate1st]) & (length(levels(data[,coVariate1st]))>2) ) {
      for (level in referenceValue) {
        tempResult <- returnResultsGlmer(model, paste0(coVariate1st,level))
        
        resultscoVariate1st[resultscoVariate1st["Variable"]==symptom &
                              resultscoVariate1st["CovariateLevel"]==level,
                              "OR"] <- tempResult[["Estimate"]]
        
        resultscoVariate1st[resultscoVariate1st["Variable"]==symptom & 
                              resultscoVariate1st["CovariateLevel"]==level,
                              "ORCILower"] <- tempResult[["CILower"]] 
        resultscoVariate1st[resultscoVariate1st["Variable"]==symptom &
                              resultscoVariate1st["CovariateLevel"]==level,
                              "ORCIUpper"] <- tempResult[["CIUpper"]] 
        
        resultscoVariate1st[resultscoVariate1st["Variable"]==symptom &
                              resultscoVariate1st["CovariateLevel"]==level,
                              "ORPValue"] <-  tempResult[["PValue"]] 
       
        rm(tempResult)
      
      }
        
      } else {
      # results for the grouping variable if it is binary or numerical ####
      tempResult <- returnResultsGlmer(model, coVariate1stCoefName)
        
      resultscoVariate1st[resultscoVariate1st["Variable"]==symptom, "OR"] <- 
        tempResult[["Estimate"]] 
      
      resultscoVariate1st[resultscoVariate1st["Variable"]==symptom, "ORCILower"] <- 
        tempResult[["CILower"]] 
      
      resultscoVariate1st[resultscoVariate1st["Variable"]==symptom, "ORCIUpper"] <- 
        tempResult[["CIUpper"]] 
      
      resultscoVariate1st[resultscoVariate1st["Variable"]==symptom, "ORPValue"] <- 
        tempResult[["PValue"]] 
      rm(tempResult)
      }
      
      
      # results for the measurement variable
      if (selectedModel=="MMmeasurement") {
                
        for (measurement in nonReferenceMeasurements) {
          tempResult <- returnResultsGlmer(model, paste0(measurementVar,measurement))
          
          resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom &
                                resultsMeasurementVar["Measurement"]==measurement,
                              "OR"] <- tempResult[["Estimate"]] 
          resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom & 
                                  resultsMeasurementVar["Measurement"]==measurement,
                                "ORCILower"] <- tempResult[["CILower"]] 
          resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom &
                                resultsMeasurementVar["Measurement"]==measurement,
                                "ORCIUpper"] <- tempResult[["CIUpper"]] 
          resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom &
                                resultsMeasurementVar["Measurement"]==measurement,
                              "ORPValue"] <- 
          tempResult[["PValue"]] 
        rm(tempResult)
      }}
      
      # results for the days since inclusion variable
      if (selectedModel=="MMtimeSinceInclusion") {
        tempResult <- returnResultsGlmer(model, time)
                
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "OR"] <- 
          tempResult[["Estimate"]] 
        
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "ORCILower"] <- 
          tempResult[["CILower"]] 
        
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "ORCIUpper"] <- 
          tempResult[["CIUpper"]] 
        
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "ORPValue"] <- 
          tempResult[["PValue"]] 
      }      
    }
    
    
    if(treatasBinary==FALSE) { #####
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
 
      # results if coVariate1st multilevel factor
      if ( is.factor(data[,coVariate1st]) & (length(levels(data[,coVariate1st]))>2) ) {
        for (level in referenceValue) {
            tempResult <- returnResultsLmer(model, paste0(coVariate1st,level))
            
            resultscoVariate1st[resultscoVariate1st["Variable"]==symptom &
                                  resultscoVariate1st["CovariateLevel"]==level,
                                "beta"] <- tempResult[["Estimate"]] 
            resultscoVariate1st[resultscoVariate1st["Variable"]==symptom & 
                                  resultscoVariate1st["CovariateLevel"]==level,
                                "betaCILower"] <- tempResult[["CILower"]] 
            resultscoVariate1st[resultscoVariate1st["Variable"]==symptom &
                                  resultscoVariate1st["CovariateLevel"]==level,
                                "betaCIUpper"] <- tempResult[["CIUpper"]] 
            resultscoVariate1st[resultscoVariate1st["Variable"]==symptom &
                                  resultscoVariate1st["CovariateLevel"]==level,
                                "betaPValue"] <- 
              tempResult[["PValue"]] 
            rm(tempResult)
        }
        } else {
      # results for the coVariate1st variable
      tempResult <- returnResultsLmer(model, coVariate1stCoefName)
      resultscoVariate1st[resultscoVariate1st["Variable"]==symptom, "beta"] <-
        tempResult[["Estimate"]]
        
      resultscoVariate1st[resultscoVariate1st["Variable"]==symptom, "betaCILower"] <-
        tempResult[["CILower"]]
      
      resultscoVariate1st[resultscoVariate1st["Variable"]==symptom, "betaCIUpper"] <-
        tempResult[["CIUpper"]]
        
      resultscoVariate1st[resultscoVariate1st["Variable"]==symptom, "betaPValue"] <-
        tempResult[["PValue"]]
        
      rm(tempResult)
        }
      
      if (selectedModel=="MMmeasurement") {
        # results for the measurement variable
        for (measurement in nonReferenceMeasurements) {
        tempResult <- returnResultsLmer(model, paste0(measurementVar,measurement))
          
          resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom &
                                resultsMeasurementVar["Measurement"]==measurement,
                              "beta"] <- tempResult[["Estimate"]]
        
        resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom &
                                resultsMeasurementVar["Measurement"]==measurement,
                              "betaCILower"] <- tempResult[["CILower"]]
        
        resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom &
                                resultsMeasurementVar["Measurement"]==measurement,
                              "betaCIUpper"] <- tempResult[["CIUpper"]]
        
        resultsMeasurementVar[resultsMeasurementVar["Variable"]==symptom &
                                resultsMeasurementVar["Measurement"]==measurement,
                              "betaPValue"] <- tempResult[["PValue"]]
        rm(tempResult)
      }}
      
      if (selectedModel=="MMtimeSinceInclusion") {
        # results for the days since inclusion variable
        tempResult <- returnResultsLmer(model, time)
        
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "beta"] <-
          tempResult[["Estimate"]]
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "betaCILower"] <-
          tempResult[["CILower"]]
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "betaCIUpper"] <-
          tempResult[["CIUpper"]]
        resultsDaysSinceInclusion[resultsDaysSinceInclusion["Variable"]==symptom, "betaPValue"] <-
          tempResult[["PValue"]]
                  
        rm(tempResult)
      }
    }
  }
  
#   # generate printable results tables
#   # for outcome non-binary
#   if (treatasBinary==FALSE) {
#   # for covariate1st
#     # TODO: for scenario with multilevel categorical
#     # TODO: all scenarios for binary variables
#   printableResultsCoVariate1st <- cbind(format(resultscoVariate1st$Variable, digits=2),
#                                         format(resultscoVariate1st$beta, digits=2),
#                                         paste(format(resultscoVariate1st$betaCILower, digits=2),
#                                               "to",
#                                               format(resultscoVariate1st$betaCIUpper, digits=2)),
#                                         format(resultscoVariate1st$betaPValue, digits=2))
#   colnames(printableResultsCoVariate1st) <- c("Variable", "Beta", "95% conf. interval", "P Value" )
#   # for evaluation occasion
#   printableResultsMeasurementVar <- cbind(format(resultsMeasurementVar$Variable, digits=2),
#                                           format(resultsMeasurementVar$Measurement, digits=2),
#                                           format(resultsMeasurementVar$beta, digits=2),
#                                           paste(format(resultsMeasurementVar$betaCILower, digits=2),
#                                                 "to",
#                                                 format(resultsMeasurementVar$betaCIUpper, digits=2)),
#                                           format(resultsMeasurementVar$betaPValue, digits=2))
#   colnames(printableResultsMeasurementVar) <- c("Variable","Measurement" ,"Beta",
#                                                 "95% conf. interval", "P Value" )
#   # for timesinceinclusion
#   printableResultsDaysSinceInclusion <- cbind(format(resultsDaysSinceInclusion$Variable, digits=2),
#                                               format(resultsDaysSinceInclusion$beta, digits=2),
#                                               paste(format(resultsDaysSinceInclusion$betaCILower, digits=2),
#                                                     "to",
#                                                     format(resultsDaysSinceInclusion$betaCIUpper, digits=2)),
#                                               format(resultsDaysSinceInclusion$betaPValue, digits=2))
#   colnames(printableResultsDaysSinceInclusion) <- c("Variable","Beta", "95% conf. interval", "P Value" )
#   
#   
#   }
 # browser()

# printable results for coVariate1st ####
if ("OR" %in% colnames(resultscoVariate1st) ) { # OR scenario ####
if ("CovariateLevel" %in% colnames(resultscoVariate1st)) { # multilevel factor
    printableResultsCoVariate1st <-
      data.frame("Variable"=resultscoVariate1st$Variable,
                 "Levels"= resultscoVariate1st$CovariateLevel,
                 "OR"=format(resultscoVariate1st$OR, digits=2),
                 "95% conf. interval"= paste(format(resultscoVariate1st$ORCILower, digits=2),
                                             "to",
                                             format(resultscoVariate1st$ORCIUpper, digits=2)),
                 "P Value"=format(resultscoVariate1st$ORPValue, digits=2), check.names=FALSE)    
} else { # non multilevel factor
  printableResultsCoVariate1st <-
    data.frame("Variable"=resultscoVariate1st$Variable,
               "OR"=format(resultscoVariate1st$OR, digits=2),
               "95% conf. interval"= paste(format(resultscoVariate1st$ORCILower, digits=2),
                                           "to",
                                           format(resultscoVariate1st$ORCIUpper, digits=2)),
               "P Value"=format(resultscoVariate1st$ORPValue, digits=2), check.names=FALSE)
}}
if ("beta" %in% colnames(resultscoVariate1st) ) { # beta scenario ####
  if ("CovariateLevel" %in% colnames(resultscoVariate1st)) { # multilevel factor
    printableResultsCoVariate1st <-
      data.frame("Variable"=resultscoVariate1st$Variable,
                 "Levels"= resultscoVariate1st$CovariateLevel,
                 "Beta"=format(resultscoVariate1st$beta, digits=2),
                 "95% conf. interval"= paste(format(resultscoVariate1st$betaCILower, digits=2),
                                             "to",
                                             format(resultscoVariate1st$betaCIUpper, digits=2)),
                 "P Value"=format(resultscoVariate1st$betaPValue, digits=2), check.names=FALSE)    
  } else { # non multilevel factor
    printableResultsCoVariate1st <-
      data.frame("Variable"=resultscoVariate1st$Variable,
                 "Beta"=format(resultscoVariate1st$beta, digits=2),
                 "95% conf. interval"= paste(format(resultscoVariate1st$betaCILower, digits=2),
                                             "to",
                                             format(resultscoVariate1st$betaCIUpper, digits=2)),
                 "P Value"=format(resultscoVariate1st$betaPValue, digits=2), check.names=FALSE)
  }}

printableResultsMeasurementVar <- data.frame()
printableResultsDaysSinceInclusion <- data.frame()

# printable results for MeasurementVar ####
if ("OR" %in% colnames(resultsMeasurementVar) ) { # OR scenario ####
        printableResultsMeasurementVar <-
          data.frame("Variable"=resultsMeasurementVar$Variable,
                     "Levels"= resultsMeasurementVar$Measurement,
                     "OR"=format(resultsMeasurementVar$OR, digits=2),
                     "95% conf. interval"= paste(format(resultsMeasurementVar$ORCILower, digits=2),
                                                 "to",
                                                 format(resultsMeasurementVar$ORCIUpper, digits=2)),
                     "P Value"=format(resultsMeasurementVar$ORPValue, digits=2), check.names=FALSE)
}

if ("beta" %in% colnames(resultscoVariate1st) ) { # beta scenario ####
         printableResultsMeasurementVar <- 
           data.frame("Variable"=resultsMeasurementVar$Variable,
                      "Levels"= resultsMeasurementVar$Measurement,
                      "Beta"=format(resultsMeasurementVar$beta, digits=2),
                      "95% conf. interval"= paste(format(resultsMeasurementVar$betaCILower, digits=2),
                                                  "to",
                                                  format(resultsMeasurementVar$betaCIUpper, digits=2)),
                      "P Value"=format(resultsMeasurementVar$betaPValue, digits=2), check.names=FALSE)
}


# printable results for daysSinceInclusion ####
if ("OR" %in% colnames(resultsDaysSinceInclusion) ) { # OR scenario ####
      printableResultsDaysSinceInclusion <-
        data.frame("Variable"=resultsDaysSinceInclusion$Variable,
                   "OR"=format(resultsDaysSinceInclusion$OR, digits=2),
                   "95% conf. interval"= paste(format(resultsDaysSinceInclusion$ORCILower, digits=2),
                                               "to",
                                               format(resultsDaysSinceInclusion$ORCIUpper, digits=2)),
                   "P Value"=format(resultsDaysSinceInclusion$ORPValue, digits=2), check.names=FALSE)
}

if ("beta" %in% colnames(resultsDaysSinceInclusion) ) { # beta scenario ####
  printableResultsDaysSinceInclusion <- 
    data.frame("Variable"=resultsDaysSinceInclusion$Variable,
               "Beta"=format(resultsDaysSinceInclusion$beta, digits=2),
               "95% conf. interval"= paste(format(resultsDaysSinceInclusion$betaCILower, digits=2),
                                           "to",
                                           format(resultsDaysSinceInclusion$betaCIUpper, digits=2)),
               "P Value"=format(resultsDaysSinceInclusion$betaPValue, digits=2), check.names=FALSE)
}


  return(list(coVariate1st=resultscoVariate1st,
              printablecoVariate1st=printableResultsCoVariate1st,
              coVariate1stComparison=coVariate1stComparison,
              measurementVar=resultsMeasurementVar,
              printablemeasurementVar=printableResultsMeasurementVar,
              measurementVarComparison=measurementVarComparison,
              daysSinceInclusion=resultsDaysSinceInclusion,
              printabledaysSinceInclusion=printableResultsDaysSinceInclusion))
}

calculateDaysSinceInclusion <- function (data,
                                         subjectIDVar,
                                         dateVar) {
  # find the day of inclusion in the study for each person
  uniquePeople <- as.data.frame(unique(data[subjectIDVar]))
  colnames(uniquePeople) <- subjectIDVar
  
  for (person in uniquePeople[,1]) {
    subset <- as.Date(data[which(data[subjectIDVar]==person), dateVar], format="%d.%m.%Y")
    uniquePeople[which(uniquePeople[subjectIDVar]==person), "minDate"] <- min(subset)
    data[which(data[subjectIDVar]==person), "minDate"] <- min(subset)
  }
  # data$minDate <- as.Date(data$minDate, format="%Y-%m-%d")
  data$daysSinceInclusion <- as.numeric(as.Date(data[,dateVar], format="%d.%m.%Y") - data$minDate) # save as numeric for melt()to work
  # TODO: tu je nekaj čudnega  - zakaj je to sploh delovlao? oba datuma sta v različnem formatu
  # morao bi biti: data$minDate <- as.Date(data$minDate, format="%d.%m.%Y")
  # in tudi : as.Date(data[,dateVar], format="%d.%m.%Y")
  # as.Date(data[,dateVar], format="%d.%m.%Y") - as.Date(data$minDate, format="%d.%m.%Y")
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
      geom_vline(xintercept=1) +
      if ("CovariateLevel" %in% colnames(calculatedStatistics)) {facet_grid(CovariateLevel ~ .)}
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
      geom_vline(xintercept=0) + 
      if ("CovariateLevel" %in% colnames(calculatedStatistics)) {facet_grid(CovariateLevel ~ .)}
  }
  
  plot <- plot + myTheme() + labs(title=graphTitle,
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
  
  plot <- plot + myTheme() + labs(title=graphTitle,
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
  
  plot <- plot + myTheme() + labs(title=graphTitle,
                                   x= xlabLabel)
  return(plot)  
}