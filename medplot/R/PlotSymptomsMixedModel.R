#' @title Return a list of result for mixed modeling
#' 
#' @description Returns a list containing results of mixed modeling. 
#' The list contains the printable (for GUI output) and raw results (for ploting).
#' 
#' @param data The data.
#' @param selectedSymptoms Which outcome variables were selected.
#' @param coVariate1st Which covariate was selected.
#' @param subjectIDVar Which variable identifies the subject.
#' @param measurementVar Which variable represent the measurement occasion.
#' @param dateVar Which variable represents the date.
#' @param thresholdValue What was the selected theshold value.
#' @param treatasBinary Should the data be treated as binary.
#' @param selectedModel Which modeling variant was selected.
#' 
#' @export
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
  resultsIntercept <- data.frame(Variable=selectedSymptoms)
  
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
      
      # results for the intercept (should be the same code, regardles of model type?)
      tempResult <- returnResultsGlmer(model, "(Intercept)")
      # TODO: intercept has odds, not odds ratio - OK for now, change later (some code 
      # depends on this. But the printable results already correct this.
      resultsIntercept[resultsIntercept["Variable"]==symptom, "OR"] <- 
        tempResult[["Estimate"]] 
      
      resultsIntercept[resultsIntercept["Variable"]==symptom, "ORCILower"] <- 
        tempResult[["CILower"]] 
      
      resultsIntercept[resultsIntercept["Variable"]==symptom, "ORCIUpper"] <- 
        tempResult[["CIUpper"]] 
      
      resultsIntercept[resultsIntercept["Variable"]==symptom, "ORPValue"] <- 
        tempResult[["PValue"]] 
      rm(tempResult)
      
      
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
      
      # results for the intercept (should be the same code, regardles of model type?)
      tempResult <- returnResultsLmer(model, "(Intercept)")
      # TODO: intercept has intercept, not Beta - OK for now, change later (some code 
      # depends on this. But the printable results already correct this.
      resultsIntercept[resultsIntercept["Variable"]==symptom, "beta"] <-
        tempResult[["Estimate"]]
      
      resultsIntercept[resultsIntercept["Variable"]==symptom, "betaCILower"] <-
        tempResult[["CILower"]]
      
      resultsIntercept[resultsIntercept["Variable"]==symptom, "betaCIUpper"] <-
        tempResult[["CIUpper"]]
      
      resultsIntercept[resultsIntercept["Variable"]==symptom, "betaPValue"] <-
        tempResult[["PValue"]] 
      rm(tempResult)
      
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
  
  #### Construct printable results tables ####
  # printable results for the intercepts ####
  if ("OR" %in% colnames(resultsIntercept) ) { # OR scenario ####
    printableResultsIntercept <- 
      data.frame("Variable"=resultsIntercept$Variable,
                 "Odds (intercept)"=format(resultsIntercept$OR, digits=2),
                 "95% conf. interval"= paste(format(resultsIntercept$ORCILower, digits=2),
                                             "to",
                                             format(resultsIntercept$ORCIUpper, digits=2)),
                 "P Value"=format(resultsIntercept$ORPValue, digits=2), check.names=FALSE)
  }
  
  if ("beta" %in% colnames(resultsIntercept) ) { # beta scenario ####
    printableResultsIntercept <- 
      data.frame("Variable"=resultsIntercept$Variable,
                 "Intercept"=format(resultsIntercept$beta, digits=2),
                 "95% conf. interval"= paste(format(resultsIntercept$betaCILower, digits=2),
                                             "to",
                                             format(resultsIntercept$betaCIUpper, digits=2)),
                 "P Value"=format(resultsIntercept$betaPValue, digits=2), check.names=FALSE)                                                  
  }
  
  
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
      
      # resort the results of printableResultsCoVariate1st
      printableResultsCoVariate1st <- resortbyVariables(printableResultsCoVariate1st, selectedSymptoms)
      
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
      
      # resort the results of printableResultsCoVariate1st
      printableResultsCoVariate1st <- resortbyVariables(printableResultsCoVariate1st, selectedSymptoms)
      
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
    
    # resort the results of printableResultsMeasurementVar, to keep Variables together
    printableResultsMeasurementVar <-
      resortbyVariables(printableResultsMeasurementVar, selectedSymptoms)
  }
  
  if ("beta" %in% colnames(resultsMeasurementVar) ) { # beta scenario ####
    printableResultsMeasurementVar <- 
      data.frame("Variable"=resultsMeasurementVar$Variable,
                 "Levels"= resultsMeasurementVar$Measurement,
                 "Beta"=format(resultsMeasurementVar$beta, digits=2),
                 "95% conf. interval"= paste(format(resultsMeasurementVar$betaCILower, digits=2),
                                             "to",
                                             format(resultsMeasurementVar$betaCIUpper, digits=2)),
                 "P Value"=format(resultsMeasurementVar$betaPValue, digits=2), check.names=FALSE)
    
    # resort the results of printableResultsMeasurementVar, to keep Variables together
    printableResultsMeasurementVar <-
      resortbyVariables(printableResultsMeasurementVar, selectedSymptoms)
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
  
  return(list( intercept=resultsIntercept,
               printableIntercept=printableResultsIntercept,
               coVariate1st=resultscoVariate1st,
               printablecoVariate1st=printableResultsCoVariate1st,
               coVariate1stComparison=coVariate1stComparison,
               measurementVar=resultsMeasurementVar,
               printablemeasurementVar=printableResultsMeasurementVar,
               measurementVarComparison=measurementVarComparison,
               daysSinceInclusion=resultsDaysSinceInclusion,
               printabledaysSinceInclusion=printableResultsDaysSinceInclusion))
}


#' @title Helper function for calculating the number of days from first date
#' 
#' @description Calculates and returns the number of days since minimum date of a certain subject. 
#' Adds the number of days since min date as column to the data frame it returns. 
#' 
#' @param data The data.
#' @param subjectIDVar Which variable identifies the subject.
#' @param dateVar Whic variable contains the date.
#' 
#' @export
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
  # TODO: tu je nekaj cudnega  - zakaj je to sploh delovlao? oba datuma sta v razlicnem formatu
  # morao bi biti: data$minDate <- as.Date(data$minDate, format="%d.%m.%Y")
  # in tudi : as.Date(data[,dateVar], format="%d.%m.%Y")
  # as.Date(data[,dateVar], format="%d.%m.%Y") - as.Date(data$minDate, format="%d.%m.%Y")
  return(data)
}

#' @title Plot the fixed effects for mixed models (helper function)
#' 
#' @param calculatedStatistics The results of modeling.
#' @param coVariate1st Which variable represents the covariate.
#' @param coVariate1stReferenceValue What is the reference level of the covariate.
#' @param treatasBinary Should outcomes be treated as binary.
#' @param variableOrder What should be the variable order on graph.
#' 
#' @export
plotFixedEffectsofcoVariate1st <- function (calculatedStatistics,
                                            coVariate1st,
                                            coVariate1stReferenceValue,
                                            treatasBinary, 
                                            variableOrder) {
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
                                  x= xlabLabel) + scale_y_discrete(limits=rev(variableOrder))
  
  return(plot)  
}

#' @title Plot the fixed effects for mixed models (helper function)
#' 
#' @param calculatedStatistics The results of modeling.
#' @param measurementVar Which variable represents the measurement occasion.
#' @param treatasBinary Should outcomes be treated as binary.
#' @param variableOrder What should be the variable order on graph.
#' 
#' @export
plotFixedEffectsofMeasurementVar <- function (calculatedStatistics,
                                              measurementVar,
                                              treatasBinary, 
                                              variableOrder) {
  
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
                                  x= xlabLabel) + scale_y_discrete(limits=rev(variableOrder))
  return(plot)  
}

#' @title Plot the fixed effects for mixed models (helper function)
#' 
#' @param calculatedStatistics The results of modeling.
#' @param treatasBinary Should outcomes be treated as binary.
#' @param variableOrder What should be the variable order on graph.
#' 
#' @export
plotFixedEffectsofDaysSinceInclusion <- function (calculatedStatistics,
                                                  treatasBinary, 
                                                  variableOrder) {
  
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
                                  x= xlabLabel) + scale_y_discrete(limits=rev(variableOrder))
  return(plot)  
}

#' @title Helper function to resort printable data frames to keep Variable levels together
#' 
#' @description Resorts the variables according the order the outcomes were selected on the GUI.
#' Returns the resorted dataframe.
#' 
#' @param dataframe The data.
#' @param selectedSymptoms Which outcome variables were selected.
#' 
#' @export
resortbyVariables <- function(dataframe, selectedSymptoms) {
  variableOrder <- match(dataframe[,"Variable"], selectedSymptoms)
  evaluationOrder <- dataframe[,"Levels"]
  totalOrder <- order(variableOrder, evaluationOrder)
  return(dataframe[totalOrder,])
}