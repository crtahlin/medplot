#' @title Logistf data in tabular format
#' 
#' @description TODO
#' 
#' @param data The data.
#' @param measurementVar Which variable represents the evaluatuon occasions.
#' @param selectedMeasurement For which evaluation occasion.
#' @param covariate Which var is the covariate.
#' @param selectedSymptoms For which outcomes.
#' @param thresholdValue For which threshold value.
#' 
#' @export
tableLogistf <- function (data,
                          measurementVar,
                          selectedMeasurement,
                          covariate,
                          selectedSymptoms,
                          thresholdValue) {
  
  # subset the data, using only the selected evaluation
  data <- data[data[,measurementVar]==selectedMeasurement,]
  #data <- data[measurementValues==selectedMeasurement,]
  # binarize the data
  data[, selectedSymptoms] <- ifelse(data[, selectedSymptoms]>thresholdValue, 1, 0)
  
  table <- data.frame("Variable"=selectedSymptoms) # table of printable results - Fixed effect
  table2 <- data.frame("Variable"=selectedSymptoms) # table of raw results
  table3 <- data.frame("Variable"=selectedSymptoms) # table of printable results - Intercept
  
  # check if covariate is binary and generate text which levels we are comparing
  if (determineTypeofVariable(data[,covariate])[["nLevels"]]=="binary") { # binary var
    levels <- levels(as.factor(data[,covariate])) # levels of the covariate
    oddsFor <- paste(levels[2],"vs",levels[1]) # text describing which variables were compared
  }
  
  if (determineTypeofVariable(data[,covariate])[["nLevels"]]=="multilevel" &  # numerical var w multi levels
      ( determineTypeofVariable(data[,covariate])[["type"]]=="integer") 
      | determineTypeofVariable(data[,covariate])[["type"]]=="numeric") { 
    #levels <- levels(as.factor(data[,covariate])) # levels of the covariate
    oddsFor <- paste("unit difference in", covariate) # text describing which variables were compared
  }
  
  for (symptom in selectedSymptoms) {
    model <- logistf(data[,symptom] ~ data[,covariate], family="binomial")
    table[table["Variable"]==symptom, "Odds ratio"] <- 
      format(exp(model$coef[2]), digits=2)
    table[table["Variable"]==symptom, "95% conf. interval"] <- 
      paste(format(exp(model$ci.lower[2]), digits=2),
            " to ",
            format(exp(model$ci.upper[2]), digits=2))
    table2[table2["Variable"]==symptom, "OR"] <- exp(model$coef[2])
    table2[table2["Variable"]==symptom, "CILower"] <- exp(model$ci.lower[2])
    table2[table2["Variable"]==symptom, "CIUpper"] <- exp(model$ci.upper[2])
    table[table["Variable"]==symptom, "P value"] <- format(model$prob[2], digits=2)
    
    # results for intercept 
    table3[table3["Variable"]==symptom, "Odds (intercept)"] <- 
      format(exp(model$coef[1]), digits=2)
    table3[table3["Variable"]==symptom, "95% conf. interval"] <- 
      paste(format(exp(model$ci.lower[1]), digits=2),
            " to ",
            format(exp(model$ci.upper[1]), digits=2))
    table3[table3["Variable"]==symptom, "P value"] <- format(model$prob[1], digits=2)
    
  }
  return(list(printableResultsTable=table,
              rawResultsTable=table2,
              referenceValue=oddsFor,
              printableInterceptTable=table3))
}