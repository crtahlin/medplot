tabelizeLogist <- function(data,
                           measurementVar,
                           selectedMeasurement,
                           covariate,
                           selectedSymptoms,
                           thresholdValue) {

  
  # subset the data, using only the selected evaluation
  data <- data[data[,measurementVar]==selectedMeasurement, ]
  
  # binarize the data
  data[, selectedSymptoms] <- ifelse(data[, selectedSymptoms]>thresholdValue, 1, 0)
  
  table <- data.frame("Variable"=selectedSymptoms) # table of printable results
  table2 <- data.frame("Variable"=selectedSymptoms) # table of raw results
  levels <- levels(as.factor(data[,covariate])) # levels of the covariate
  oddsFor <- paste(levels[2],"vs",levels[1]) # text describing which variables were compared
  
  for (symptom in selectedSymptoms) {
    model <- glm(data[,symptom] ~ data[,covariate], family=binomial())
    table[table["Variable"]==symptom, "Odds ratio"] <- 
      exp(model$coef[2])
    table[table["Variable"]==symptom, "95% conf. interval"] <- 
      paste(format(exp(confint(model)[2,1]), nsmall=2, digits=2),
            " to ",
            format(exp(confint(model)[2,2]), nsmall=2, digits=2))
    table2[table2["Variable"]==symptom, "OR"] <- exp(model$coef[2])
    table2[table2["Variable"]==symptom, "CILower"] <- exp(confint(model)[2,1])
    table2[table2["Variable"]==symptom, "CIUpper"] <- exp(confint(model)[2,2])
  }
  return(list(printableResultsTable=table, rawResultsTable=table2, referenceValue=oddsFor))

}