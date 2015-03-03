#' @title Return a list of result for linear modeling
#' 
#' @description Returns a list containing results of linear modeling. 
#' The list contains the printable (for GUI output) and raw results (for ploting).
#' 
#' @param data The data.
#' @param measurementVar Which variable represent the measurement occasion.
#' @param selectedMeasurement Which measurement occasion was selected.
#' @param Which covariates are to be used.
#' @param Which outcome variables were selected.
#' 
#' @export
tableLinear <- function(data,
                        measurementVar,
                        selectedMeasurement,
                        covariate,
                        selectedSymptoms) { 
  
  # subset the data, using only the selected evaluation
  data <- data[data[,measurementVar]==selectedMeasurement, ]
  
  table <- data.frame("Variable"=selectedSymptoms) # table of printable results - Fixed effect
  table2 <- data.frame("Variable"=selectedSymptoms) # table of raw results
  table3 <- data.frame("Variable"=selectedSymptoms) # table of printable results - Intercept
  
  for (symptom in selectedSymptoms) {
    model <- glm(data[,symptom] ~ data[,covariate], family=gaussian())
    
    # printable and raw table of coefficients
    table[table["Variable"]==symptom, "Beta (slope)"] <- 
      format(model$coef[2], digits=2)
    table[table["Variable"]==symptom, "95% conf. interval"] <- 
      paste(format((confint(model)[2,1]), digits=2),
            " to ",
            format((confint(model)[2,2]), digits=2))
    table2[table2["Variable"]==symptom, "beta"] <- (model$coef[2])
    table2[table2["Variable"]==symptom, "CILower"] <- (confint(model)[2,1])
    table2[table2["Variable"]==symptom, "CIUpper"] <- (confint(model)[2,2])
    table[table["Variable"]==symptom, "P value"] <-
      format(summary(model)$coefficients[2,"Pr(>|t|)"],digits=2)
    
    # printable table of intercepts
    table3[table3["Variable"]==symptom, "Intercept"] <- 
      format(model$coef[1], digits=2)
    table3[table3["Variable"]==symptom, "95% conf. interval"] <- 
      paste(format((confint(model)[1,1]), digits=2),
            " to ",
            format((confint(model)[1,2]), digits=2))
    table3[table["Variable"]==symptom, "P value"] <-
      format(summary(model)$coefficients[1,"Pr(>|t|)"],digits=2)
    }
  
  return(list(printableResultsTable=table,
              rawResultsTable=table2,
              printableInterceptTable=table3))
  }