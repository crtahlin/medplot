.mixedModel <- function(data,               # dataFiltered()
                        selectedSymptoms,    # input$selectedSymptoms
                        groupingVar,         # input$groupingVar
                        subjectIDVar,        # input$patientIDVar
                        measurementVar,      # input$measurementVar
                        dateVar,             # input$dateVar
                        thresholdValue,      # input$thresholdValue
                        treatasBinary,       # input$treatasBinary
                        selectedModel){      # input$selectedMixedModelType
  
  # if response variable is binary, do a data transformation based on the thresholdvalue
  if (treatasBinary==TRUE) {data[, selectedSymptoms] <- 
                              ifelse(data[,selectedSymptoms]>thresholdValue, 1, 0)}
  # if the model includes days since inclusion, add this info to the data (column "daysSinceInclusion")
  data <- .calculateTimeSinceInclusion(data, subjectIDVar, dateVar)
  time <- "daysSinceInclusion"
  
  # prepare data frame for the results, depending on what kind of response variable we have
  if (treatasBinary==TRUE) {
    results <- data.frame(expand.grid(Variable=selectedSymptoms), OR=NA, ORCILower=NA, ORCIUpper=NA, ORPValue=NA)
  }
  if (treatasBinary==FALSE) {
    results <- data.frame(expand.grid(Variable=selectedSymptoms), beta=NA, betaCILower=NA, betaCIUpper=NA, betaPValue=NA)
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
      results[results["Variable"]==symptom, "OR"] <- exp(summary(model)$coef[2,1])
      results[results["Variable"]==symptom, "ORCILower"] <- 
        exp(summary(model)$coef[2,1] - qnorm(.975)*summary(model)$coef[2,2])
      results[results["Variable"]==symptom, "ORCIUpper"] <- 
        exp(summary(model)$coef[2,1] + qnorm(.975)*summary(model)$coef[2,2])
      results[results["Variable"]==symptom, "ORPValue"] <- 
        summary(model)$coef[2,4]
      
      
    }
    if(treatasBinary==FALSE) {
      model <- lmer(formula, na.action=na.omit, data=data)
      results[results["Variable"]==symptom, "beta"] <-
        summary(model)$coef[2,1]
      results[results["Variable"]==symptom, "betaCILower"] <-
        confint(model)[4,1]
      results[results["Variable"]==symptom, "betaCIUpper"] <-
        confint(model)[4,2]
      results[results["Variable"]==symptom, "betaPValue"] <-
        summary(model)$coef[2,5]
    }
  }
  return(results)
}

.calculateDaysSinceInclusion <- function (data,
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