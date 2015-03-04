#' @title Tables summarizing data
#' 
#' @description Outputs a summary of the data.
#' 
#' @param data The data.
#' @param personIDVar Which variable identifies the person.
#' @param measurementVar Which variable identifies the evaluation occasion.
#' @param selectedSymptoms Which outcome variables were selected.
#' @param groupingVar Which variable is used for grouping.
#' 
#' @export
summarizeData <- function(data,
                          personIDVar,
                          measurementVar,
                          selectedSymptoms,
                          groupingVar) {
  
  #how many different subjects are included
  num.obs=nrow(data)
  cat(paste("Number of observations included in the data set: ", num.obs, "\n"))
  
  #how many different subjects are included
  num.samples.unique=length(unique(data[,personIDVar]))
  cat(paste("Number of unique subjects: ", num.samples.unique, "\n"))
  
  my.summary=summary(data)
  
  #how many measurement occasions
  num.measurement.occasions=length(unique(data[,measurementVar]))
  cat(paste("Number of evaluation occasions: ", num.measurement.occasions, "\n"))
  
  #how many subjects were measured at each measurement occasion, treated as a data frame for better display
  num.measurement.per.occasion= as.data.frame(table(data[,measurementVar]))
  dimnames(num.measurement.per.occasion)[[2]]=c("Evaluation occasion",
                                                "Number of subjects")
  
  cat("\nNumber of observations per evaluation occasion \n")
  
  #print(as.table(num.measurement.per.occasion))
  print(num.measurement.per.occasion)
  
  #how many times was a single subject measured
  num.measurement.per.patient=as.data.frame(table(table(data[,personIDVar]))  )
  dimnames(num.measurement.per.patient)[[2]]=c("Number of evaluation",
                                               "Number of subjects")
  
  cat("\nNumber of evaluations per subject\n")
  print(num.measurement.per.patient)
  
  cat("\nNumber of subjects with missing values (by evaluation occasion, for the evaluated subjects) \n")
  #gets a table with the number of missing values for each variable, at each occasion
  num.missing.values.occ=addmargins((t(apply(data[,selectedSymptoms], 2,
            function(symptom) {tapply(symptom, INDEX=data[,measurementVar], FUN=function(x) sum(is.na(x)))}))))
  
  print(num.missing.values.occ)
  
  ######### summary for the grouping variable
  summary.grouping.variable=table(data[,groupingVar])
  cat("\nSummary of grouping variable data (all observations) \n")
  print(summary.grouping.variable)
  
  #levels of the grouping variable
  levels.grouping=levels(as.factor(data[,groupingVar]))
  #number of levels
  num.levels.grouping=length(levels.grouping)
  
  summary.grouping.variable.occ=tapply(data[,groupingVar], INDEX=data[,measurementVar], FUN=function(x) table(x))
  summary.grouping.variable.occ2=tapply(data[,groupingVar], INDEX=data[,measurementVar], FUN=function(x) 
  {my.res=numeric(num.levels.grouping)
  for(i in 1:num.levels.grouping) my.res[i]=sum(x==levels.grouping[i], na.rm=T)
  return(my.res)
  })
  
  summary.grouping.variable.occ2= matrix(unlist(summary.grouping.variable.occ2), ncol=num.levels.grouping, byrow=TRUE)
  dimnames(summary.grouping.variable.occ2)[[2]]=levels.grouping
  dimnames(summary.grouping.variable.occ2)[[1]]=sort(unique(data[,measurementVar]))
  
  cat("\nSummary of grouping variable data (per evaluation occasion) \n")
  print(addmargins(summary.grouping.variable.occ2))
  
}