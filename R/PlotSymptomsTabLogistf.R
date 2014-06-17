# #' @title OBSOLETE Plot logistf
# #' 
# #' @description TODO write description
# #' 
# #' @param TODO TODO write instructions
# plotLogistf <- function (data,
#                          data.yn,
#                          measurement,
#                          measurementSelectedlogistf,
#                          logistfIDVar,
#                          selectedSymptoms,
#                          numSymptoms) {
#   
#   
#   ########### plot of the results
#   my.data.symptoms.yn=data.yn[measurement==measurementSelectedlogistf,]
#   
#   #which variable is used in the model
#   my.var=data[measurement==measurementSelectedlogistf, logistfIDVar]
#   
#   #fix this problem: if a variable is selected and it has just one value - like in our example Response at t=0, the program freezes
#   if(length(unique(my.var))==1) return()
#   
#   my.mod.firth=vector("list", numSymptoms)
#   
#   #estimate the logistics model with Firth correction for each symptom
#   for(i in 1:numSymptoms){
#     my.mod.firth[[i]]=logistf(my.data.symptoms.yn[,i]~ my.var, family="binomial")
#   }
#   
#   linch <-  max(strwidth(selectedSymptoms, "inch")+0.4, na.rm = TRUE)
#   par(mai=c(1.02,linch,0.82,0.42))
#   
#   #number of levels of the variable
#   num.levels=nrow(my.mod.firth[[1]]$coef)-1
#   
#   par(mfrow=c(max(num.levels,1), 1))
#   
#   for(i in 1:max(num.levels, 1)){
#     OR.b.0=matrix(unlist(lapply(my.mod.firth,
#                                 function(x) exp(cbind(x$coef, x$ci.lower, x$ci.upper)[i+1,]))),
#                   ncol=3, byrow=3)
#     dimnames(OR.b.0)[[1]]=selectedSymptoms
#     
#     plot(1:numSymptoms,
#          ylim=c(0, numSymptoms),
#          xlim=c(max(min(OR.b.0)-0.1, 0), max(OR.b.0)),
#          type="n",
#          axes=FALSE,
#          xlab="OR",
#          ylab="")
#     
#     segments(OR.b.0[,2], c(1:(numSymptoms)), OR.b.0[,3],  c(1:(numSymptoms)))
#     points(OR.b.0[,1],c(1:(numSymptoms)))
#     axis(2, at=c(1:(numSymptoms))+.17, labels=selectedSymptoms, las=2)
#     axis(1)
#     abline(v=1, lwd=2)
#     
#     #string that expresses the levels of the categorical variables being compared
#     
#     my.level.string=ifelse(is.numeric(my.var), "", paste0(levels(my.var)[i+1], " versus ", levels(my.var)[1], " (reference)"))
#     
#     title(paste0("T = ", measurementSelectedlogistf, ";\n  Odds ratios and 95% confidence intervals\n", my.level.string))
#     ##### fix the title, to express what the OR represents - in case of categorical variables or more than 1 level
#   }
# }

#' @title Logistf data in tabular format
#' 
#' @description TODO
#' 
#' @param TODO
tabelizeLogistf <- function (data,
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
  
  table <- data.frame("Variable"=selectedSymptoms) # table of printable results
  table2 <- data.frame("Variable"=selectedSymptoms) # table of raw results
  
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
  }
  return(list(printableResultsTable=table, rawResultsTable=table2, referenceValue=oddsFor))
}