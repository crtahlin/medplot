#' @title Plot proportions with symptoms
#' @description Function that plots proportion of subjects that have a certain 
#' symptom present.
#' 
#' @param data Data to be passed to the function as a data frame. 
#' @param grouping The column name of the binary grouping variable used to define the groups on the plot.
#' @param measurements The column name of the variable containing measurement occasions. 
#' @param symptomsNames A vector of column names containing the symptom severity.
#' 
#' @export
plotPropPositive <- function (data,
                              grouping="Sex",
                              measurements="Measurement",
                              symptomsNames) {
  # code will be used without changing variable names
  # mapping new names to old names
  my.data.expanded.nNOIS <- data
  which.var <- grouping
  which.symptoms <- symptomsNames # TODO: pass the symptom names as char vector
  names.symptoms <- symptomsNames
  ### EXISTING CODE ####
  
  ##################### which of the variables are symptoms
  
  #derive the matrix with the symptom intensity only
  my.data.symptoms=my.data.expanded.nNOIS[, which.symptoms, drop=FALSE]
  #derive the matrix with the symptom positivity/negativity only
  my.data.symptoms.yn=ifelse(my.data.expanded.nNOIS[,which.symptoms, drop=FALSE]>0, 1, 0)
  
  #how many symptoms
  num.symptoms=ncol(my.data.symptoms)
  
  
  ################ two bargraphs ######
  
  #select the variable based on which the graphs are drawn separately
  #index of the variable in the db
  
  my.var=my.data.expanded.nNOIS[,which.var]
  my.levels=sort(unique(my.var))
  num.levels=length(my.levels)
  
  #time for each record
  my.times.all=my.data.expanded.nNOIS[,measurements]
  #unique times
  my.times=sort(unique(my.data.expanded.nNOIS[,measurements]))
  num.times=length(my.times)
  
  prop.with.symptoms=lapply(1:num.times,  function(i) {
    tmp=vector("list", num.levels)
    for(j in 1:num.levels){ 
      tmp[[j]]=apply(my.data.symptoms.yn[which(my.times.all==my.times[i] &
                                                 my.var==my.levels[j]),,drop=FALSE],
                     2, function(x) mean(x==TRUE, na.rm=TRUE))
    }
    tmp  
  })
  
  if(num.levels==2) {
    linch <-  max(strwidth(names.symptoms, "inch")+.4, na.rm = TRUE)
    par(mai=c(1.02, linch,0.82,0.42))
    
    my.order.symptoms=order(prop.with.symptoms[[1]][[1]], decreasing=FALSE)
    
    prop.with.symptoms.1=lapply(prop.with.symptoms, function(x) x[[1]])
    prop.with.symptoms.2=lapply(prop.with.symptoms, function(x) x[[2]])
    
    prop.with.symptoms.1=matrix(unlist(prop.with.symptoms.1), nrow=num.times, byrow=TRUE)
    prop.with.symptoms.2=matrix(unlist(prop.with.symptoms.2), nrow=num.times, byrow=TRUE)
    
    linch <-  max(strwidth(names.symptoms, "inch")+.4, na.rm = TRUE)
    par(mai=c(1.02, linch,0.82,0.42))
    
    barplot(prop.with.symptoms.1[num.times:1,my.order.symptoms],
            beside=TRUE, hor=TRUE, xlim=c(-1,1),
            names.arg=names.symptoms[my.order.symptoms],
            las=1, xlab="Proportion of subjects", add=FALSE,
            legend.text=c(paste0("T=", my.times[num.times:1])), 
            args.legend=list(x=par("usr")[2],
                             y=par("usr")[3], yjust = 0 ),
            axes=FALSE) 
    
    barplot(-prop.with.symptoms.2[num.times:1,my.order.symptoms],
            beside=TRUE, hor=TRUE,
            names.arg=names.symptoms[my.order.symptoms],
            las=1, xlab="Proportion of subjects", add=TRUE,
            axes=FALSE) 
    
    abline(v=seq(-1, 1, by=.1), lty=2, col="light gray")
    
    axis(side=1, labels=c(1, 0.5, 0, 0.5, 1), at=c(-1, -0.5, 0, 0.5, 1))
    
    text(x=0, par("usr")[4],
         labels=paste(which.var, "=" , my.levels[1]), xpd=T, adj=c(0))
    
    text(x=0, par("usr")[4],
         labels=paste(which.var, "="  ,my.levels[2]), xpd=T, adj=c(1))
  }# end if		
}


#' @title Table with proportions 
#' 
#' @description Table of proportions of patients that have a certain outcome present.
#' 
#' @param data The data.
#' @param groupingVar Which is the grouping variable.
#' @param measurementVar Which is the measurement variable.
#' @param forMeasurement For which measuremet occasion?
#' @param symptomsNames For which outcome variables?
#' @param thresholdValue Dichotomizing threshold.
#' @param doPValueAdjustments Should P value adjustments be done?
#' 
#' @export
tablePropPosGroups <- function (data,
                                groupingVar="Sex",
                                measurementVar,
                                forMeasurement,
                                symptomsNames,
                                thresholdValue=0,
                                doPValueAdjustments) {
  
  
  # removing entries with missing data for groupingVar
  data <- data[!is.na(data[groupingVar]),]
  # TODO: create a Groupinga() reactive vector to pass to this and other functions?
  groupingLevels <- as.character(unlist(unique(data[groupingVar])))
  
  data <- data[data[measurementVar]==forMeasurement,]
  
  tableData <- data.frame("Variable"=symptomsNames)
  
  column1Name <- paste("Prop. of ", groupingLevels[1])
  column2Name <- paste("Positive/all for ", groupingLevels[1])
  column3Name <- paste("Prop. of ", groupingLevels[2])
  column4Name <- paste("Positive/all for ", groupingLevels[2])
  column5Name <- paste("P value")
  column6Name <- paste("95% CI for the difference")
  if (doPValueAdjustments==TRUE) {
    column7Name <- paste("Adj. P value (Holm-Bonferroni)")
    column9Name <- paste("Adj. P value (permutations)")
    column10Name <- paste("Q-value (Benjamini-Hochberg)")
    column8Name <- paste("Q-value (Benjamini-Yekutieli)")
  }
  
  group1Data <- data[data[groupingVar]==groupingLevels[1],]
  group1Data[, symptomsNames] <- (group1Data[,symptomsNames]>thresholdValue)
  group2Data <- data[data[groupingVar]==groupingLevels[2],]
  group2Data[, symptomsNames] <- (group2Data[,symptomsNames]>thresholdValue)
  
  for (symptom in symptomsNames) {
    group1Positive <- sum(group1Data[,symptom], na.rm=TRUE)
    group1Negative <- sum(!group1Data[,symptom], na.rm=TRUE)
    group2Positive <- sum(group2Data[,symptom], na.rm=TRUE)
    group2Negative <- sum(!group2Data[,symptom], na.rm=TRUE)
    
    testMatrix <- matrix(c(group1Positive, group1Negative,
                           group2Positive, group2Negative),
                         byrow=TRUE, ncol=2)
    results <- prop.test(testMatrix)
    
    tableData[tableData["Variable"]==symptom, column1Name ] <- 
      format(results$estimate[1], digits=2)
    tableData[tableData["Variable"]==symptom, column2Name ] <- 
      paste(group1Positive, "/", group1Positive+group1Negative)
    tableData[tableData["Variable"]==symptom, column3Name ] <- 
      format(results$estimate[2], digits=2)
    tableData[tableData["Variable"]==symptom, column4Name ] <- 
      paste(group2Positive, "/", group2Positive+group2Negative)
    tableData[tableData["Variable"]==symptom, column5Name ] <- 
      format(results$p.value, digits=2)
    tableData[tableData["Variable"]==symptom, column6Name ] <- 
      paste(format(results$conf.int[[1]], digits=2),
            format(results$conf.int[[2]], digits=2), sep=" to ")
    
  }
  
  if (doPValueAdjustments==TRUE) {
    
    # add a Holm-Bonferoni corrected P value column
    tableData[, column7Name] <- format(p.adjust(p=tableData[, column5Name], method="holm"), digits=2)
    # add a Benjamini-Hochberg Q-value
    tableData[, column10Name] <- format(p.adjust(p=tableData[, column5Name], method="BH"), digits=2)
    # add a Benjamini-Yekutieli Q-value - see ?p.adjust
    tableData[, column8Name] <- format(p.adjust(p=tableData[, column5Name], method="BY"), digits=2)
    
    # run a a multivariate permutation correcting P value
    permutationCorectedPValue <- permCorrPValue(data=data, 
                                                nPermutations=999,
                                                selectedSymptoms=symptomsNames,
                                                groupingVar=groupingVar,
                                                measurementVar=measurementVar,
                                                measurementOccasion=forMeasurement,
                                                FUN="propPValue", # function that returns a P value
                                                thresholdValue=0
    )
    
    for (symptom in symptomsNames) {
      # add a permutation calculated corrected P value
      tableData[tableData["Variable"]==symptom, column9Name ] <- 
        format(permutationCorectedPValue[symptom], digits=2)
    }
  }
  return(tableData)
}


#' @title Table with medians 
#' 
#' @description Table with median values for certain outcomes.
#' Ignores threshold value.
#' 
#' @param data The data.
#' @param groupingVar Which is the grouping variable.
#' @param measurementVar Which is the measurement variable.
#' @param forMeasurement For which measuremet occasion?
#' @param symptomsNames For which outcome variables?
#' @param thresholdValue Dichotomizing threshold.
#' @param doPValueAdjustments Should P value adjustments be done?
#' 
#' @export
tableMeGroups <- function (data,
                           groupingVar="Sex",
                           measurementVar,
                           forMeasurement,
                           symptomsNames,
                           thresholdValue=0,
                           doPValueAdjustments) {
  
  # removing entries with missing data for groupingVar
  data <- data[!is.na(data[groupingVar]),]
  
  # list grouping levels
  groupingLevels <- as.character(unlist(unique(data[groupingVar])))
  
  # only use data for a particular occasion
  data <- data[data[measurementVar]==forMeasurement,]
  
  # construct table with solutions
  tableData <- data.frame("Variable"=symptomsNames)
  
  # name columns of the table with solutions
  column1Name <- paste("Median of ", groupingLevels[1])
  column2Name <- paste("IQR for ", groupingLevels[1])
  column3Name <- paste("Median of ", groupingLevels[2])
  column4Name <- paste("IQR for ", groupingLevels[2])
  column5Name <- paste("P value")
  # column6Name <- paste("Conf. int. for diff. of prop. ")
  
  if (doPValueAdjustments==TRUE) {
    column7Name <- paste("Adj. P value (Holm-Bonferroni)")
    column9Name <- paste("Adj. P value (permutations)")
    column10Name <- paste("Q-value (Benjamini-Hochberg)")
    column8Name <- paste("Q-value (Benjamini-Yekutieli)")
    
  }
  
  group1Data <- data[data[groupingVar]==groupingLevels[1],]
  # group1Data[, symptomsNames] <- (group1Data[,symptomsNames]>thresholdValue)
  group2Data <- data[data[groupingVar]==groupingLevels[2],]
  # group2Data[, symptomsNames] <- (group2Data[,symptomsNames]>thresholdValue)
  
  for (symptom in symptomsNames) {
    group1Median <- median(group1Data[,symptom], na.rm=TRUE)
    group1IQR <- quantile(group1Data[,symptom], c(0.25, 0.75), na.rm=TRUE)
    group2Median <- median(group2Data[,symptom], na.rm=TRUE)
    group2IQR <- quantile(group2Data[,symptom], c(0.25, 0.75), na.rm=TRUE)
    
    result <- wilcox.test(x=group1Data[,symptom], y=group2Data[,symptom])
    
    tableData[tableData["Variable"]==symptom, column1Name ] <- format(group1Median, digits=2)
    tableData[tableData["Variable"]==symptom, column2Name ] <- paste(group1IQR[1], " to ", group1IQR[2])
    tableData[tableData["Variable"]==symptom, column3Name ] <- format(group2Median, digits=2)
    tableData[tableData["Variable"]==symptom, column4Name ] <- paste(group2IQR[1], " to ", group2IQR[2])
    tableData[tableData["Variable"]==symptom, column5Name ] <- format(result$p.value, digits=2, nsmall=2)
  }
  
  if (doPValueAdjustments==TRUE) {
    # add a Holm-Bonferoni corrected P value column
    tableData[, column7Name] <- format(p.adjust(p=tableData[, column5Name], method="holm"), digits=2, nsmall=2)
    # add a Benjamini-Hochberg Q-value - see ?p.adjust
    tableData[, column10Name] <- format(p.adjust(p=tableData[, column5Name], method="BH"), digits=2, nsmall=2)
    # add a Benjamini-Yekutieli Q-value - see ?p.adjust
    tableData[, column8Name] <- format(p.adjust(p=tableData[, column5Name], method="BY"), digits=2, nsmall=2)
    
    # run a a multivariate permutation correcting P value
    permutationCorectedPValue <- permCorrPValue(data=data, 
                                                nPermutations=999,
                                                selectedSymptoms=symptomsNames,
                                                groupingVar=groupingVar,
                                                measurementVar=measurementVar,
                                                measurementOccasion=forMeasurement,
                                                FUN="MannWhitneyPValue", # function that returns a P value
                                                thresholdValue=0
    )
    
    for (symptom in symptomsNames) {
      # add a permutation calculated corrected P value
      tableData[tableData["Variable"]==symptom, column9Name ] <- 
        format(permutationCorectedPValue[symptom], digits=2, nsmall=2)
    }
  }
  
  return(tableData)
}

#' @title Plot of confidence intervals
#' 
#' @description Plot of confidence intervals for proportions for groups and measurements.
#' 
#' @param data The data.
#' @param groupingVar The grouping variable.
#' @param measurementVar Which is the measurement variable.
#' @param selectedSymptoms For which outcomes.
plotPropPositiveCI <- function (data,
                                groupingVar,
                                measurementVar,
                                selectedSymptoms) {
  
  dataWithCIs <- returnPropCIs(data=data,
                               groupingVar=groupingVar,
                               measurementVar=measurementVar,
                               selectedSymptoms=selectedSymptoms)
  
  # make values of CIs negative for first group
  dataWithCIs[dataWithCIs["Group"]==as.character(unique(data[,groupingVar])[1]),
              c("Mean","UpperCI","LowerCI")] <- 
    - dataWithCIs[dataWithCIs["Group"]==as.character(unique(data[,groupingVar])[1])
                  , c("Mean","UpperCI","LowerCI")]
  
  plot <- ggplot() +
    geom_errorbarh(data=dataWithCIs, 
                   mapping=aes(y=Measurement, x=UpperCI, xmin=UpperCI, xmax=LowerCI,
                               colour=Group),
                   height=0.2, size=1) +
    geom_point(data=dataWithCIs, 
               mapping=aes(y=Measurement, x=Mean), size=4, shape=21, fill="white")  +
    facet_grid(Variable~.) + 
    scale_x_continuous(limits=c(-1,1),
                       breaks=seq(-1,1,by=0.2),
                       labels=abs(seq(-1,1,by=0.2)),
                       minor_breaks=((seq(-1,1, by=0.1)))) +
    geom_vline(xintercept=0)+
    myTheme() + labs(title="Proportions of positive values (with confidence intervals)",
                     x= "Proportions")
  
  return(plot)
}

#' @title Multivariate permutation test for correcting the P value
#' 
#' @description Multivariate P values correction.
#' 
#' @param data The data.
#' @param nPermutations Number of permutations.
#' @param selectedSymptoms Which outcomes were selected.
#' @param groupingVar The grouping variable.
#' @param measurementVar The evaluation occasion variable.
#' @param measurementOccasion For which evaluation occasion.
#' @param Which function to use for returning a P value.
#' @param thresholdValue Dichotomizing threshold.
#' 
#' @export
permCorrPValue <- function(data, 
                           nPermutations=999,
                           selectedSymptoms, # vector of selected variables - defines order
                           groupingVar,
                           measurementVar,
                           measurementOccasion,
                           FUN, # function that returns a P value,
                           thresholdValue=0 # threshold for positivity
) {
  
  data <- data[data[measurementVar]==measurementOccasion,]
  
  groupingLevels <- as.character(unique(data[,groupingVar]))
  
  # calculate the P value based on the sample
  calculatedPValue <- numeric()
  
  # if calculating proportions
  if (FUN=="propPValue") {
    for (symptom in selectedSymptoms) {
      calculatedPValue[symptom] <- propPValue(
        successG1=sum(na.omit(data[data[groupingVar]==groupingLevels[1],symptom]) >  thresholdValue),
        failureG1=sum(na.omit(data[data[groupingVar]==groupingLevels[1],symptom]) <= thresholdValue),
        successG2=sum(na.omit(data[data[groupingVar]==groupingLevels[2],symptom]) >  thresholdValue),
        failureG2=sum(na.omit(data[data[groupingVar]==groupingLevels[2],symptom]) <= thresholdValue)
      )
    }}  
  
  # if calculating difference of samples (Mann-Whitney)
  if (FUN=="MannWhitneyPValue") {
    for (symptom in selectedSymptoms) {
      calculatedPValue[symptom] <- MannWhitneyPValue(
        na.omit(data[data[groupingVar]==groupingLevels[1],symptom]),
        na.omit(data[data[groupingVar]==groupingLevels[2],symptom])
      )  
    }}
  
  globalMinPValues <- numeric()
  permutationPValues <- numeric()
  for (permutation in 1:nPermutations) {
    for (symptom in selectedSymptoms) {
      data[,groupingVar] <- sample(data[,groupingVar]) # shuffle group membership
      
      
      # if calculating proportions
      if (FUN=="propPValue") {
        permutationPValues[symptom] <- propPValue(
          sum(na.omit(data[data[groupingVar]==groupingLevels[1],symptom]) >  thresholdValue),
          sum(na.omit(data[data[groupingVar]==groupingLevels[1],symptom]) <= thresholdValue),
          sum(na.omit(data[data[groupingVar]==groupingLevels[2],symptom]) >  thresholdValue),
          sum(na.omit(data[data[groupingVar]==groupingLevels[2],symptom]) <= thresholdValue)
        )} 
      
      # if calculating difference of samples (Mann-Whitney)
      if (FUN=="MannWhitneyPValue") {
        permutationPValues[symptom] <- MannWhitneyPValue(
          na.omit(data[data[groupingVar]==groupingLevels[1],symptom]),
          na.omit(data[data[groupingVar]==groupingLevels[2],symptom])
        )
      }
      
    }
    globalMinPValues[permutation] <- min(permutationPValues)
  }
  correctedPvalues <- numeric()
  for (symptom in selectedSymptoms) {
    correctedPvalues[symptom] <- sum(calculatedPValue[symptom]>globalMinPValues)/(nPermutations+1) 
  }
  return(correctedPvalues)
}

### Helper functions ####
#' @title Helper function to return proportions with CIs
#' 
#' @param data The data.
#' @param groupingVar The grouping varible.
#' @param measurementVar The measurement variable.
#' @param selectedSymptoms The symptoms selected.
#' 
#' @export
returnPropCIs <- function(data,
                          groupingVar,
                          measurementVar,
                          selectedSymptoms) {
  
  # define levels for covariates
  group <- unique(na.omit(data[,groupingVar]))
  measurement <- unique(na.omit(data[,measurementVar]))
  
  results <- data.frame(expand.grid(Group=group,
                                    Measurement=measurement,
                                    Variable=selectedSymptoms), Mean=NA, LowerCI=NA, UpperCI=NA)
  for (i in group) {
    for(j in measurement) {
      for(k in selectedSymptoms) {
        # omit missing values, create boolean vector
        symptomData <- na.omit((data[(data[groupingVar]==i & data[measurementVar]==j), k])==1)
        
        testResults <- prop.test(x= sum(symptomData),  n= (sum(symptomData) + sum(!symptomData)))
        
        mean  <- testResults$estimate
        lower <- testResults$conf.int[1]
        upper <- testResults$conf.int[2]  
        
        results[(results["Group"]==i &
                   results["Measurement"]==j &
                   results["Variable"]==k), c("Mean","LowerCI","UpperCI")] <-
          c(mean, lower, upper)
      }
    }
  }
  # make Measurement, Variable and Group a factor
  results[, "Measurement"] <- as.factor(results[, "Measurement"])
  results[, "Group"] <- as.factor(results[, "Group"])
  results[, "Variable"] <- as.factor(results[, "Variable"])
  
  return(results) 
}

#' @title Helper function that returns a P value for proportions
#' 
#' @param successG1 Number of successes in group 1.
#' @param successG2 ... in group 2.
#' @param failureG1 Number of failures in group 1.
#' @param failureG2 ... in group 2.
#' 
#' @export
propPValue <- function (successG1, failureG1, successG2, failureG2) {
  result <- prop.test(matrix(data=c(successG1,
                                    failureG1,
                                    successG2,
                                    failureG2),
                             byrow=TRUE, nrow=2))
  return(result$p.value)
}

#' @title Helper function that returns a P values calculated by Mann Whitney
#' 
#' @param group1 Values for group 1.
#' @param group2 Values for group 2
MannWhitneyPValue <- function (group1, group2) {
  result <- wilcox.test(x=group1, y=group2)
  return(format(result$p.value, digits=2))
}