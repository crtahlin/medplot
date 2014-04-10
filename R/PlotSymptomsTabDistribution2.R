#' @title Plot proportions with symptoms
#' @description Function that plots proportion of subjects that have a certain 
#' symptom present.
#' 
#' @param data Data to be passed to the function as a data frame. 
#' @param grouping The column name of the binary grouping variable used to define the groups on the plot.
#' @param measurements The column name of the variable containing measurement occasions. 
#' @param symptomsNames A vector of column names containing the symptom severity.
plotPropWithSymptoms <- function (data,
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
    plot(1, xlim=c(-1, 1), ylim=c(0, num.symptoms), axes=FALSE, xlab="", ylab="", type="n")
    
    my.order.symptoms=order(prop.with.symptoms[[1]][[1]], decreasing=FALSE)
    
    prop.with.symptoms.1=lapply(prop.with.symptoms, function(x) x[[1]])
    prop.with.symptoms.2=lapply(prop.with.symptoms, function(x) x[[2]])
    
    prop.with.symptoms.1=matrix(unlist(prop.with.symptoms.1), nrow=num.times, byrow=TRUE)
    prop.with.symptoms.2=matrix(unlist(prop.with.symptoms.2), nrow=num.times, byrow=TRUE)
    
    linch <-  max(strwidth(names.symptoms, "inch")+.4, na.rm = TRUE)
    par(mai=c(1.02, linch,0.82,0.42))
    
    tmp=barplot(prop.with.symptoms.1[num.times:1,my.order.symptoms],
                beside=TRUE, hor=TRUE, xlim=c(0,1),
                names.arg=names.symptoms[my.order.symptoms],
                las=1, xlab="Proportion of subjects",
                legend.text=c(paste0("T=", my.times[num.times:1])),
                plot=FALSE)
    
    plot(1, xlim=c(-1, 1), ylim=c(0, max(tmp)), axes=FALSE, xlab="", ylab="", type="n")
    
    abline(v=seq(-1, 1, by=.1), lty=2, col="light gray")
    
    tmp=barplot(prop.with.symptoms.1[num.times:1,my.order.symptoms],
                beside=TRUE, hor=TRUE, xlim=c(0,1),
                names.arg=names.symptoms[my.order.symptoms],
                las=1, xlab="Proportion of subjects", add=TRUE, 
                legend.text=c(paste0("T=", my.times[num.times:1])), 
                args.legend=list(x=par("usr")[2],
                                 y=par("usr")[3], yjust = 0 )
    ) 
    
    tmp=barplot(-prop.with.symptoms.2[num.times:1,my.order.symptoms],
                beside=TRUE, hor=TRUE,
                names.arg=names.symptoms[my.order.symptoms],
                las=1, xlab="Proportion of subjects", add=TRUE ) 
    
    text(x=0, par("usr")[4],
         labels=paste(which.var, "=" , my.levels[1]), xpd=T, adj=c(0))
    
    text(x=0, par("usr")[4],
         labels=paste(which.var, "="  ,my.levels[2]), xpd=T, adj=c(1))
  }# end if		
}


#' @title Table with proportions 
#' 
#' @description TODO
#' 
#' @param TODO
tablePropWithSymptoms <- function (data,
                                   groupingVar="Sex",
                                   measurementVar,
                                   forMeasurement,
                                   symptomsNames,
                                   thresholdValue=0) {
  
  
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
  column7Name <- paste("cor. P value (Holm-Bonferroni)")
  column8Name <- paste("Q-value (Benjamini-Yekutieli)")

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
      results$estimate[1]
    tableData[tableData["Variable"]==symptom, column2Name ] <- 
      paste(group1Positive, "/", group1Positive+group1Negative)
    tableData[tableData["Variable"]==symptom, column3Name ] <- 
      results$estimate[2]
    tableData[tableData["Variable"]==symptom, column4Name ] <- 
      paste(group2Positive, "/", group2Positive+group2Negative)
    tableData[tableData["Variable"]==symptom, column5Name ] <- 
      format(results$p.value, digits=2)
    tableData[tableData["Variable"]==symptom, column6Name ] <- 
      paste(format(results$conf.int[[1]], digits=2),
            format(results$conf.int[[2]], digits=2), sep=" to ")
  }
  
  # add a Holm-Bonferoni corrected P value column
  tableData[, column7Name] <- p.adjust(p=tableData[, column5Name], method="holm")
  # add a Benjamini-Yekutieli Q-value - see ?p.adjust
  tableData[, column8Name] <- p.adjust(p=tableData[, column5Name], method="BY")
  
  return(tableData)
}


#' @title Table with medians 
#' 
#' @description TODO Ignores threshold value.
#' 
#' @param TODO
tableMediansWithSymptoms <- function (data,
                                   groupingVar="Sex",
                                   measurementVar,
                                   forMeasurement,
                                   symptomsNames,
                                   thresholdValue=0) {
  
  # removing entries with missing data for groupingVar
  data <- data[!is.na(data[groupingVar]),]
  
  groupingLevels <- as.character(unlist(unique(data[groupingVar])))
  
  data <- data[data[measurementVar]==forMeasurement,]
  
  tableData <- data.frame("Variable"=symptomsNames)
  
  column1Name <- paste("Median of ", groupingLevels[1])
  column2Name <- paste("IQR for ", groupingLevels[1])
  column3Name <- paste("Median of ", groupingLevels[2])
  column4Name <- paste("IQR for ", groupingLevels[2])
  column5Name <- paste("P value")
  # column6Name <- paste("Conf. int. for diff. of prop. ")
  column7Name <- paste("cor. P value (Holm-Bonferroni)")
  column8Name <- paste("Q-value (Benjamini-Yekutieli)")
  
  group1Data <- data[data[groupingVar]==groupingLevels[1],]
  # group1Data[, symptomsNames] <- (group1Data[,symptomsNames]>thresholdValue)
  group2Data <- data[data[groupingVar]==groupingLevels[2],]
  # group2Data[, symptomsNames] <- (group2Data[,symptomsNames]>thresholdValue)
  
  for (symptom in symptomsNames) {
    group1Median <- median(group1Data[,symptom], na.rm=TRUE)
    group1IQR <- quantile(group1Data[,symptom], c(0.25, 0.75), na.rm=TRUE)
    group2Median <- median(group2Data[,symptom], na.rm=TRUE)
    group2IQR <- quantile(group2Data[,symptom], c(0.25, 0.75), na.rm=TRUE)
#     testMatrix <- matrix(c(group1Positive, group1Negative,
#                            group2Positive, group2Negative),
#                          byrow=TRUE, ncol=2)
#     results <- prop.test(testMatrix)
#     

result <- wilcox.test(x=group1Data[,symptom], y=group2Data[,symptom])

tableData[tableData["Variable"]==symptom, column1Name ] <- group1Median
tableData[tableData["Variable"]==symptom, column2Name ] <- paste(group1IQR[1], " to ", group1IQR[2])
tableData[tableData["Variable"]==symptom, column3Name ] <- group2Median
tableData[tableData["Variable"]==symptom, column4Name ] <- paste(group2IQR[1], " to ", group2IQR[2])
tableData[tableData["Variable"]==symptom, column5Name ] <- format(result$p.value, digits=2)
# tableData[tableData["Variable"]==symptom, column6Name ] <- 
#   paste(format(results$conf.int[[1]], digits=2),
#         format(results$conf.int[[2]], digits=2), sep=";")
  }
  
# add a Holm-Bonferoni corrected P value column
tableData[, column7Name] <- p.adjust(p=tableData[, column5Name], method="holm")
# add a Benjamini-Yekutieli Q-value - see ?p.adjust
tableData[, column8Name] <- p.adjust(p=tableData[, column5Name], method="BY")

  return(tableData)
}

