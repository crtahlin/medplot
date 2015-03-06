#' @title Plot profiles for variables.
#' 
#' @description Plots different kind of profile (spaghetti) plots
#' for the outcome values over time.
#' 
#' @param data The data.
#' @param plotType What type of plot (oneGraph, randomSample, multipleGraphs).
#' @param personIDVar Which variable identifies the person.
#' @param measurementVar Which var identifies the evaluation occasion.
#' @param selectedSymptoms Which outcomes were selected.
#' @param sizeofRandomSample Size of random sample (in case of oneGraph variant).
#' @param sizeofGroup Size of group (in case of multipleGraphs).
#' 
#' @export
plotTimelineProfiles <- function (data,
                                  plotType="oneGraph",
                                  personIDVar,
                                  measurementVar,
                                  selectedSymptoms,
                                  sizeofRandomSample=10,
                                  sizeofGroup=25) {
  
  
  # prepare data for different type of plots
  # one plot per variable - all data
  if (plotType=="oneGraph") {
    # prepare data
    dataMelted <- melt(data=data,
                       id.vars=c(personIDVar, measurementVar),
                       measure.vars=selectedSymptoms )
    
    # rename column names to make sure ggplot recognizes them
    colnames(dataMelted)[which(colnames(dataMelted)==personIDVar)] <- "PersonID"
    colnames(dataMelted)[which(colnames(dataMelted)==measurementVar)] <- "Measurement"
    
    # set some variables as factors
    dataMelted[,"PersonID"] <- as.factor(dataMelted[,"PersonID"])
    dataMelted[,"Measurement"] <- as.factor(dataMelted[,"Measurement"])
    
  }
  
  # one plot per variable - random subsample of patients
  if (plotType=="randomSample") {
    # draw a random sample of people
    peopleInSample <- sample(unique(data[, personIDVar]), sizeofRandomSample)
    dataRandomSample <- data[data[, personIDVar] %in% peopleInSample, ]
    
    # prepare data
    dataMelted <- melt(data=dataRandomSample,
                       id.vars=c(personIDVar, measurementVar),
                       measure.vars=selectedSymptoms )
    
    # rename column names to make sure ggplot recognizes them
    colnames(dataMelted)[which(colnames(dataMelted)==personIDVar)] <- "PersonID"
    colnames(dataMelted)[which(colnames(dataMelted)==measurementVar)] <- "Measurement"
    
    # set some variables as factors
    dataMelted[,"PersonID"] <- as.factor(dataMelted[,"PersonID"])
    dataMelted[,"Measurement"] <- as.factor(dataMelted[,"Measurement"])
    
  }
  
  # more plots per variable
  if (plotType=="multipleGraphs") {
    # add info about group membership to the data
    uniqueSubjects <- unique(data[personIDVar])
    numSubjects <- dim(uniqueSubjects)[1]
    numGroups <- ceiling(numSubjects/sizeofGroup)
    
    for (i in 1:numSubjects) {
      uniqueSubjects[i,"MemberOfGroup"] <- i%%numGroups
    }
    # join info about group membership to the data
    data <- join(x=data, y=uniqueSubjects, by=personIDVar, type="inner")
    
    # prepare data
    dataMelted <- melt(data=data,
                       id.vars=c(personIDVar, measurementVar, "MemberOfGroup" ),
                       measure.vars=selectedSymptoms )
    
    # rename column names to make sure ggplot recognizes them
    colnames(dataMelted)[which(colnames(dataMelted)==personIDVar)] <- "PersonID"
    colnames(dataMelted)[which(colnames(dataMelted)==measurementVar)] <- "Measurement"
    
    # set some variables as factors
    dataMelted[,"PersonID"] <- as.factor(dataMelted[,"PersonID"])
    dataMelted[,"Measurement"] <- as.factor(dataMelted[,"Measurement"])
    
    # create the plot object
    p <- ggplot(data=dataMelted, aes(x=Measurement, y=value, group=PersonID, colour=PersonID)) +
      # draw points, draw lines, facet by symptom, use black & white theme
      geom_point() + geom_line() +  facet_grid(variable + MemberOfGroup ~ .) + myTheme() +
      # add summary statistics at each point
      stat_summary(aes(group=1), geom="point", fun.y=median, shape=16, size=5, colour="red") 
    return(p)
    
  }
  
  # create the plot object
  p <- ggplot(data=dataMelted, aes(x=Measurement, y=value, group=PersonID, colour=PersonID)) +
    # draw points, draw lines, facet by symptom, use black & white theme
    geom_point() + geom_line() +  facet_grid(variable~.) + myTheme() +
    # add summary statistics at each point
    stat_summary(aes(group=1), geom="point", fun.y=median, shape=16, size=5, colour="red") 
  return(p)
  
}


#' @title Plot timeline boxplots
#' 
#' @description Plots boxplots for each evaluation occasion.
#' 
#' @param data The data.
#' @param personIDVar Which var identifies the person.
#' @param measurementVar Which var identifies the evaluation occasions.
#' @param selectedSymptoms Which outcome variables wre selected.
#' @param faceting Should faceting be used.
#' 
#' @export
plotTimelineBoxplots <- function(data,
                                 personIDVar,
                                 measurementVar,
                                 selectedSymptoms,
                                 faceting) {
  # prepare data
  dataMelted <- melt(data=data,
                     id.vars=c(personIDVar, measurementVar),
                     measure.vars=selectedSymptoms )
  
  # rename column names to make sure ggplot recognizes them
  colnames(dataMelted)[which(colnames(dataMelted)==personIDVar)] <- "PersonID"
  colnames(dataMelted)[which(colnames(dataMelted)==measurementVar)] <- "Measurement"
  
  # set some variables as factors
  dataMelted[,"PersonID"] <- as.factor(dataMelted[,"PersonID"])
  dataMelted[,"Measurement"] <- as.factor(dataMelted[,"Measurement"])
  
  # code to draw graph
  #   # define x, y axis, groups, coloring
  if (faceting=="variablesOnYaxis") {
    p <- ggplot(data=dataMelted, aes(x=Measurement, y=value)) +
      # draw points, jitter points, draw boxplots, facet by variable, use black & white theme
      geom_boxplot(width=0.5) +
      geom_jitter(alpha=I(1/5)) +
      facet_grid(variable ~.) +
      myTheme() +
      ylab("Value") + xlab("Evaluation occasion")
  }
  
  if (faceting=="variablesOnXaxis") {
    p <- ggplot(data=dataMelted, aes(x=variable, y=value)) +
      # draw points, jitter points, draw boxplots, facet by variable, use black & white theme
      geom_boxplot(width=0.5) +
      geom_jitter(alpha=I(1/5)) +   
      facet_grid(Measurement ~.) +
      myTheme() +
      theme(axis.text.x=element_text(angle=90, hjust=1)) +
      ylab("Value") + xlab("Variable")
  }
  
  # return ggplot
  return(p)
}

#' @title Boxplots data in form of a table
#' 
#' @description The data that is also plotted, presented in a table.
#' 
#' @param data The data.
#' @param measurements Vector of measurements?
#' @param measurementVar Which var identifies the evaluation occasions.
#' @param selectedSymptoms Which outcome variables were selected.
#' 
#' @export
tabelizeBoxplots <- function(measurements,
                             measurementVar,
                             data,
                             selectedSymptoms  ) {
  
  tables <- list()
  for (measurement in measurements) {
    table <- tableMedians(measurement=measurement,
                          measurementVar=measurementVar,
                          data=data,
                          selectedSymptoms=selectedSymptoms)
    tables[[as.character(measurement)]] <- 
      print(xtable(table, caption=paste("Evaluation occasion:", measurement)),
            type="html",
            html.table.attributes='class="data table table-bordered table-condensed"',
            caption.placement="top")
  }
  return(lapply(tables, paste))
}

#' @title Helper function for tabelizeBoxplots
#' 
#' @param data The data.
#' @param measurement Selected evaluation occasion.
#' @param measurementVar Which var identifies the evaluation occasions.
#' @param selectedSymptoms Which outcome variables were selected.
#' 
#' @export
tableMedians <- function(measurement,
                         measurementVar,
                         data,
                         selectedSymptoms) {
  
  result <- data.frame("Variables"=selectedSymptoms, "Median"=NA)
  result2 <- data.frame("Variables"=selectedSymptoms)
  data <- data[data[,measurementVar]==measurement, ]
  
  for (symptom in selectedSymptoms) {
    result[result[,"Variables"]==symptom,"Median"] <-
      median(na.omit(data[ ,symptom]))
    result2[result2[,"Variables"]==symptom,"Median"] <-
      median(na.omit(data[ ,symptom]))
    
    calculateMedian <- function(data, indices) {median(data[indices], na.rm=TRUE)}
    temp <- boot(data=data[,symptom], R=2000, statistic=calculateMedian)
    res <- boot.ci(temp, type="perc", conf=c(0.95))
    
    result[result[,"Variables"]==symptom, "95% CI (bootstrap)"] <-
      paste(format(res$percent[4], digits=2), "to", format(res$percent[5], digits=2) )
    result2[result2[,"Variables"]==symptom,"CILower"] <- res$percent[4]
    result2[result2[,"Variables"]==symptom,"CIUpper"] <- res$percent[5]
    
    result[result[,"Variables"]==symptom,"IQR"] <-
      IQR(na.omit(data[ ,symptom]))
    
    result[result[,"Variables"]==symptom,"25th perc."] <-
      quantile(na.omit(data[ ,symptom]), 0.25)
    
    result[result[,"Variables"]==symptom,"75th perc."] <-
      quantile(na.omit(data[ ,symptom]), 0.75)
    
    result[result[,"Variables"]==symptom, "# NAs"] <-
      sum(is.na(data[,symptom]))
  }
  return(list(printableTable=result, rawTable=result2))  
}

#' @title Construct a table of proportions
#' 
#' @param data The data.
#' @param measurement Selected evaluation occasion.
#' @param measurementVar Which var identifies the evaluation occasions.
#' @param selectedSymptoms Which outcome variables were selected.
#' 
#' @export
tableProportions <- function(measurement,
                             measurementVar,
                             data,
                             selectedSymptoms) {
  
  result <- data.frame("Variables"=selectedSymptoms, "Positive"=NA)
  data <- data[data[,measurementVar]==measurement, ]
  
  for (symptom in selectedSymptoms) {
    positive <- sum(na.omit(data[ ,symptom]))
    all <- length(data[ ,symptom])
    
    res <- binom.test(x=positive, n=all, conf.level=0.95)
    
    result[result[,"Variables"]==symptom,"Positive"] <- positive
    
    result[result[,"Variables"]==symptom,"All"] <- all      
    
    result[result[,"Variables"]==symptom,"Proportion"] <- format(res$estimate, digits=2)
    
    result[result[,"Variables"]==symptom, "95% CI for proportion"] <-
      paste(format(res$conf.int[1], digits=2), "to", format(res$conf.int[2], digits=2) )
    
    result[result[,"Variables"]==symptom, "# NAs"] <-
      sum(is.na(data[,symptom]))
    
  }
  return(result)  
}