#' @title Plot profiles for variables.
#' 
#' @description TODO
#' 
#' @param TODO
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
      geom_point() + geom_line() +  facet_grid(variable + MemberOfGroup ~ .) + theme_bw() +
      # add summary statistics at each point
      stat_summary(aes(group=1), geom="point", fun.y=median, shape=16, size=5, colour="red") 
    return(p)
    
  }
  
  # create the plot object
  p <- ggplot(data=dataMelted, aes(x=Measurement, y=value, group=PersonID, colour=PersonID)) +
    # draw points, draw lines, facet by symptom, use black & white theme
    geom_point() + geom_line() +  facet_grid(variable~.) + theme_bw() +
    # add summary statistics at each point
    stat_summary(aes(group=1), geom="point", fun.y=median, shape=16, size=5, colour="red") 
  return(p)

}


#' @title Plot timeline boxplots
#' 
#' @description TODO
#' 
#' @param TODO
#' 
plotTimelineBoxplots <- function(data,
                                 personIDVar,
                                 measurementVar,
                                 selectedSymptoms) {
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
p <- ggplot(data=dataMelted, aes(x=Measurement, y=value)) +
  # draw points, jitter points, draw boxplots, facet by variable, use black & white theme
  geom_point() + 
  geom_jitter() +
  geom_boxplot(width=0.5) +
  facet_grid(variable ~.) +
  theme_bw() 

# return ggplot
return(p)
}