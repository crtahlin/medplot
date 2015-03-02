#' @title Plot timeline of symptom presence
#' 
#' @description Function that plots symptom severity presence for patients at a certain time.
#' 
#' @param data The data frame used by the plot. 
#' @param date Name of variable containing dates.
#' @param personID Name of variable containing person ID.
#' @param measurement Name of variable containing measuring occasion info.
#' @param symptoms Vector of variable names representing measured symptoms. 
#' 
#' for ggplot() (see melt()). Returns a ggplot object that has to be plotted via print().
plotTimeline <- function (data,
                                  date,
                                  personID,
                                  measurement,
                                  symptoms, 
                                  displayFormat = "dates",
                                  treatasBinary=FALSE) {
  
  
  # Scenario - timeFromInclusion
  # add info about days since inclusion in the study in a column if needed for ploting
  # TODO: I will rewrite this as .calculateTimeSinceInclusion(); use this function here instead
  if (displayFormat == "timeFromInclusion") {
    # find the day of inclusion in the study for each person
    uniquePeople <- as.data.frame(unique(data[personID]))
    colnames(uniquePeople) <- personID
    
    for (person in uniquePeople[,1]) {
      subset <- data[which(data[personID]==person), date]
      uniquePeople[which(uniquePeople[personID]==person), "minDate"] <- as.character(min(subset))
      data[which(data[personID]==person), "minDate"] <- as.character(min(subset))
    }
    data$minDate <- as.Date(data$minDate, format="%Y-%m-%d")
    data$daysSinceInclusion <- as.numeric(data[,date] - data$minDate) # save as numeric for melt()to work
  }
    
  # keep only relevant data and melt() it into ggplot format
  if (displayFormat == "timeFromInclusion") { # in case of ploting days since inclusion on horizontal axis
    
    data <- data[ , c("daysSinceInclusion", personID, measurement, symptoms)]
    data <- melt(data=data, id.vars = c(personID, "daysSinceInclusion", measurement))
    #horizontalAxisVariable <- "daysSinceInclusion" 
    colnames(data)[which(colnames(data)=="daysSinceInclusion")] <- "horizontalAxisVariable"
    
  } 
  
  # Scenario - dates
  # in case dates are used on the horizontal axis
  if (displayFormat == "dates") { # in case of ploting with dates on horzontal axis
    data <- data[ , c(date, personID, measurement, symptoms)]
    data <- melt(data=data, id.vars = c(personID, date, measurement))
    
    #horizontalAxisVariable <- "Date"
    colnames(data)[which(colnames(data)==date)] <- "horizontalAxisVariable"
  }
  
  # Scenario - measurement occasions
  if (displayFormat == "measurementOccasions") {
    
    data <- data[ , c(date, personID, measurement, symptoms)]
    data[ ,measurement] <- as.factor(data[ ,measurement])
    
    # calculate how many subject were measured on a occasion
    peopleMeasured <- data.frame(horizontalAxisVariable=unique(data[,measurement]), Number=NA)
    for (i in (peopleMeasured[,"horizontalAxisVariable"])) { # go through all measurement occasions
      dataForMeasurement <- data[data[measurement]==i,]
      peopleMeasured[peopleMeasured["horizontalAxisVariable"]==i,"Number"] <-
        (length(unique(dataForMeasurement[,personID])))
    }
    # Y coord for annotations above graph
    yCoord <- 0.65
    
    # melt the data
    data <- melt(data=data, id.vars = c(personID, date, measurement))
    #horizontalAxisVariable <- "Measurement"
    colnames(data)[which(colnames(data)==measurement)] <- "horizontalAxisVariable"
    
  }
  
  # rename column names to make sure ggplot recognizes them and that the code below works
  colnames(data)[which(colnames(data)==personID)] <- "PersonID"
  #colnames(data)[which(colnames(data)==measurement)] <- "Measurement"
 
  # Ploting function ####
  if (treatasBinary==FALSE) {
    plot <-  ggplot(data, aes(x = horizontalAxisVariable, y = PersonID, size = value, colour = variable)) +
    geom_point(shape = 1) + myTheme() + 
    geom_point(subset=.(value == 0), shape=2, size=1) + 
      scale_size_area(breaks=c(1:10),minor_breaks=c(1:10),
                      guide="legend",
                      limits=c(1,10),
                      max_size=10)}
  
  if (treatasBinary==TRUE) {
    data[,"value"] <- ifelse(data[,"value"]==1, "PRESENT", "ABSENT")
    plot <- ggplot(data, aes(x = horizontalAxisVariable, y = PersonID, size = value,
                             colour = variable)) +
      geom_point(shape=1) + myTheme() + scale_size_manual(values=c(1,5))
      
  }
  
  # if plotting dates on horizontal axis
  if (displayFormat == "dates") {
    plot <- plot + scale_x_date(breaks = date_breaks("1 week"),
                                labels = date_format("%d %b"),
                                minor_breaks = date_breaks("1 day")) +
      xlab("Date") +
      theme(axis.text.x = element_text(angle=90))
  }
  
  # if ploting days since inclusion on horizontal axis
  if (displayFormat == "timeFromInclusion") { 
    plot <- plot + xlab("Days since inclusion") 
  }
  
  # if ploting measurement occasion on horizontal axis
  if (displayFormat == "measurementOccasions") { 
    plot <- plot + xlab("Measurement occasions") 
    plot <- plot + annotate(geom="text", x=as.factor(peopleMeasured$horizontalAxisVariable),
                 y=yCoord,
                 label=paste("Measured:",peopleMeasured$Number))
  }
  
  return(plot)
}