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
plotSymptomsTimeline <- function (data,
                                  date,
                                  personID,
                                  measurement,
                                  symptoms, 
                                  displaySinceInclusion = FALSE) {
  
  
  
  # add info about days since inclusion in the study in a column if needed for ploting
  if (displaySinceInclusion == TRUE) {
    # find the day of inclusion in the study for each person
    uniquePeople <- as.data.frame(unique(data[personID]))
    colnames(uniquePeople) <- personID
    
    for (person in uniquePeople[,1]) {
      subset <- data[which(data[personID]==person), date]
      uniquePeople[which(uniquePeople[personID]==person), "minDate"] <- as.character(min(subset))
      data[which(data[personID]==person), "minDate"] <- as.character(min(subset))
    }
    
    data$minDate <- as.Date(data$minDate, format="%Y-%m-%d")
    data$daysSinceInclusion <- as.numeric(data$Date - data$minDate) # save as numeric for melt()to work
  }
    
  # keep only relevant data and melt() it into ggplot format
  if (displaySinceInclusion == TRUE) { # in case of ploting days since inclusion on horizontal axis
    
    data <- data[ , c("daysSinceInclusion", personID, measurement, symptoms)]
    data <- melt(data=data, id.vars = c(personID, "daysSinceInclusion", measurement))
    horizontalAxisVariable <- "daysSinceInclusion" 
    colnames(data)[which(colnames(data)=="daysSinceInclusion")] <- "horizontalAxisVariable"
    
  } else { # in case of ploting with dates on horzontal axis
    data <- data[ , c(date, personID, measurement, symptoms)]
    data <- melt(data=data, id.vars = c(personID, date, measurement))
    
    horizontalAxisVariable <- "Date"
    colnames(data)[which(colnames(data)==date)] <- "horizontalAxisVariable"
  }
  
  # rename column names to make sure ggplot recognizes them and that the code below works
  colnames(data)[which(colnames(data)==personID)] <- "PersonID"
  colnames(data)[which(colnames(data)==measurement)] <- "Measurement"
  
  plot <-  ggplot(data, aes(x = horizontalAxisVariable, y = PersonID, size = value, colour = variable)) +
    geom_point(shape = 1) + theme_bw() + 
    scale_size_area(breaks=c(1:10),minor_breaks=c(1:10),
                    guide="legend",
                    limits=c(1,10),
                    max_size=10) +
    theme(axis.text.x = element_text(angle=90))
  
  # if plotting dates
  if (displaySinceInclusion==FALSE) {
    plot <- plot + scale_x_date(breaks = date_breaks("1 week"),
                                labels = date_format("%d %b"),
                                minor_breaks = date_breaks("1 day")) +
      xlab("Date")
  }
  
  # if ploting days since inclusion
  if (displaySinceInclusion==TRUE) { 
    plot <- plot + xlab("Days since inclusion") }
  
  return(plot)
}