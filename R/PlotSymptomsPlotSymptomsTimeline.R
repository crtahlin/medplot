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
plotSymptomsTimeline <- function (data, date, personID, measurement, symptoms) {
  # keep only relevant data and melt() it into ggplot format
  data <- data[ , c(date, personID, measurement, symptoms)]
  # TODO: the reference to x = Date, y = PersonID below is hardcoded at the moment,
  # make it dynamic - probably by adding another aes(), because this one looks into "data"
  plot <-  ggplot(data, aes(x = Date, y = PersonID, size = value, colour = variable)) +
    geom_point(shape = 1) + theme_bw() + 
    scale_size_area(breaks=c(1:10),minor_breaks=c(1:10),
                    guide="legend",
                    limits=c(1,10),
                    max_size=10) +
    scale_x_date(breaks = date_breaks("1 week"),
                 labels = date_format("%d %b"),
                 minor_breaks = date_breaks("1 day")) +
    theme(axis.text.x = element_text(angle=90))
  return(plot)
}