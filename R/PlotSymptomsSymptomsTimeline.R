#' @title Plot timeline of symptom presence
#' 
#' @description Function that plots symptom severity presence for patients at a certain time.
#' 
#' @param data The data frame used by the plot. Data must be in appropriate format
#' for ggplot() (see melt()). Returns a ggplot object that has to be plotted via print().
plotSymptomsTimeline <- function (data) {
  plot <-  ggplot(data, aes(x = Date, y = PersonID, size = value, colour = variable)) +
    geom_point(shape = 1) + theme_bw() + 
    scale_size_area(breaks=c(1:10),minor_breaks=c(1:10),
                    guide="legend",
                    limits=c(1,10),
                    max_size=10) +
    scale_x_date(breaks = date_breaks("1 week"),
                 labels = date_format("%d %b"),
                 minor_breaks = date_breaks("1 day"))
  return(plot)
}

# 
# function() {
#   # if no symbols are selected, do not plot
#   if (dim(data())[1]>0) {
#     print(
#       ggplot(data(), aes(x = Date, y = PersonID, size = value, colour = variable)) +
#         geom_point(shape = 1) + theme_bw() +
#         # old scale - scale according to diameter
#         #scale_size(range= c(1,20)) +
#         # scale according to area
#         scale_size_area(breaks=c(1:10),minor_breaks=c(1:10),
#                         guide="legend",
#                         limits=c(1,10),
#                         max_size=10) +
#         scale_x_date(breaks = date_breaks("1 week"),
#                      labels = date_format("%d %b"),
#                      minor_breaks = date_breaks("1 day"))
#       
#     )}
# })
