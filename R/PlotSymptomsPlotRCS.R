#' @title Plot RCS
#' 
#' @description Plot RCS.
#' 
#' @param data Data for ploting.
plotRCS <- function (data, measurementVar) {


}

# 
# output$plot.rcs=renderPlot({
#   print("rcs")
#   
#   num.symptoms=length(input$SymptomsIDVar)
#   
#   my.data.symptoms.yn=symptomsData.yn()[Measurement()==input$measurementSelectedrcs,]
#   
#   #temp: use age
#   my.var=data.all()[Measurement()==input$measurementSelectedrcs,Which.rcs.ID()]
#   
#   
#   par(mfrow=c(ceiling(num.symptoms/4), 4))
#   
# for(i in c(1:num.symptoms)){
#  
#   
#   
#   my.mod=glm(my.data.symptoms.yn[,i]~rcs(my.var), family="binomial", x=T, y=T)
#   
#   plot.rcs.mod(my.mod, my.mod$x[,2], my.ylab="Probability of reporting the symptom", my.xlab="Age", my.title=input$SymptomsIDVar[i])
#   my.p=ifelse(anova(my.mod, test="Chi")[2,5]<0.001, "P<0.001", paste("P=", round(anova(my.mod, test="Chi")[2,5],4)))
#   text(x=par("usr")[1]+10, y=par("usr")[4]-.1, labels=my.p, xpd=T)
#   
# }
# 
# }, height=1000)
