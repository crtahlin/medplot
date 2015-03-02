#' @title Plot RCS
#' 
#' @description Plot RCS.
#' 
#' @param data Data for ploting.
plotRCS <- function (data.all,
                     data.yn=NULL,
                     measurement,
                     selectedSymptoms,
                     measurementSelectedrcs,
                     rcsIDVar,
                     binaryVar=TRUE) {

  num.symptoms=length(selectedSymptoms)
  # only if binary data is passed to function
  if(binaryVar==TRUE & is.null(data.yn)) {return()}
  if(binaryVar) {my.data.symptoms.yn=data.yn[measurement==measurementSelectedrcs,]} 
  my.data.symptoms = data.all[measurement==measurementSelectedrcs, selectedSymptoms]
  
  #temp: use age
  my.var=data.all[measurement==measurementSelectedrcs,rcsIDVar] # should be which.rcsId ? or will it work like this?
  
  par(mfrow=c(ceiling(num.symptoms/3), 3))
  
  for(i in c(1:num.symptoms)){
    if (binaryVar==TRUE) {my.data <- my.data.symptoms.yn[,i] } else {
    my.data <- my.data.symptoms[,i]}
    
    my.mod=glm(my.data~rcs(my.var),
    				 family=ifelse(binaryVar, "binomial", "gaussian"), x=T, y=T)
    plotRCSmod(my.mod,
                 my.mod$x[,2],
                 my.ylab=ifelse(binaryVar, "Probability of positive variable", "Estimated value"),
                 my.xlab=rcsIDVar,
                 my.title=selectedSymptoms[i])
    my.p=ifelse(anova(my.mod, test="Chi")[2,5]<0.001,
                "P<0.001", 
                paste("P=", round(anova(my.mod, test="Chi")[2,5],4)))
    text(x=par("usr")[1]+10, y=par("usr")[4]-.1, labels=my.p, xpd=T)
  }
}

#' @title Plot RCS MOD ??? TODO : help contents
#' 
#' @param My.mod ???
plotRCSmod <- function(My.mod,
                         my.var,
                         my.ylab="Response",
                         my.xlab="Covariate",
                         my.xlim=NULL,
                         my.ylim=NULL,
                         my.knots=NULL,
                         my.title=NULL,
                         vlines=NULL,
                         hlines=NULL,
                         my.cex.lab=NULL,
                         my.axes=TRUE,
                         my.cex.main=NULL,
                         my.cex.axis=NULL ){
  
  my.intervals<-c(0.0001, 0.001, .01, .1, 1, 10, 100, 1000, 10000, 100000)
  my.range<- diff(range(my.var, na.rm=T))
  
  if(is.null(vlines)){
    my.by<-my.intervals[which(my.range/my.intervals<10)[1]]
    my.vlines=seq(-100000, 10000, by=my.by)
  } 
  
  my.pred<-predict(My.mod, type="response", se=T)
  
  my.range.y=diff(range(my.pred, na.rm=T))
  
  if(is.null(hlines)){
    my.by.h<-my.intervals[which(my.range/my.intervals<10)[1]]
    my.hlines=seq(-100000, 10000, by=my.by.h)
  } 
  
  matplot( rbind(my.var, my.var, my.var),
           rbind(my.pred$fit, my.pred$fit-1.96*my.pred$se, my.pred$fit+1.96*my.pred$se),
           pch=rep(1, length(my.pred$fit)*3),
           col=1, type="n", xlab=my.xlab, ylab=my.ylab, xlim=my.xlim, ylim=my.ylim,
           main=my.title, cex.lab=my.cex.lab,
           axes=my.axes, cex.main=my.cex.main, cex.axis=my.cex.axis)
  lines( my.var[order(my.var)], my.pred$fit[order(my.var)])
  lines( my.var[order(my.var)], (my.pred$fit+1.96*my.pred$se)[order(my.var)], lty=2)
  lines( my.var[order(my.var)], (my.pred$fit-1.96*my.pred$se)[order(my.var)],  lty=2)
  
  # abline(h=seq(0,1,by=.1), v= vlines  , lty=3, col="light grey")
  if(!is.null(hlines) | !is.null(vlines)) abline(h=hlines, v= vlines  , lty=3, col="light grey") else grid()
  if(!is.null(my.knots)) axis(1, at=my.knots, line=2, cex.axis=.65)
}

#' @title Restricted cubic spline P values in tabular form
#' 
#' @description TODO
#' 
#' @param TODO
tableRCS <- function(data.all,
                        data.yn,
                        measurement,
                        selectedSymptoms,
                        measurementSelectedrcs,
                        rcsIDVar, 
						binaryVar=TRUE) {
  
  if(binaryVar==TRUE) {
  data <- data.yn[measurement==measurementSelectedrcs,]
  } else {
  data <- data.all[measurement==measurementSelectedrcs, selectedSymptoms]
  }
  variable <- data.all[measurement==measurementSelectedrcs,rcsIDVar]  
  
  table <- data.frame("Variable"=selectedSymptoms)
  
  for (symptom in selectedSymptoms) {
    model <- glm(data[,symptom]~rcs(variable), family=ifelse(binaryVar, "binomial", "gaussian"), x=T, y=T)
    table[table[,"Variable"]==symptom, "P value"] <- 
      ifelse(anova(model, test="Chi")[2,5]<0.001,
             "P<0.001", 
             paste(format(round(anova(model, test="Chi")[2,5],4), digits=2)))
  }
  return(table)
}