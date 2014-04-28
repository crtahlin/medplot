#' @title Plot logistf
#' 
#' @description TODO write description
#' 
#' @param TODO TODO write instructions
plotLogistf <- function (data,
                         data.yn,
                         measurement,
                         measurementSelectedlogistf,
                         logistfIDVar,
                         selectedSymptoms,
                         numSymptoms) {
  
  
  ########### plot of the results
  
  
  my.data.symptoms.yn=data.yn[measurement==measurementSelectedlogistf,]
  
  #which variable is used in the model
  my.var=data[measurement==measurementSelectedlogistf, logistfIDVar]
 
  #fix this problem: if a variable is selected and it has just one value - like in our example Response at t=0, the program freezes
  if(length(unique(my.var))==1) return()
  
  my.mod.firth=vector("list", numSymptoms)
  
  #estimate the logistics model with Firth correction for each symptom
  for(i in 1:numSymptoms){
    my.mod.firth[[i]]=logistf(my.data.symptoms.yn[,i]~ my.var, family="binomial")
  }
 
  linch <-  max(strwidth(selectedSymptoms, "inch")+0.4, na.rm = TRUE)
  par(mai=c(1.02,linch,0.82,0.42))
  
  #number of levels of the variable
  num.levels=nrow(my.mod.firth[[1]]$coef)-1
  
  par(mfrow=c(max(num.levels,1), 1))
  
  for(i in 1:max(num.levels, 1)){
    
    OR.b.0=matrix(unlist(lapply(my.mod.firth,
                                function(x) exp(cbind(x$coef, x$ci.lower, x$ci.upper)[i+1,]))),
                  ncol=3, byrow=3)
    dimnames(OR.b.0)[[1]]=selectedSymptoms
    
    plot(1:numSymptoms,
         ylim=c(0, numSymptoms),
         xlim=c(max(min(OR.b.0)-0.1, 0), max(OR.b.0)),
         type="n",
         axes=FALSE,
         xlab="OR",
         ylab="")
    
    segments(OR.b.0[,2], c(1:(numSymptoms)), OR.b.0[,3],  c(1:(numSymptoms)))
    points(OR.b.0[,1],c(1:(numSymptoms)))
    axis(2, at=c(1:(numSymptoms))+.17, labels=selectedSymptoms, las=2)
    axis(1)
    abline(v=1, lwd=2)
    
    #string that expresses the levels of the categorical variables being compared
    
    my.level.string=ifelse(is.numeric(my.var), "", paste0(levels(my.var)[i+1], " versus ", levels(my.var)[1], " (reference)"))
    
    title(paste0("T = ", measurementSelectedlogistf, ";\n  Odds ratios and 95% confidence intervals\n", my.level.string))
    ##### fix the title, to express what the OR represents - in case of categorical variables or more than 1 level
  }
}

#' @title Logistf data in tabular format
#' 
#' @description TODO
#' 
#' @param TODO
tabelizeLogistf <- function (data,
                             data.yn,
                             measurement,
                             measurementSelectedlogistf,
                             logistfIDVar,
                             selectedSymptoms) {


  dataSubset <- data.yn[measurement==measurementSelectedlogistf,]
  variable <- data[measurement==measurementSelectedlogistf, logistfIDVar]
  
  table <- data.frame("Variable"=selectedSymptoms)
  for (symptom in selectedSymptoms) {
  model <- logistf(dataSubset[,symptom] ~ variable, family="binomial")
  table[table["Variable"]==symptom, "Odds ratio"] <- 
    exp(model$coef[2])
  table[table["Variable"]==symptom, "95% conf. interval"] <- 
    paste(format(exp(model$ci.lower[2]), nsmall=2, digits=2),
          " to ",
          format(exp(model$ci.upper[2]), nsmall=2, digits=2))
  }
  return(table)
}