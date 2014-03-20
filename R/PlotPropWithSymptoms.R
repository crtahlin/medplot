
# napiši funkcijo, ki bo narisala graf
# inputi
# - podatki, izbira katera spremenljivka je za grupiranje,

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
  print(which.symptoms)
  print(names.symptoms)
  ### EXISTING CODE ####
  
  ##################### which of the variables are symptoms
  # which.symptoms=c(4:13) #TODO: remove line after testing
  my.data.expanded.nNOIS
  
  #derive the matrix with the symptom intensity only
  my.data.symptoms=my.data.expanded.nNOIS[, which.symptoms]
  #derive the matrix with the symptom positivity/negativity only
  my.data.symptoms.yn=ifelse(my.data.expanded.nNOIS[,which.symptoms]>0, 1, 0)
  
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
         labels=paste0(names(my.data.expanded.nNOIS)[which.var], "=" , my.levels[1]), xpd=T, adj=c(0))
    
    text(x=0, par("usr")[4],
         labels=paste0(names(my.data.expanded.nNOIS)[which.var], "="  ,my.levels[2]), xpd=T, adj=c(1))
    }# end if		
}


