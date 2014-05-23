plotLasagna <- function(data, # dataFiltered()
                        treatasBinary, # input$treatasBinary
                        symptom, # input$selectedSymptoms
                        dateVar, # input$dateVar
                        personIDVar, # input$patientIDVar
                        measurementVar, # input$measurementVar
                        groupingVar, # input$groupingVar
                        thresholdValue # input$thresholdValue
) {
  
  if (treatasBinary==TRUE) {
    # make values binary
    dataFiltered.yn=ifelse(dataFiltered[ ,selectedSymptoms] > thresholdValue, 1, 0)
    
    time=tapply(dataFiltered[,measurementVar], INDEX=dataFiltered[,1], FUN=function(x) x )
    res=tapply(dataFiltered.yn[,symptom], INDEX=dataFiltered[,1], FUN=function(x) x )
    
    my.levels.m=unique(unlist(time))
    
    my.mat=matrix(NA, ncol=length(my.levels.m), nrow=length(res))
    
    lapply(1:length(res), function(i) {
      for(j in 1:length(my.levels.m)) {
        k=which(my.levels.m[j]==time[[i]])
        #cat(k)
        if(length(k)>0) my.mat[i,j]<<- res[[i]][k]
      }
    })
    
    pheatmap(my.mat, cluster_cols=FALSE, main=symptom)
    
  }
  
#   output$mytabs = renderUI({
#     nTabs = input$nTabs
#     myTabs = lapply(paste('Tab', 1: nTabs), tabPanel)
#     do.call(tabsetPanel, myTabs)
#   })
  
  if (treatasBinary==FALSE) {
    #a <- layout(matrix(c(1:16), 16, 1, byrow=TRUE), heights=rep(lcm(0.5),16) )
    #layout(matrix(c(1:16), 16, 1, byrow=TRUE), heights=rep(lcm(0.5),16) )
    #layout.show(a)
    #class(a)
    #png("./test.png")
    
    time=tapply(dataFiltered[,measurementVar], INDEX=dataFiltered[,1], FUN=function(x) x )
    res=tapply(dataFiltered[,symptom], INDEX=dataFiltered[,1], FUN=function(x) x )
    
    my.levels.m=unique(unlist(time))
    
    my.mat=matrix(NA, ncol=length(my.levels.m), nrow=length(res))
    
    lapply(1:length(res), function(i) {
      for(j in 1:length(my.levels.m)) {
        k=which(my.levels.m[j]==time[[i]])
        #cat(k)
        if(length(k)>0) my.mat[i,j]<<- res[[i]][k]
      }
    })
    
    pheatmap(my.mat, cluster_cols=FALSE, main=symptom)
    #plot(1)
    
    #layout.show(a)
    #dev.off()
  }
  
}

pastePlotFilenames <- function(filenames) {
  out <- vector()
  for (file in filenames) {
    #data <- readPNG(file, native=TRUE)
    out <- paste(out, img(src=paste0(file)))
    #out <- paste(out, (img(src=data)))
  }
  return(out)
}