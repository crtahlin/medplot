plotValueswithCIs <- function (data,
                               variableName="Variable",
                               valueName="OR",
                               CILowerName="CILower",
                               CIUpperName="CIUpper",
                               xLabel,
                               yLabel,
                               graphTitle,
                               vLine=NULL,
                               variableOrder=NULL) {
  
  plot <- ggplot() +
  geom_errorbarh(data=data,
                 mapping=aes_string(y=variableName, x=CIUpperName, xmin=CIUpperName, xmax=CILowerName),
                 height=0.2, size=1) +
  geom_point(data=data, 
             mapping=aes_string(y=variableName, x=valueName),
             size=4, shape=21, fill="white") +
  #theme_bw() + 
    myTheme() +
  labs(title=graphTitle, x= xLabel, y=yLabel) + 
    geom_vline(xintercept = vLine) +
    scale_y_discrete(limits=rev(variableOrder))

return(plot)  

}
# 
# # define a ggplot2 theme
# myTheme <- theme_bw() + theme(
#   text = element_text(size=18)
#   )
# 
# # test plot 
# plotValueswithCIs(data=my.data,
#                                variableName="Headache",
#                                valueName="Concentration",
#                                CILowerName="Insomnia",
#                                CIUpperName="Nausea",
#                                xLabel="Test X label",
#                                yLabel="Test Y label",
#                                graphTitle="Test title",
#                                vLine=NULL)
