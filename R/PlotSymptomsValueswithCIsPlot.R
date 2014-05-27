plotValueswithCIs <- function (data,
                               variableName="Variable",
                               valueName="OR",
                               CILowerName="CILower",
                               CIUpperName="CIUpper",
                               xLabel,
                               yLabel,
                               graphTitle) {
  
  plot <- ggplot() +
  geom_errorbarh(data=data,
                 mapping=aes_string(y=variableName, x=CIUpperName, xmin=CIUpperName, xmax=CILowerName),
                 height=0.2, size=1) +
  geom_point(data=data, 
             mapping=aes_string(y=variableName, x=valueName),
             size=4, shape=21, fill="white") +
  theme_bw() + 
  labs(title=graphTitle, x= xLabel, y=yLabel)

return(plot)  

}