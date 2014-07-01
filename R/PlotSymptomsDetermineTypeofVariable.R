determineTypeofVariable <- function(variableValues) { # takes a vector of values
if(!is.vector(variableValues)) {return("Error: Function expects a vector as argument")}
  #variableValues <- as.vector(variableValues)
  nLevels <- length(unique(na.omit(variableValues)))
if (nLevels==1) {numLevels <- "unary"}
if (nLevels==2) {numLevels <- "binary"}
if (nLevels>2) {numLevels <- "multilevel"}
return(list(nLevels=numLevels, type=class(variableValues)))
}