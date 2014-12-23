# This file contains code to load libraries required by the medplot package

# load library for generation interactive web pages
library(shiny)
if(!require(shinyIncubator)) { # NOTE!!!: this is not available on CRAN, might not be best to include it?
  devtools::install_github("shiny-incubator", "rstudio")
  library(shinyIncubator)
}
# load library for generating graph scales
library(scales)
# load library for melting data
library(reshape2)
# library for plotting data
library(ggplot2)
# library for reading Excel files
library(gdata)
# library for manipulating data
library(plyr)
# library for clustering
library(pheatmap)
# load medplot library
library(medplot)
#library for date management
library(lubridate)
# library for regression modeling 
library(rms)
# library for logistic regression modeling
library(logistf) # note: this library has a myriad of dependencies - it might cause problems
# library for permutation tests
library(permute)
# bootstrap library
library(boot)
# libraries for mixed models
library(lme4)
library(lmerTest)
