.mixedModel <- function(data,               # dataFiltered()
                       selectedSymptoms,    # input$selectedSymptoms
                       groupingVar,         # input$groupingVar
                       subjectIDVar,        # input$patientIDVar
                       measurementVar,      # input$measurementVar
                       dateVar,             # input$dateVar
                       thresholdValue,      # input$thresholdValue
                       treatasBinary,       # input$treatasBinary
                       selectedModel){      # input$selectedMixedModelType

  # if variable binary, do a data transformation based on the thresholdvalue
  if (treatasBinary==TRUE) {data[, selectedSymptoms] <- 
                              ifelse(data[,selectedSymptoms]>thresholdValue, 1, 0)}

  # define the variables in the model
#   variable <- data[, groupingVar]
#   PersonID <- data[, subjectIDVar]
#   Measurement <- data[, measurementVar]
  
  # cycle through response variables
  for (symptom in selectedSymptoms) {}

  # choose the right model depending on user selected option
  if(selectedModel=="MMsimple" & treatasBinary==TRUE) {
    formula <- as.formula(paste(symptom, "~", groupingVar, "+(1|", subjectIDVar, ")"))
    model <- glmer(formula , family=binomial,  na.action=na.omit, data=data)
  }
# TODO from here on - list all combos of models possible
if(selectedModel=="MMsimple" & treatasBinary==FALSE) {
    model <- glmer(y~my.var + (1|PersonID) , family=binomial,  na.action=na.omit, data=data)
  }        
    

  
  
  # save results in a data frame
  results <- data.frame(expand.grid(Group=unique(data[,groupingVar]),
                                    Measurement=unique(data[,measurementVar]),
                                    Variable=selectedSymptoms), Mean=NA, LowerCI=NA, UpperCI=NA)
  
  # see .returnPropCIs for values it returns
  # implement this in same way
  # then call it to make a ggplot and a table with output
  
}

### .returnPropCIs ####
.returnPropCIs <- function(data,
                           groupingVar,
                           measurementVar,
                           selectedSymptoms) {
  
  results <- data.frame(expand.grid(Group=unique(data[,groupingVar]),
                                    Measurement=unique(data[,measurementVar]),
                                    Variable=selectedSymptoms), Mean=NA, LowerCI=NA, UpperCI=NA)
  for (i in unique(data[,groupingVar])) {
    for(j in unique(data[,measurementVar])) {
      for(k in selectedSymptoms) {
        # omit missing values, create boolean vector
        symptomData <- na.omit((data[(data[groupingVar]==i & data[measurementVar]==j), k])==1)
        
        testResults <- prop.test(x= sum(symptomData),  n= (sum(symptomData) + sum(!symptomData)))
        
        mean  <- testResults$estimate
        lower <- testResults$conf.int[1]
        upper <- testResults$conf.int[2]  
        
        results[(results["Group"]==i &
                   results["Measurement"]==j &
                   results["Variable"]==k), c("Mean","LowerCI","UpperCI")] <-
          c(mean, lower, upper)
      }
    }
  }
  # make Measurement, Variable and Group a factor
  results[, "Measurement"] <- as.factor(results[, "Measurement"])
  results[, "Group"] <- as.factor(results[, "Group"])
  results[, "Variable"] <- as.factor(results[, "Variable"])
  
  return(results) 
}


# Binary outcome ####

library(lme4)
j=4

#the response is binary
y=ifelse(dataFiltered[,j]>0, 1, 0)
my.var=dataFiltered[,"Sex"]

########### mixed model: including only one variable
my.mod=glmer(y~my.var + (1|PersonID) , family=binomial,  na.action=na.omit, data=dataFiltered)


OR=exp(summary(my.mod)$coef[2,1])
CI.OR=exp(summary(my.mod)$coef[2,1]+c(-1, 1)*qnorm(.975)*summary(my.mod)$coef[2,2])
p.OR=summary(my.mod)$coef[2,4]

##################### testiram
my.var2 <- dataFiltered[,j]
my.mod2 <- lmer(y~my.var2 + (1|PersonID) , family=gaussian,  na.action=na.omit, data=dataFiltered)

# Numerical outcome ####
library(lme4)
library(lmerTest) ### add to required libraries
j=10

#the response is numerical
y=dataFiltered[,j]
my.var=dataFiltered[,input$groupingVar]

########### mixed model: including only one variable
my.mod=lmer(y~my.var + (1|PersonID) , na.action=na.omit, data=dataFiltered)


beta=summary(my.mod)$coef[2,1]
#CI.beta=summary(my.mod)$coef[2,1]+c(-1, 1)*qnorm(.975)*summary(my.mod)$coef[2,2]
#confidence intervals 
CI.beta=confint(my.mod)[4,]
p.beta=summary(my.mod)$coef[2,5]

# Adjustment for measurement occasion ####
################### adjust the analysis for measurement occasion

#the response is binary
y=ifelse(dataFiltered[,j]>0, 1, 0)
my.var=dataFiltered[,input$groupingVar]

########### mixed model: including only one variable and occasion measurement
my.mod=glmer(y~ as.factor(Measurement) +  my.var + (1|PersonID) , family=binomial,  na.action=na.omit, data=dataFiltered)

my.summary=summary(my.mod)$coef
num.coef=nrow(my.summary)

OR=exp(my.summary[num.coef,1])

CI.OR=exp(summary(my.mod)$coef[num.coef,1]+c(-1, 1)*qnorm(.975)*summary(my.mod)$coef[num.coef,2])

p.OR=summary(my.mod)$coef[num.coef,4]


############# numerical variables

j=10

#the response is numerical
y=dataFiltered[,j]
my.var=dataFiltered[,input$groupingVar]

########### mixed model: including only one variable
my.mod=lmer(y~ as.factor(Measurement) + my.var + (1|PersonID) , na.action=na.omit, data=dataFiltered)

my.summary=summary(my.mod)$coef
num.coef=nrow(my.summary)


beta=summary(my.mod)$coef[num.coef,1]
#CI.beta=summary(my.mod)$coef[2,1]+c(-1, 1)*qnorm(.975)*summary(my.mod)$coef[2,2]
#confidence intervals 
CI.beta=confint(my.mod)[num.coef,]
p.beta=summary(my.mod)$coef[num.coef,5]

