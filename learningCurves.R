# Generate learning curve data
# Load Libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(mlr)
library(entropy)
library(urltools)
library(stringr)
library(ngram)


# Generate Learning Curve Data --------------------------------------------

learningtask=makeClassifTask(data = traindomains[1:1000,],target = "status")

r=generateLearningCurveData(
  learners = c("classif.C50","classif.earth","classif.plr","classif.knn","classif.ksvm"),task = learningtask,
  resampling = makeResampleDesc(method="CV",iters=3,predict = "both"),
  percs = seq(0.1,1,by=0.2),
  measures = list(acc,setAggregation(acc,train.mean))
    
)

# plot learning curve
plotLearningCurve(r,facet = "learner")
