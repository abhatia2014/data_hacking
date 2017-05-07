
# Load Libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(mlr)

#load data files
path="./dga_detection/data/"
alexa=read.csv(paste0(path,"alexa_100k.csv"),stringsAsFactors = FALSE,header = FALSE)
#give names to the headers
colnames(alexa)=c("no","domain.name")
tail(alexa)
head(alexa)

#To extract the domain name out of the host name, we install the library urltools

#install.packages('urltools')
library(urltools)

#we use the function suffix_extract to get the domain name

head(suffix_extract(alexa$domain.name))
alexa$domain=suffix_extract(alexa$domain.name)$domain

#add a class 'legit' to the alexa dataframe

alexa$status="legit"

#we drop variables that are no longer needed

alexa$no=NULL
alexa$domain.name=NULL
head(alexa)
#we also need to drop na and duplicates

sum(is.na(alexa$domain))
#763 NAs , lets remove them

alexa=alexa[-which(is.na(alexa$domain)),]

#remove duplicates

alexa=unique(alexa)
#removed all duplicates, remaining 91701 entries

#lets do the similar for dga_domains

bad.domains=read.csv(paste0(path,"dga_domains.txt"),header = FALSE)

#get some more bad domains

#bad.domains2=read.csv("https://raw.githubusercontent.com/abhatia2014/DGA-1/master/all_dga.txt",header = FALSE)

colnames(bad.domains)="baddomain"

#convert all bad domains to lower case

bad.domains$baddomain=tolower(bad.domains$baddomain)

#we have to split the string on '.' and only keep the first part

library(stringr)


mydomains=sapply(X = bad.domains,function(x) str_split(x,"\\."))
domains=vector()
#get the first item in each
for (i in 1:2669){
  domains[i]=mydomains[[i]][1]
}
bad.domains$domain=domains
bad.domains$baddomain=NULL
bad.domains$status="dga"
head(bad.domains)
rm(mydomains,domains,i)
sum(is.na(bad.domains))
#no NAs
#currently 2669, we check if there are any duplicates
bad.domains=unique(bad.domains)
#there were 5 duplicates that were removed
#balance 2664 
# combine both alexa and bad.domains

alldomains=as.data.frame(rbind(alexa,bad.domains))
prop.table(table(alldomains$status))

#get the length of the domain name

alldomains=alldomains %>% 
  mutate(len=str_length(domain))

tail(alldomains,20)


#Now calculate entropy 

#first load the entropy package

library(entropy)

#write a function to calculate entropy of a word

myentropy=function(myword){
  lengthword=length(myword)
  splitword=unlist(str_split(string = myword,pattern = ""))
  freqword=table(splitword)/lengthword
  wordentropy=entropy.empirical(freqword,unit="log2")
  return(wordentropy)
}

# myentropy("abcdefghifkl")
# myentropy("abcabcabcabc")
# myentropy("transworld")
# myentropy("aquickbrownfoxjumpedoverthelazydog")
#we use this function to calculate entropy of the all domains

myentropy1=sapply(alldomains$domain,myentropy)

head(myentropy1)
#loop to write all entropy values to alldomains
mydomains=vector()
for (i in 1:length(myentropy1)){
  mydomains[i]=myentropy1[i][[1]]
}

alldomains$entropy=mydomains

#let's draw some plots to understand the behavious of length and entropy by status

#first by length

library(ggplot2)

ggplot(alldomains,aes(status,len,fill=status))+geom_boxplot()+
  ggtitle("Box Plot for Domain Length by Status")


alldomains %>% 
  group_by(status) %>% 
  summarise(median=median(len))

#generally the median for length is slightly higher for dga (10 vs 9)

#Let's do another box plot by entropy

ggplot(alldomains,aes(status,entropy,fill=status))+geom_boxplot()+
  ggtitle("Box Plot for Domain Entropy by Status")

#again the entropy is slightly higher for dga (3 vs 2.75)

#finding the median entropy
alldomains %>% 
  group_by(status) %>% 
  summarise(median=median(entropy))

#lets do a scatter plot between entropy and length

ggplot(alldomains,aes(len,entropy))+geom_point(aes(color=status))+
  ggtitle("Scatter Plot of Domains by Entropy and Domain Length")

#its hard to differentiate the dga domains by entropy and length alone
#first let's count the numeric characters in the domain name in the alldomains dataset

pattern='[0-9]'

digits=str_extract_all(string = alldomains$domain,pattern = pattern)
length(digits[[55]])

lengthvector=vector()

for (i in 1:length(digits)){
  
  lengthvector[i]=length(digits[[i]])
  
}

#let's put the numeric count as a percentage of length of domain 
alldomains$numcount=lengthvector/alldomains$len

#we make a box plot of the domains by status, numcount

ggplot(alldomains,aes(status,numcount,fill=status))+geom_boxplot()

#its hard to make out the difference since the median is zero
summary(alldomains$numcount)
with(alldomains,tapply(numcount,status,summary))
with(alldomains,tapply(entropy,status,summary))
with(alldomains,tapply(len,status,summary))
#The dga entries do have higher number of numcount compared to legit but it is hard to differentiate the two based on this count

#we'll do a scatter plot between entropy and numcount

ggplot(alldomains,aes(numcount,entropy))+geom_point(aes(color=status))+
  ggtitle("Scatter plot showing Count of Numerals \n in the domain name Vs Entropy")

#let us now try and find the percentage count of vowels and consonents 

#first find the number of vowels in the domain name

patternvowels='[aeiou]'

numvowels=str_extract_all(alldomains$domain,pattern = patternvowels)

vowelvector=vector()

for (i in 1:length(numvowels)){
  
  vowelvector[i]=length(numvowels[[i]])
  
}

alldomains=alldomains %>% 
  mutate(vowelpercent=vowelvector/len)

#lets try and do boxplot of vowel count percentage against the status

ggplot(alldomains,aes(status,vowelpercent,fill=status))+geom_boxplot()

#significantly lower percentage  of vowels in dga compared with legit

#let's see a scatter plot of vowelpercent vs entropy

ggplot(alldomains,aes(vowelpercent,entropy))+geom_point(aes(color=status))+
  ggtitle("Scatter plot showing Entropy Vs VowelPercent")

#also let's see a scatter plot of vowels vs length 

ggplot(alldomains,aes(vowelpercent,len))+geom_point(aes(color=status))

#let's try and find the consonent percentage in the domain names, we also exclude numeric characters

patterncons='[^aeiou[0-9]]'

numconsonents=str_extract_all(alldomains$domain,pattern = patterncons)

consvector=vector()

for (i in 1:length(numconsonents)){
  
  consvector[i]=length(numconsonents[[i]])
  
}

alldomains=alldomains %>% 
  mutate(conspercent=consvector/len)

#let's box plot conspercent with status

ggplot(alldomains,aes(status,conspercent,fill=status))+geom_boxplot()


with(alldomains,tapply(conspercent,status,summary))

#dga sites have much higher number of consonents as compared to legit sites

#let's do a scatter plot of entropy vs conspercent percentage 

ggplot(alldomains,aes(conspercent,entropy))+geom_point(aes(color=status))+
  ggtitle("Scatter plot of domain name entropy vs consonent percentage")

#finally, let's see which all domain names have consonents repeated continously 4 or more times

consonentcont='[^aeiou[0-9]]{4,}'
alldomains$repconsonent=as.numeric(str_detect(alldomains$domain,pattern = consonentcont))

table(alldomains$repconsonent)

#plot to understand count of status showing this behavious

prop.table(table(alldomains$status,alldomains$repconsonent),margin = 1)

plot(prop.table(table(alldomains$status,alldomains$repconsonent),margin = 1),col=c("green","red"))

#let's try to classify the status using these two features

library(mlr)

#create task

#take a subset of alldomains with only len and entropy as the features

names(alldomains)

subdomains=alldomains[,c(2:8)]

#create a sample dataset using 65% data

sampleid=sample(nrow(subdomains),0.65*nrow(subdomains),replace = FALSE)
traindomains=subdomains[sampleid,]
testdomains=subdomains[-sampleid,]

#find the % of dga entries in each training and test set

prop.table(table(traindomains$status))
prop.table(table(testdomains$status))
#almost the same percentage

#create the classification task

traintask=makeClassifTask(data=traindomains,target = "status")

#since it is unbalanced classification task, we weight the dga status over
task.over=oversample(traintask,rate = 30)

table(getTaskTargets(traintask))
table(getTaskTargets(task.over))
prop.table(table(getTaskTargets(task.over)))
task.over
#with task over we get the same proportion of dga and legit

#get learners

alllearners=listLearners(task.over)

#let's benchmark performance using 5 learners

# benchlearners=list(makeLearner("classif.boosting",predict.type = "prob"),makeLearner("classif.C50",predict.type = "prob"),
#                    makeLearner("classif.earth",predict.type = "prob"),makeLearner("classif.ksvm",predict.type = "prob"),
#                    makeLearner("classif.randomForest",predict.type = "prob"))
lrn=makeLearner("classif.C50",predict.type = "prob")
#Define sampling

benchsample=makeResampleDesc("CV",iters=3)
listMeasures(task.over)
measures=list(acc,auc)

#library(parallelMap)
#parallelStartSocket(cpus = 4)
#perform the experiment

#train the model using the learner

model_C50=train(learner = lrn,task = task.over)

#let's see the prediction w/o tuning parameters

#first create a task for the test domain

test.task=makeClassifTask(data=testdomains,target = "status")
test.task
test.task.over=oversample(test.task,rate=30)
prop.table(table(getTaskTargets(test.task)))
prop.table(table(getTaskTargets(test.task.over)))
names(testdomains)

predict_C50=predict(model_C50,task = test.task.over)
library(caret)
test.task.over

calculateConfusionMatrix(pred = predict_C50,relative = TRUE,sums = TRUE)
confusionMatrix(predict_C50$data$response,predict_C50$data$truth)
calculateROCMeasures(pred = predict_C50)
performance(pred = predict_C50,task = test.task.over,measures = list(mmce,acc,auc))

#Let's try and train the model on actual and not over samples

model_C50_act=mlr::train(learner = lrn,task = traintask)

#predict using the model

predict_C50_act=predict(model_C50,task = test.task)
confusionMatrix(predict_C50_act$data$response,predict_C50_act$data$truth)
performance(pred = predict_C50_act,task = test.task.over,measures = list(mmce,acc,auc))
#we now tune the parameters of the C_50 learner and see if we can get better performance

getParamSet("classif.C50")

#get the search parameter
C50_Search=makeParamSet(
  makeIntegerParam("trials",lower=20,upper=100),
  makeIntegerParam("bands",lower = 5,upper=100),
  makeLogicalParam("winnow",default = FALSE),
  makeNumericParam("CF",lower=0,upper=1),
  makeNumericParam("sample",lower=0.25,upper=0.75)
)

#make the search algorithm

C50_algorithm=makeTuneControlRandom(maxit = 100L)

C50_CV=makeResampleDesc("CV",iters=3)

#tune the parameters

C50_tune=tuneParams(learner = lrn,task = task.over,resampling = C50_CV,measures = measures,par.set = C50_Search,control = C50_algorithm)

# #benchexpt=benchmark(learners = benchlearners,tasks = task.over,resamplings = benchsample,
#                   measures = measures)

# benchexpt2=benchmark(learners = benchlearners2,tasks = traintask,resamplings = benchsample,
#                     measures = measures)
# benchexpt2
# 
# benchexpt3=benchmark(learners = benchlearners,tasks = task.over,resamplings = benchsample,
#                     measures = measures)
# myresampling=resample(learner = makeLearner("classif.C50",predict.type = "prob"),task = task.over,
#                       resampling = benchsample,measures = measures)
# 
# myresampling
# plot(myresampling)
# myresampling2=resample(learner = makeLearner("classif.cvglmnet",predict.type = "prob"),task = task.over,
#                       resampling = benchsample,measures = measures)
# 
# 
# myresampling3=resample(learner = makeLearner("classif.ksvm",predict.type = "prob"),task = task.over,
#                        resampling = benchsample,measures = measures)
# 
# myresampling4=resample(learner = makeLearner("classif.C50",predict.type = "prob"),task = traintask,
#                       resampling = benchsample,measures = measures)
# 
