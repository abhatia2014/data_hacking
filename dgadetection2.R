# Load Libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(mlr)
library(entropy)
library(urltools)
library(stringr)
library(ngram)

#load data files
#path="./dga_detection/data/"
alexa=read.csv("https://raw.githubusercontent.com/abhatia2014/DGA-1/master/all_legit.txt",stringsAsFactors = FALSE,header = FALSE)
#give names to the headers
colnames(alexa)=c("domain.name")
tail(alexa)
head(alexa)
head(predict.test)


#first we remove the 0 and space from the end of the domain names

#write a function to do that


domains=alexa$domain.name
domains=str_sub(domains,start = 1,end = str_length(domains)-2)
head(domains)
alexa$domain.name=domains

#To extract the domain name out of the host name, we install the library urltools

#install.packages('urltools')

#example suffix extract of dga domain

domainname="mmsdlijfajkkldfajlelkl.ptu"
suffix_extract(domainname)$domain
#we use the function suffix_extract to get the domain name
head(suffix_extract(alexa$domain.name))
alexa$domain=suffix_extract(alexa$domain.name)$domain

#add a class 'legit' to the alexa dataframe

alexa$status="legit"

#we drop variables that are no longer needed


alexa$domain.name=NULL
head(alexa)
#we also need to drop na and duplicates

sum(is.na(alexa$domain))
#1377 NAs , lets remove them

alexa=alexa[-which(is.na(alexa$domain)),]

#remove duplicates
rm(domains)
alexa=unique(alexa)
#removed all duplicates, remaining 890099 entries
nrow(alldomains[alldomains$status=="legit",])


goodnames=sample(1:890000,size = 20,replace = FALSE)

alldomains[goodnames,"domain"]
#lets do the similar for dga_domains

bad.domains=read.csv("https://raw.githubusercontent.com/abhatia2014/DGA-1/master/all_dga.txt",header = FALSE)

colnames(bad.domains)="baddomain"

# again remove last 2 characters from the name of bad domain which is space and 1

bad.domains$baddomain=str_sub(bad.domains$baddomain,1,str_length(bad.domains$baddomain)-2)

#convert all bad domains to lower case

bad.domains$baddomain=tolower(bad.domains$baddomain)

bad.domains$baddomain[[35]]

#we have to split the string on '.' and only keep the first part

#str_split(string = bad.domains$baddomain[[35]],pattern = "\\.")[[1]][1]
bad.domains$baddomain=sapply(bad.domains$baddomain,function(x) str_split(x,"\\.")[[1]][1])
bad.domains$domain=bad.domains$baddomain
bad.domains$baddomain=NULL
bad.domains$status="dga"
head(bad.domains)
rm(cleandomain,mydomains)
sum(is.na(bad.domains))
#no NAs
#currently 801667 bad domains
#to check if duplicates
bad.domains=unique(bad.domains)
801667-693090
#there were 108577 duplicates that were removed
#balance 693090 bad domains
# combine both alexa and bad.domains

badnames=sample(900000:1583189,size = 20,replace = FALSE)

alldomains[badnames,"domain"]

alldomains=as.data.frame(rbind(alexa,bad.domains))

#total 1.58M domain names 
prop.table(table(alldomains$status))
# with 43% dga and 56% legitimate domains
#get the length of the domain name

alldomains=alldomains %>% 
  mutate(len=str_length(domain))

tail(alldomains,20)


#Now calculate entropy 

#first load the entropy package



#write a function to calculate entropy of a word

myentropy=function(myword){
  lengthword=length(myword)
  splitword=unlist(str_split(string = myword,pattern = ""))
  freqword=table(splitword)/lengthword
  wordentropy=entropy.empirical(freqword,unit="log2")
  return(wordentropy)
}

#myentropy("abcdefghifkl")
# myentropy("abcabcabcabc")
# myentropy("transworld")
# myentropy("aquickbrownfoxjumpedoverthelazydog")
#we use this function to calculate entropy of the all domains

alldomains$entropy=sapply(alldomains$domain,function(x) myentropy(x))


#let's draw some plots to understand the behavious of length and entropy by status

#first by length

library(ggplot2)

ggplot(alldomains,aes(status,len,fill=status))+geom_boxplot()+
  ggtitle("Box Plot for domain name length by status")


alldomains %>% 
  group_by(status) %>% 
  summarise(median=median(len))

#generally the median for length is slightly higher for dga (14 vs 10)

#Let's do another box plot by entropy

ggplot(alldomains,aes(status,entropy,fill=status))+geom_boxplot()+
  ggtitle("Box Plot for domain name Entropy by Status")

#again the entropy is slightly higher for dga (3.16 vs 2.9)

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

name="1pvbd831h3ubdmac9dp0g3mwgm"
length(str_extract_all(name,pattern = pattern)[[1]])

alldomains$numcount=sapply(alldomains$domain,function(x) length(str_extract_all(x,pattern = pattern)[[1]]))



#let's put the numeric count as a percentage of length of domain 
alldomains$numcount=alldomains$numcount/alldomains$len

#we make a box plot of the domains by status, numcount

ggplot(alldomains,aes(status,numcount,fill=status))+geom_boxplot()+
  ggtitle("Box Plot for domain name numeral count  by Status")

#its hard to make out the difference since the median is zero
summary(alldomains$numcount)
with(alldomains,tapply(numcount,status,summary))
with(alldomains,tapply(entropy,status,summary))
with(alldomains,tapply(len,status,summary))
#The dga entries do have higher number of numcount compared to legit but it is hard to differentiate the two based on this count

#we'll do a scatter plot between entropy and numcount

ggplot(alldomains,aes(numcount,entropy))+geom_point(aes(color=status,alpha=0.1))+
  ggtitle("Scatter plot showing Count of Numerals \n in the domain name Vs Entropy")

#let us now try and find the percentage count of vowels and consonents 

#first find the number of vowels in the domain name

patternvowels='[aeiou]'

alldomains$numvowel=sapply(alldomains$domain,function(x) length(str_extract_all(x,pattern = patternvowels)[[1]]))

alldomains=alldomains %>% 
  mutate(vowelpercent=numvowel/len)

#lets try and do boxplot of vowel count percentage against the status

ggplot(alldomains,aes(status,vowelpercent,fill=status))+geom_boxplot()+
  ggtitle("Box Plot for Vowel Percent by Status")

#significantly lower percentage  of vowels in dga compared with legit

#let's see a scatter plot of vowelpercent vs entropy

ggplot(alldomains,aes(vowelpercent,entropy))+geom_point(aes(color=status,alpha=0.2))+
  ggtitle("Scatter plot showing Entropy Vs VowelPercent")

#also let's see a scatter plot of vowels vs length 

ggplot(alldomains,aes(vowelpercent,len))+geom_point(aes(color=status,alpha=0.1))

alldomains$numvowel=NULL
#let's try and find the consonent percentage in the domain names, we also exclude numeric characters

patterncons='[^aeiou[0-9]]'

alldomains$numconsonents=sapply(alldomains$domain,function(x) length(str_extract_all(x,pattern = patterncons)[[1]]))

alldomains=alldomains %>% 
  mutate(conspercent=numconsonents/len)

alldomains$numconsonents=NULL

#let's box plot conspercent with status

ggplot(alldomains,aes(status,conspercent,fill=status))+geom_boxplot()+
  ggtitle("Box Plot for Consonent Percent by Status")


with(alldomains,tapply(conspercent,status,summary))

#dga sites have much higher number of consonents as compared to legit sites

#let's do a scatter plot of entropy vs conspercent percentage 

ggplot(alldomains,aes(conspercent,entropy))+geom_point(aes(color=status,alpha=0.1))+
  ggtitle("Scatter plot of domain name entropy vs consonent percentage")

#finally, let's see which all domain names have consonents repeated continously 4 or more times

consonentcont='[^aeiou[0-9]]{4,}'
alldomains$repconsonent=as.numeric(str_detect(alldomains$domain,pattern = consonentcont))

table(alldomains$repconsonent)

#plot to understand count of status showing this behavious

prop.table(table(alldomains$status,alldomains$repconsonent),margin = 1)

plot(prop.table(table(alldomains$status,alldomains$repconsonent),margin = 1),col=c("grey","pink"),main="Tree Plot showing domain seggregation \n using repeated consonant feature")

ggplot(alldomains,aes(repconsonent,status))+geom_tile(aes(stat = "identity",col=status))
#let's try to classify the status using these two features


# Ngrams Analysis ---------------------------------------------------------

#testing the package


test=c("1vz89zm5b2e981bgfhqdzbke3m","n0whky19miuk71dctsi047na1i")
test_split=sapply(test,function(x) splitter(x,split.char = TRUE))

ng=ngram(test_split,n = 4)
ng
print(ng,output="full")
get.phrasetable(ng)

get.ngrams(ng)


#for this study let's create a corpus of ngrams from all the domains of all domains (legit)

legitdomains=alldomains[alldomains$status=="legit",]

#also only keep words with length >4

legitdomains=legitdomains[legitdomains$len>4,]

legitwords=legitdomains$domain

#split the words in letters separated by space
legit_split=sapply(legitwords,function(x) splitter(x,split.char = TRUE))

#now create ngrams of the split words

legit_grams=ngram(legit_split,n=4)

print(legit_grams,output="truncated")

#get all ngrams in a vector

legitngrams=get.ngrams(legit_grams)
legitngrams=c("ibm",legitngrams)
legitngrams=c("ibma","ibme","ibmi","ibmo","ibmu",legitngrams)
#now compare each 4 or greater word in the legit and dga domains to these ngrams to find how many matches

#let's try working using an example

testword="198u81s1b2cfnfvunbp4t9saem"
test_split=splitter(testword,split.char = TRUE)
test_split_ngram=ngram(test_split,n=4)
listngrams=get.ngrams(test_split_ngram)

sum(listngrams %in% legitngrams)
sum(listngrams %in% legitngrams)/length(listngrams)

# this works so, let's use the dataset to find the matches 

# the logic is if there is less than 4 letters, then match = 1 ,else find the % match

# we write a function

findngrammatch=function(mytestvector) {
  #browser()
  if (str_count(mytestvector) <5 ){
    matchpercent=1
  } else {
    test_split=splitter(mytestvector,split.char = TRUE)
    test_split_ngram=ngram(test_split,n=4)
    listngrams=get.ngrams(test_split_ngram)
    matchpercent=sum(listngrams %in% legitngrams)/length(listngrams)
  }
  return(matchpercent)
}

#run the function for all values of alldomains$domains
#trial run for bad.domains
# bad.domains=alldomains[alldomains$status=="dga",]
# 
# bad.domains$matchpercent=sapply(bad.domains$domain,function(x) findngrammatch(x))

# Now do it for all domains

#create an empty vector

matchpercent=vector()
#put all values in the vector
matchpercent=sapply(alldomains$domain,function(x) findngrammatch(x))
alldomains$matchpercent=matchpercent
rm(bad.domains,legitdomains,legit_split,legit_grams,legitwords,listngrams,testword,test_split_ngram,test_split,test,sampleid,ng)
save.image("alldomain.RData")
#let's try and plot the chart for ngrams percent match

ggplot(alldomains,aes(status,matchpercent,fill=status))+geom_boxplot()+
  ggtitle("Box Plot for Domain Percent Ngram Match by Status")

ggplot(alldomains,aes(matchpercent,len))+geom_point(aes(color=status,alpha=0.1))+
  ggtitle("Scatter plot of domain name length vs n-gram match percent")

ggplot(alldomains,aes(matchpercent,repconsonent))+geom_point(aes(color=status,alpha=0.1))+
  ggtitle("Scatter plot of domain name repeated consonants vs n-gram match percent")

library(mlr)

#create task

#take a subset of alldomains with only len and entropy as the features

names(alldomains)

subdomains=alldomains[,c(2:9)]

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

# #since it is unbalanced classification task, we weight the dga status over
# task.over=oversample(traintask,rate = 30)

table(getTaskTargets(traintask))
# table(getTaskTargets(task.over))
# prop.table(table(getTaskTargets(task.over)))
#task.over
#with task over we get the same proportion of dga and legit

#get learners

alllearners=listLearners(traintask)

#let's benchmark performance using 5 learners

# benchlearners=list(makeLearner("classif.boosting",predict.type = "prob"),makeLearner("classif.C50",predict.type = "prob"),
#                    makeLearner("classif.earth",predict.type = "prob"),makeLearner("classif.ksvm",predict.type = "prob"),
#                    makeLearner("classif.randomForest",predict.type = "prob"))
lrn=makeLearner("classif.C50",predict.type = "prob")
#Define sampling

benchsample=makeResampleDesc("CV",iters=3)
listMeasures(traintask)
measures=list(acc,auc)

#library(parallelMap)
#parallelStartSocket(cpus = 4)
#perform the experiment

#train the model using the learner

model_C50=mlr::train(learner = lrn,task = traintask)

#let's see the prediction w/o tuning parameters

#lets find the results on the test domain

predict.test=predict(model_C50,newdata = testdomains)
#first create a task for the test domain

# test.task=makeClassifTask(data=testdomains,target = "status")
# test.task
# test.task.over=oversample(test.task,rate=30)
# prop.table(table(getTaskTargets(test.task)))
# prop.table(table(getTaskTargets(test.task.over)))
# names(testdomains)

#predict_C50=predict(model_C50,task = test.task.over)

#take a sample of 20 records from test set and predict outcome
testsample=sample(x = nrow(testdomains),size = 20,replace = FALSE)

testsamplepredict=predict(model_C50,newdata = testdomains[testsample,])
testsamplepredict$data
calculateConfusionMatrix(pred = testsamplepredict,sums = TRUE)
library(caret)


calculateConfusionMatrix(pred = predict.test,sums = TRUE)
confusionMatrix(predict.test$data$response,predict.test$data$truth)

calculateROCMeasures(pred = predict.test)
performance(pred = predict.test,task = traintask,measures = list(mmce,acc,auc))

#fantastic performance
#auc- 98.4%
#accuracy 94.3%
# mmce is 5.6%

#save the results of the prediction

save.image("testresults.RData")


#visual analysis of the prediction
#find the positive class
getRRTaskDesc(traintask)$positive
predict.test$threshold
pred.test2=setThreshold(predict.test,0.9)
pred.test2$threshold
confusionMatrix(predict.test$data$response,predict.test$data$truth)
performance(pred = pred.test2,task = traintask,measures = list(mmce,acc,auc) )

#much higher misclassification rate when the dga threshold is increased to 90%

# to just get the prediction probabilities of the positive class (dga) only

head(getPredictionProbabilities(predict.test))

#prediction visualization
plotLearnerPrediction(lrn,task=traintask)
plot(predict.test$data$truth,predict.test$data$response)

library(ggplot2)
ggplot(predict.test$data,aes(truth,response))+geom_tile()

#plotting classifier performance as a function of decision threshold
library(mlr)
performancedata=generateThreshVsPerfData(obj = predict.test,measures = list(tpr,fpr,mmce))
#plot the threshold against the mmce

plotThreshVsPerf(performancedata)
head(performancedata)
#indeed the minimum mmce is at threshold =50%
names(predict.test$data)
lattice::xyplot(mmce~threshold,data=performancedata$data,type="l")
#generating partial dependencies

pdtask=getTaskData(traintask)
pd=generatePartialDependenceData(model_C50,input =pdtask,features = "matchpercent")
names(pd)
pd$features
head(pd$data)
ggplot(pd$data,aes(matchpercent,Probability,col="red"))+geom_line(linetype=2,linejoin = 'bevel')
str(plotThreshVsPerf)
plt=plotPartialDependence(pd)
plt
#excellent chart to explain the effect of matchpercent in probability of prediction of dga class

#let's try to find the probability of estimation based on repconsonent

pd2=generatePartialDependenceData(model_C50,input = pdtask,features = "repconsonent")

plotPartialDependence(pd2)

#let's try and plot partial dependence on conspercent

pd3=generatePartialDependenceData(model_C50,input=pdtask,features = "conspercent")
plotPartialDependence(pd3)

#let's try on the combined effect of two variables matchpercent and conspercent

pd4=generatePartialDependenceData(model_C50,input = pdtask,features = c("matchpercent","conspercent"))

plotPartialDependence(pd4)

#potting ROC curves using the generate threshold vs performance data

ROC_data=generateThreshVsPerfData(obj = predict.test,measures = list(fpr,tpr,mmce))
plotROCCurves(ROC_data)
performance(predict.test,auc)
plotThreshVsPerf(ROC_data)

#generate variable importance

varImp=generateFeatureImportanceData(task = traintask,learner = lrn,measure = mmce)


#taking lot of time, alternatively generating feature importance using caret's varimp function

# Model to Predict Unknown Domains ----------------------------------------

#we write a function to predict unknown domains not part of the dataset

predictDomain=function(mydomain){
  #browser()
  #create a dataframe from the domain name with length as the first feature
  mydomaindf=data.frame(domain=mydomain)
  #convert all bad domains to lower case
  mydomaindf$domain=tolower(mydomaindf$domain)
  #first do a suffix extract on the domain name if there is NA then do a str_split on "."
  if(is.na(suffix_extract(mydomaindf$domain)$domain)){
    #we have to split the string on '.' and only keep the first part
    #check if there is a "." in the string
    if (is.na((str_locate(string = mydomaindf$domain,pattern = "\\.")[[1]][1]))){
      mydomaindf$domain
    }else {
      mydomaindf$domain=sapply(mydomaindf$domain,function(x) str_split(x,"\\.")[[1]][1])
    }
  }else {
    mydomaindf$domain=suffix_extract(mydomaindf$domain)$domain
  }
  
  #calculate string length
  mydomaindf=mydomaindf %>% 
    mutate(len=str_length(domain))
  #find entropy of the domain
  mydomaindf$entropy=sapply(mydomaindf$domain,function(x) myentropy(x))
  #find numcount in the domain name and find the percentage by len
  pattern='[0-9]'
  mydomaindf$numcount=sapply(mydomaindf$domain,function(x) length(str_extract_all(x,pattern = pattern)[[1]]))
  #let's put the numeric count as a percentage of length of domain 
  mydomaindf$numcount=mydomaindf$numcount/mydomaindf$len
  #find the number of vowels in the domain name
  patternvowels='[aeiou]'
  mydomaindf$numvowel=sapply(mydomaindf$domain,function(x) length(str_extract_all(x,pattern = patternvowels)[[1]]))
  mydomaindf=mydomaindf %>% 
    mutate(vowelpercent=numvowel/len)
  mydomaindf$numvowel=NULL
  #let's try and find the consonent percentage in the domain names, we also exclude numeric characters
  patterncons='[^aeiou[0-9]]'
  mydomaindf$numconsonents=sapply(mydomaindf$domain,function(x) length(str_extract_all(x,pattern = patterncons)[[1]]))
  mydomaindf=mydomaindf %>% 
    mutate(conspercent=numconsonents/len)
  mydomaindf$numconsonents=NULL
  #let's see which all domain names have consonents repeated continously 4 or more times
  consonentcont='[^aeiou[0-9]]{4,}'
  mydomaindf$repconsonent=as.numeric(str_detect(mydomaindf$domain,pattern = consonentcont))
  #N-gram analysis
  #first find the length of the domain name
  #if the length of the domain name is< 4, find the length difference from 4 and add as many random letters
  if (mydomaindf$len<4) {
    lendiff=4-str_length(mydomaindf$len)
    for (i in 1:lendiff){
      mydomaindf$domain=paste0(mydomaindf$domain,sample(c("a","e","i","o","u"),1))
    }
  }
  #create empty vector
  matchpercent=vector()
  #put all values in the vector
  matchpercent=sapply(mydomaindf$domain,function(x) findngrammatch(x))
  mydomaindf$matchpercent=matchpercent
  mydomaindf$domain=NULL
  #predict using the created model
  mydomainPredict=predict(model_C50,newdata =mydomaindf)
  mydomainPredict$data$domainname=mydomain
  mydomainPredict$data=mydomainPredict$data[c(4,1,2,3)]
  return(mydomainPredict$data)
}


predictDomain("greengrass")

save.image("finalmodel.RData")

trysample=sample(nrow(alldomains),10,replace = FALSE)
checkdomains=alldomains[trysample,'domain']
predictDomain(checkdomains)
