library(dplyr)
library(tidyverse)
library(mlr)
library(stringr)

# Model to Predict Unknown Domains ----------------------------------------

# #we write a function to predict unknown domains not part of the dataset
# 
# predictDomain=function(mydomain){
#   
#   #create a dataframe from the domain name with length as the first feature
#   mydomaindf=data.frame(domain=mydomain)
#   #convert all bad domains to lower case
#   mydomaindf$domain=tolower(mydomaindf$domain)
#   #we have to split the string on '.' and only keep the first part
#   #check if there is a "." in the string
#   
#   if (is.na((str_locate(string = mydomaindf$domain,pattern = "\\.")[[1]][1]))){
#     mydomaindf$domain
#   }else {
#     mydomaindf$domain=sapply(mydomaindf$domain,function(x) str_split(x,"\\.")[[1]][1])
#   }
#   #calculate string length
#   mydomaindf=mydomaindf %>% 
#     mutate(len=str_length(domain))
#   #find entropy of the domain
#   mydomaindf$entropy=sapply(mydomaindf$domain,function(x) myentropy(x))
#   #find numcount in the domain name and find the percentage by len
#   pattern='[0-9]'
#   mydomaindf$numcount=sapply(mydomaindf$domain,function(x) length(str_extract_all(x,pattern = pattern)[[1]]))
#   #let's put the numeric count as a percentage of length of domain 
#   mydomaindf$numcount=mydomaindf$numcount/mydomaindf$len
#   #find the number of vowels in the domain name
#   patternvowels='[aeiou]'
#   mydomaindf$numvowel=sapply(mydomaindf$domain,function(x) length(str_extract_all(x,pattern = patternvowels)[[1]]))
#   mydomaindf=mydomaindf %>% 
#     mutate(vowelpercent=numvowel/len)
#   mydomaindf$numvowel=NULL
#   #let's try and find the consonent percentage in the domain names, we also exclude numeric characters
#   patterncons='[^aeiou[0-9]]'
#   mydomaindf$numconsonents=sapply(mydomaindf$domain,function(x) length(str_extract_all(x,pattern = patterncons)[[1]]))
#   mydomaindf=mydomaindfs %>% 
#     mutate(conspercent=numconsonents/len)
#   mydomaindf$numconsonents=NULL
#   #let's see which all domain names have consonents repeated continously 4 or more times
#   consonentcont='[^aeiou[0-9]]{4,}'
#   mydomaindf$repconsonent=as.numeric(str_detect(mydomaindf$domain,pattern = consonentcont))
#   #N-gram analysis
#   #first find the length of the domain name
#   #if the length of the domain name is< 4, find the length difference from 4 and add as many random letters
#   if (mydomaindf$len<4) {
#     lendiff=4-str_length(mydomaindf$len)
#     mydomaindf$domain=paste(mydomaindf$domain,sample(letters,lendiff))
#   }
#   #create empty vector
#   matchpercent=vector()
#   #put all values in the vector
#   matchpercent=sapply(mydomaindf$domain,function(x) findngrammatch(x))
#   mydomaindf$matchpercent=matchpercent
#   mydomaindf$domain=NULL
#   #predict using the created model
#   mydomainPredict=predict(model_C50,newdata =mydomaindf)
#   return(mydomainPredict)
# }
# 
# predictDomain("dhfeuihjksdhjkejkhiu")
# 
