library(readr)
library(caTools)
library(rpart)
library(rpart.plot)
library(tidyverse)

#read combined dataset
CHACFpriceACLED <- read_csv("CHACFpriceACLED.csv")

#split into fictious Sep 2018 and rest datasets
master <- CHACFpriceACLED %>% filter(referenceconcat != 12018)
Seppredict <- CHACFpriceACLED %>% filter(referenceconcat == 12018)
#split rest into training and test data
set.seed(3000)
spl = sample.split(master$phase_class, SplitRatio = 0.8)
train = subset(master, spl == TRUE)
test = subset(master, spl == FALSE)

#make the model - adding adm1 or adm2 seems to overload the processing - this model sux 
CHphasepredict <- rpart(phase_class ~ adm0_name.x + reference_year + avgvi + avgprice + count, data=train, method = "class", minbucket = 25)
prp(CHphasepredict)
#see how this works on the test data - not good
PredictCART = predict(CHphasepredict, newdata=test, type="class")
table(test$phase_class, PredictCART)
