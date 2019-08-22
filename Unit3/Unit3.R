getwd()
setwd("C:/Users/DELL/Downloads/abdallah/Unit3")
quality=read.csv("quality.csv")
summary(quality)
str(quality)
table(quality$PoorCare)
# 33 recieved 33 poor care
98/(98+33)
#percentage of good care

#install and load a package
install.packages("caTools")
library(caTools)

set.seed(88)
split=sample.split(quality$PoorCare, SplitRatio = 0.75)
#split contains True and false -> True is for training set and false for testing  set 
#so now we will create the subsets based on these values

qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality , split == FALSE)
QualitLog = glm(PoorCare ~ OfficeVisits + Narcotics , data = qualityTrain , family = binomial )
#glm -> generalized linear model
#family = binomial is for true or false
summary(QualitLog)

predictTrain = predict(QualitLog, type="response") #type="response" to get probas
summary(predictTrain)
tapply(predictTrain,qualityTrain$PoorCare,mean)

QualitLog2 = glm(PoorCare ~ StartedOnCombination + ProviderCount , data = qualityTrain , family = binomial )
summary(QualitLog2)


table(qualityTrain$PoorCare,predictTrain>0.5)
table(qualityTrain$PoorCare,predictTrain>0.7)
table(qualityTrain$PoorCare,predictTrain>0.2)


################Video6###################

install.packages("ROCR")
library(ROCR)
ROCRpred= prediction(predictTrain,qualityTrain$PoorCare)
ROCRperf= performance(ROCRpred,"tpr","fpr")
plot(ROCRperf)
plot(ROCRperf,colorize=TRUE)
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))

###############Video7####################
predictTest = predict(QualitLog, type="response", newdata=qualityTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc


################THE FIRMINGHAM HEART STUDY ################
framingham=read.csv("framingham.csv")
str(framingham)

library(caTools)
set.seed(1000)
split=sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
train=subset(framingham,split==TRUE)
test= subset(framingham,split==FALSE)
framinghamLog=glm(TenYearCHD ~ . ,data=train, family=binomial)
summary(framinghamLog)
predictTest=predict(framinghamLog,type="response", newdata=test)
table(test$TenYearCHD,predictTest>0.5)

#accuracy of our model (1096+11)/(1069+6+187+11) ( ratio of good prediction (TT FF))
#accuracy of baseline method (1069+6)/(1069+6+187+11)
ROCRpred=prediction(predictTest,test$TenYearCHD)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc
#0.7 which is good
#it can differentiate low-risk from high-risk patients


####################ElectionForcasting####################
polling=read.csv("PollingData.csv")
str(polling)
table(polling$Year)
summary(polling)

#Dealing with missing data

install.packages("mice")
library("mice")
simple=polling[c("Rasmussen","SurveyUSA","PropR","DiffCount")]
summary(simple)
str(simple)

set.seed(144)
imputed=complete(mice(simple))
polling$Rasmussen=imputed$Rasmussen
polling$SurveyUSA=imputed$SurveyUSA


#Video3
Train=subset(polling,Year==2004 | Year==2008)
Test= subset(polling,Year==2012)
table(Train$Republican)
table(sign(Train$Rasmussen))

table(Train$Republican,sign(Train$Rasmussen))
#Rasmussen= baseline predictions

#video4
cor(Train[c("Rasmussen","SurveyUSA","PropR","DiffCount","Republican")])
mod1=glm(Republican ~ PropR ,data=Train, family=binomial)
summary(mod1)
pred1=predict(mod1,type="response")
table(Train$Republican,pred1>=0.5)

mod2=glm(Republican ~ SurveyUSA + DiffCount ,data=Train, family=binomial)
pred2=predict(mod2,type="response")
table(Train$Republican,pred2>=0.5)

table(Test$Republican,sign(Test$Rasmussen))

TestPrediction=predict(mod2,newdata = Test,type="response")
table(Test$Republican,TestPrediction>=0.5)

subset(Test,TestPrediction>=0.5 & Republican==0)


