
#unit 4 ~ Assignment3 ~ Predicting Earings
#Abdallah DAABOUL 8/1/2019
#<><><><><><><><><><><><><><><><><><><><><><><><><>

getwd()
setwd("C:/Users/DELL/Downloads/abdallah/Unit4/Assignment3_PredictingEarings")
census=read.csv("census.csv")
str(census)

#<><><><><><><><><>Problem1<><><><><><><><><><><><><>
  #logistic regression
library(caTools)
set.seed(2000)
spl=sample.split(census$over50k,0.6)
train=subset(census,spl==TRUE)
test=subset(census,spl==FALSE)
modlog=glm(over50k ~ . , data=train, family=binomial)
summary(modlog)

prediction=predict(modlog,newdata=test,type="response")
table(test$over50k,prediction>0.5)

#The baseline accuracy is
#9713/(9713+3078) = 0.7593621.

install.packages("ROCR")
library(ROCR)
ROCRpred = prediction(prediction, test$over50k)
ROCRperf= performance(ROCRpred,"tpr","fpr")
plot(ROCRperf)
plot(ROCRperf,colorize=TRUE)
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))
as.numeric(performance(ROCRpred, "auc")@y.values)

#<><><><><><><><><>Problem2<><><><><><><><><><><><><>
  #classification tree
CART50 = rpart(over50k ~ . - over50k, data=train, method="class")
prp(CART50)
prediction=predict(CART50,newdata=test,type="class")
table(test$over50k,prediction)
#(9243+1596)/nrow(test) = 0.847

