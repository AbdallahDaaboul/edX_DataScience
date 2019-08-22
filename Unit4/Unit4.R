getwd()
setwd("C:/Users/DELL/Downloads/abdallah/Unit4")
stevens = read.csv("stevens.csv")
str(stevens)
library(caTools)
set.seed(3000)
spl=sample.split(stevens$Reverse,SplitRatio=0.7)
Train = subset(stevens, spl == TRUE)
Test = subset(stevens, spl == FALSE)
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, method="class",minbucket=25)
prp(StevensTree)
predictCART= predict(StevensTree, newdata=Test,type="class")
table(Test$Reverse,predictCART)
library(ROCR)
predictROC=predict(StevensTree,newdata = Test)
predictROC

pred = prediction(predictROC[,2], Test$Reverse)
perf=performance(pred,"tpr","fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)

StevensTree2 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, method="class",minbucket=5)
prp(StevensTree2)
StevensTree3 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, method="class",minbucket=100)
prp(StevensTree3)


########RANDOM FOREST ########
Train$Reverse = as.factor(Train$Reverse)

install.packages("randomForest")
library(randomForest)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, nodesize=25,ntree=200)
PredictForest=predict(StevensForest,newdata = Test)
table(Test$Reverse,PredictForest)

#QuickQuestion
set.seed(100)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, nodesize=25,ntree=200)
PredictForest=predict(StevensForest,newdata = Test)
table(Test$Reverse,PredictForest)
(46+74)/(46+31+19+74)

set.seed(200)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, nodesize=25,ntree=200)
PredictForest=predict(StevensForest,newdata = Test)
table(Test$Reverse,PredictForest)
(43+75)/(43+75+34+18)

###########VIDEO7##############
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071) 
numFolds = trainControl(method="cv", number=10) #cv for cross validation number for folds
cpGrid=expand.grid(.cp=seq(0.01,0.5,0.01))
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data=Train,method="rpart",trControl=numFolds,tuneGrid=cpGrid)
StevensTreeCV = rpart ( Reverse ~Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data=Train,method="class",cp=0.18)
predictCV = predict(StevensTreeCV,newdata=Test, type="class")
table(Test$Reverse,predictCV)
accuracy=(59+64)/(59+18+29+64)
prp(StevensTreeCV)

#<><><><><><><><>TheStoryOfD2Hawkeye<><><><><><><><>

claims=read.csv("ClaimsData.csv")
str(claims)
table(claims$bucket2009)/nrow(claims)
library(caTools)
spl=sample.split(claims$bucket2009,SplitRatio=0.6)
ClaimsTrain = subset(claims, spl==TRUE)
ClaimsTest = subset(claims,spl==FALSE)

table(ClaimsTest$bucket2009,ClaimsTest$bucket2008)
accuracyy=(110111+10757+2711+1607+107)/nrow(ClaimsTest)
PenaltyMatrix=matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0),byrow=TRUE,nrow=5)

#total error of our model ( sum of penalty )
TotalError=sum(as.matrix(table(ClaimsTest$bucket2009,ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest)
#(as.matrix to convert our matrix to a matrice that can be multiplied by another one)
#We multiply the 2 matrix and divide by the total of observations 

#video8
library(rpart)
library(rpart.plot)
ClaimsTree=rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain,method="class",cp=0.00005)
prp(ClaimsTree)
predictTest=predict(ClaimsTree,ClaimsTest,type="class")
table(ClaimsTest$bucket2009,predictTest)
sum(as.matrix(table(ClaimsTest$bucket2009,predictTest))*PenaltyMatrix)/nrow(ClaimsTest)


ClaimsTree=rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain,method="class",cp=0.00005,parms=list(loss=PenaltyMatrix))
predictTest=predict(ClaimsTree,ClaimsTest,type="class")
table(ClaimsTest$bucket2009,predictTest)
sum(as.matrix(table(ClaimsTest$bucket2009,predictTest))*PenaltyMatrix)/nrow(ClaimsTest)
#with the new parameter for rpart penalty errors declined but we risk a declining accuracy


#<><><>Location, Location, Location: Regression Trees for Housing Data (Recitation)<><><><>
  
#video2
boston=read.csv("boston.csv",pch="$")
str(boston)
plot(boston$LON, boston$LAT)
#to mark points that have CHAS ==1
points(boston$LON[boston$CHAS==1], boston$LAT[boston$CHAS==1],col="blue",pch=19)
points(boston$LON[boston$TRACT==3531], boston$LAT[boston$TRACT==3531],col="red",pch=19)
summary(boston$NOX)
points(boston$LON[boston$NOX>=0.551],boston$LAT[boston$NOX>= 0.551],col="green",pch=19)
summary(boston$MEDV)
plot(boston$LON,boston$LAT)
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>= 21.2],col="red",pch=19)
summary(boston$MEDV)
plot(boston$LAT,boston$MEDV)
plot(boston$LON,boston$MEDV)
latlonlm= lm(MEDV ~ LAT + LON, data=boston)
summary(latlonlm)
plot(boston$LON,boston$LAT)
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>= 21.2],col="red",pch=19)
latlonlm$fitted.values
points(boston$LON[latlonlm$fitted.values>=21.2],boston$LAT[latlonlm$fitted.values>=21.2],col="blue",pch="$")

 #regression tree method
library(rpart)
library(rpart.plot)
latlontree= rpart(MEDV ~ LAT + LON, data=boston)
prp(latlontree)
plot(boston$LON,boston$LAT)
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>= 21.2],col="red",pch=19)
fittedvalues = predict(latlontree)
points(boston$LON[fittedvalues>=21.2],boston$LAT[fittedvalues>=21.2],col="blue",pch="$")

latlontree= rpart(MEDV ~ LAT + LON, data=boston,minbucket=50)
plot(latlontree)
text(latlontree)

plot(boston$LON,boston$LAT)
abline(v=-71.07)
abline(h=42.21)
abline(h=42.17)

points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>= 21.2],col="red",pch=19)
fittedvalues = predict(latlontree)
points(boston$LON[fittedvalues>=21.2],boston$LAT[fittedvalues>=21.2],col="blue",pch="$")


#video5
  
library(caTools)
set.seed(123)
split=sample.split(boston$MEDV,SplitRatio = 0.7)
train=subset(boston,split==TRUE)
test=subset(boston,split==FALSE)
linreg = lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD +TAX + PTRATIO, data=train)
summary(linreg)
linreg.pred=predict(linreg,newdata=test)
linreg.sse=sum((linreg.pred - test$MEDV)^2)
tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD +TAX + PTRATIO, data=train)
prp(tree)
tree.pred=predict(tree,newdata=test)
tree.sse=sum((tree.pred-test$MEDV)^2)

#video7
library(caret)
library(e1071)
tr.control= trainControl(method="cv",number=10)
cp.grid= expand.grid(.cp=(0:10)*0.001)
#cp and accuracy
tr = train(MEDV ~ LAT + LON +CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO,data=train,method="rpart",trControl=tr.control,tuneGrid=cp.grid)
tr
#tree:
best.tree = tr$finalModel
prp(best.tree)
#predictions:
best.tree.pred= predict(best.tree,newdata=test)
best.tree.sse=sum((best.tree.pred-test$MEDV)^2)
