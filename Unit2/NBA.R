#NBA 
setwd("C:/Users/DELL/Downloads/abdallah/Unit2")
NBA=read.csv("NBA_Train.csv")
str(NBA)
summary(NBA)
table(NBA$W,NBA$Playoffs)
NBA$PtsDiff=NBA$PTS-NBA$oppPTS
plot(NBA$PtsDef,NBA$W)

WinsReg=lm(W ~ PtsDiff,data=NBA)
summary(WinsReg)

PointsReg=lm(PTS~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK,data=NBA)
summary(PointsReg)
#To visulize errors 
PointsReg$residuals
SSE=sum(PointsReg$residuals^2)
#not significant not interpretable
RMSE=sqrt(SSE/nrow(NBA))
RMSE

PointsReg2=lm(PTS~ X2PA + X3PA + FTA + AST + ORB + DRB  + STL + BLK,data=NBA)
summary(PointsReg2)

PointsReg3=lm(PTS~ X2PA + X3PA + FTA + AST + ORB + STL + BLK,data=NBA)
summary(PointsReg3)

PointsReg4=lm(PTS~ X2PA + X3PA + FTA + AST + ORB  + STL ,data=NBA)
summary(PointsReg4)

SSE4=sum(PointsReg4$residuals^2)
SSE4
RMSE4=sqrt(SSE4/nrow(NBA))
RMSE4

#Video4
NBA_test=read.csv("NBA_test.csv")
PointsPredictions=predict(PointsReg4,newdata=NBA_test)
PointsPredictions
SSE=sum((PointsPredictions - NBA_test$PTS)^2)
SST=sum((mean(NBA$PTS)- NBA_test$PTS)^2)
R2=1-SSE/SST
R2
RMSE=sqrt(SSE/nrow(NBA_test))
RMSE

