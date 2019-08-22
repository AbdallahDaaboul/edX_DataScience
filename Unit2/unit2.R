  
setwd("C:/Users/DELL/Downloads/abdallah/Unit2")
wine=read.csv("wine.csv")
str(wine)
#linear regression with one variable
model1=lm(Price ~ AGST, data=wine)
summary(model1)

#to see the sse "sum of squared errors"
model1$residuals #give us the error of each point
SSE=sum(model1$residuals^2)

model2=lm(Price ~ AGST + HarvestRain, data=wine )
summary(model2)
SSE=sum(model2$residuals^2)

model3=lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine )
summary(model3)
SSE=sum(model3$residuals^2)


model4=lm(Price ~ HarvestRain + WinterRain , data=wine )
summary(model4)
SSE=sum(model4$residuals^2)

model5=lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine )
summary(model5)
SSE=sum(model5$residuals^2)


cor(wine$WinterRain,wine$Price)
cor(wine$WinterRain,wine$HarvestRain)

WineTest=read.csv("Wine_test.csv")
str(WineTest)

PredictTest=predict(model5,newdata=WineTest)
PredictTest

SSE=sum((WineTest$Price- PredictTest)^2)
SST=sum((WineTest$Price - mean(wine$Price))^2)
1-SSE/SST
