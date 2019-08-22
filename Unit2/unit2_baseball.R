
setwd("C:/Users/DELL/Downloads/abdallah/Unit2")
baseball=read.csv("baseball.csv")
str(baseball)
MoneyBall=subset(baseball,Year<2002)
str(MoneyBall)
MoneyBall$RD=MoneyBall$RS - MoneyBall$RA
str(MoneyBall)
plot(MoneyBall$RD,MoneyBall$W)
WinsReg=lm(W ~ RD , data=MoneyBall)
summary(WinsReg)
#We can extract the wins equation in function of RD and then put the equation > 95
#To get the Rd value to win more than 95 games 
#we get 135
713


#Video3

str(MoneyBall)
Runs=lm(RS ~ OBP+SLG, data=MoneyBall)
summary(Runs)

summary(MoneyBall)

Runs2=lm(RA ~ OOBP+OSLG, data=MoneyBall)
summary(Runs2)
  

#video5
teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)

