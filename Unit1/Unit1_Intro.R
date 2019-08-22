#Quizz unit 1
getwd()
setwd("C:/Users/DELL/Downloads/abdallah")
who=read.csv("WHO.csv")
who
print(mean(who$Over60))
print(who$Country[which.min(who$Over60)])

print(who$Country[which.max(who$LiteracyRate)])

tapply(who$ChildMortality,who$Region,mean)
