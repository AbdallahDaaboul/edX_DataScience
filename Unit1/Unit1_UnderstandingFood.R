#unit 1 ~ Understanding food 
#<><><><><><><><><><><><><><><><><><><><><><>

#Video3
getwd()
setwd("C:/Users/DELL/Downloads/abdallah")
USDA=read.csv("USDA.csv")
str(USDA)
summary(USDA)
which.max(USDA$Sodium)
names(USDA)
USDA$Description[265]

#To get all the items with sodium > 10000
HighSodium=subset(USDA,Sodium > 10000)

#nbr of items with this amount of sodium
nrow(HighSodium)

#To find the position of a specific item
match("CAVIAR",USDA$Description)
#Sodium in caviar
USDA$Sodium[4154]

#mean of sodium in all items
summary(USDA$Sodium,mean)
#Or mean(USDA$Sodium,na.rm=TRUE)

#Standart deviation(ecartype)
sd(USDA$Sodium,na.rm=TRUE)

#<><><><><><><><><><><><><><><><><><><><><><>
##video 4

plot(USDA$Protein,USDA$TotalFat,xlab="Protein",ylab="Fat",main="Protein Vs Fat",col="red")
#Food with high proteine are lower in fat 

#visulize the frequence of vitaminC values
hist(USDA$VitaminC,xlab="VitaminC",main="histogram of vitaminC levels",xlim=c(0,100),breaks=2000)
#breaks is to divide my sequence (avoir des intervalles plus petits)

boxplot(USDA$Sugar,main="Sugar levels",ylab="Sugar in grams")
mean(USDA$Sugar,na.rm=TRUE)
#we can see that the mean is around 5g and there are too many outliers.

#<><><><><><><><><><><><><><><><><><><><><><>
##video 5

#add a new variable to our data frame, 1 if higher than sodium average 0 if not
USDA$HighSodium=as.numeric(USDA$Sodium>mean(USDA$Sodium,na.rm=TRUE))
 #as.numeric to transform logical to numerical (true -> 1 ...)
 #HighSodium isnt a variable of USDA so it will be added automatically to my dataframe
#Do the same for Protein and fat
USDA$HighProtein=as.numeric(USDA$Protein>mean(USDA$Protein,na.rm=TRUE))
USDA$HighFat=as.numeric(USDA$TotalFat>mean(USDA$TotalFat,na.rm=TRUE))
USDA$HighCarbs=as.numeric(USDA$Carbohydrate>mean(USDA$Carbohydrate,na.rm=TRUE))

str(USDA)

#<><><><><><><><><><><><><><><><><><><><><><>
##video 6

#Visulize how many items are higher than the average in sodium
  table(USDA$HighSodium)

#Visulize how many items are higher than the average in sodium and fat
  table(USDA$HighSodium,USDA$HighFat)
  #3529 low sodium low fat, 1355 low sodium high fat ...
  
#The average of amount of iron sorted by high and low protein
  tapply(USDA$Iron,USDA$HighProtein,mean,na.rm=TRUE)
  
#The max value of VitaminC sorted by high and low carbs
  tapply(USDA$VitaminC,USDA$HighCarbs,max,na.rm=TRUE)

#Summary of VitaminC sorted by high and low carbs
  tapply(USDA$VitaminC,USDA$HighCarbs,summary,na.rm=TRUE)

  #We can see that in general, on average, food with higher carbs are richer in VitaminC  