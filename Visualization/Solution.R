#unit 7
#Abdallah DAABOUL 7/10/2019
#<><><><><><><><><><><><><><><><><><><><><><><><><>

getwd()
setwd("C:/Users/DELL/Downloads/abdallah/Unit7")

WHO=read.csv("WHO.csv")
str(WHO)
plot(WHO$GNI, WHO$FertilityRate)

install.packages("ggplot2")
library(ggplot2)
scatterplot=ggplot(WHO,aes(x=GNI,y=FertilityRate))
scatterplot+geom_line()
scatterplot+geom_point()
scatterplot+geom_point(color="blue",size=3,shape=15)
fertilityGNIplot = scatterplot+geom_point(color="blue",size=3,shape=17)+ ggtitle("FertilityRate vs GNI")
pdf("Myplot.pdf")
print(fertilityGNIplot)
dev.off()

ggplot(WHO,aes(x=GNI , y=FertilityRate, col=LifeExpectancy)) + geom_point()

ggplot(WHO,aes(x= FertilityRate, y=Under15)) + geom_point()

ggplot(WHO,aes(x= log(FertilityRate), y=Under15)) + geom_point()

model=lm(Under15 ~ log(FertilityRate),data=(WHO))

ggplot(WHO,aes(x= log(FertilityRate), y=Under15)) + geom_point() + stat_smooth(method=lm,level=0.99,col='orange')


ggplot(WHO,aes(x= log(FertilityRate), y=Under15)) + geom_point() + stat_smooth(method=lm,se=FALSE,col='orange')


ggplot(WHO, aes(x = FertilityRate, y = Under15,col=Region)) + geom_point() + scale_color_brewer(palette="Dark2")



#The analytic Policeman

mvt = read.csv("mvt.csv", stringsAsFactors = FALSE)
str(mvt)
mvt$Date = strptime(mvt$Date, format="%m/%d/%y %H:%M")
mvt$Weekday=weekdays(mvt$Date)
mvt$Hour= mvt$Date$hour

str(mvt)
table(mvt$Weekday)
WeekdayCounts = as.data.frame(table(mvt$Weekday))
str(WeekdayCounts)
library(ggplot2)
ggplot(WeekdayCounts, aes(x=Var1,y=Freq)) + geom_line(aes(group=1),linetype=1,alpha=0.3) #group=1 so we have one line grouping all points
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE, levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

ggplot(WeekdayCounts, aes(x=Var1,y=Freq)) + geom_line(aes(group=1)) #group=1 so we have one line grouping all points

ggplot(WeekdayCounts, aes(x=Var1,y=Freq)) + geom_line(aes(group=1)) + xlab("Day of the week") +ylab("Total motor Vehicle Thefts")

table(mvt$Weekday, mvt$Hour)
DayHourCounts=as.data.frame(table(mvt$Weekday, mvt$Hour))
str(DayHourCounts)

DayHourCounts$Hour=as.numeric(as.character(DayHourCounts$Var2)) #factor variable to numeric
ggplot(DayHourCounts,aes(x=Hour,y=Freq))+ geom_line(aes(group=Var1,color=Var1),size=1.5)
DayHourCounts$Var1=factor(DayHourCounts$Var1,ordered=TRUE,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
ggplot(DayHourCounts,aes(x=Hour,y=Var1)) + geom_tile(aes(fill=Freq)) #geom tile ->heat map
ggplot(DayHourCounts,aes(x=Hour,y=Var1)) + geom_tile(aes(fill=Freq)) + scale_fill_gradient(name="Total MV Thefts") + theme(axis.title.y = element_blank())
ggplot(DayHourCounts,aes(x=Hour,y=Var1)) + geom_tile(aes(fill=Freq)) + scale_fill_gradient(name="Total MV Thefts",low="white",high = "red") + theme(axis.title.y = element_blank())

#video5

install.packages("maps")
install.packages("ggmap")
library(maps)
library(ggmap)


if(!requireNamespace("devtools"))
  install.packages("devtools") 
devtools::install_github("dkahle/ggmap", ref = "tidyup", force = TRUE)

register_google(key = "AIzaSyDt11sVGsOURWI3A_kjBRkcSvlmpyR6qho")
chicago=get_map(location = "chicago",zoom=11)

str(mvt)
table(mvt$Latitude,mvt$Longitude)
LatLonCounts= as.data.frame(table(round(mvt$Longitude,2),round(mvt$Latitude,2)))
str(LatLonCounts)
LatLonCounts$Long=as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat=as.numeric(as.character(LatLonCounts$Var2))
sum(LatLonCounts$Freq>0)-1638
#Or LatLonCounts2 = subset(LatLonCounts, Freq > 0)
#str(LatLonCounts2)



#video6

murders=read.csv("murders.csv")
statesMap=map_data("state")
str(statesMap)
ggplot(statesMap,aes(x=long,y=lat,group=group)) + geom_polygon(fill="white",color="black")

murders$region=tolower(murders$State)
murderMap=merge(statesMap,murders,by="region")

ggplot(murderMap,aes(x=long,y=lat,group=group,fill=Murders)) +geom_polygon(color="black") +scale_fill_gradient(low="black",high="red",guide="legend")
ggplot(murderMap,aes(x=long,y=lat,group=group,fill=Population)) +geom_polygon(color="black") +scale_fill_gradient(low="black",high="red",guide="legend")

#we visulized murders and popultion , it doesnt make sense we should represent murders/population
murderMap$MurderRate= murderMap$Murders / murderMap$Population * 100000
ggplot(murderMap,aes(x=long,y=lat,group=group,fill=MurderRate)) +geom_polygon(color="black") +scale_fill_gradient(low="black",high="red",guide="legend",limits=c(0,10))



#Recitation

#video3
# Load our data, which lives in intl.csv
intl = read.csv("intl.csv")
str(intl)
ggplot(intl, aes(x=Region, y=PercentOfIntl)) +
  geom_bar(stat="identity") + #use the y variable as it is
  geom_text(aes(label=PercentOfIntl)) # labeling

#re-order command and transform command based on percentages not alahabetical order 
intl = transform(intl, Region = reorder(Region, -PercentOfIntl))
# Make the percentages out of 100 instead of fractions
intl$PercentOfIntl = intl$PercentOfIntl * 100

ggplot(intl, aes(x=Region, y=PercentOfIntl)) +
  geom_bar(stat="identity", fill="dark blue") + #use the y variable as it is
  geom_text(aes(label=PercentOfIntl), vjust=-0.4) + # adjust labels position
  ylab("Percentage of international students") +
  theme(axis.title.x = element_blank(),axis.text.x = element_text(angle=45,hjust=1))

# VIDEO 5 - World map
install.packages('mapproj')   
library(ggmap)
library(ggplot2)
intlall = read.csv("intlall.csv",stringsAsFactors=FALSE)
head(intlall)
intlall[is.na(intlall)] = 0
world_map=map_data("world")
str(world_map)

# Lets merge intlall into world_map using the merge command
world_map = merge(world_map, intlall, by.x ="region", by.y = "Citizenship")
str(world_map)
ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", color="black") +
  coord_map("mercator")

world_map= world_map[order(world_map$group,world_map$order),]


ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") +
  coord_map("mercator")


# We can try other projections - this one is visually interesting
ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") +
  coord_map("ortho", orientation=c(20, 30, 0))

ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") +
  coord_map("ortho", orientation=c(-37, 175, 0))



# VIDEO 7 - Line Charts

# First, lets make sure we have ggplot2 loaded
library(ggplot2)

# Now lets load our dataframe
households = read.csv("households.csv")
str(households)

# Load reshape2
library(reshape2)

# Lets look at the first two columns of our households dataframe
households[,1:2]

# First few rows of our melted households dataframe
head(melt(households, id="Year"))

households[,1:3]

melt(households, id="Year")[1:10,3]
melt(households, id="Year")[1:10,]

# Plot it
ggplot(melt(households, id="Year"),       
       aes(x=Year, y=value, color=variable)) +
  geom_line(size=2) + geom_point(size=5) +  
  ylab("Percentage of Households")

# First, lets make sure we have ggplot2 loaded
library(ggplot2)

# Now lets load our dataframe
households = read.csv("households.csv")
str(households)

# Load reshape2
library(reshape2)

# Lets look at the first two columns of our households dataframe
households[,1:2]

# First few rows of our melted households dataframe
head(melt(households, id="Year"))

households[,1:3]

melt(households, id="Year")[1:10,3]
melt(households, id="Year")[1:10,]

# Plot it
ggplot(melt(households, id="Year"),       
       aes(x=Year, y=value, color=variable)) +
  geom_line(size=2) + geom_point(size=5) +  
  ylab("Percentage of House