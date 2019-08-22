#unit 6 ~ 
#Abdallah DAABOUL 8/7/2019
#<><><><><><><><><><><><><><><><><><><><><><><><><>
setwd("C:/Users/DELL/Downloads/abdallah/Unit6")
movies=read.table("movieLens.txt",header=FALSE,sep="|",quote="\"")
str(movies)

colnames(movies)=c("ID","Title","ReleaseDate","VideoReleaseDate","IMDB","Unknown","Action","Adventure","Animation","Childrens","Comedy","Crime","Documentary","Drama","Fantasy","FilmNoir","Horror","Musical","Mystery","Romance","SciFi","Thiller","War","Western")
str(movies)
movies$ID=NULL
movies$ReleaseDate=NULL
movies$VideoReleaseDate=NULL
movies$IMDB=NULL
movies=unique(movies)
str(movies)
sum(movies$Comedy)
sum(movies$Western)
table(movies$Romance,movies$Drama)




#video7
distances=dist(movies[2:20],method="euclidean")
clusterMovies = hclust(distances, method = "ward.D")
plot(clusterMovies)
clusterGroups=cutree(clusterMovies,k=10)
tapply(movies$Action, clusterGroups, mean)

subset(movies,Title=="Men in Black (1997)")
clusterGroups[257]
cluster2 = subset(movies, clusterGroups==2)
cluster2$Title[1:10]


#recitation
flower = read.csv("flower.csv",header = FALSE)
str(flower)
#data to matrix
flowerMatrix=as.matrix(flower)

#matrix to vector

flowerVector=as.vector(flowerMatrix)
str(flowerVector)

distance=dist(flowerVector,method="euclidean")
clusterIntensity=hclust(distance,method="ward.D")
plot(clusterIntensity)

rect.hclust(clusterIntensity, k=3 , border="red")

#divide points into clusters
flowerClusters=cutree(clusterIntensity,k=3)
#mean intensity of points
tapply(flowerVector, flowerClusters, mean)

#convert  to a matrix
dim(flowerClusters)=d=c(50,50)

image(flowerClusters,axes=FALSE)

image(flowerMatrix,axes=FALSE,col=grey(seq(0,1,length=265)))


#MRI image
healthy = read.csv("healthy.csv", header= FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix)
image(healthyMatrix,axes=FALSE, col=grey(seq(0,1,length=256)))
healthyVector = as.vector(healthyMatrix)
distance=dist(healthyVector,method="euclidean")

#K clustering
k=5
set.seed(1)
KMC=kmeans(healthyVector,centers=k,iter.max=1000)
str(KMC)
healthyClusters=KMC$cluster
KMC$centers[2] # average intensity of cluster 2

dim(healthyClusters) = c(nrow(healthyMatrix),ncol(healthyMatrix))
image(healthyClusters,axes=FALSE,col = rainbow(k))


#Video6

tumor = read.csv("tumor.csv", header=FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)

# Apply clusters from before to new image, using the flexclust package
install.packages("flexclust")
library(flexclust)

#1st argument: method , 2nd argument: training Vector
KMC.kcca = as.kcca(KMC, healthyVector)

tumorClusters = predict(KMC.kcca, newdata = tumorVector)
# Visualize the clusters
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))

image(tumorClusters, axes = FALSE, col=rainbow(k))
