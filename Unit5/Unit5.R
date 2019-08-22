#unit 5
#Abdallah DAABOUL 9/1/2019
#<><><><><><><><><><><><><><><><><><><><><><><><><>

#Video6
getwd()
setwd("C:/Users/DELL/Downloads/abdallah/Unit5")
tweets=read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)

tweets$Negative = as.factor(tweets$Avg<=-1)
table(tweets$Negative)
install.packages("tm")
install.packages("SnowballC")
library(tm)
library(SnowballC)

# Create corpus
corpus = VCorpus(VectorSource(tweets$Tweet)) 

# Look at corpus
corpus

corpus[[1]]$content

#lowercase
corpus = tm_map(corpus, content_transformer(tolower))
corpus[[1]]$content

#remove punctuation
corpus=tm_map(corpus,removePunctuation)
corpus[[1]]$content

#to see english stopwords
stopwords("english")[1:11]
corpus= tm_map(corpus,removeWords,c("apple",stopwords("english")))
corpus[[1]]$content

#stem the document
corpus=tm_map(corpus,stemDocument)
corpus[[1]]$content


#word frequencies

frequencies = DocumentTermMatrix(corpus)
frequencies
#to visulize them
inspect(frequencies[1000:1005,505:515])

findFreqTerms(frequencies, lowfreq = 20)

#remove inutil words
sparse = removeSparseTerms(frequencies,0.995)

#convert our reduced data  to a dataFrame
tweetsSparse=as.data.frame(as.matrix(sparse))
str(tweetsSparse)
summary(tweetsSparse)

#since R struggle with names beginning with a number
#we can solve this prblm with this commmand
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
tweetsSparse$Negative=tweets$Negative
str(tweetsSparse)
summary(tweetsSparse)

#Dividing our set to train and test dataframes
library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)

#Video7 

#CART Prediction 
library(rpart)
library(rpart.plot)
tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")
prp(tweetCART)

predictCART=predict(tweetCART,newdata = testSparse,type="class")
table(testSparse$Negative, predictCART)
#Accuracy=0.87
  #baseline model
table(testSparse$Negative)
#Accuracy= 300/355=0.84

#randomForest Model

library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative ~ ., data=trainSparse)
# Make predictions:
predictRF = predict(tweetRF, newdata=testSparse)
table(testSparse$Negative, predictRF)
# Accuracy:
(293+21)/(293+7+34+21)
#0.8845

#So RF model is slightly better than CART but regarding interpretability
# we may prefer cart model. Plus if we input the best cp we ll get an
# accuracy at RF's level.


