#unit 5 Predictive coding
#Abdallah DAABOUL 9/1/2019
#<><><><><><><><><><><><><><><><><><><><><><><><><>
setwd("C:/Users/DELL/Downloads/abdallah/Unit5")
emails=read.csv("energy_bids.csv", stringsAsFactors = FALSE)
str(emails)

emails$email[1]
strwrap(emails$email[1])
emails$responsive[1]

strwrap(emails$email[2])
emails$responsive[2]

table(emails$responsive)

library(tm)
corpus=VCorpus(VectorSource(emails$email))
strwrap(corpus[[1]])
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,content_transformer(tolower))
corpus=tm_map(corpus,removeWords,stopwords("english"))
corpus=tm_map(corpus,stemDocument)
strwrap(corpus[[1]])

dtm=DocumentTermMatrix(corpus)
dtm
dtm= removeSparseTerms(dtm,0.97)
labeledTerms=as.data.frame(as.matrix(dtm))
labeledTerms$responsive = emails$responsive
str(labeledTerms)


library(caTools)
set.seed(144)
spl = sample.split (labeledTerms$responsive, 0.7)
train= subset(labeledTerms,spl ==TRUE)
test = subset(labeledTerms,spl==FALSE)

library(rpart)
library(rpart.plot)
emailCART = rpart(responsive ~ . ,data=train , method="class")
prp(emailCART)

pred=predict(emailCART,newdata=test)
pred[1:10,]

pred.prob=pred[,2]
table(test$responsive,pred.prob>0.5)

#accuracy=(195+25)/(195+20+17+25) =0.856

#baseline
table(test$responsive)
#accuracy=215/257=0.83

#video7
library(ROCR)
predROCR=prediction(pred.prob,test$responsive)
perfROCR=performance(predROCR,"tpr","fpr")
plot(perfROCR,colorize=TRUE)
performance(predROCR,"auc")@y.values
