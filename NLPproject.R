library(tm)
library(wordcloud)
library(syuzhet)
library(e1071)


reviews<-read.csv(file.choose(),header=T)
str(reviews)
#Set corpus
corpus<-iconv(reviews$text[-1])
corpus<-Corpus(VectorSource(corpus))
inspect(corpus[1:5])
#cleaning corpus
corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,removeNumbers)
corpus<-tm_map(corpus,removeWords, stopwords("english"))
corpus<-tm_map(corpus,removeWords, c("book","read","life"))
corpus<-tm_map(corpus,stripWhitespace)

inspect(corpus[1:5])

reviews_final<-corpus

#Term document

tdm<-TermDocumentMatrix(reviews_final)
tdm<-as.matrix(tdm)
tdm[1:1000,1:5]

w<-rowSums(tdm)
w<-subset(w,w>=50)
barplot(w, las=2,col="blue")

w<-sort(rowSums(tdm), decreasing = T)
set.seed(2000)
wordcloud(words=names(w),
          freq=w,
          max.words=50,
          random.order=T,
          min.freq=5,
          colors=brewer.pal(25,"Dark2"),
          scale=c(3,0.3))
 
sentiment_data<-iconv(reviews$text[-1])
s<-get_nrc_sentiment(sentiment_data)
s[1:15,]
s$score<-s$positive-s$negative
s$final<-s$score
s$final[s$score<=0] <- 0
s$final[s$score>0] <- 1
s[1:10,]


dt = sort(sample(nrow(s), nrow(s)*.7))
train<-s[dt,]
test<-s[-dt,]

#SVM MODEL

mymodel<-svm(final~., data=s,type='C')
summary(mymodel)
plot(mymodel,s,positive~negative,color.palette = topo.colors)
pred<-predict(mymodel,s)
table(Predicted=pred, Actual=s$final)
fact <- as.factor(s$final)
confusionMatrix(pred,fact)
