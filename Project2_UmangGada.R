library(twitteR)
library(ROAuth)
library(RCurl)
library(httr)

library(stringr)
library(plyr)
library(dplyr)
library(tm)
library(wordcloud)
library(rtweet)
library(ggplot2)

key="6Ibsq51RoXpy5fY8svGkVpJtK"
secret="lO9NsqmrFUyJJWi82647tt4Rd9uZ4eLREYpi2PKnZTY29gPq0i"

atoken =  "1892923902-hxbRyRtvsK8f6RqP9lbex9VhLQ87PgW8jwlNYa8"
asecret = "WQrCpY6QjwE1LKITWCZxoKHZ5SOquC5NxgumbhL2Pd9QW"

setup_twitter_oauth(key, secret, atoken, asecret)

tweets = searchTwitter("Champions League -filter:retweets", "from:user",n=1000, lan="en",
              until="2021-05-01", since = "2021-04-28",
              geocode="51.5020758,-0.142869,200mi",resultType = "")

tweettext = sapply(tweets, function(x) x$getText())

tweetdate=lapply(tweets, function(x) x$getCreated())

tweetdate=sapply(tweetdate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))

tweetdate

tweettext=lapply(tweettext, function(x) iconv(x, "latin1", 
                                              "ASCII", sub=""))
tweettext=lapply(tweettext, function(x) gsub("htt.*",' ',x))
tweettext=lapply(tweettext, function(x) gsub("#",'',x))
tweettext=unlist(tweettext)

pos = readLines("positive_words.txt")
neg = readLines("negative_words.txt")

sentimentfun = function(tweettext, pos, neg, .progress='non')
{
  scores = laply(tweettext,
                 function(singletweet, pos, neg)
                 {
                   singletweet = gsub("[[:punct:]]", "", singletweet)
                   singletweet = gsub("[[:cntrl:]]", "", singletweet)
                   singletweet = gsub("\\d+", "", singletweet)
                  
                   tryTolower = function(x)
                   {
                     y = NA
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     return(y)
                   }
                   singletweet = sapply(singletweet, tryTolower)
                   
                   word.list = str_split(singletweet, "\\s+")
                   words = unlist(word.list)
                   
                   pos.matches = match(words, pos)
                   neg.matches = match(words, neg)
                   
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos, neg, .progress=.progress )

  sentiment.df = data.frame(text=tweettext, score=scores)
  return(sentiment.df)
}

scores = sentimentfun(tweettext, pos, neg, .progress='text')

tweetdate=lapply(tweets, function(x) x$getCreated())
tweetdate=sapply(tweetdate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))

isretweet=sapply(tweets, function(x) x$getIsRetweet())

retweetcount=sapply(tweets, function(x) x$getRetweetCount())

favoritecount=sapply(tweets, function(x) x$getFavoriteCount())

data=as.data.frame(cbind(ttext=tweettext,
                         date=tweetdate,
                         isretweet=isretweet, 
                         retweetcount=retweetcount,
                         favoritecount=favoritecount,
                         score = scores)
)

data2 = duplicated(data[,1])

data$duplicate = data2

write.csv(data, file= "data.csv")
tweet=read.csv("data.csv")
attach(tweet)
names(tweet)

tweetcleandata=read.csv("data.csv")
names(data)
attach(data)

wordcloud(ttext, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

length(tweetcleandata$duplicate[tweetcleandata$duplicate==TRUE])
length(tweetcleandata$isretweet[tweetcleandata$isretweet==TRUE])

hist(tweetcleandata$favoritecount,breaks=500, xlim=range(0,1000), xlab="Favorite Count", main="Favorite count")
hist(tweetcleandata$score.score, breaks=1, xlim=range(-10,10), xlab="Score", main="Score range")

library(syuzhet)

ew_sentiment<-get_nrc_sentiment((tweetcleandata$score.text))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()
