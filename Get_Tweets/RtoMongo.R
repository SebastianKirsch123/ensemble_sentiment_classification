library(rmongodb)
library(jsonlite)
library(twitteR)

# Connect to Mongo DB
mongo = mongo.create(host = "localhost")
mongo.is.connected(mongo)

#mongo.get.databases(mongo)
#mongo.count(mongo, ns = "tweetDB2.FrenchTweets")

#Get and process tweets (using custom functions)
ATweets = searchTwitter("covfefe", since='2017-05-22', lang = "en", n=1000)
ATweets <- sapply(ATweets, function(x) x$getText())
cleantweets = clean_all_tweets(ATweets)

# write to MongoDB
for (i in 1:length(cleantweets)){
    bson <- mongo.bson.from.JSON(toJSON(cleantweets[i]))
    mongo.insert(mongo, "tweetDB2.EnglishTweets", bson)
}



# read from MongoDB
EnglishTweets = mongo.find.all(mongo,"tweetDB2.EnglishTweets",data.frame=TRUE)
EnglishTweets$`_id` = NULL


colnames(EnglishTweets) = "Tweet"
cleanedTweets = clean_all_tweets(EnglishTweets)


Engl = EnglishTweets$Tweet
 
fileConn<-file("Eng.txt")
writeLines(Engl, fileConn)
close(fileConn)


write.csv(EnglishTweets, file = "EnglishTweets.csv")
head(EnglishTweets)