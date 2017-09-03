#Ensure that languages are displayed correctly not in UTF-8
Sys.setlocale("LC_ALL", "en_US.UTF-8")

Tweets = searchTwitter("??????????", since='2017-03-01', n=20)
readLines(Tweets,encoding='UTF-8')
Locs <- availableTrendLocations()
Egypt <- subset(Locs, country=="Egypt")
woeidEgypt <- subset(Egypt, name=="Cairo")$woeid
trends = getTrends(woeidEgypt)

trends$query


#Location Trend search
Locs <- availableTrendLocations()
UAE <- subset(Locs, country=="United Arab Emirates")
woeidUAE <- subset(UAE, name=="Dubai")$woeid
trends = getTrends(woeidUAE)


ATweets = searchTwitter("London", since='2017-03-22', n=20)

head(Tweets)
#writing a function for cleaning Arabic tweets

ATweets <- sapply(ATweets, function(x) x$getText())
cleantweets = clean_all_tweets(ATweets)

