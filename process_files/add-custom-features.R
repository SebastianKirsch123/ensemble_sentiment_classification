# Process the file of tweets further by adding custom features

Sdata = Sdata[!duplicated(Sdata$Tweet),]



# need to pass the whole dataframe, because sentiments corresponding to Tweets
# also need to be removed
# pass a specific dataset to Tweetdf
Tweetdf = clean_all_tweets_df(Sdata)


Tweetdf = add_distance_subj_scores(Tweetdf)
Tweetdf = add_Tweet_lengths(Tweetdf)
Tweetdf = add_word_lengths(Tweetdf)
Tweetdf = add_lexicon_scores(Tweetdf)


write.csv(Tweetdf, file = "election_feature_enhanced.csv")

write.csv(Tweetdf, file = "mich_feature_enhanced.csv")
# Following variables are all 0
Sdata$POS = NULL
Sdata$WP. = NULL
write.csv(Sdata, file = "stanford_feature_enhanced.csv")


#reduce further so it can be trained in reasonable time
Tweetdf = Sdata[c(3678:5678),]
Tweetdf$X = NULL
write.csv(Sdata, file = "red_stanford_feature_enhanced.csv")
