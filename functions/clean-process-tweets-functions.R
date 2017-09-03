catch.error = function(x)
{
    y = NA
    catch_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(catch_error, "error"))
        y = tolower(x)
    return(y)
}


# inspired by https://stackoverflow.com/questions/12403312/find-the-number-of-spaces-in-a-string
countSpaces <- function(s) { sapply(gregexpr(" ", s), function(p) { sum(p>=0) } ) }


# remove all unnecessary expressions from one tweet
cleanTweets = function(tweet)
{
    tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)"," ",tweet)
    tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)"," ",tweet)
    tweet = gsub("#\\w+"," ",tweet)
    tweet = gsub("@\\w+", " ",tweet)
    tweet = gsub("[[:punct:]]", " ",tweet)
    tweet = gsub("[[:digit:]]", " ",tweet)
    tweet = gsub("[\t]{2,}", " ",tweet)
    tweet = gsub("^\\s+|\\s+$", "",tweet)
    tweet = catch.error(tweet)
    tweet
}

# remove NAs and clean Tweetlist
clean_all_tweets <- function(Tweets){
    TweetsCleaned = sapply(Tweets, cleanTweets)
    TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
        names(TweetsCleaned) = NULL
    TweetsCleaned = unique(TweetsCleaned)
    TweetsCleaned = TweetsCleaned[nchar(TweetsCleaned)>15]
    TweetsCleaned = TweetsCleaned[countSpaces(TweetsCleaned)]
    TweetsCleaned
}




clean_all_tweets_df <- function(Edata){
  
  Edata$Tweet = sapply(Edata$Tweet, cleanTweets)
  TweetsCleaned = Edata[!is.na(Edata$Tweet),]
  names(TweetsCleaned$Tweet) = NULL
  TweetsCleaned = TweetsCleaned[!duplicated(TweetsCleaned$Tweet),]
  TweetsCleaned = TweetsCleaned[nchar(TweetsCleaned$Tweet)>15,]
  TweetsCleaned = TweetsCleaned[countSpaces(TweetsCleaned$Tweet) > 1,]
  TweetsCleaned
}



# load sentiment lexicons


lexicon.pos = scan('opinion-lexicon-English/positive-words.txt', 
                   what = 'character', comment.char=';')
lexicon.neg = scan('opinion-lexicon-English/negative-words.txt', 
                   what = 'character', comment.char=';')




# score_sentiment

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}


# add POS tags to tweets
tagPOS <-  function(x, ...) {
  s <- as.String(x)
  s = gsub(",", "", s)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- NLP::annotate(s, word_token_annotator, a2)
  a3 <- NLP::annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  Posdf = data.frame(POStags, s[a3w])
  return(Posdf)
}


# count tags
add_POStag_counts = function(Tweetdf)
{
  tags = Penn_Treebank_POS_tags$entry[-c(1:9)]
  for(i in 1:length(tags)){
    new_col = rep(0, nrow(Tweetdf))
    Tweetdf = cbind(Tweetdf, new_col)
    colnames(Tweetdf)[ncol(Tweetdf)] =tags[i]
  }
  
  Tweets = as.character(Tweetdf$Tweet)
  
  
  for (i in 1:length(Tweets))
  {
    tw = Tweets[i]
    
    # find most frequent terms
    terms = tagPOS(tw)
    twtags = as.vector(unique(terms$POStags))
    
    for(j in twtags)
    {
      Tweetdf[i,j] = length(grep(j, terms$POStags))
    }
  }
  Twee = Tweetdf[,-c((ncol(Tweetdf)-4):ncol(Tweetdf))]
  return(Tweetdf)
}
  





# calculate score based on distance of negative and positive adjectives from
# nouns in Tweets and append it to dataframe as a new column 


add_distance_subj_scores = function(Tweetdf)
{
  Tweets = as.character(Tweetdf$Tweet)
  
  
  dist.score = rep(NA, length(Tweets))
  subj.score = rep(NA, length(Tweets))
  
  
  for (i in 1:length(Tweets))
  {
    tw = Tweets[i]
    
    # find most frequent terms
    terms = tagPOS(tw)
    
    #get freq nouns
    nouns = terms[terms$POStags=="NN" | terms$POStags=="NNS", ]$s.a3w.
    
    #get freq adjectives
    adv_adj = terms[terms$POStags=="JJ" | terms$POStags=="JJR"
                    | terms$POStags=="JJS" | terms$POStags=="RB", ]$s.a3w.
    
    
    #get freq adjectives
    prp_adj = terms[terms$POStags=="JJ" | terms$POStags=="JJR"
                    | terms$POStags=="JJS" | terms$POStags=="PRP"
                    | terms$POStags=="RB"| terms$POStags=="RBR"
                    | terms$POStags=="RBS", ]$s.a3w.
    
    subscore = length(nouns) - length(prp_adj)
    subj.score[i] = subscore
    
    
    # get sentiment
    word.sent = score.sentiment(adv_adj, lexicon.pos, lexicon.neg)
    
    
    # get word distance
    words <- unique(unlist(strsplit(tw, split=" ")))
    indices = 1:length(words)
    names(indices) = words
    
    
    score = 0
    
    for (n in nouns)
    {
      for (a in adv_adj)
      {
        dist = abs(indices[n] - indices[a])
        score = score + (1/dist) * word.sent[word.sent$text == a, ]$score
      }
    }
    
    
    dist.score[i] = score
  }
  
  dist.score[is.na(dist.score)] = 0
  Tweetdf$dist.score = dist.score
  subj.score[is.na(subj.score)] = 0
  Tweetdf$subj.score = subj.score
  
  return(Tweetdf)
}



# add a column of Tweet lengths to dataframe

add_Tweet_lengths = function(Tweetdf)
{
  Tweetlength = rep(NA, length(Tweetdf$Tweet))
  Tweets = as.character(Tweetdf$Tweet)
  
  
  for ( i in 1:length(Tweets))
  {
    Tweetlength[i] = length(unlist(strsplit(Tweets[i], split=" ")))
  }
  Tweetdf$Tweetlength = Tweetlength
  return(Tweetdf)
}



#add sentiment scores determined by sentiment lexicon
add_lexicon_scores = function(Tweetdf)
{
  
  Tweets = as.character(Tweetdf$Tweet)
  lexicon.score = rep(NA, length(Tweets))
  
  for (i in 1:length(Tweets))
  {
    tw = Tweets[i]
    tw = unlist(strsplit(tw, split=" "))
    
    word.sent = score.sentiment(tw, lexicon.pos, lexicon.neg)
    lexicon.score[i] =  sum(word.sent$score)
  }  
  Tweetdf$lexicon.score = lexicon.score 
  return(Tweetdf)
}



# calculate word lengths
add_word_lengths = function(Tweetdf)
{
  Tweets = as.character(Tweetdf$Tweet)
  word.length = rep(NA, length(Tweets))
  
  for (i in 1:length(Tweets))
  {
    tw = Tweets[i]
    tw = unlist(strsplit(tw, split=" "))
    word.length[i] = sum(sapply(tw, function(x) nchar(x) ))/length(tw)
  }  
  Tweetdf$word.length = word.length
  return(Tweetdf)
}

# code from https://stackoverflow.com/questions/28764056/could-not-find-function-tagpos
tagPOS <-  function(x, ...) {
  s <- as.String(x)
  s = gsub(",", "", s)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- NLP::annotate(s, word_token_annotator, a2)
  a3 <- NLP::annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  Posdf = data.frame(POStags, s[a3w])
  return(Posdf)
}

addPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- NLP::annotate(s, word_token_annotator, a2)
  a3 <- NLP::annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  return(POStagged)
}

Tags_to_Tweets = function(Tweets)
{
  Tweets =  as.character(Sdata$Tweet)
  newtweets = rep(NA, length(Tweets))
  for(i in 1:length(Tweets)){
  newtweets[i] = addPOS(Tweets[i]) 
  }
  return(newtweets)
}

