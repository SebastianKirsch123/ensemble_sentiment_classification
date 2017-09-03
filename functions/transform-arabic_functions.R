# read stopwords from file
con = file("arabic_stop_words.txt", open = "r")
lines = readLines(con, encoding = "UTF-8")
stopw = rep(NA, length(lines))

for (i in 1:length(lines)){
    stopw[i] = unlist(strsplit(lines[i], "\t"))
}
close(con)


#since the corpus clean functions don't  work on Arabic letters, we need
# to use our own custom clean function defined in "file_level_functions".
reddata$Tweet = sapply(reddata$Tweet, cleanTweets)


# stopwords were taken from http://www.ranks.nl/stopwords/arabic
reddata$Tweet = sapply(reddata$Tweet, function(tw)
{
    tw = strsplit(tw, " ")
    tw = unlist(tw)
    tw = tw[!tw %in% stopw]
    tw = paste(tw, collapse = " ")
})
reddata$Tweet = as.vector(reddata$Tweet)

# convert tp vector corpus representation


get_ar_onegram_mat = function(reddata){
    Tweets = reddata$Tweet
    
    myCorpus <- Corpus(VectorSource(Tweets))

    #Arabic stopwords list from: https://github.com/mohataher/arabic-stop-words/blob/master/list.txt

    #myCorpus = tm_map(myCorpus, stripWhitespace)
    # following step is required for the function to run on a MAC OS
    if(Sys.info()["sysname"] == "Darwin" | Sys.info()["sysname"] == "Linux") {myCorpus = tm_map(myCorpus, PlainTextDocument)}
   # myCorpus = tm_map(myCorpus, stemDocument)
    
    tdm = DocumentTermMatrix(myCorpus)
    # need to reduce matrix as R throws an error otherwise
    # at 200.000 rows a max of 4000-5000 cols is ok.
    redtdm = removeSparseTerms(tdm, 0.999)
    nTerms(redtdm)
    
    ar.onegram.matrix = as.matrix(redtdm)
    return(ar.onegram.matrix)
}



get_ar_bigram_mat = function(reddata){
    # convert tp vector corpus representation
    myCorpus <- VCorpus(VectorSource(reddata$Tweet))

    #create tokenizer function
    BiTok<- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
    #need to specify the number of threads in the parallel library when
    # running on a Mac
    options(mc.cores=1)
    #
    tdm <- DocumentTermMatrix(myCorpus,control=list(tokenize=BiTok))
    
    # need to reduce matrix as R throws an error otherwise
    # at 200.000 rows a max of 4000-5000 cols is ok.
    redtdm = removeSparseTerms(tdm, 0.999)
    nTerms(redtdm)
    
    ar.bigram.matrix = as.matrix(redtdm)
    return(ar.bigram.matrix)
}



get_trigram_mat = function(reddata){
    # convert tp vector corpus representation
    myCorpus <- VCorpus(VectorSource(reddata$Tweet))
    # remove urls and punctuation

    #create tokenizer function
    TriTok<- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
    #need to specify the number of threads in the parallel library when
    # running on a Mac
    options(mc.cores=1)
    #
    tdm <- DocumentTermMatrix(myCorpus,control=list(tokenize=TriTok))
    
    # need to reduce matrix as R throws an error otherwise
    # at 200.000 rows a max of 4000-5000 cols is ok.
    redtdm = removeSparseTerms(tdm, 0.999)
    nTerms(redtdm)
    
    ar.trigram.matrix = as.matrix(redtdm)
    return(ar.trigram.matrix)
}