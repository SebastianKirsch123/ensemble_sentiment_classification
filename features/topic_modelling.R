library(topicmodels)
library(ldatuning)
library(cluster)
library(slam)
library(wordcloud)
library(tm)


# load required data
Sdata = read.csv("stanford_feature_enhanced.csv", header = T)
Sdata$X = NULL


reddata = Sdata
# convert tp vector corpus representation
myCorpus <- Corpus(VectorSource(reddata$Tweet))

# remove urls and punctuation
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

myCorpus = tm_map(myCorpus, tolower)
myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))
myCorpus = tm_map(myCorpus, stripWhitespace)
# following step is required for the function to run on a MAC OS
if(Sys.info()["sysname"] == "Darwin" | Sys.info()["sysname"] == "Linux") {myCorpus = tm_map(myCorpus, PlainTextDocument)}

tdm = DocumentTermMatrix(myCorpus)
tdm

# When running on the cluster need to save tdm here
save(tdm, file = "termdocm.RData")

#redtdm = removeSparseTerms(tdm, 0.999)
#results in reduction from 4229 to 1454 terms





# inspiration for the following code came from here: 
# https://www.r-bloggers.com/topic-modeling-in-r-2/

# calculate term frequency inverse document frequency
term_tfidf <- tapply(tdm$v/row_sums(tdm)[tdm$i], tdm$j, mean) * log2(nDocs(tdm)/col_sums(tdm > 0))
summary(term_tfidf)

# remove terms whose frequency is below 0.1
tdm <- tdm[,term_tfidf > 0]
# Tweets that do not contain frequent terms need to be filtered out. and removed from the
# dataframe. This requires getting their index
ind = row_sums(tdm) == 0
sparsetw = as.vector(which(ind == T))
reddata = reddata[-sparsetw, ]

# remove sparse terms from tdm
tdm <- tdm[row_sums(tdm) > 0,]
summary(col_sums(tdm))


# determine ideal number of topics
best.model <- lapply(seq(5, 250, by = 5), function(d){LDA(tdm, d)})
best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
plot(c(1:49), best.model.logLik$V1, type = "l")





# do LDA on 11 topics
lda_score =  LDA(tdm, 20)
lda_tops = topics(lda_score)
reddata$topics = lda_tops
