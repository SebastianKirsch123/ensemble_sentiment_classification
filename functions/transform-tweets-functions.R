# This is required for the POS tagger not to run out of memory
options(java.parameters = "- Xmx3000m")
library(readxl)
library(tm)
library(rpart)
library(e1071)
library(RTextTools)
library(RWeka)
library(SnowballC)
library(caret)
library(NLP)
library(openNLP)
library(stringr)
library(plyr)



nacleaner = function(reddata)
{
  for(i in 3:ncol(reddata))
  {
    print(is.numeric(reddata[,i]))
    reddata = reddata[!is.infinite(reddata[,i]),]
    reddata = reddata[!is.na(reddata[,i]),]
    reddata = reddata[!is.nan(reddata[,i]),]
  }
  return(reddata)
}


# transform to one word document term matrix
get_onegram_mat = function(reddata){
 
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
  myCorpus = tm_map(myCorpus, stemDocument)

  tdm = DocumentTermMatrix(myCorpus)
  # need to reduce matrix as R throws an error otherwise
  # at 200.000 rows a max of 4000-5000 cols is ok.
  redtdm = removeSparseTerms(tdm, 0.999)
  nTerms(redtdm)
  
  onegram.matrix = as.matrix(redtdm)
  return(onegram.matrix)
}





# transform to two word document term matrix
get_bigram_mat = function(reddata){
  # convert tp vector corpus representation
  myCorpus <- VCorpus(VectorSource(reddata$Tweet))
  # remove urls and punctuation
  removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
  myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
  myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
  myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))
  myCorpus = tm_map(myCorpus, stripWhitespace)
  myCorpus = tm_map(myCorpus, stemDocument)
  
  
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
  
  bigram.matrix = as.matrix(redtdm)
  return(bigram.matrix)
}



# transform to three word document term matrix
get_trigram_mat = function(reddata){
  # convert tp vector corpus representation
  myCorpus <- VCorpus(VectorSource(reddata$Tweet))
  # remove urls and punctuation
  removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
  myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
  myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
  myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))
  myCorpus = tm_map(myCorpus, stripWhitespace)
  myCorpus = tm_map(myCorpus, stemDocument)
  
  
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
  
  trigram.matrix = as.matrix(redtdm)
  return(trigram.matrix)
}





# join matrices
get_full_mat = function(one = onegram.matrix, bi = bigram.matrix, tri = trigram.matrix)
{
  if(missing(tri)){
    full.matrix = cbind(onegram.matrix, bigram.matrix)
  }
  else{
    full.matrix = cbind(onegram.matrix, bigram.matrix, trigram.matrix)
  }
  
  return(full.matrix)
}



# create object containing train-, test- data and response variables.
create_learning_data = function(reddata, full.matrix){
  
  sent = as.factor(reddata$Sentiment)
  spars = cbind(full.matrix, sent)
  colnames(spars)[ncol(spars)] = "Sentiment"
  
  #convert to data frame
  # sparsdf = as.data.frame(spars)
  
  
  #reattach sentiment score to the new data frame
  # sparsdf$sentiment = reddata$Sentiment
  # convert back to matrix
  # spars = as.matrix(sparsdf)
  #remove all tweets that don't contain frequent words
  #spars = spars[rowSums(spars)!=0,]
  #convert reduced data back to dataframe
  sparsdf = as.data.frame(spars)
  #sparsdf = sparsdf[complete.cases(sparsdf),]
  #sparsdf = sparsdf[!is.infinite(rowSums(sparsdf)),]
  # we see that in this case on rows were extracted. Since the tweets are 
  # all on a specific topic, it seems that each tweet is using at least
  # one of the frequent terms
  ncol(sparsdf)
  # extract sentiment vector
  sentiment = as.factor(sparsdf$Sentiment)
  # split into training and test set
  trainer = sample(length(sentiment), length(sentiment) * 0.6)
  sentitrain = sentiment[trainer]
  sentitest = sentiment[-trainer]
  test = sparsdf[-trainer,]
  test = test[, -ncol(sparsdf)]
  train = sparsdf[trainer,]
  train = train[, -ncol(sparsdf)]
  
  
  trainmat = as.matrix(train)
  # create container object
  traincontainer = create_container(trainmat, 
                               sentitrain,
                               trainSize = 1:nrow(trainmat), 
                               virgin = FALSE)
  
  # create test matrix
  
  
  testmat = as.matrix(test)
  testcontainer = create_container(testmat, labels = rep(0, nrow(test)), 
                                   testSize = 1:nrow(test), virgin = FALSE)
  
  learningdata = list(traindata = train, testdata = test, 
                      sentitrain = sentitrain, 
                      train = traincontainer, test = testcontainer, 
                      sentim = sentitest)
  return(learningdata)
  
}


# further functions for creating some initial models with the RText Tools package
test_results = function(model, learningdata){
  results = classify_model(learningdata$test, model)
  preds = results[,1]
  return(confusionMatrix(table(preds, learningdata$sentim)))
}






get_model_outputs = function(learningdata)
{
  print("Linear SVM:")
  model <- train_model(learningdata$train , "SVM", kernel="linear", cost=1)
  print(test_results(model, learningdata))
  
  print("Radial SVM:")
  model <- train_model(learningdata$train , "SVM", kernel="radial", cost=1)
  print(test_results(model, learningdata))
  
  print("TREE:")
  model <- train_model(learningdata$train, "TREE")
  print(test_results(model, learningdata))
  
  print("MAXENT:")
  model <- train_model(learningdata$train, "MAXENT")
  print(test_results(model, learningdata))
  
  #model <- train_model(learningdata$train, "NNET")
  #test_results(model, learningdata)
  
  print("RF:")
  model <- train_model(learningdata$train, "RF")
  print(test_results(model, learningdata))
  
  print("BAGGING:")
  model <- train_model(learningdata$train, "BAGGING")
  print(test_results(model, learningdata))
  
  print("BOOSTING:")
  model <- train_model(learningdata$train, "BOOSTING")
  print(test_results(model, learningdata))
}








train_unigram_model = function(reddate)
{
  full.matrix = get_onegram_mat(reddata)
  learningdata = create_learning_data(reddata, full.matrix)
  get_model_outputs(learningdata)
}



train_bigram_model = function(reddata)
{
  full.matrix = get_bigram_mat(reddata)
  learningdata = create_learning_data(reddata, full.matrix)
  get_model_outputs(learningdata)
}



train_uni_bigram_model = function(reddata)
{
  onegram.matrix = get_onegram_mat(reddata)
  bigram.matrix = get_bigram_mat(reddata)
  full.matrix = get_full_mat(onegram.matrix, bigram.matrix)
  learningdata = create_learning_data(reddata, full.matrix)
  get_model_outputs(learningdata)
}




generate_models = function(dat)
{
  for(i in dat)
  {
    print("Unigram")
    train_unigram_model(i)
    print("bigram")
    train_bigram_model(i)
    print("Uni_bi_gram")
    train_uni_bigram_model(i)
    print("POS model")
    train_POS_model(i)
  }
}
