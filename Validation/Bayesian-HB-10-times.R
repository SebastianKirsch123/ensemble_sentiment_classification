library(pROC)
library(caret)
library(caretEnsemble)
library(glmnet)
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



#required sourcing befor execution: load_processed_files, functions and ensemble_functions
source('functions.R')
source('ensemble_functions.R')

#load functions
load(file = "feature_names658.RData")
load(file = "feature_names127.RData")
load(file = "feature_names5.RData")

Sdata = read.csv("red_stanford_feature_enhanced.csv", header = T)
Sdata$X = NULL




options(digits = 4)
#initialize data frames for single models
accuracies = data.frame(matrix(ncol = 12, nrow = 0))
sensitivities = data.frame(matrix(ncol = 12, nrow = 0))
specificities = data.frame(matrix(ncol = 12, nrow = 0))
alg_names = c("C5.0_658", "gbm_127", "glmn_127", "linsvm_127",
              "nb_5", "nnet_658", "nnet_overs_127", "nnet_127",
              "pda_127", "pda_overs_127", "rf_658", "rf_127")

names(accuracies) <- alg_names
names(sensitivities) <- alg_names
names(specificities) <- alg_names

bayes_boost_ens = data.frame(accuracies = numeric(), sensitivities = numeric(), specificities = numeric())


#step by step Sdata
onegram.matrix = get_onegram_mat(Sdata)
bigram.matrix = get_bigram_mat(Sdata)
trigram.matrix = get_trigram_mat(Sdata)
full.matrix = get_full_mat(onegram.matrix, bigram.matrix, trigram.matrix)
full.matrix = cbind(full.matrix, Sdata[, 3:c(ncol(Sdata))])


# run through the whole process 10 times each time randomly partitioning train,test set

for(l in 1:10){
  learningdata = create_learning_data(Sdata, full.matrix)
  
  
  # remove the container objects (used for testing but not needed now)
  learningdata = learningdata[-c(4,5)]
  learningdata5 = learningdata
  learningdata127 = learningdata
  
  #reduce to 658 features
  learningdata$traindata = learningdata$traindata[,feature_names_658]
  learningdata$testdata = learningdata$testdata[,feature_names_658]
  #assign to this variable as "learningdata" is overwritten later on
  learningdata658 = learningdata
  
  #reduce to 127 features
  learningdata127$traindata = learningdata$traindata[,feature_names_127]
  learningdata127$testdata = learningdata$testdata[,feature_names_127]
  
  #reduce to 5 features
  learningdata5$traindata = learningdata$traindata[,feature_names_5]
  learningdata5$testdata = learningdata$testdata[,feature_names_5]
  
  
  
  
  
  
  trainset = learningdata$traindata
  testset = learningdata$testdata
  sentitr = as.factor(learningdata$sentitrain)
  sentitest = as.factor(learningdata$sentim)
  
  
  
  # may be necessary for generating class probabilities
  sentitr <- factor(paste0('class', sentitr))
  sentitest <- factor(paste0('class', sentitest))
  
  
  control <- trainControl(method="cv", number=10, savePredictions = TRUE,  classProbs = TRUE,
                          summaryFunction = twoClassSummary,
                          index=createResample(sentitr, 10))
  
  
  
  C5.0.model658 = train(trainset, sentitr, method = 'C5.0', metric = "ROC", trControl = control, tuneLength = 10)
  C5.0.tpreds658 = predict(C5.0.model658, trainset, type = "raw")
  C5.0.tprobs658 = predict(C5.0.model658, trainset, type = "prob")
  C5.0.preds658 = predict(C5.0.model658, testset, type = "raw")
  C5.0.probs658 = predict(C5.0.model658, testset, type = "prob")
  C5.0.conf658 = confusionMatrix(C5.0.preds658, sentitest)
  
  
  
  rf.model658 <- train(trainset, sentitr, method="rf", metric='ROC', trControl=control,
                       verbose = FALSE)
  rf.tprobs658 = predict(rf.model658, trainset, type = "prob")
  rf.tpreds658 = predict(rf.model658, trainset, type = "raw")
  rf.probs658 = predict(rf.model658, testset, type = "prob")
  rf.preds658 = predict(rf.model658, testset, type = "raw")
  rf.conf658 = confusionMatrix(table(rf.preds658, sentitest))
  
  

  
  
  
  learningdata = learningdata127
  trainset = learningdata$traindata
  testset = learningdata$testdata
  sentitr = as.factor(learningdata$sentitrain)
  sentitest = as.factor(learningdata$sentim)
  
  
  
  # may be necessary for generating class probabilities
  sentitr <- factor(paste0('class', sentitr))
  sentitest <- factor(paste0('class', sentitest))
  
  
  linsvm.model127 = train(trainset, sentitr, method = 'svmLinear',metric = "ROC", trControl = control,
                          tuneLength = 10)
  linsvm.tprobs127 = predict(linsvm.model127, trainset, type = "prob")
  linsvm.tpreds127 = predict(linsvm.model127, trainset, type = "raw")
  linsvm.probs127 = predict(linsvm.model127, testset, type = "prob")
  linsvm.preds127 = predict(linsvm.model127, testset, type = "raw")
  linsvm.conf127 = confusionMatrix(linsvm.preds127, sentitest)
  
  nnet.model127 <- train(trainset, sentitr, method="nnet", metric='ROC', trControl=control,
                         verbose = FALSE, trace = FALSE, tuneLength = 10)
  nnet.tprobs127 = predict(nnet.model127, trainset, type = "prob")
  nnet.tpreds127 = predict(nnet.model127, trainset, type = "raw")
  nnet.probs127 = predict(nnet.model127, testset, type = "prob")
  nnet.preds127 = predict(nnet.model127, testset, type = "raw")
  nnet.conf127 = confusionMatrix(table(nnet.preds127, sentitest))
  
  rf.model127 <- train(trainset, sentitr, method="rf", metric='ROC', trControl=control,
                       verbose = FALSE)
  rf.tprobs127 = predict(rf.model127, trainset, type = "prob")
  rf.tpreds127 = predict(rf.model127, trainset, type = "raw")
  rf.probs127 = predict(rf.model127, testset, type = "prob")
  rf.preds127 = predict(rf.model127, testset, type = "raw")
  rf.conf127 = confusionMatrix(table(rf.preds127, sentitest))
  
  
  gbm.model127 <- train(trainset, sentitr, method="gbm", metric='ROC', trControl=control,
                        verbose = FALSE)
  gbm.tpreds127 = predict(gbm.model127, trainset, type = "raw")
  gbm.tprobs127 = predict(gbm.model127, trainset, type = "prob")
  gbm.preds127 = predict(gbm.model127, testset, type = "raw")
  gbm.probs127 = predict(gbm.model127, testset, type = "prob")
  gbm.conf127 = confusionMatrix(table(gbm.preds127, sentitest))
  
  
  # generaliyed linear model alpha = 1 = lasso, alpha = 0 = ridgeregr
  glmn.model127 = train(trainset, sentitr, method = "glmnet", metric = "ROC", 
                        trControl = control)
  glmn.tprobs127 = predict(glmn.model127, trainset, type = "prob")
  glmn.tpreds127 = predict(glmn.model127, trainset, type = "raw")
  glmn.probs127 = predict(glmn.model127, testset, type = "prob")
  glmn.preds127 = predict(glmn.model127, testset, type = "raw")
  glmn.conf127 = confusionMatrix(table(glmn.preds127, sentitest))
  #best is a 0.1 lmbda model which is closer ridge regression but
  
  predictors(glmn.model127)
  
  
  pda.model127 = train(trainset, sentitr, method = "pda", metric = "ROC", 
                       trControl = control)
  pda.tprobs127 = predict(pda.model127, trainset, type = "prob")
  pda.tpreds127 = predict(pda.model127, trainset, type = "raw")
  pda.probs127 = predict(pda.model127, testset, type = "prob")
  pda.preds127 = predict(pda.model127, testset, type = "raw")
  pda.conf127 = confusionMatrix(table(pda.preds127, sentitest))
  
  
  
  
  
  # oversampling positive classes to increase sensitivity
  # get index of class1 on first 300 training response
  index = sentitr == "class1"
  index = index[1:800]
  
  #get first 300 observations and extract those corresponding to positive tweets
  posobs = trainset[1:800,]
  posval = posobs[index==T,]
  trainset = rbind(trainset, posval)  
  
  # append to training set
  postr = as.factor(rep("class1", nrow(posval))) 
  sentitr = c(sentitr, postr)
  sentitr <- factor(paste0('class', sentitr))
  
  
  
  nnet.model127.overs <- train(trainset, sentitr, method="nnet", metric='ROC', trControl=control,
                               verbose = FALSE, trace = FALSE, tuneLength = 10)
  nnet.tprobs127.overs = predict(nnet.model127.overs, trainset, type = "prob")
  nnet.tpreds127.overs = predict(nnet.model127.overs, trainset, type = "raw")
  nnet.probs127.overs = predict(nnet.model127.overs, testset, type = "prob")
  nnet.preds127.overs = predict(nnet.model127.overs, testset, type = "raw")
  nnet.conf127.overs = confusionMatrix(table(nnet.preds127.overs, sentitest))
  
  
  
  pda.model127.overs = train(trainset, sentitr, method = "pda", metric = "ROC", 
                             trControl = control)
  pda.tprobs127.overs = predict(pda.model127.overs, trainset, type = "prob")
  pda.tpreds127.overs = predict(pda.model127.overs, trainset, type = "raw")
  pda.probs127.overs = predict(pda.model127.overs, testset, type = "prob")
  pda.preds127.overs = predict(pda.model127.overs, testset, type = "raw")
  pda.conf127.overs = confusionMatrix(table(pda.preds127.overs, sentitest))
  
  
  
  
  learningdata = learningdata5
  trainset = learningdata$traindata
  testset = learningdata$testdata
  sentitr = as.factor(learningdata$sentitrain)
  sentitest = as.factor(learningdata$sentim)
  
  
  
  # may be necessary for generating class probabilities
  sentitr <- factor(paste0('class', sentitr))
  sentitest <- factor(paste0('class', sentitest))
  
  
  
  control = trainControl(method = "cv", number = 10, savePredictions = TRUE, classProbs = T)
  
  nb.model5 <- train(trainset, sentitr, method="nb", metric='ROC', trControl=control,
                     verbose = FALSE)
  nb.tprobs5 = predict(nb.model5, trainset, type = "prob")
  nb.tpreds5 = predict(nb.model5, trainset, type = "raw")
  nb.probs5 = predict(nb.model5, testset, type = "prob")
  nb.preds5 = predict(nb.model5, testset, type = "raw")
  nb.conf5 = confusionMatrix(table(nb.preds5, sentitest))
  
  

  
  
  #Hybrid Boost 
  control <- trainControl(method="cv", number=10, savePredictions = TRUE,  classProbs = TRUE,
                          summaryFunction = twoClassSummary,
                          index=createResample(sentitr, 10))
  
  
  load(file = "Stan_2k_658_learningdata.RData")
  trainset = learningdata$traindata
  testset = learningdata$testdata
  sentitr = as.factor(learningdata$sentitrain)
  sentitest = as.factor(learningdata$sentim)
  
  # may be necessary for generating class probabilities
  sentitr <- factor(paste0('class', sentitr))
  sentitest <- factor(paste0('class', sentitest))
  
  
  iter = 2
  algno = 2
  totalweight = rep(0, iter*algno) 
  totalalpha = rep(0, iter*algno) 
  totalerror = rep(0, iter*algno) 
  totalcorr = rep(0, iter*algno) 
  
  init = rep(0, nrow(trainset))
  preds = as.data.frame(init)
  misses = as.data.frame(init)
  
  init = rep(0, nrow(testset))
  preds_test = as.data.frame(init)
  misses_test = as.data.frame(init)
  
  w = rep(1, nrow(trainset))
  w <- w/sum(w)
  prevcorr = 0
  
  tc = 0
  
  
  for(i in  1:iter){ 
    tc = tc + 1
    fit = train(trainset, sentitr, method = 'C5.0', weights = w,
                trControl = control, tuneLength = 10)
    g<-  predict(fit,trainset, type = "raw")
    preds[,tc] = g
    g_test <-  predict(fit,testset, type = "raw")
    preds_test[,tc] = g_test
    miss = g!= sentitr 
    misses[,tc] = miss
    misses_test[,tc] =  g_test!= sentitest
    prevcorr = ifelse(i>1, sum(w[misses[,i] == FALSE & misses[,i-1] == TRUE]), 0)
    totalcorr[tc] = prevcorr
    e <- (sum(w * miss))/sum(w) - prevcorr/10
    totalerror[tc] = e
    miss = ifelse(g!=sentitr, 1, -1) 
    alpha <- 0.5 * log ( (1-e) / e )
    totalalpha[tc] = alpha
    w = (w * exp(alpha * miss))
    w = w/sum(w)
  } 
  
  
  learningdata = learningdata127
  trainset = learningdata$traindata
  testset = learningdata$testdata
  sentitr = as.factor(learningdata$sentitrain)
  sentitest = as.factor(learningdata$sentim)
  
  # may be necessary for generating class probabilities
  sentitr <- factor(paste0('class', sentitr))
  sentitest <- factor(paste0('class', sentitest))
  
  
  
  
  w = rep(1, nrow(trainset))
  w <- w/sum(w)
  prevcorr = 0
  
  
  for(i in  1:iter){ 
    tc = tc + 1
    fit = train(trainset, sentitr, method = 'nnet', weights = w,
                trControl = control, tuneLength = 10, trace = F)
    g<-  predict(fit,trainset, type = "raw")
    preds[,tc] = g
    g_test <-  predict(fit,testset, type = "raw")
    preds_test[,tc] = g_test
    miss = g!= sentitr 
    misses[,tc] = miss
    misses_test[,tc] =  g_test!= sentitest
    prevcorr = ifelse(i>1, sum(w[misses[,i] == FALSE & misses[,i-1] == TRUE]), 0)
    totalcorr[tc] = prevcorr
    e <- (sum(w * miss))/sum(w) - prevcorr/10
    totalerror[tc] = e
    miss = ifelse(g!=sentitr, 1, -1) 
    alpha <- 0.5 * log ( (1-e) / e )
    totalalpha[tc] = alpha
    w = (w * exp(alpha * miss))
    w = w/sum(w)
  }
  
  
  c5.0.preds.boost = preds_test[,2]
  nnet.preds.boost = preds_test[,4]
  
  # Bayesian Model Combination
  predlist = data.frame(C5.0.preds658, gbm.preds127, glmn.preds127, linsvm.preds127,
                        nb.preds5, nnet.preds127, nnet.preds127.overs, pda.preds127, c5.0.preds.boost,
                        pda.preds127.overs, rf.preds658, rf.preds127, nnet.preds.boost)
  
  
  init = rep(0, nrow(predlist))
  enspreds = as.data.frame(init)
  ensno = 2^ncol(predlist)
  posteriors = seq(0, ensno)
  
  priors = rep(1, ncol(predlist))
  
  for(i in seq(1, ensno,2))
  {
    tempprior1 = rdirichlet(priors)
    tempprior2 = rdirichlet(priors)
    
    p1 = get_final_w_preds(predlist, tempprior1)
    enspreds[,i] = p1    
    p2 = get_final_w_preds(predlist, tempprior2)
    enspreds[,i+1] = p2  
    
    
    conf1 = confusionMatrix(p1, sentitest)
    conf2 = confusionMatrix(p2, sentitest)
    
    misscl = conf1$table[1,2] + conf1$table[2,1]
    corrcl = conf1$table[1,1] + conf1$table[2,2]
    error = 1 - conf1$overall[1]
    acc = conf1$overall[1]
    posterior1 = (1/ensno)*(((1 - error)^corrcl)*error^misscl)
    posteriors[i] = posterior1
    
    
    misscl = conf2$table[1,2] + conf2$table[2,1]
    corrcl = conf2$table[1,1] + conf2$table[2,2]
    error = 1 - conf2$overall[1]
    acc = conf2$overall[1]
    posterior2 = (1/ensno)*(((1 - error)^corrcl)*error^misscl)
    if(i+1 <= length(posteriors)){
      posteriors[i+1] = posterior2
    }
    
    addprior = if(posterior1 > posterior2){
      addprior = tempprior1
    }else{
      addprior = tempprior1
    }
    
    priors = priors + addprior
  }
  
  # last prediction is useless
  posteriors = posteriors[-length(posteriors)]
  
  bayesian_final = get_final_w_preds(enspreds, posteriors)
  
  bayes.conf = confusionMatrix(bayesian_final, sentitest)
  bayes_acc = bayes.conf$overall[1]
  bayes_sens = bayes.conf$byClass[1]
  bayes_spec = bayes.conf$byClass[2]
  
  
  #finally append all the ensemble predictions to data frame
  bayes_pred = c(bayes_acc, bayes_sens, bayes_spec)
  bayes_boost_ens = rbind(bayes_boost_ens, bayes_pred)
  
  # print current iteration plus the best combinations for every ensemble strategy
  print(i)
  print("Majority Voting")
  print(maj_res$combinations)
  print("Boosting")
  print(boost_res$combinations)
  print("Full")
  print(full_res$combinations)
  
}





save(bayes_boost_ens, file = "bayes_boost_ens.RData")


#after loop


print("Single Accuracy")
print(accuracies)
print("Single Sensitivity")
print(sensitivities)
print("Single Specificity")
print(specificities)
print("Ensembles")
print(ensembles)

