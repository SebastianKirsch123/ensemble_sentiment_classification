adab.model127 = train(trainset, sentitr, method = "adaboost", metric = "ROC", 
                     trControl = control)
adab.tprobs127 = predict(adab.model127, trainset, type = "prob")
adab.tpreds127 = predict(adab.model127, trainset, type = "raw")
adab.probs127 = predict(adab.model127, testset, type = "prob")
adab.preds127 = predict(adab.model127, testset, type = "raw")
adab.conf127 = confusionMatrix(table(adab.preds127, sentitest))






load(file = "feature_names127.RData")
Sdata = read.csv("red_stanford_feature_enhanced.csv", header = T)
Sdata$X = NULL

options(digits = 4)


adaboost_ens = data.frame(accuracies = numeric(), sensitivities = numeric(), specificities = numeric())


#step by step Sdata
onegram.matrix = get_onegram_mat(Sdata)
bigram.matrix = get_bigram_mat(Sdata)
trigram.matrix = get_trigram_mat(Sdata)
full.matrix = get_full_mat(onegram.matrix, bigram.matrix, trigram.matrix)
full.matrix = cbind(full.matrix, Sdata[, 3:c(ncol(Sdata))])


# run through the whole process 10 times each time randomly partitioning train,test set

for(l in 1:5){
  learningdata = create_learning_data(Sdata, full.matrix)
  learningdata127$traindata = learningdata$traindata[,feature_names_127]
  learningdata127$testdata = learningdata$testdata[,feature_names_127]
  learningdata = learningdata127
  
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
  
  
  
  adab.model127 = train(trainset, sentitr, method = "adaboost", metric = "ROC", 
                        trControl = control)
  adab.tprobs127 = predict(adab.model127, trainset, type = "prob")
  adab.tpreds127 = predict(adab.model127, trainset, type = "raw")
  adab.probs127 = predict(adab.model127, testset, type = "prob")
  adab.preds127 = predict(adab.model127, testset, type = "raw")
  adab.conf127 = confusionMatrix(table(adab.preds127, sentitest))
  
  
  perf = c(adab.conf127$overall[1], adab.conf127$byClass[1], adab.conf127$byClass[2])
  adaboost_ens = rbind(adaboost_ens, perf)
}
