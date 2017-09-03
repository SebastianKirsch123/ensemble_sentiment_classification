library(caret)
bootstraps = as.data.frame(rep(0,nrow(trainset)))

# generate bootstrap sample of full data set length
for(i in 1:10)
{
    bootstraps[,i] = sample(nrow(trainset), size = nrow(trainset), replace = T)
}

modelno = 5

bootpreds = as.data.frame(rep(0,nrow(testset)))

#set control parameters 
control <- trainControl(method="cv", number=10, savePredictions = TRUE,  classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        index=createResample(sentitr, 10))


count = 1



learningdata = learningdata127
trainset = learningdata$traindata
testset = learningdata$testdata
sentitr = as.factor(learningdata$sentitrain)
sentitest = as.factor(learningdata$sentim)


# may be necessary for generating class probabilities
sentitr <- factor(paste0('class', sentitr))
sentitest <- factor(paste0('class', sentitest))



# create predictions with each of the bootstrap models and write to dataframe
for(i in 1:ncol(bootstraps)){
  #train model
  C5.0.model658 = train(trainset[bootstraps[,i],], sentitr[bootstraps[,i]], method = 'C5.0', metric = "ROC", trControl = control, tuneLength = 10)
  bootpreds[,count] = predict(C5.0.model658, testset, type = "raw")
  count = count + 1
}



for(i in 1:ncol(bootstraps)){
  #train model
  linsvm.model127 = train(trainset[bootstraps[,i],], sentitr[bootstraps[,i]], method = 'svmLinear',metric = "ROC", trControl = control,
                          tuneLength = 10)
  bootpreds[,count] = predict(linsvm.model127, testset, type = "raw")
  count = count + 1
}



for(i in 1:ncol(bootstraps)){
  #train model
  nnet.model127 = train(trainset[bootstraps[,i],], sentitr[bootstraps[,i]], method = 'nnet',metric = "ROC", trControl = control,
                          tuneLength = 10)
  bootpreds[,count] = predict(nnet.model127, testset, type = "raw")
  count = count + 1
}



for(i in 1:ncol(bootstraps)){
  #train model
  glmn.model127 = train(trainset[bootstraps[,i],], sentitr[bootstraps[,i]], method = 'glmnet',metric = "ROC", trControl = control,
                          tuneLength = 10)
  bootpreds[,count] = predict(glmn.model127, testset, type = "raw")
  count = count + 1
}




for(i in 1:ncol(bootstraps)){
  #train model
  pda.model127 = train(trainset[bootstraps[,i],], sentitr[bootstraps[,i]], method = 'pda',metric = "ROC", trControl = control,
                          tuneLength = 10)
  bootpreds[,count] = predict(pda.model127, testset, type = "raw")
  count = count + 1
}





final = get_final_preds(bootpreds)
x = confusionMatrix(sentitest, final)

bootaccs = rep(0,5)
bootsens = rep(0,5)
bootspec = rep(0,5)
for(i in seq(10, 50, 10)){
   final = get_final_preds(bootpreds[,(i-9): i])
   bootaccs[i/10] = confusionMatrix(final, sentitest)$overall[1]
   bootsens[i/10] = confusionMatrix(final, sentitest)$byClass[1]
   bootspec[i/10] = confusionMatrix(final, sentitest)$byClass[2]
}

# check how ech of the bagged models compares to the cv model 
final = get_final_preds(bootpreds[,1:10])
sum(final !=  C5.0.preds658)

final = get_final_preds(bootpreds[,11:20])
sum(final !=  linsvm.preds127)

final = get_final_preds(bootpreds[,21:30])
sum(final !=  nnet.preds127)

final = get_final_preds(bootpreds[,31:40])
sum(final !=  glmn.preds127)

final = get_final_preds(bootpreds[,41:50])
sum(final !=  pda.preds127)




plot(bootaccs, type = "l", ylim = c(0.65, 0.9), xaxt = "none", main = "Bagging Results", ylab = "Metric" , xlab = "Model")
lines(bootsens, col = "blue")
lines(bootspec, col = "red")
legend("topleft", legend = c("Accuracy", "Sensitivity", "Specificity"), lty = 1,
       col = c("black", "blue", "red"))
axis(1, at = 1:5, labels = c("C5.0", "SVM", "NNET", "GLMNET", "PDA"))

