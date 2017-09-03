# caret Ensemble
load(file = "Stan_2k_5_learningdata.RData")
load(file = "Stan_2k_658_learningdata.RData")
load(file = "Stan_2k_127_learningdata.RData")
library(pROC)
library(caret)
library(caretEnsemble)
learningdata = learningdata127
trainset = learningdata$traindata
testset = learningdata$testdata
sentitr = as.factor(learningdata$sentitrain)
sentitest = as.factor(learningdata$sentim)



# may be necessary for generating class probabilities
sentitr <- factor(paste0('class', sentitr))
sentitest <- factor(paste0('class', sentitest))


control <- trainControl(method="cv", number=5, savePredictions = TRUE,  classProbs = TRUE,
                        summaryFunction = twoClassSummary)



# assemble modesl for stacking in one list
algorithmList <- c('pda', 'nnet', 'gbm', 'svmLinear', 'rf', 'C5.0', 'glmnet')

# create modellist (models are extremely correlated when using the default list)
# Version 1 containing the default models
models <- caretList(trainset, sentitr, trControl=control, methodList=algorithmList)


# some model info
summary(models)
res = resamples(models)
summary(res)
modelCor(res)
# lda and nnet extremely closely correlated



# create model stacks with different meta models

stack.c5.0 <- caretStack(models, method="C5.0", metric="Accuracy", trControl=control)
print(stack.glmn)

stack.c50.pred = predict(stack.c5.0, testset)
stackc50.conf = confusionMatrix(stack.c50.pred, sentitest)



stack.svm <- caretStack(models, method="svmLinear", metric="Accuracy", trControl=control)
print(stack.svm)

stack.svm.pred = predict(stack.svm, testset)
stacksvm.conf = confusionMatrix(stack.svm.pred, sentitest)



stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=control)
print(stack.rf)

stack.rf.pred = predict(stack.rf, testset)
stackrf.conf = confusionMatrix(stack.rf.pred, sentitest)



stack.nnet <- caretStack(models, method="nnet", metric="Accuracy", trControl=control)
print(stack.nnet)

stack.nnet.pred = predict(stack.nnet, testset)
stacknnet.conf = confusionMatrix(stack.nnet.pred, sentitest)



stack.gbm <- caretStack(models, method="gbm", metric="Accuracy", trControl=control)
print(stack.gbm)

stack.gbm.pred = predict(stack.gbm, testset)
stackgbm.conf = confusionMatrix(stack.gbm.pred, sentitest)



stack.pda <- caretStack(models, method="pda", metric="Accuracy", trControl=control)
print(stack.pda)

stack.pda.pred = predict(stack.rf, testset)
stackpda.conf = confusionMatrix(stack.pda.pred, sentitest)



stack.glmn <- caretStack(models, method="glmnet", metric="Accuracy", trControl=control)
print(stack.glmn)

stack.glmn.pred = predict(stack.glmn, testset)
stackglmn.conf = confusionMatrix(stack.glmn.pred, sentitest)



stackpreds = data.frame(stack.c50.pred, stack.svm.pred, stack.rf.pred, stack.nnet.pred,
                        stack.gbm.pred, stack.pda.pred, stack.glmn.pred)



stackacc = c(stackc50.conf$overall[1], stacksvm.conf$overall[1], stackrf.conf$overall[1],
             stacknnet.conf$overall[1], stackgbm.conf$overall[1], stackpda.conf$overall[1], stackglmn.conf$overall[1])

stacksens = c(stackc50.conf$byClass[1], stacksvm.conf$byClass[1], stackrf.conf$byClass[1],
             stacknnet.conf$byClass[1], stackgbm.conf$byClass[1], stackpda.conf$byClass[1], stackglmn.conf$byClass[1])

stackspec = c(stackc50.conf$byClass[2], stacksvm.conf$byClass[2], stackrf.conf$byClass[2],
              stacknnet.conf$byClass[2], stackgbm.conf$byClass[2], stackpda.conf$byClass[2], stackglmn.conf$byClass[2])


plot(stackacc, type = "l", ylim = c(0.65, 0.85), xaxt = "none", main = "Stacking Results", ylab = "Metric" , xlab = "Model")
lines(stacksens, col = "blue")
lines(stackspec, col = "red")
legend("bottomleft", legend = c("Accuracy", "Sensitivity", "Specificity"), lty = 1,
       col = c("black", "blue", "red"))
axis(1, at = 1:7, labels = c("C5.0", "SVM", "RF", "NNET", "GBM", "PDA", "GLMNET"))







sum(stack.c50.pred != C5.0.preds658)

# these seven stack models to the 14 prediction dataframe wuld be too computationally expensive to evaluate
# exhaustively
#instead add to the best subsets of the 14 prediction dataframe 
# (11 models + 3 obtained through boosting) are the following


 "rf.preds658" "V6"          "V10"        


 "V2"  "V6"  "V10"
 
# this results in 3 + 7 = 10

newpreds = predlist[,unlist(bestcomb$combinations[1])]
newpreds = cbind(newpreds, stackpreds) 
beststack = exhaustive_ens_search(newpreds, sentitest)

