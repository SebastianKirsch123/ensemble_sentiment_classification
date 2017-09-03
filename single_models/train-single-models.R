library(pROC)
library(caret)
library(caretEnsemble)
library(glmnet)

#load Stanford dataset
load(file = "Stan_2k_127_learningdata.RData")
load(file = "Stan_2k_5_learningdata.RData")
load(file = "Stan_2k_658_learningdata.RData")

# or load Cairo (Arabic) dataset
#load(file = "Ar_learningdata.RData")
trainset = learningdata$traindata
testset = learningdata$testdata
sentitr = as.factor(learningdata$sentitrain)
sentitest = as.factor(learningdata$sentim)



# may be necessary for generating class probabilities
sentitr <- factor(paste0('class', sentitr))
sentitest <- factor(paste0('class', sentitest))



control = trainControl(method = "cv", number = 10, savePredictions = TRUE, classProbs = T)

control <- trainControl(method="cv", number=10, savePredictions = TRUE,  classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        index=createResample(sentitr, 10))



C5.0.model658 = train(trainset, sentitr, method = 'C5.0', metric = "ROC", trControl = control, tuneLength = 10)
C5.0.tpreds658 = predict(C5.0.model658, trainset, type = "raw")
C5.0.tprobs658 = predict(C5.0.model658, trainset, type = "prob")
C5.0.preds658 = predict(C5.0.model658, testset, type = "raw")
C5.0.probs658 = predict(C5.0.model658, testset, type = "prob")
C5.0.conf658 = confusionMatrix(C5.0.preds658, sentitest)
print(C5.0.model658)


rf.model658 <- train(trainset, sentitr, method="rf", metric='ROC', trControl=control,
                     verbose = FALSE)
rf.tprobs658 = predict(rf.model658, trainset, type = "prob")
rf.tpreds658 = predict(rf.model658, trainset, type = "raw")
rf.probs658 = predict(rf.model658, testset, type = "prob")
rf.preds658 = predict(rf.model658, testset, type = "raw")
rf.conf658 = confusionMatrix(table(rf.preds658, sentitest))
print(rf.model658)


# this nnet has extremly high sensitivity values
nnet.model658 <- train(trainset, sentitr, method="nnet", metric='ROC', trControl=control,
                       verbose = FALSE, trace = FALSE, tuneLength = 10)
nnet.tprobs658 = predict(nnet.model658, trainset, type = "prob")
nnet.tpreds658 = predict(nnet.model658, trainset, type = "raw")
nnet.probs658 = predict(nnet.model658, testset, type = "prob")
nnet.preds658 = predict(nnet.model658, testset, type = "raw")
nnet.conf658 = confusionMatrix(table(nnet.preds658, sentitest))
print(nnet.model658)





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
print(linsvm.model127)


nnet.model127 <- train(trainset, sentitr, method="nnet", metric='ROC', trControl=control,
                       verbose = FALSE, trace = FALSE, tuneLength = 10)
nnet.tprobs127 = predict(nnet.model127, trainset, type = "prob")
nnet.tpreds127 = predict(nnet.model127, trainset, type = "raw")
nnet.probs127 = predict(nnet.model127, testset, type = "prob")
nnet.preds127 = predict(nnet.model127, testset, type = "raw")
nnet.conf127 = confusionMatrix(table(nnet.preds127, sentitest))
print(nnet.model127)


rf.model127 <- train(trainset, sentitr, method="rf", metric='ROC', trControl=control,
                     verbose = FALSE)
rf.tprobs127 = predict(rf.model127, trainset, type = "prob")
rf.tpreds127 = predict(rf.model127, trainset, type = "raw")
rf.probs127 = predict(rf.model127, testset, type = "prob")
rf.preds127 = predict(rf.model127, testset, type = "raw")
rf.conf127 = confusionMatrix(table(rf.preds127, sentitest))
print(rf.model127)



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
print(glmn.model127)
#best is a 0.1 lmbda model which is closer ridge regression but

predictors(glmn.model127)


pda.model127 = train(trainset, sentitr, method = "pda", metric = "ROC", 
                     trControl = control)
pda.tprobs127 = predict(pda.model127, trainset, type = "prob")
pda.tpreds127 = predict(pda.model127, trainset, type = "raw")
pda.probs127 = predict(pda.model127, testset, type = "prob")
pda.preds127 = predict(pda.model127, testset, type = "raw")
pda.conf127 = confusionMatrix(table(pda.preds127, sentitest))
print(pda.model127)




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
print(nnet.model127.overs)


pda.model127.overs = train(trainset, sentitr, method = "pda", metric = "ROC", 
                           trControl = control)
pda.tprobs127.overs = predict(pda.model127.overs, trainset, type = "prob")
pda.tpreds127.overs = predict(pda.model127.overs, trainset, type = "raw")
pda.probs127.overs = predict(pda.model127.overs, testset, type = "prob")
pda.preds127.overs = predict(pda.model127.overs, testset, type = "raw")
pda.conf127.overs = confusionMatrix(table(pda.preds127.overs, sentitest))
print(pda.model127.overs)










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




roc.c5.0658 = roc(sentitest, C5.0.probs658$class1)
plot((1-roc.c5.0658$specificities), roc.c5.0658$sensitivities, main = "C5.0 (658)", 
     lwd = 0.1, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity")
abline(0,1)


roc.rf658 = roc(sentitest, rf.probs658$class1)
plot((1-roc.rf658$specificities), roc.rf658$sensitivities, main = "RF (658)", 
     lwd = 0.1, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity")
abline(0,1)


roc.nnet658 = roc(sentitest, nnet.probs658$class1)
plot((1-roc.nnet658$specificities), roc.nnet658$sensitivities, main = "NNET (658)", 
     lwd = 0.1, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity")
abline(0,1)


roc.nnet127 = roc(sentitest, nnet.probs127$class1)
plot((1-roc.nnet127$specificities), roc.nnet127$sensitivities, main = "NNET (127)", 
     lwd = 0.1, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity")
abline(0,1)


roc.nnet127.overs = roc(sentitest, nnet.probs127.overs$class1)
plot((1-roc.nnet127.overs$specificities), roc.nnet127.overs$sensitivities, main = "NNET OVERSAMPLED (127)", 
     lwd = 0.1, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity")
abline(0,1)


roc.svm127 = roc(sentitest, linsvm.probs127$class1)
plot((1-roc.svm127$specificities), roc.svm127$sensitivities, main = "SVM (127)", 
     lwd = 0.1, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity")
abline(0,1)


roc.rf127 = roc(sentitest, rf.probs127$class1)
plot((1-roc.rf127$specificities), roc.rf127$sensitivities, main = "RF (127)", 
     lwd = 0.1, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity")
abline(0,1)


roc.gbm127 = roc(sentitest, gbm.probs127$class1)
plot((1-roc.gbm127$specificities), roc.gbm127$sensitivities, main = "GBM (127)", 
     lwd = 0.1, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity")
abline(0,1)



roc.glmn127 = roc(sentitest, glmn.probs127$class1)
plot((1-roc.glmn127$specificities), roc.glmn127$sensitivities, main = "GLMNET (127)", 
     lwd = 0.1, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity")
abline(0,1)



roc.pda127 = roc(sentitest, pda.probs127$class1)
plot((1-roc.pda127$specificities), roc.pda127$sensitivities, main = "PDA (127)", 
     lwd = 0.1, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity")
abline(0,1)



roc.pda127.overs = roc(sentitest, pda.probs127.overs$class1)
plot((1-roc.pda127.overs$specificities), roc.pda127.overs$sensitivities, main = "PDA OVERSAMPLED (127)", 
     lwd = 0.1, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity")
abline(0,1)



roc.nb5 = roc(sentitest, nb.probs5$class1)
plot((1-roc.nb5$specificities), roc.nb5$sensitivities, main = "NB (5)", 
     lwd = 0.1, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity")
abline(0,1)



pROC::auc(roc.c5.0)


plot(roc.c5.0, legacy.axes = T, main = "C5.0", xaxs = "i")



# generate tables

preds = list(C5.0.preds658, gbm.preds127, glmn.preds127, linsvm.preds127,
             nb.preds5, nnet.preds127, nnet.preds127.overs, pda.preds127,
             pda.preds127.overs, rf.preds658, rf.preds127)

names = c("C5.0.preds658", "gbm.preds127", "glmn.preds127", "linsvm.preds127",
          "nb.preds5", "nnet.preds127", "nnet.preds127.overs", "pda.preds127",
          "pda.preds127.overs", "rf.preds658", "rf.preds127")



b = matrix(rep(0, 11*11), nrow = 11, ncol = 11)
colnames(b) = names
rownames(b) = names

options(digits = 4)

for(i in 1:(11)){
  for(j in 1:11){
    model1 = unlist(preds[i])
    model2 = unlist(preds[j])
    
    bothtrue = sum((sentitest == model1) & (sentitest == model2))
    trueA = sum((sentitest == model1) & (sentitest != model2))
    trueB = sum((sentitest != model1) & (sentitest == model2))
    bothfalse = sum((sentitest != model1) & (sentitest != model2))
    
    tabone = c(bothfalse, trueA)
    tabtwo = c(trueB, bothtrue)
    
    misscl.table = cbind(tabone, tabtwo)
    mcn = mcnemar.test(misscl.table)
    b[i,j] = trueA + trueB
  }
}

options(digits = 4)

# p - value of McNemar test
algocomp = as.data.frame(b)

write.xlsx(algocomp, file = "algorithm_comparison.xlsx")


#total number of observations that were classified differently by each algorithm
algo_class_diff = as.data.frame(b)

write.xlsx(algo_class_diff, file = "algo_class_diff.xlsx")


cor(sentitest, C5.0.preds658)

cor()


















