library(pROC)
library(caret)
library(caretEnsemble)
library(glmnet)



# set control parameters for caret model training
control <- trainControl(method="cv", number=10, savePredictions = TRUE,  classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        index=createResample(sentitr, 10))


# load learningdata object which contains training and test data and the corresponding
# response variables
load(file = "Stan_2k_658_learningdata.RData")
trainset = learningdata$traindata
testset = learningdata$testdata
sentitr = as.factor(learningdata$sentitrain)
sentitest = as.factor(learningdata$sentim)

# caret has issues processing simple integers as response, therefore call it "class1"
# and "class2"
sentitr <- factor(paste0('class', sentitr))
sentitest <- factor(paste0('class', sentitest))

# set number of boosting iterations and algorithms
iter = 2
algno = 3
# Initialize errors and weights to 0
totalweight = rep(0, iter*algno) 
totalalpha = rep(0, iter*algno) 
totalerror = rep(0, iter*algno) 
totalcorr = rep(0, iter*algno) 

# initialize dataframe to which predictions and misses in every iteration are saved
# misses is a logical vector denoting whether the model has correctly predicted a
# sample
init = rep(0, nrow(trainset))
preds = as.data.frame(init)
misses = as.data.frame(init)

init = rep(0, nrow(testset))
preds_test = as.data.frame(init)
misses_test = as.data.frame(init)

# initialize weights (equal for every model) and prevcorr (the sum of all 
# samples that have been corrected from the previous iteration)
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



# using a different feature subset for pda and nnets
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
  fit = train(trainset, sentitr, method = 'pda', weights = w,
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





preds_test[,1][misses_test[,1] == TRUE] = preds_test[,2][misses_test[,1] == TRUE] 
sum(misses_test[,2] == FALSE & misses_test[,1] == TRUE) 



final = get_final_preds(preds_test[,c(9,10,11,12)])
confusionMatrix(final, sentitest)

final = get_final_w_preds(preds_test, totalalpha)
confusionMatrix(final, sentitest)


cc = cpr[cpr$winnow == F & cpr$trials == 1 & cpr$ model == 'tree',]








# Plot Boosting Results

accuracies = c(0.775, 0.7163, 0.7725, 0.8325, 0.8412)
names(accuracies) = c("Majority", "AdaBoost", "StandaloneHybridBoost", "HybridBoost+Maj", "ExhHybridBoost+Maj")
nam = c("Majority", "AdaBoost", "StandaloneHybridBoost", "HybridBoost+Maj", "ExhHybridBoost+Maj")


par(mar = c(11,4,4,2))
barplot(accuracies, ylim = c(0.65, 0.9),  main = "Hybrid Boost Comparison", ylab = "Accuracy" , xpd = F,
        names.arg = c("Majority Voting", "AdaBoost", "Standalone Hybrid Boost", "Hybrid Boost + Maj", "Exh Hybrid Boost + Maj"), horiz =F, las = 2)

axis(1, at = 0:4, labels = c(c("AdaBoost", "Standalone Hybrid Boost", "Hybrid Boost + Maj", "Exh Hybrid Boost + Maj")))

