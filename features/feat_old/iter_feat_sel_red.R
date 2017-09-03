library(mlbench)

source('functions.R')
Sdata = read.csv("red_stanford_feature_enhanced.csv", header = T)
Sdata$X = NULL

# Feature Selection
# RF
#save(full.matrix, file = "fullmat.red.RData")
#save(learningdata, file = "learningdata.red.RData")
load("learningdata.red.RData")
load("fullmat.red.RData")

model <- train_model(learningdata$train, "RF")


# get important variables and order them
impvar = varImp(model)
impvar$featnames = as.character(colnames(full.matrix))
impvar.ord = impvar[order(impvar$Overall, decreasing = T),] 


modeltype = rep("Linear SVM", 100)
cutoff = c(0, 0.1, 0.5, 0.75, 1)
accuracy = rep(NA, 100)
sensitivity = rep(NA, 100)
specificity = rep(NA, 100)
featcutoff = rep(NA, 100)
cost = rep(NA, 100)
           
for( i in 1: 5){
  
  most_imp_feat = impvar.ord$featnames[impvar.ord$Overall > cutoff[i]]
  red.mat = full.matrix[,most_imp_feat]
  sent = Sdata$Sentiment
  
  for (j in 1:20)
  {
    print("Linear SVM:")
    
    model <- train_model(learningdata$train , "SVM", kernel="linear", cost=j)
    result = test_results(model, learningdata)
    c = (i-1)*20 + j
    accuracy[c] = result$overall[1]
    sensitivity[c] =  result$byClass[1]
    specificity[c] = result$byClass[2]
    featcutoff[c] = cutoff[i]
    cost[c] = j 
  }
} 


bestfeat1 = data.frame(modeltype, featcutoff, cost, accuracy, sensitivity, specificity)  



modeltype = rep("Radial SVM", 100)
cutoff = c(0, 0.1, 0.5, 0.75, 1)
accuracy = rep(NA, 100)
sensitivity = rep(NA, 100)
specificity = rep(NA, 100)
featcutoff = rep(NA, 100)
cost = rep(NA, 100)


for( i in 1: 5){
  
  most_imp_feat = impvar.ord$featnames[impvar.ord$Overall > cutoff[i]]
  red.mat = full.matrix[,most_imp_feat]
  sent = Sdata$Sentiment
  
  for (j in 1:20)
  {
    print("Radial SVM:")
    
    model <- train_model(learningdata$train , "SVM", kernel="radial", cost=j*20)
    result = test_results(model, learningdata)
    c = (i-1)*20 + j
    accuracy[c] = result$overall[1]
    sensitivity[c] =  result$byClass[1]
    specificity[c] = result$byClass[2]
    featcutoff[c] = cutoff[i]
    cost[c] = j *20
  }
} 

bestfeat2 = data.frame(modeltype, featcutoff, cost, accuracy, sensitivity, specificity)  


modeltype = rep("Tree", 5)
cutoff = c(0, 0.1, 0.5, 0.75, 1)
accuracy = rep(NA, 5)
sensitivity = rep(NA, 5)
specificity = rep(NA, 5)
featcutoff = rep(NA, 5)
cost = rep(NA, 5)


for( i in 1: 5){
  
  most_imp_feat = impvar.ord$featnames[impvar.ord$Overall > cutoff[i]]
  red.mat = full.matrix[,most_imp_feat]
  sent = Sdata$Sentiment
  
    print("TREE:")
    model <- train_model(learningdata$train, "TREE")
    result = test_results(model, learningdata)
    accuracy[i] = result$overall[1]
    sensitivity[i] =  result$byClass[1]
    specificity[i] = result$byClass[2]
    featcutoff[i] = cutoff[i]
    cost[i] = 0
} 

bestfeat3 = data.frame(modeltype, featcutoff, cost, accuracy, sensitivity, specificity)  


modeltype = rep("Maxent:", 5)
cutoff = c(0, 0.1, 0.5, 0.75, 1)
accuracy = rep(NA, 5)
sensitivity = rep(NA, 5)
specificity = rep(NA, 5)
featcutoff = rep(NA, 5)
cost = rep(NA, 5)

for( i in 1: 5){
  most_imp_feat = impvar.ord$featnames[impvar.ord$Overall > cutoff[i]]
  red.mat = full.matrix[,most_imp_feat]
  print(ncol(red.mat))
  sent = Sdata$Sentiment
  
  print("MAXENT:")
  model <- train_model(learningdata$train, "MAXENT")
  result = test_results(model, learningdata)
  accuracy[i] = result$overall[1]
  sensitivity[i] =  result$byClass[1]
  specificity[i] = result$byClass[2]
  featcutoff[i] = cutoff[i]
  cost[i] = 0
} 

bestfeat4 = data.frame(modeltype, featcutoff, cost, accuracy, sensitivity, specificity)  



modeltype = rep("Random Forest", 100)
cutoff = c(0, 0.1, 0.5, 0.75, 1)
accuracy = rep(NA, 100)
sensitivity = rep(NA, 100)
specificity = rep(NA, 100)
featcutoff = rep(NA, 100)
cost = rep(NA, 100)


for( i in 1: 5){
  
  most_imp_feat = impvar.ord$featnames[impvar.ord$Overall > cutoff[i]]
  red.mat = full.matrix[,most_imp_feat]
  sent = Sdata$Sentiment
  
  for (j in 1:20)
  {
    print("RF:")
    
    model <- train_model(learningdata$train , "RF", ntree = j*100)
    result = test_results(model, learningdata)
    c = (i-1)*20 + j
    accuracy[c] = result$overall[1]
    sensitivity[c] =  result$byClass[1]
    specificity[c] = result$byClass[2]
    featcutoff[c] = cutoff[i]
    cost[c] = j*100
  }
}

bestfeat5 = data.frame(modeltype, featcutoff, cost, accuracy, sensitivity, specificity)  






modeltype = rep("Bagging", 5)
cutoff = c(0, 0.1, 0.5, 0.75, 1)
accuracy = rep(NA, 5)
sensitivity = rep(NA, 5)
specificity = rep(NA, 5)
featcutoff = rep(NA, 5)
cost = rep(NA, 5)

for( i in 1: 5){
  most_imp_feat = impvar.ord$featnames[impvar.ord$Overall > cutoff[i]]
  red.mat = full.matrix[,most_imp_feat]
  print(ncol(red.mat))
  sent = Sdata$Sentiment
  
  print("BAGGING:")
  model <- train_model(learningdata$train, "BAGGING")
  result = test_results(model, learningdata)
  accuracy[i] = result$overall[1]
  sensitivity[i] =  result$byClass[1]
  specificity[i] = result$byClass[2]
  featcutoff[i] = cutoff[i]
  cost[i] = 0
} 


bestfeat6 = data.frame(modeltype, featcutoff, cost, accuracy, sensitivity, specificity)  




modeltype = rep("Boosting", 100)
cutoff = c(0, 0.1, 0.5, 0.75, 1)
accuracy = rep(NA, 100)
sensitivity = rep(NA, 100)
specificity = rep(NA, 100)
featcutoff = rep(NA, 100)
cost = rep(NA, 100)



for( i in 1: 5){
  
  most_imp_feat = impvar.ord$featnames[impvar.ord$Overall > cutoff[i]]
  red.mat = full.matrix[,most_imp_feat]
  sent = Sdata$Sentiment
  
  for (j in 1:20)
  {
    print("BOOSTING:")
    
    model <- train_model(learningdata$train , "BOOSTING", maxitboost = j * 50 )
    result = test_results(model, learningdata)
    c = (i-1)*20 + j
    accuracy[c] = result$overall[1]
    sensitivity[c] =  result$byClass[1]
    specificity[c] = result$byClass[2]
    featcutoff[c] = cutoff[i]
    cost[c] = j*100
  }
}


bestfeat7 = data.frame(modeltype, featcutoff, cost, accuracy, sensitivity, specificity)  

bestfeat = rbind.data.frame(bestfeat1, bestfeat2, bestfeat3, bestfeat4,
                            bestfeat5, bestfeat6, bestfeat7)



write.csv(bestfeat, file = "feature_selection.csv")


