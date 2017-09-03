library(mlbench)
library(caret)
# Feature Selection
# RF

model <- train_model(learningdata$train, "RF")
featsel = read.csv("feature_selection.csv", header = T)

# get important variables and order them
impvar = varImp(model)
impvar$featnames = as.character(colnames(full.matrix))
impvar.ord = impvar[order(impvar$Overall, decreasing = T),] 



#varImpPlot(model, type = 2)


# Remove all features whose mean Gini Decrease is 0 (have not been used
# during tree splits in Random Forest)
most_imp_feat = impvar.ord$featnames[impvar.ord$Overall > 0.1]
plot(c(1:82), impvar.ord$Overall[impvar.ord$Overall > 1], main = "Random Forest
    Feature Importance", ylab = "Mean Gini Decrease",xlab = "82 most important Variables")
# most_imp_feat = impvar.ord$featnames[1:500]

#get reduced matrix with most important features
red.mat = full.matrix[,most_imp_feat]
sent = Sdata$Sentiment



learningdata = create_learning_data(Sdata, red.mat)

# It was evident from the Feature plotting that features with a mean gini decrease of less than 0.1
# in the random forest model also do not add value to other models. On the contrary,
# accuracy declines with additional features. Equally, sensitivity and specificity peak at much lower
# numbers of features (not yet plotted). Thus we can confidently reduce to 658 features.
# Now we use recursive elimination to weed out unusable features in the remaining set.

pred = Sdata$Sentiment
outc = Sdata[,-1]



# genetic algorithm
ctrl = gafsControl(functions = rfGA, method = "cv", number = 5)
bestfeat = gafs(red.mat, sent, , iters = 5, popSize = 20, gafsControl = ctrl)


learningdata = create_learning_data(Sdata, red.mat)
get_model_outputs(learningdata)


# Correlation analysis
#identify columns that sum to 0
nacols = apply(full.matrix, 2, function(x) sum(x) == 0 )
nacols = which(nacols == T)


full.matrix = full.matrix[ ,-nacols]

correlation.mat = cor(full.matrix)

corvar = findCorrelation(correlation.mat, cutoff = 0.75)
red.mat = full.matrix[, -corvar]







cutoff = c(0, 0.1, 0.5, 0.75, 1)
most_imp_feat5 = impvar.ord$featnames[impvar.ord$Overall > 1]
most_imp_feat4 = impvar.ord$featnames[impvar.ord$Overall > 0.75]
most_imp_feat3 = impvar.ord$featnames[impvar.ord$Overall > 0.5]
most_imp_feat2 = impvar.ord$featnames[impvar.ord$Overall > 0.1]
most_imp_feat1 = impvar.ord$featnames[impvar.ord$Overall > 0]
save()


no.features = c(1219, 658, 174, 104, 79)


red.mat = full.matrix[,most_imp_feat2]
pred = Sdata$Sentiment
# Perform Recursive feature elimination
control = rfeControl(functions = rfFuncs, method = "cv", number = 5)
results = rfe(red.mat, pred, rfeControl = control)


