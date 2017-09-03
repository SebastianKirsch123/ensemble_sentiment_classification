library(mlbench)
library(caret)
# Feature Selection
# RF

model <- train_model(learningdata$train, "RF")
#featsel = read.csv("feature_selection.csv", header = T)

# get important variables and order them
impvar = varImp(model)
impvar$featnames = as.character(colnames(full.matrix))
impvar.ord = impvar[order(impvar$Overall, decreasing = T),] 



# Remove all features whose mean Gini Decrease is 0 (have not been used
# during tree splits in Random Forest)
cutoff = c(0, 0.1, 0.5, 0.75, 1)
most_imp_feat = impvar.ord$featnames[impvar.ord$Overall > 0.5]

#or
no.features = c(1219, 658, 174, 104, 79)
most_imp_feat = impvar.ord$featnames[1:658]
#get reduced matrix with most important features
red.mat = full.matrix[,most_imp_feat]
sent = Sdata$Sentiment

learningdata = create_learning_data(Sdata, red.mat)
save(learningdata, file = "Stan_2k_658_learningdata.RData")





# measuring correlation between each predictor and response with chisq.test
# p values
corel = apply(red.mat, 2,  function(x) chisq.test(x, sent)$p.value < 0.05)

# This gives us 135 significant predictors

# Filter out those predictors that are insignificant
cor.red.mat = red.mat[,which(corel == TRUE)]


learningdata = create_learning_data(Sdata, cor.red.mat)
save(learningdata, file = "Stan_2k_135_learningdata.RData")


# Bonferroni adjusted
0.05 / 658  # we have 658 comparisons
corel = apply(red.mat, 2,  function(x) chisq.test(x, sent)$p.value < 0.000076)
sum(corel)




# if this doesn' work filter out redundant variables
redundant.pred = apply(red.mat, 2, function(x) length(table(x)) < 2)
redundant.pred[redundant.pred == TRUE]

# 104 predictors

# Filter out those predictors that are insignificant
cor.red.mat = red.mat[,which(corel == TRUE)]

learningdata = create_learning_data(Sdata, cor.red.mat)
save(learningdata, file = "Stan_2k_104_learningdata.RData")

#save features to ensure consistency and save time in future iterations
feature_names_658 = colnames(learningdata$traindata)
feature_names_5 = colnames(learningdata5$traindata)
feature_names_127 = colnames(learningdata127$traindata)

save(feature_names_658, file = "feature_names658.RData")
save(feature_names_127, file = "feature_names127.RData")
save(feature_names_5, file = "feature_names5.RData")