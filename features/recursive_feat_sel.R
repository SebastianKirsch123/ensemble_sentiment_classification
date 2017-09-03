
model <- train_model(learningdata$train, "RF")
featsel = read.csv("feature_selection.csv", header = T)

# get important variables and order them
impvar = varImp(model)
impvar$featnames = as.character(colnames(full.matrix))
impvar.ord = impvar[order(impvar$Overall, decreasing = T),] 



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
control = rfeControl(functions = rfFuncs, method = "cv", number = 5, metric = "Accuracy")
results = rfe(red.mat, pred, rfeControl = control, metric = "Accuracy")

results
plot(results, type = c("g", "o"))


red.mat = full.matrix[, c("farrah", "wish", "sad", "lol")]
samp = sample(2000, 1000)
train = red.mat[samp,]
test = red.mat[-samp,]
sentitr = Sdata$Sentiment[samp]
sentites = Sdata$Sentiment[-samp]


learningdata = create_learning_data(Sdata, red.mat)
print(colnames(Sdata)[i])
get_model_outputs(learningdata)


table(red.mat$farrah)
table(red.mat$lexicon.score)
table(red.mat$wish)
table(red.mat$lol)
table(red.mat$sad)
