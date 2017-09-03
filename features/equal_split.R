# Problem with different feature dataset is that they have different training test splits

load(file = "Stan_2k_658_learningdata.RData")
load(file = "Stan_2k_135_learningdata.RData")
load(file = "Stan_2k_104_learningdata.RData")
load(file = "Stan_2k_5_learningdata.RData")


learningdata658 = learningdata
learningdata658$train = NULL
learningdata658$test = NULL

# load 135 feature set
load(file = "Stan_2k_135_learningdata.RData")

coln135 = colnames(learningdata$traindata)

# check which observations are contained in col135 but not in col 658
cont = rep(0,135)
for( i in 1:135){
  cont[i] = coln135[i] %in% coln658
}

#remove them
newcol135 = coln135[which(cont == 1)]

newtrain = learningdata658$traindata[,newcol135]
newtest = learningdata658$testdata[,newcol135]


learningdata127 = learningdata658
learningdata127$traindata = newtrain
learningdata127$testdata = newtest



# load 5 feature set
load(file = "Stan_2k_5_learningdata.RData")

coln5 = colnames(learningdata$traindata)



newtrain = learningdata658$traindata[,coln5]
newtest = learningdata658$testdata[,coln5]


learningdata5 = learningdata658
learningdata5$traindata = newtrain
learningdata5$testdata = newtest


save(learningdata127, file = "Stan_2k_127_learningdata.RData")
save(learningdata5, file = "Stan_2k_5_learningdata.RData")







