Stanford = read.csv("stanford_train.csv", header = T)

# rename colnames
colnames(Stanford)[1] = "Sentiment"
colnames(Stanford)[6] = "Tweet"
#remove unnecessary columns
Sdata = Stanford[,-c(2,3,4,5)]
Sdata = Sdata[795000:805000,]
Sdata$Sentiment[which(Sdata$Sentiment==0)] = 1
Sdata$Sentiment[which(Sdata$Sentiment==2)] = 3
Sdata$Sentiment[which(Sdata$Sentiment==4)] = 2
table(Sdata$Sentiment)