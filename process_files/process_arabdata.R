Sys.setlocale("LC_ALL", "en_US.UTF-8")

Sys.setlocale("LC_TIME", "English")


con = file("Arabic_Tweets.txt", open = "r")
lines = readLines(con, encoding = "UTF-8")
Sentiment = rep(NA, length(lines))
Tweet = rep(NA, length(lines))


for (i in 1:length(lines)){
  Sentiment[i] = as.character(unlist(strsplit(lines[i], "\t"))[2])
  Tweet[i] = unlist(strsplit(lines[i], "\t"))[1]
}
close(con)

Arabdata = data.frame(as.character(Tweet), Sentiment)
colnames(Arabdata) = c("Tweet", "Sentiment")
write.csv(Arabdata, file = "Arabdata.csv", fileEncoding =  'UTF-8')

Adata = read.csv("Arabdata.csv", header = T, encoding = "arabic")
Adata$X = NULL
