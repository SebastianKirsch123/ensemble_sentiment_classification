none.accuracy = c(73.04, 60.27, 57.64, 70.62, 73.06, 65.8, 55.01)
none.sensitivity = c(70.39, 24.87, 18.05, 71.14, 72.68, 43.45, 97.28)
none.specificity = c(75.77, 96.96, 98.67, 70.09, 73.45, 88.96, 11.20)

dist_score.accuracy = c(73.88, 63.79, 60.14, 71.63, 72.76, 67.99, 58.16)
dist_score.sensitivity = c(69.41, 57.70, 28.13, 71.02, 70.37, 54.87, 92.03)
dist_score.specificity = c(78.46, 70.05, 93.02, 72.25, 75.22, 81.84, 23.35)

subj.score.accuracy = c(72.12, 61.32, 57.78, 70.24, 71.25, 64.77, 53.35)
subj.score.sensitivity = c(68.70, 56.13, 18.3, 69.93, 67.63, 45.37, 96.84)
subj.score.specificity = c(75.65, 66.69, 98.616, 70.56, 74.99, 84.84, 83.56)

tweetl.accuracy = c(73.94, 62, 58.4, 72.26, 73, 65.44, 57.91)
tweetl.sensitivity = c(70.17, 50, 19.05, 72.47, 71.02, 47.65, 93.12)
tweetl.spcecificity = c(77.83, 74.37, 98.95, 72.06, 75.03, 83.77, 21.62)

wordl.accuracy = c(73.27, 61.65, 58.69, 71.45, 72.75, 65.9, 57.69)
wordl.sensitivity = c(67.99, 54.78, 19.28, 70.19, 70.19, 47.26, 83.94)
wordl.specificity = c(78.63, 68.63, 98.8, 72.73, 75.36, 84.86, 30.98)

lex.score.accuracy = c(73.64, 67.34, 64.88, 71.09, 73.91, 70.55, 56.41)
lex.score.sensitivity = c(72.55, 64.86, 41.74, 73.69, 74.28, 61.45, 95.18)
lex.score.specificity = c(74.73, 69.84, 88.1, 68.48, 73.53, 79.67, 17.5)

topic.accuracy = c(74.88, 64.12, 68.38, 74.88, 77, 71.88, 57.62)
topic.sensitivity = c(72.46, 63.77, 37.22, 78.66, 72.46, 56.33, 96.77)
topic.specificity = c(77.33, 60.45, 100, 71.03, 81.61, 87.66, 17.88)



plot(1:7, none.accuracy, type = "l", ylim = c(0,100), xlab = "Models",
     ylab = "Accuracy in Percent", main = "Changes in Accuracy by Single Features")
lines(dist_score.accuracy, col = "violet")
lines(subj.score.accuracy, col = "red")
lines(tweetl.accuracy, col = "green")
lines(wordl.accuracy, col = "blue")
lines(lex.score.accuracy, col = "orange")
lines(topic.accuracy, col = "brown")
legend("bottomright", c("none", "dist.score", "subj.score", "tweetlength", "wordlength",
                        "lexicon.score", "topic allocation"),
       text.col = c("black", "violet", "red", "green", "blue", "orange", "brown"))

legend("bottomleft", c("1 : Linear SVM", "2 : Radial SVM", "3 : Tree", 
                       "4 : Maxent", "5 : Random Forest", "6 : Bagging", "7 : Boosting"))



plot(1:7, none.sensitivity, type = "l", ylim = c(0,100), xlab = "Models",
     ylab = "Sensitivity in Percent", main = "Changes in Sensitivity by Single Features")
lines(dist_score.sensitivity, col = "violet")
lines(subj.score.sensitivity, col = "red")
lines(tweetl.sensitivity, col = "green")
lines(wordl.sensitivity, col = "blue")
lines(lex.score.sensitivity , col = "orange")
lines(topic.sensitivity, col = "brown")
legend("bottomright", c("none", "dist.score", "subj.score", "tweetlength", "wordlength",
                        "lexicon.score", "topic allocation"),
       text.col = c("black", "violet", "red", "green", "blue", "orange", "brown"))

legend("bottomleft", c("1 : Linear SVM", "2 : Radial SVM", "3 : Tree", 
                       "4 : Maxent", "5 : Random Forest", "6 : Bagging", "7 : Boosting"))


plot(1:7, none.specificity, type = "l", ylim = c(0,100), xlab = "Models",
     ylab = "Specificity in Percent", main = "Changes in Specificity by Single Features")
lines(dist_score.specificity, col = "violet")
lines(subj.score.specificity, col = "red")
lines(tweetl.spcecificity, col = "green")
lines(wordl.specificity, col = "blue")
lines(lex.score.specificity , col = "orange")
lines(topic.specificity, col = "brown")
legend("bottomright", c("none", "dist.score", "subj.score", "tweetlength", "wordlength",
                        "lexicon.score", "topic allocation"),
       text.col = c("black", "violet", "red", "green", "blue", "orange", "brown"))

legend("bottomleft", c("1 : Linear SVM", "2 : Kernel SVM", "3 : Tree", 
                       "4 : Maxent", "5 : Random Forest", "6 : Bagging", "7 : Boosting"))


