bestfeat[max(bestfeat$accuracy) == bestfeat$accuracy,]
bestfeat[max(bestfeat$sensitivity) == bestfeat$sensitivity,]
bestfeat[max(bestfeat$specificity) == bestfeat$specificity,]



#Here we plot The accuracy, specificity and sensitivity for every model depending on
# various parameter variations

#Linear SVM
plot(bestfeat1$cost[0:20], bestfeat1$accuracy[0:20], type = "l", ylim = c(0.6,0.9), 
     main = "SVM (Linear Kernel) Performance", xlab = "Cost", ylab = "Performance")
lines(bestfeat1$cost[0:20], bestfeat1$sensitivity[0:20], type = "l", col = "red")
lines(bestfeat1$cost[0:20], bestfeat1$specificity[0:20], type = "l", col = "green")
legend("bottomleft", legend = c("Accuracy", "Sensitivity", "Specificity"), lty = 1,
       col = c("black", "red", "green"))



#Radial SVM
plot(bestfeat2$cost[0:20], bestfeat2$accuracy[0:20], type = "l", ylim = c(0.6,0.9), 
     main = "SVM (Radial Kernel) Performance", xlab = "Cost", ylab = "Performance")
lines(bestfeat2$cost[0:20], bestfeat2$sensitivity[0:20], type = "l", col = "red")
lines(bestfeat2$cost[0:20], bestfeat2$specificity[0:20], type = "l", col = "green")
legend("bottomleft", legend = c("Accuracy", "Sensitivity", "Specificity"), lty = 1,
       col = c("black", "red", "green"))





# Random Forest
plot(bestfeat5$cost[0:20], bestfeat5$accuracy[0:20], type = "l", ylim = c(0.6,0.9), 
     main = "Random Forest Performance", xlab = "Number of Trees", ylab = "Performance")
lines(bestfeat5$cost[0:20], bestfeat5$sensitivity[0:20], type = "l", col = "red")
lines(bestfeat5$cost[0:20], bestfeat5$specificity[0:20], type = "l", col = "green")
legend("bottomleft", legend = c("Accuracy", "Sensitivity", "Specificity"), lty = 1,
       col = c("black", "red", "green"))



#Boosting
plot(bestfeat7$cost[0:20], bestfeat7$accuracy[0:20], type = "l", ylim = c(0.2,0.9), 
     main = "Boosting Performance", xlab = "Boosting Iterations", ylab = "Performance")
lines(bestfeat7$cost[0:20], bestfeat7$sensitivity[0:20], type = "l", col = "red")
lines(bestfeat7$cost[0:20], bestfeat7$specificity[0:20], type = "l", col = "green")
legend("bottomleft", legend = c("Accuracy", "Sensitivity", "Specificity"), lty = 1,
       col = c("black", "red", "green"))



# Here we plot the change in accuracy for every model depending on the number of features

cutoff = c(0, 0.1, 0.5, 0.75, 1)
most_imp_feat = impvar.ord$featnames[impvar.ord$Overall > 1]


no.features = c(1219, 658, 174, 104, 79)

rf.acc.means = rep(NA, 5)
rf.acc.means[1] = mean(bestfeat5$accuracy[0:20])
rf.acc.means[2] = mean(bestfeat5$accuracy[21:40])
rf.acc.means[3] = mean(bestfeat5$accuracy[41:60])
rf.acc.means[4] = mean(bestfeat5$accuracy[61:80])
rf.acc.means[5] = mean(bestfeat5$accuracy[81:100])

svml.acc.means = rep(NA, 5)
svml.acc.means[1] = mean(bestfeat1$accuracy[0:20])
svml.acc.means[2] = mean(bestfeat1$accuracy[21:40])
svml.acc.means[3] = mean(bestfeat1$accuracy[41:60])
svml.acc.means[4] = mean(bestfeat1$accuracy[61:80])
svml.acc.means[5] = mean(bestfeat1$accuracy[81:100])


svmr.acc.means = rep(NA, 5)
svmr.acc.means[1] = mean(bestfeat2$accuracy[0:20])
svmr.acc.means[2] = mean(bestfeat2$accuracy[21:40])
svmr.acc.means[3] = mean(bestfeat2$accuracy[41:60])
svmr.acc.means[4] = mean(bestfeat2$accuracy[61:80])
svmr.acc.means[5] = mean(bestfeat2$accuracy[81:100])

boost.acc.means = rep(NA, 5)
boost.acc.means[1] = mean(bestfeat7$accuracy[0:20])
boost.acc.means[2] = mean(bestfeat7$accuracy[21:40])
boost.acc.means[3] = mean(bestfeat7$accuracy[41:60])
boost.acc.means[4] = mean(bestfeat7$accuracy[61:80])
boost.acc.means[5] = mean(bestfeat7$accuracy[81:100])

tree.acc = bestfeat3$accuracy
maxent.acc = bestfeat4$accuracy
bag.acc = bestfeat6$accuracy



par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(no.features, rf.acc.means, type = "l", ylim = c(0.6, 0.8),bty='L', 
     main = "Accuracy by Number of Features", xlab = "Number of Features", ylab = "Accuracy")
lines(no.features, svml.acc.means, col = "red")
lines(no.features, svmr.acc.means, col = "green")
lines(no.features, tree.acc, col = "blue")
lines(no.features, maxent.acc, col = "orange")
lines(no.features, bag.acc, col = "purple")
lines(no.features, boost.acc.means, col = "brown")
legend("bottomright", legend = c("RF", "SVM (Linear)", "SVM (Radial)", "Tree", 
                                "Maxent", "Bagging", "Boosting"), lty = 1,
       col = c("black", "red", "green", "blue", "orange", "purple", "brown"), inset=c(-0.35,0))