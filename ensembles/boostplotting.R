
accs = rep(0,12)
sens = rep(0,12)
spec = rep(0,12)
corr = rep(0,12)

for(i in 1:12)
{
  cm = confusionMatrix(preds_test[,i], sentitest)
  accs[i] = cm$overall[1]
  sens[i] = cm$byClass[1]
  spec[i] = cm$byClass[2]
  if(i>1) corr[i] = sum(misses_test[,i] == FALSE & misses_test[,i-1] == TRUE) 
  
}

corr[5] = 0
corr[9] = 0

iterations = c(1,2,3,4)

par(mfrow = c(2,2))

plot(iterations, accs[1:4], ylim = c(0.6, 0.8), main = "Accuracy", type = "l", col = "blue",
    xaxt = "n",  ylab = "Accuracy", xlab = "Boosting Iterations")
axis(1, at = seq(1, 4, by = 1), las = 1)
lines(accs[5:8], col = "red")
lines(accs[9:12], col = "green")
legend("bottomleft", legend = c("C5.0", "pda", "nnet"), lty = 1,
       col = c("blue", "red", "green"))

plot(iterations, sens[1:4], ylim = c(0.6, 0.8), main = "Sensitivity", type = "l", col = "blue",
     xaxt = "n",  ylab = "Sensitivity", xlab = "Boosting Iterations")
axis(1, at = seq(1, 4, by = 1), las = 1)
lines(sens[5:8], col = "red")
lines(sens[9:12], col = "green")
legend("bottomleft", legend = c("C5.0", "pda", "nnet"), lty = 1,
       col = c("blue", "red", "green"))

plot(iterations, spec[1:4], ylim = c(0.5, 0.9), type = "l", main = "Specificity", col = "blue",
     xaxt = "n",  ylab = "Specificity", xlab = "Boosting Iterations")
axis(1, at = seq(1, 4, by = 1), las = 1)
lines(spec[5:8], col = "red")
lines(spec[9:12], col = "green")
legend("bottomleft", legend = c("C5.0", "pda", "nnet"), lty = 1,
       col = c("blue", "red", "green"))


plot(iterations, corr[1:4], ylim = c(0, 160), type = "l", main = "Corrected Observations", col = "blue",
     xaxt = "n",  ylab = "Corrected Observations", xlab = "Boosting Iterations")
axis(1, at = seq(1, 4, by = 1), las = 1)
lines(corr[5:8], col = "red")
lines(corr[9:12], col = "green")
legend("bottomleft", legend = c("C5.0", "pda", "nnet"), lty = 1,
       col = c("blue", "red", "green"))
