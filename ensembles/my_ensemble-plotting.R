#my_ensembles

# exhaustive search on base models
predlist = data.frame(C5.0.preds658, gbm.preds127, glmn.preds127, linsvm.preds127,
                      nb.preds5, nnet.preds127, nnet.preds127.overs, pda.preds127,
                      pda.preds127.overs, rf.preds658, rf.preds127)

#maj vote of base
maj.vote = get_final_preds(predlist)
confusionMatrix(maj.vote, sentitest)

#exh search
exh.maj.11 = exhaustive_ens_search(predlist, sentitest)


#simple boosting vote
final = get_final_preds(preds_test)
confusionMatrix(final, sentitest)

exh.boost = exhaustive_ens_search(preds_test, sentitest)



#base with boosting
predlist = data.frame(C5.0.preds658, gbm.preds127, glmn.preds127, linsvm.preds127,
                      nb.preds5, nnet.preds127, nnet.preds127.overs, pda.preds127, c5.0.preds.boost,
                      pda.preds127.overs, rf.preds658, rf.preds127, nnet.preds.boost)

#maj vote of all
maj.vote = get_final_preds(predlist)
all.maj = confusionMatrix(maj.vote, sentitest)

#exh search
exh.maj.boost14 = exhaustive_ens_search(predlist, sentitest)



#bayesian base models



# require generating enspreds through the Bayesian Model combination file 
bayesian_final = get_final_w_preds(enspreds, posteriors)


#bayesian boosted models
bayesian_final = get_final_w_preds(enspreds, posteriors)




accuracies = c(0.775, 0.7163, 0.7725, 0.8325, 0.8412, 0.7812, 0.8438)
names(accuracies) = c("Majority", "AdaBoost", "StandaloneHybridBoost", "HybridBoost+Maj", "ExhHybridBoost+Maj", 
                       "BaseBayes", "BayesianHBoost")
nam = c("Majority", "AdaBoost", "StandaloneHybridBoost", "HybridBoost+Maj", "ExhHybridBoost+Maj", "BaseBayes", "BayesianHBoost")


par(mar = c(11,4,4,2))
barplot(accuracies, ylim = c(0.65, 0.9),  main = "New Ensemble Comparison", ylab = "Accuracy" , xpd = F,
        names.arg = c("Majority Voting", "AdaBoost", "Standalone Hybrid Boost", "Hybrid Boost + Maj", "Exh Hybrid Boost + Maj",
                      "Bayesian Model C", "Bayesian Hybrid Boost" ), horiz =F, las = 2)





accuracies = c(0.7725, 0.8325, 0.8412, 0.7812, 0.8438)
names(accuracies) = c("Boost", "Simple Boost + Maj", "Exh Boost + Maj", "Base Bayes", "Boost + Bayes")
nam = c("Boost", "Simple Boost + Maj", "Exh Boost + Maj", "Base Bayes", "Boost + Bayes")

par(mar = c(11,4,4,2))
barplot(accuracies, ylim = c(0.65, 0.9),  main = "New Ensemble Results", ylab = "Accuracy" , xpd = F,
        names.arg = nam, horiz =F, las = 2)

axis(1, at = 0:4, labels = c("Boost", "Simple Boost + Maj", "Exh Boost + Maj", "Base Bayes", "Boost + Bayes"))
