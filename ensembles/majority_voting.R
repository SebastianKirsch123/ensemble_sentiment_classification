# simple majority voting preds
predlist = data.frame(C5.0.preds658, gbm.preds127, glmn.preds127, linsvm.preds127,
                      nb.preds5, nnet.preds127, nnet.preds127.overs, pda.preds127,
                      pda.preds127.overs, rf.preds658, rf.preds127)

predlist = data.frame(lapply(predlist, as.numeric))
rowtotal = rowSums(predlist)
#scale it back to a value between 1 and 2
average = rowtotal/ncol(predlist)
totpreds = ifelse(average < 1.5, 1, 2)
sentitest = as.numeric(sentitest)
confusionMatrix(table(totpreds, sentitest))


# simple majority voting probs
problist = data.frame(C5.0.probs658$class2, gbm.probs127$class2, glmn.probs127$class2, 
                      linsvm.probs127$class2,
                      nnet.probs127$class2, nnet.probs127.overs$class2, pda.probs127$class2,
                      pda.probs127.overs$class2, rf.probs658$class2, rf.probs127$class2)

problist = data.frame(lapply(problist, as.numeric))
rowtotal = rowSums(problist)
#scale it back to a value between 1 and 2
average = rowtotal/ncol(problist)
totprob = ifelse(average < .5, 1, 2)
sentitest = as.numeric(sentitest)
confusionMatrix(table(totprob, sentitest))


# maj vote
maj.vote = get_final_preds(predlist)
confusionMatrix(maj.vote, sentitest)



# to shorten varaible names
colnames(problist) = colnames(predlist)[-5]




#exhaustive search
bestcomb = exhaustive_ens_search(predlist, sentitest)



#exhaustive search with boosting

predlist = data.frame(C5.0.preds658, gbm.preds127, glmn.preds127, linsvm.preds127,
                      nb.preds5, nnet.preds127, nnet.preds127.overs, pda.preds127, c5.0.preds.boost,
                      pda.preds127.overs, rf.preds658, rf.preds127, nnet.preds.boost)
#or
#secondboost = preds_test[, c(2, 6)]
#predlist = cbind(predlist, secondboost)

bestcomb = exhaustive_ens_search(predlist, sentitest)