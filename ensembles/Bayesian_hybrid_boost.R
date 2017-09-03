#create df 0f single model predictions 
predlist = data.frame(C5.0.preds658, gbm.preds127, glmn.preds127, linsvm.preds127,
                      nb.preds5, nnet.preds127, nnet.preds127.overs, pda.preds127, 
                      pda.preds127.overs, rf.preds658, rf.preds127)


# with Hybrid Boost generated models
predlist = data.frame(C5.0.preds658, gbm.preds127, glmn.preds127, linsvm.preds127,
                      nb.preds5, nnet.preds127, nnet.preds127.overs, pda.preds127, c5.0.preds.boost,
                      pda.preds127.overs, rf.preds658, rf.preds127, nnet.preds.boost)

# initialize dataframe towhich to save ensemble predictions
init = rep(0, nrow(predlist))
enspreds = as.data.frame(init)
ensno = 2^ncol(predlist)

# initialize vectors in which posteriors and priors will be saved
posteriors = seq(0, ensno)

priors = rep(1, ncol(predlist))

# create bayesian weighted ensembles
for(i in seq(1, ensno,2))
{
  tempprior1 = rdirichlet(priors)
  tempprior2 = rdirichlet(priors)

  p1 = get_final_w_preds(predlist, tempprior1)
  enspreds[,i] = p1    
  p2 = get_final_w_preds(predlist, tempprior2)
  enspreds[,i+1] = p2  
  
  
  conf1 = confusionMatrix(p1, sentitest)
  conf2 = confusionMatrix(p2, sentitest)
  
  misscl = conf1$table[1,2] + conf1$table[2,1]
  corrcl = conf1$table[1,1] + conf1$table[2,2]
  error = 1 - conf1$overall[1]
  acc = conf1$overall[1]
  posterior1 = (1/ensno)*(((1 - error)^corrcl)*error^misscl)
  posteriors[i] = posterior1

  
  misscl = conf2$table[1,2] + conf2$table[2,1]
  corrcl = conf2$table[1,1] + conf2$table[2,2]
  error = 1 - conf2$overall[1]
  acc = conf2$overall[1]
  posterior2 = (1/ensno)*(((1 - error)^corrcl)*error^misscl)
   if(i+1 <= length(posteriors)){
    posteriors[i+1] = posterior2
   }
 
  addprior = if(posterior1 > posterior2){
    addprior = tempprior1
  }else{
    addprior = tempprior1
  }
  
  priors = priors + addprior
}

# last prediction is useless
posteriors = posteriors[-length(posteriors)]

bayesian_final = get_final_w_preds(enspreds, posteriors)

bayesian.conf = confusionMatrix(bayesian_final, sentitest)


