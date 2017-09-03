
# powerset function taken from https://stackoverflow.com/questions/18715580/algorithm-to-calculate-power-set-all-possible-subsets-of-a-set-in-r
powerset = function(s){
  len = length(s)
  l = vector(mode="list",length=2^len) ; l[[1]]=numeric()
  counter = 1L
  for(x in 1L:length(s)){
    for(subset in 1L:counter){
      counter=counter+1L
      l[[counter]] = c(l[[subset]],s[x])
    }
  }
  return(l)
}



#combine prediction columns into final prediction
get_final_preds = function(preds)
{
  finalpred = rep(0,nrow(testset))
  for(i in 1:nrow(testset))
  {
    totmarg = sum(preds[i,]=="class1") - sum(preds[i,]=="class2")
    finalpred[i] = ifelse(totmarg > 0, "class1", "class2")
  }
  return(finalpred)
}


#combine weighted predictions
get_final_w_preds = function(preds, weights)
{
  finalpred = rep(0,nrow(preds))
  for(i in 1:nrow(preds))
  {
    r = preds[i,]
    totmarg = sum(weights[r  =="class1"]) - sum(weights[r  =="class2"])
    finalpred[i] = ifelse(totmarg > 0, "class1", "class2")
  }
  return(finalpred)
}



# exhaustively searches the ensemble space for all possible model combinations and returns the "best" combination + accuracy
# optionally a vector of weights for every model can be specified
exhaustive_ens_search = function(preds, sentiment, weights){
  s = names(preds)
  combs = powerset(s)
  
  #the first combination is empty
  combs[1] = NULL
  
  accs = rep(0, length(combs))
  
  if(missing(weights)){
    #generate vector of accuracies of all combinations
    for(i in 1:length(combs))
    {
      temppred = preds[,unlist(combs[i])]
      if(is.data.frame(temppred)==T){
        tempfinal <- get_final_preds(temppred)
      }else{
        tempfinal <- temppred
      }
      acc = confusionMatrix(tempfinal, sentitest)
      accs[i] = acc$overall[1]
    }
  }
  
  else{
    names(weights) = names(preds)
    for(i in 1:length(combs))
    {
      tempweights = weights[unlist(combs[i])]
      temppred = preds[,unlist(combs[i])]
      if(is.data.frame(temppred)==T){
        tempfinal <- get_final_w_preds(temppred, tempweights)
      }else{
        tempfinal <- temppred
      }
      acc = confusionMatrix(tempfinal, sentitest)
      accs[i] = acc$overall[1]
    }  
  }
  
  # return list where first element is max accuracy and second element contains combinations which achieved this acc
  bestcomb = combs[which(accs %in% max(accs))]
  best = list(accuracy = max(accs), combinations = bestcomb)
  return(best)
}
  




#function to randomly generate dirichlet variables based on an input vector 
#rdirichlet taken from https://stats.stackexchange.com/questions/23536/generating-from-dirichlet-distribution-with-the-differences-in-a-sequence-of-ord
rdirichlet = function(x){
  y = rgamma(length(x), x , 1)
  return(y/sum(y))
}




