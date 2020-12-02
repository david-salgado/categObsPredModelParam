rocCurve <- function(data, targets, targetVars, probs){
  
  
  roc <- lapply(seq(along = targets), function(index.targets){
    
    lapply(probs, function(probs){
      roc(response = eval(parse(text=paste0('data$',targets[index.targets]))),
          predictor = eval(parse(text=paste0('data$',probs,index.targets))) )
      
      
    })
  })
  
  for (j in 1:length(targets)){
    names(roc[[j]]) <- paste0(probs,j)
    
  }
  
  names(roc) <- c(targetVars)
  
  return(roc)

}