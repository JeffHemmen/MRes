quickfix = function() {
  for(i in 1:length(analyses.HAPPY)) {
    
    if(analyses.noHAPPY[[i]]$speaker != analyses.HAPPY[[i]]$speaker || analyses.noHAPPY[[i]]$accent != analyses.HAPPY[[i]]$accent)
      cat("Mismatch at index", i)
    analyses.noHAPPY[[i]]$features[[7]] = analyses.HAPPY[[i]]$features[[1]]
    analyses.noHAPPY[[i]]$LR = analyses.noHAPPY[[i]]$LR * analyses.HAPPY[[i]]$LR
  }
  
}