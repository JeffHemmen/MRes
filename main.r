init = function() {
  if(exists(".INIT"))
    return(T)
  print("Loading external functions...")
  source("common_functions.r")
  source("features.r")
  source("fitting.r")
  print("Loading external data...")
  loadData()
  .INIT <<- T
  
  maxSOE <<- 3000
  #any one feature can only shift the LR by a factor of maxSOE either way
  
  return(T)
}

runAnalysis = function(sp, acc) {
  #dichotomise speaker's data
  sp1.all     = data.all[which(data.all$SPEAKER==sp),]
  sp1.gender  <<- sp1.all[1, 2]
  #sp1.avg     = averageOut(sp1.all)
  
  bg.all	<<- data.all[which(data.all$SPEAKER!=sp & data.all$GENDER==sp1.gender),]
  
  #All other speakers of the same accent and same gender
  bg_acc.all <<- bg.all[which(bg.all$ACCENT==acc),]
  #bg_acc.avg = averageOut(bg_acc.all)
  bg_acc.names <<- unique(bg_acc.all$SPEAKER)
  
  #All speakers of all other accents, but the same gender
  bg_null.all <<- bg.all[which(bg.all$ACCENT!=acc),]
  #bg_null.avg = averageOut(bg_null.all)
  bg_null.names <<- unique(bg_null.all$SPEAKER)
  
  #initialise data structure for return vlaue
  res = list()
  res$speaker=sp
  res$accent=acc
  res$features = list()
  
  endLR = 1
  
  for(i in 1:numFeatures) {
    print(paste("[", count, "/", maxcount, "] Investigating feature", featureNames[i]))
    thisFeature = list()
    thisFeature$name = paste(i,featureNames[[i]])
    
    PFeat_bg    = runFeature(i, sp, bg_acc.names)
    PFeat_null  = runFeature(i, sp, bg_null.names)
    if(is.nan(PFeat_bg) || is.nan(PFeat_null)) {
      featLR = NaN
    } else if(PFeat_null==0 && PFeat_bg==0) {
      featLR = NaN
    } else if(PFeat_bg==0) {
      featLR = 1/maxSOE
    } else if(PFeat_null==0) {
      featLR=maxSOE
    } else {
      featLR = PFeat_bg/PFeat_null
      if(featLR >   maxSOE) {featLR = maxSOE}
      if(featLR < 1/maxSOE) {featLR = 1/maxSOE}
    }

    
    thisFeature$p_acc   = PFeat_bg
    thisFeature$p_null  = PFeat_null
    thisFeature$LR      = featLR
    res$features[[i]] = thisFeature
    
    if(is.finite(featLR)) {
      endLR = endLR * featLR
    }
  }
  
  res$LR = endLR
  return(res)
}

interactive = function() {
  init()
  
  print("All speakers:")
  print(unique(data.all[,3]))
  sp = readline(prompt="Select one speaker: ")
  
  print("All accents:")
  print(unique(data.all[,1]))
  acc <<- readline(prompt="Select one accent: ")
  count<<-1; maxcount<<-1
  analyses <<- list()
  analyses[[1]] <<- runAnalysis(sp, acc)
}

complete = function() {
  init()
  
  numAnalyses = 0
  analyses <<- list()
  
  count<<-1; maxcount<<-length(unique(data.all$SPEAKER))
  
  for(sp in unique(data.all$SPEAKER)) {
    for(acc in unique(data.all$ACCENT)) {
      print(paste("[", count, "/", maxcount, "] Analsying speaker ", sp, " for accent ", acc))
      flush.console()
      count = count + 1
      numAnalyses = numAnalyses + 1
      analyses[[numAnalyses]] <<- runAnalysis(sp, acc)
      flush.console()
    }
  }
  print(" D O N E .")
}

sampleTest=function() {
  init()
  count<<-1; maxcount<<-1
  sp = "brm_f_01"
  acc ="crn"
  
  analyses<<-list()
  analyses[[1]] <<- runAnalysis(sp, acc)
}

export = function() {
  
  exp = data.frame()
  for(i in 1:length(analyses)){
    ii=analyses[[i]]
    newLine = data.frame(SPEAKER=ii$speaker, ACCENT=ii$accent, LR=ii$LR)
    for(f in 1:numFeatures) {
      newFeat = data.frame(ii$features[[f]]$LR)
      colnames(newFeat) = ii$features[[f]]$name
      newLine = cbind(newLine, newFeat)
    }
    exp = rbind(exp, newLine)
  }
  write.table(exp, "out.csv", sep=",")
}
