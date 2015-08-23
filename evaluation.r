Cllr = function() {
  Nso = 0;
  Ndo = 0;
  sub_so = 0;
  sub_do = 0;
  FALSE_NEGATIVES = 0;
  FALSE_POSITIVES = 0;
  for(i in 1:length(analyses.xls[,1])) {
    #cat("i=", i)
    if(substr(analyses.xls[i, "SPEAKER"], 1, 3)==analyses.xls[i, "ACCENT"]) {
      #LR SHOULD be >1
      tmp = log2(1+(1/analyses.xls[i,"LR"]))
      sub_so = sub_so + tmp
      Nso = Nso + 1
      if(analyses.xls[i,"LR"]<1) {FALSE_NEGATIVES = FALSE_NEGATIVES+1}
    } else {
      #LR SHOULD be <1
      tmp = log2(1+analyses.xls[i,"LR"])
      sub_do = sub_do + tmp
      Ndo = Ndo + 1
      if(analyses.xls[i,"LR"]>1) {FALSE_POSITIVES = FALSE_POSITIVES+1}
    }
  }
  print(cat(Nso, "TRUE  comparisons"))
  print(cat(Ndo, "FALSE comparisons"))
  print(cat(FALSE_POSITIVES, "FALSE POSITIVES"))
  print(cat(FALSE_NEGATIVES, "FALSE NEGATIVES"))
  sub_so = sub_so / Nso
  sub_do = sub_do / Ndo
  return((sub_do+sub_so)/2)
}

evaluate = function() {
  TP=0;FN=0;
  evalTable <<- data.frame()
  for(a in unique(data.all$ACCENT)) {
    print("----------")
    print(cat("ACCENT: ", a))
    sp.a = unique(data.all[which(data.all$ACCENT==a), "SPEAKER"])
    for(sp in sp.a) {
      newLine = data.frame(SPEAKER=sp, ACCENT=a)
      print(cat("    ", "Speaker: ", sp))
      index_best = 0
      LR_best = 0
      for(i in 1:length(analyses.xls[,1])) {
        if(!analyses.xls[i,"SPEAKER"]==sp) {
          next()
        }
        if(analyses.xls[i,"ACCENT"]==a) {
          print(cat("        LR for ", a, ": ", analyses.xls[i,"LR"]))
          newLine = cbind(newLine, data.frame(TRUE.LR=analyses.xls[i,"LR"]))
          if(analyses.xls[i,"LR"] < 1) {
            FN=FN+1
            #print(cat("    FALSE NEGATIVE"));
          }
        }
        if(analyses.xls[i,"LR"] > LR_best) {
          LR_best = analyses.xls[i,"LR"]
          index_best = i
        }
      }
      print(cat("        Best fit: ", analyses.xls[index_best,"ACCENT"], " (LR=", analyses.xls[index_best,"LR"], ")"))
      newLine = cbind(newLine, data.frame(BEST.ACC=analyses.xls[index_best,"ACCENT"], BEST.LR=analyses.xls[index_best,"LR"]))
      if(analyses.xls[index_best,"ACCENT"]==a) {
        print(cat("    TRUE POSITIVE"));
        TP=TP+1
      }
      evalTable <<- rbind(evalTable, newLine)
    }
  }
  #print("")
  print(cat("TRUE POSITIVES:  ", TP))
  print(cat("FALSE NEGATIVES: ", FN))
  print(cat("Correct 2nd, 3rd, ... choices: ", length(unique(data.all$SPEAKER))-TP-FN))
}