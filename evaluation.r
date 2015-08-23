Cllr = function(analyses) {
  Nso = 0;
  Ndo = 0;
  sub_so = 0;
  sub_do = 0;
  FALSE_NEGATIVES = 0;
  FALSE_POSITIVES = 0;
  for(i in 1:length(analyses)) {
    if(!is.finite(analyses[[i]]$LR)) {next()}
    if(substr(analyses[[i]]$speaker, 1, 3)==analyses[[i]]$accent) {
      #LR SHOULD be >1
      tmp = log2(1+(1/analyses[[i]]$LR))
      sub_so = sub_so + tmp
      Nso = Nso + 1
      if(analyses[[i]]$LR<1) {FALSE_NEGATIVES = FALSE_NEGATIVES+1}
    } else {
      #LR SHOULD be <1
      tmp = log2(1+analyses[[i]]$LR)
      sub_do = sub_do + tmp
      Ndo = Ndo + 1
      if(analyses[[i]]$LR>1) {FALSE_POSITIVES = FALSE_POSITIVES+1}
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