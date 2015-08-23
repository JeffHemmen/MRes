myplot = function() {
  
  #obs <<- distances[which(distances$SPEAKER==sp & distances$FEATURE.NAME==featureNames[featureID]),"FEATURE.VALUE"]
  #pop <<- distances[which(distances$SPEAKER %in% pop_n & distances$FEATURE.NAME==featureNames[featureID]),"FEATURE.VALUE"]
  #
  #obs.avg = mean(obs, na.rm=T)
  #
  #fit = bestFit(pop)
  #p_obs = getFitImage(fit, obs.avg)
  
  pop_acc = distances[which(distances$SPEAKER %in% bg_acc.names & distances$FEATURE.NAME==featureNames[featureID]),"FEATURE.VALUE"]
  pop_null = distances[which(distances$SPEAKER %in% bg_null.names & distances$FEATURE.NAME==featureNames[featureID]),"FEATURE.VALUE"]
  
  pop_acc.d = density(pop_acc, na.rm = T)
  pop_null.d = density(pop_null, na.rm = T)
  
  plot(pop_acc.d, xlim=c(0.1, 1.45), ylim=c(0, 3), col="light blue")
  par(new=T)
  curve(dgamma(x, shape=14.69572, scale=0.03597472), xlim=c(0.1, 1.45), ylim=c(0, 3), col="blue")
  
  par(new=T)
  
  plot(pop_null.d, xlim=c(0.1, 1.45), ylim=c(0, 3), col="orange")
  par(new=T)
  curve(dgamma(x, shape=7.241072, scale=0.139504), xlim=c(0.1, 1.45), ylim=c(0, 3), col="red")
  
  obs.avg  = mean(obs, na.rm=T)
  acc.avg  = mean(pop_acc, na.rm=T)
  null.avg = mean(pop_null, na.rm=T)
  
  par(new=T)
  
  
}