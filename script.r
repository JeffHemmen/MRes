fitGauss = function(d) {
	dd = density(d)
	tab = data.frame(x=dd$x, y=dd$y)
	r = nls(y ~ dnorm(x, mean=mu, sd=sig), start=c(mu=mean(d), sig=sd(d)), data=tab)
	return(coef(r))
}

##############################################
#### FUNCTIONS ACTING ON GLOBAL VARIABLES ####
##############################################

runFeature = function(fDist) {
	sp1.d			<<- fDist(sp1.all)
	sp1.dd		<<- density(sp1.d)
	sp1.obs		<<- fDist(sp1.avg)[1]

	bg_acc.d		<<- fDist(bg_acc.avg)
	bg_acc.dd		<<- density(bg_acc.d)
	bg_acc.gauss	<<- fitGauss(bg_acc.d)

	bg_null.d		<<- fDist(bg_null.avg)
	bg_null.dd		<<- density(bg_null.d)
	bg_null.gauss	<<- fitGauss(bg_null.d)
}

allocateBackgroundPopulations = function(acc) {

	#All other speakers of the same accent and same gender
	bg_acc.all <<- bg.all[which(bg.all$ACCENT==acc),]
	bg_acc.avg <<- averageOut(bg_acc.all)

	#All speakers of all other accents, but the same gender
	bg_null.all <<- bg.all[which(bg.all$ACCENT!=acc),]
	bg_null.avg <<- averageOut(bg_null.all)
}

splitSpeakerOff = function(sp_str){
	sp1.all	<<- rawdata[which(rawdata$SPEAKER==sp_str),]
	sp1.accent	<<- sp1.all[1, 1]
	sp1.gender	<<- sp1.all[1, 2]
	sp1.avg	<<- averageOut(sp1.all)
	bg.all	<<- rawdata[which(rawdata$SPEAKER!=sp_str & rawdata$GENDER==sp1.gender),]
}

#######################
#### MAIN FUNCTION ####
#######################

main = function() {

  
  rawdata <<- read.csv(file=file.choose(), sep=",", header=T);
  print("All speakers:")
  print(unique(rawdata[,3]))
  sp_str = readline(prompt="Select one speaker: ")
  splitSpeakerOff(sp_str);
  
  print("All accents:")
  print(unique(rawdata[,1]))
  acc <<- readline(prompt="Select one accent: ")
  allocateBackgroundPopulations(acc)
  
  ####VARIABLES DEFINED AFTER THIS POINT:
  #sp1.	all | avg | accent | gender
  #bg.	all
  #bg_acc.	all | avg
  #bg_null.	all | avg
  
  LRs = c() #all LRs for the selected accent and speaker, one for each feature
  
  for(feature in ACTIVE_FEATURES) {
  	runFeature(feature)
  	p_acc = dnorm(sp1.obs, sd=bg_acc.gauss["sig"], mean=bg_acc.gauss["mu"])
  	cat("P(obs|acc) = ", p_acc)
  	p_null = dnorm(sp1.obs, sd=bg_null.gauss["sig"], mean=bg_null.gauss["mu"])
  	cat("P(obs|null) = ", p_null)
  	LR = p_acc/p_null
  	cat("LR = ", LR)
	LRs = c(LRs, LR)
  }
  
  LR = 1;
  for(fac in LRs) {
  	LR = LR * fac
  }
  
  cat("Final LR: ", LR)
}







###############taken out of code
#dichotomise speaker's data
#sp1.all     = data.all[which(data.all$SPEAKER==sp),]
#sp1.gender  = sp1.all[1, 2]
#sp1.avg     = averageOut(sp1.all)

#bg.all	= data.all[which(data.all$SPEAKER!=sp & data.all$GENDER==sp1.gender),]

#All other speakers of the same accent and same gender
#bg_acc.all = bg.all[which(bg.all$ACCENT==acc),]
#bg_acc.avg = averageOut(bg_acc.all)
#bg_acc.names

#All speakers of all other accents, but the same gender
#bg_null.all = bg.all[which(bg.all$ACCENT!=acc),]
#bg_null.avg = averageOut(bg_null.all)