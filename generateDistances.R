init = function() {
  if(exists(".INIT"))
    return(T)
  print("Loading external functions...")
  source("common_functions.r")
  source("features.r")
  source("fitting.r")
  print("Loading external data...")
  loadData()
  .INIT = T
  return(T)
}


################################
################################

generateDistances = function() {
  init()
	loadData()
	distances <<- data.frame()
	for(sp in unique(data.all[,3])) {
	#for each speaker
		print(paste("Now processing: ", sp))
		print("    Isolating data from this speaker...")
		flush.console()
		data.sp <<- data.avg[which(data.avg$SPEAKER==sp),]
		print("        done.")
		flush.console()
		newLine <<- data.frame(ACCENT=data.sp[1,1], GENDER=data.sp[1,2], SPEAKER=sp, SPEAKER.NO=data.sp[1,4])
		for(i in 1:numFeatures) {
		#for each feature
			print(paste("    Current feature: ", featureNames[i]))
			featVal = getFeatures(data.sp, featureFunctions[[i]])
			newFeat = data.frame(featVal)
			colnames(newFeat) = featureNames[i]
			newLine <<- cbind(newLine, newFeat)
		}
		
		distances <<- rbind(distances, newLine)
	}
	print(" D O N E .")
	flush.console()
}