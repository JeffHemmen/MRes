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
	distances <<- data.frame()
	count = 1; maxcount = length(unique(data.all[,3]))
	for(sp in unique(data.all[,3])) {
	#for each speaker
		print(paste("[", count, "/", maxcount, "] Now processing: ", sp))
	  count = count+1
		print("    Isolating data from this speaker...")
		flush.console()
		data.sp <<- data.all[which(data.all$SPEAKER==sp),]
		print("        done.")
		flush.console()
		newLine.sp <<- data.frame(ACCENT=data.sp[1,1], GENDER=data.sp[1,2], SPEAKER=sp, SPEAKER.NO=data.sp[1,4])
		for(i in 1:numFeatures) {
		#for each feature
			print(paste("    Current feature: ", featureNames[i]))
			newField = data.frame(FEATURE.NAME=featureNames[[i]])
			newLine.feat <<- cbind(newLine.sp, newField)
			featVals = getFeatures(data.sp, featureFunctions[[i]])
			for(f in featVals) {
			  newEntry = cbind(newLine.feat, data.frame(FEATURE.VALUE=f))
			  distances <<- rbind(distances, newEntry)
			}
		}
	}
	print(" D O N E .")
	flush.console()
}