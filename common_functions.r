vowelDist = function(a, b){
	r = 0;
	for(i in 10:35) {
		p = a[i]-b[i];
		r = r + p*p;
	}
	r = sqrt(r);
	r = log(r)
	return(r[1,1]);
}

diphthongDist = function(a) {
  r = 0;
  for(i in 10:22) {
    p = a[i]-a[i+13];
    r = r + p*p;
  }
  r = sqrt(r);
  r = log(r)
  return(r[1,1]);
}

createDist = function(v1, v2) {
	v1l = length(v1[,1])
	v2l = length(v2[,1])
	if(v1l==0 || v2l==0)
		return(c(NaN))
	r = c();
	for(i1 in 1:v1l) { for(i2 in 1:v2l) {
		r = c(r, vowelDist(v1[i1,], v2[i2,]))
	} }
	return(r)
}

createDiphthongDist = function(v1) {
  v1l = length(v1[,1])
  if(v1l==0)
    return(c(NaN))
  r = c();
  for(i1 in 1:v1l) {
    r = c(r, diphthongDist(v1[i1,]))
  }
  return(r)
}

averageMFCCs = function(l) {
	res = l[1,]	#template only, MFCCs will be replaced
	#fields 1-4, 6-7, 9 should be the same throughout
	res[5] = "[aggregate]" #many contexts mushed into one aggregate value
	res[8] = "[aggregate]" #idem
	for(i in 10:35) {
		res[i] = mean(l[,i])
	}
	return(res)
}

#averageOut = function(all) {
#	res = data.frame()
#	for(sp in unique(all[, 3])) {
#		for(wls in unique(all[, 9])) {
#			tmp = averageMFCCs(all[which(all$SPEAKER==sp & all$WELLS==wls),])
#			res = rbind(res, tmp)
#		}	
#	}
#	return(res)
#}

loadData = function() {
	if(!exists("data.all")) {
		print("Loading data from file...")
		print("(Please select file 'anal2mfcc.csv'.)")
		flush.console()
		data.all <<- read.csv(file=file.choose(), sep=",", header=T)
		print("done.")
		flush.console()
	}
	#if(!exists("data.avg")) {
	#	print("Averaging tokens for each speaker...")
	#	flush.console()
	#	data.avg <<- averageOut(data.all)
	#	print("done.")
	#	flush.console()
	#}
}
	
