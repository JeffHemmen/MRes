fitGauss = function(d) {
  dd = density(d, na.rm=T)
  tab = data.frame(x=dd$x, y=dd$y)
  r = nls(y ~ dnorm(x, mean=mu, sd=sig), start=c(mu=mean(d, na.rm=T), sig=sd(d, na.rm=T)), data=tab)
  res = list(fit="GAUSS", fitID=1, mu=coef(r)[["mu"]], sig=coef(r)[["sig"]], residual=r$m$deviance())
  return(res)
}

fitGamma = function(d) {
  dd = density(d, na.rm=T)
  tab = data.frame(x=dd$x, y=dd$y)
  mu = mean(d, na.rm=T)
  sig = sd(d, na.rm=T)
  var = sig*sig
  r = nls(y ~ dgamma(x, shape=shp, scale=scl), start=c(scl=var/mu, shp=mu*mu/var), data=tab)
  res = list(fit="GAMMA", fitID=2, shp=coef(r)[["shp"]], scl=coef(r)[["scl"]], residual=r$m$deviance())
  return(res)
}

fitWeibull = function(d) {
  dd = density(d, na.rm=T)
  tab = data.frame(x=dd$x, y=dd$y)
  r = nls(y ~ dweibull(x, shape=shp, scale=scl), start=c(shp=5, scl=mean(d, na.rm=T)), data=tab)
  res = list(fit="WEIBULL", fitID=3, shp=coef(r)[["shp"]], scl=coef(r)[["scl"]], residual=r$m$deviance())
  return(res)
}
#####
numFits = 3
fitFunctions = list(fitGauss, fitGamma, fitWeibull)
#make sure this order matches the "fitID"s in the respective fit functions above and in getFitImage below!
#####

bestFit = function(d) {
  fits = list()
  lowi = 1
  for(i in 1:numFits) {
    fits[[i]]=list(residual=Inf)
    tryCatch(expr={fits[[i]]=fitFunctions[[i]](d)}, warning=function(e){}, error=function(e){})
    if(fits[[i]]$residual<fits[[lowi]]$residual) {lowi = i}
  }
  if(fits[[lowi]]$residual==Inf) {
    res = list(fit="SIMPLE", fitID=0, mu=mean(d, na.rm=T), sig=sd(d, na.rm=T), residual=NaN)
    return(res)
  }
  return(fits[[lowi]])
}

getFitImage = function(fit, obs) {
  if(fit$fitID==0 | fit$fitID==1) {
    return(dnorm(obs, mean=fit$mu, sd=fit$sig))
  }
  if(fit$fitID==2) {
    return(dgamma(obs, shape=fit$shp, scale=fit$scl))
  }
  if(fit$fitID==3) {
    return(dweibull(obs, shape=fit$shp, scale=fit$scl))
  }
}