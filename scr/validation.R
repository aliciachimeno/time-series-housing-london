#################Validation#################################
validation=function(model){
  s=frequency(get(model$series))
  resi=model$residuals
  par(mfrow=c(2,2),mar=c(3,3,3,3))
  #Residuals plot
  plot(resi,main="Residuals")
  abline(h=0)
  abline(h=c(-3*sd(resi),3*sd(resi)),lty=3,col=4)
  #Square Root of absolute values of residuals (Homocedasticity)
  scatter.smooth(sqrt(abs(resi)),main="Square Root of Absolute residuals",
                 lpars=list(col=2))
  
  #Normal plot of residuals
  qqnorm(resi)
  qqline(resi,col=2,lwd=2)
  
  ##Histogram of residuals with normal curve
  hist(resi,breaks=20,freq=FALSE)
  curve(dnorm(x,mean=mean(resi),sd=sd(resi)),col=2,add=T)
  
  
  #ACF & PACF of residuals
  par(mfrow=c(1,2))
  acf(resi,ylim=c(-1,1),lag.max=60,col=c(2,rep(1,s-1)),lwd=2)
  pacf(resi,ylim=c(-1,1),lag.max=60,col=c(rep(1,s-1),2),lwd=2)
  par(mfrow=c(1,1))
  
  #Ljung-Box p-values
  par(mar=c(2,2,1,1))
  tsdiag(model,gof.lag=7*s)
  cat("\n--------------------------------------------------------------------\n")
  print(model)
  
  #Stationary and Invertible
  cat("\nModul of AR Characteristic polynomial Roots: ", 
      Mod(polyroot(c(1,-model$model$phi))),"\n")
  cat("\nModul of MA Characteristic polynomial Roots: ",
      Mod(polyroot(c(1,model$model$theta))),"\n")
  
  suppressMessages(require(forecast,quietly=TRUE,warn.conflicts=FALSE))
  plot(model)
  
  #Model expressed as an MA infinity (psi-weights)
  psis=ARMAtoMA(ar=model$model$phi,ma=model$model$theta,lag.max=36)
  names(psis)=paste("psi",1:36)
  cat("\nPsi-weights (MA(inf))\n")
  cat("\n--------------------\n")
  print(psis[1:24])
  
  #Model expressed as an AR infinity (pi-weights)
  pis=-ARMAtoMA(ar=-model$model$theta,ma=-model$model$phi,lag.max=36)
  names(pis)=paste("pi",1:36)
  cat("\nPi-weights (AR(inf))\n")
  cat("\n--------------------\n")
  print(pis[1:24])
  
  cat("\nDescriptive Statistics for the Residuals\n")
  cat("\n----------------------------------------\n") 
  
  suppressMessages(require(fBasics,quietly=TRUE,warn.conflicts=FALSE))
  ##Anderson-Darling test
  print(basicStats(resi))
  
  ## Add here complementary tests (use with caution!)
  ##---------------------------------------------------------
  cat("\nNormality Tests\n")
  cat("\n--------------------\n")
  
  ##Shapiro-Wilks Normality test
  print(shapiro.test(resi))
  
  suppressMessages(require(nortest,quietly=TRUE,warn.conflicts=FALSE))
  ##Anderson-Darling test
  print(ad.test(resi))
  
  suppressMessages(require(tseries,quietly=TRUE,warn.conflicts=FALSE))
  ##Jarque-Bera test
  print(jarque.bera.test(resi))
  
  cat("\nHomoscedasticity Test\n")
  cat("\n--------------------\n")
  suppressMessages(require(lmtest,quietly=TRUE,warn.conflicts=FALSE))
  ##Breusch-Pagan test
  obs=get(model$series)
  print(bptest(resi~I(obs-resi)))
  
  cat("\nIndependence Tests\n")
  cat("\n--------------------\n")
  
  ##Durbin-Watson test
  print(dwtest(resi~I(1:length(resi))))
  
  ##Ljung-Box test
  cat("\nLjung-Box test\n")
  print(t(apply(matrix(c(1:4,(1:4)*s)),1,function(el) {
    te=Box.test(resi,type="Ljung-Box",lag=el)
    c(lag=(te$parameter),statistic=te$statistic[[1]],p.value=te$p.value)})))
  
}
################# Fi Validation #################################