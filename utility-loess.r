
# a simulation of an agent that observes noisy by noisily calculationg p*v only.
# it builds a model of a function that maps three calculation observations to expected value.

	# --------------------------------

deleteObjects <- function() rm(list=ls())
deleteObjects()

	# --------------------------------
	# use all but one of the cores on the machine. Once a cluster has been defined (here) then the function parLapply() distributes processing across the cluster.
	
library(parallel)

# Calculate the number of cores on the machine
nCores <- detectCores() - 1

# Initiate cluster
cores <- makeCluster(nCores, type="FORK")

print(paste("Using", nCores, "cores."))

	# --------------------------------
	
	# Wedell parameters

paramsWedell <- function(x) {	
	x$betaShape <- 1
	x$vDF <- 100
	x$vLocation <- 19.94583
	x$vScale <- 7.48888
	return(x)
}

params1 <- function() {
 	x <- list()
 	x <- paramsWedell(x)
 	x$uSD <- c(0, 1, 5)
 	x$alpha <- c(0.001, 0.5, 0.75, 1, 1.25, 1.5, 5, 10)
 	x$span <- c(0.25, 0.5, 1)
	x$nGenerate=1000
	x$nTest=1000
 	return(x)
}


params2 <- function() {
 	x <- list()
 	x$betaShape <- 1
	x$vMean <- 100
	x$vSD <- 5
 	x$uSD <- seq(0,100,by=10)
 	x$alpha <- 1
 	x$span <- c(0.4, 0.7, 1.00, 1.3)
	x$nGenerate=1000
	x$nTest=rep(100000,10)
 	return(x)
}

	# define which set of parameters will be used.
	
paramFunc <- params2

	# --------------------------------

	# shifted and scaled t distribution
	
rtls <- function(n, df, location, scale) {
  return(rt(n, df)*scale + location)
}

	# Gaussian noise
	
GaussianNoise <- function(n,sd) rnorm(n, mean=0, sd=sd)

	# sample matrices of probabilities and values from the distributions.
	# calcObs = Random Utility Theory

sampleV <- function(n, prms) {
	if(is.null(prms$vMean))
		v <- matrix(nrow=n, ncol=3, rtls(3*n, prms$vDF, prms$vLocation, prms$vScale))
	else 
		v <- matrix(nrow=n, ncol=3, rnorm(3*n, mean=prms$vMean, sd=prms$vSD))
	return(v)
}

	# sample P and V distributions and calculated EV and observations.
	
samplePV <- function(n, prms) {
	r <- list()
	r$p <- matrix(nrow=n, ncol=3, rbeta(3*n, shape1=prms$betaShape, shape2=prms$betaShape))
	r$v <- sampleV(n, prms)
	r$trueEV <- r$p*r$v
	r$calcObs <- (r$p^prms$alpha) * r$v + GaussianNoise(n*3, sd=prms$uSD)
	return(r)
}

	# return the max EV for each row of EV.
	
chooseMaxEV <- function(EV) {
	apply(EV,1,max)
}

	# utility fit finds a function that predicts trueEV given three calc observations.
	
utilityFit <- function(e, observation, prms) {
	eA <- e[,1]
	eB <- e[,2]
	eD <- e[,3]
	obsA <- observation[,1]
	obsB <- observation[,2]
	obsD <- observation[,3]
	A <- loess(eA ~ obsA+obsB+obsD, data=data.frame(eA,obsA,obsB,obsD), span=prms$span)
	B <- loess(eB ~ obsA+obsB+obsD, data=data.frame(eB,obsA,obsB,obsD), span=prms$span)
	D <- loess(eD ~ obsA+obsB+obsD, data=data.frame(eD,obsA,obsB,obsD), span=prms$span)
	return(list(Afit=A, Bfit=B, Dfit=D))
}

generateModel <- function(prms) {
	r <- samplePV(n=prms$nGenerate, prms)
	func <- utilityFit(r$trueEV, r$calcObs, prms)
	return(func)
}

	# given the loess func and an observation obs of all three optoins, predict the EV of each choice.
	# NOTE need to check whether calcObs are named as expected.  In utility fit they are named obsA, obsB, obsD but these names are not used here. Is that okay?
	
predictEV <- function(obs, func) {
		A <- predict(func$Afit,newdata=obs$calcObs)
		B <- predict(func$Bfit,newdata=obs$calcObs)
		D <- predict(func$Dfit,newdata=obs$calcObs)
	return(cbind(A,B,D))
}

	# this is the predicted EV for the calc observation.
	# works by extracting funcChoice on every row of trueEV.
	# funcChoice is the choice predicted by the loess func approximation for calc Obs.
	
computecalcEV <- function(n, trueEV, funcChoice) {
	mean(as.numeric(sapply(1:n, function(x) trueEV[x,funcChoice[x]])), na.rm=T)
}

	# sample randomly from each row of EV and take the mean.
	
computeRandomChoiceEV <- function(n, trueEV, funcChoice) {
	mean(apply(trueEV,1,function(x) sample(x,1)), na.rm=T)
}

	# test model by predicting choice on a new set of samples.
	# func is the loess function approximation for the calculation observation.
	
testModel <- function(prms, func) {
	n <- prms$nTest
	r <- samplePV(n=n, prms)
	funcChoice <- as.numeric(apply(predictEV(r, func),1,which.max))
	smmry <- list()
	smmry$calcEV <- computecalcEV(n, r$trueEV, funcChoice)
	smmry$trueEV <- mean(chooseMaxEV(r$trueEV), na.rm=T)
	smmry$randomChoiceEV <- computeRandomChoiceEV(n, r$trueEV, funcChoice)
	return(smmry)
}

printf <- function(...) {
	print(paste(..., sep="")	)
}

	# used in the filename
	
prettyDate <- function() {
	dt <- gsub(" ","-",date())
	return(gsub(":","-",dt))
}

	# write the results to a csv file
	
writeResults <- function(results) {
	R <- list()
	for( i in 1:length(results)) {
		r <- results[[i]]
		R[[i]] <- as.character(c( r$prms, r$smmry))
	}
	rn <- c(names(results[[1]]$prms), names(results[[1]]$smmry))
	R <- t(as.data.frame(R, row.names=rn))
	csvName <- paste("../results/results-",prettyDate(),sep="")
	write.csv(file=paste(csvName,".csv",sep=""), R, quote=F, row.names=F)
	save(file="../results/latest-results-filename", csvName)
	return(R)
}

	# generate and test a value function for parameter set i.
	
evalSettings <- function(i, prmsGrid) {
	printf("processing parameter set ",i, " of ", nrow(prmsGrid))
	r <- list()
	r$prms <- prmsGrid[i,]
	r$func <- generateModel(prmsGrid[i,])
	r$smmry <- testModel(prmsGrid[i,], r$func)
	return(r)
}

	# build a parameter grid with one row for each set of parameters and then use parallel apply to generate predictions (results) for each set of parameters. 
	
main <- function() {
	Start <- Sys.time()
	prmsList <- paramFunc()
	print(prmsList)
	prmsGrid <- expand.grid(prmsList)
	results <- list()
	#results <- lapply( 1:nrow(prmsGrid), evalSettings, prmsGrid)
	print("Distribute computing...")
	results <- parLapply( cores, 1:nrow(prmsGrid), evalSettings, prmsGrid)
	writeResults(results)
	End <- Sys.time()
	print(End-Start)
}

	# export the environment to the cores.
	
clusterExport(cores, ls())

out <- main()

print("finished.")

