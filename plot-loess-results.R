deleteObjects <- function() rm(list=ls())
deleteObjects()

results <- "results-Wed-Mar-16-17-18-19-2016"

load(file="../results/latest-results-filename")
results <- csvName
print(results)


	# get a matrix of values y with x columns and z rows.
	
getMatrix <- function(x,y,z) {
	xSet <- as.numeric(levels(factor(d[,x])))
	zSet <- as.numeric(levels(factor(d[,z])))
	dm <- list()
	dm[[z]]=zSet; dm[[x]]=xSet
	r <- matrix(nrow=length(zSet), ncol=length(xSet), dimnames=dm)
	for(i in 1:nrow(r)) {
		for(j in 1:ncol(r)) {
			s <- subset(d, d[,x]==xSet[j] & d[,z]==zSet[i])
			r[i,j] <- mean(s[,y], na.rm=T)
		}
	}
	return(r)
}

	# takes three strings that define the a plot of x against y for all levels of z. 
	# works by first getting a matrix of x by z values of y. Then plots each z.
	
plotResults <- function(xParam, yParam, zParam, lgnd="topright") {
	m <- getMatrix(xParam, yParam, zParam)
	x <- as.numeric(colnames(m))
	yNames <- rownames(m)
	xx <- c(min(x),max(x))
	print(m)
	yy <- c(min(m)-sd(m),max(m)+sd(m))

	plot(0, type="n", xlim=xx, ylim=yy, xlab=xParam, ylab=yParam)
	for( i in 1:nrow(m)) {
		lines(x, m[i,], col=i)
		points(x, m[i,], col=i, pch=i)
	}
	legend(lgnd, yNames, col=1:length(yNames), pch=1:length(yNames), title=zParam)
	return(list(m,x,yy))
}

plotMore <- function(xParam, yParam, zParam) {
	m <- getMatrix( xParam, yParam, zParam )
	x <- as.numeric(colnames(m))
	print(m)
	print(x)
	for( i in 1:nrow(m)) {
		lines(x, m[i,], col=i, lty=2)
		points(x, m[i,], col=i, pch=i, lty=2)
	}
}

d <- read.csv(file=paste(results,".csv",sep=""))

pdf(file=paste(results,".pdf",sep=""))
#plotResults("alpha","calcEV","uSD")
#plotResults("alpha","funcChoiceP","uSD")
#plotResults("alpha","calcEV","span")
#plotResults("uSD","calcEV","vLocation")

plotResults("uSD", "calcEV", "span")
plotMore( "uSD", "randomChoiceEV", "span" )
plotMore( "uSD", "trueEV", "span" )

graphics.off()
