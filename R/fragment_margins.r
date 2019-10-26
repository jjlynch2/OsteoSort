fragment_margins <- function(configuration = NULL) {
	shiftm <- function(d, k) rbind(tail(d,k), head(d,-k), deparse.level = 0)
	nrr <- nrow(configuration)
	ed1 <- sqrt(sum( (configuration[1,] - configuration[2,]) ^2 ) )
	ed2 <- sqrt(sum( (configuration[2,] - configuration[3,]) ^2 ) )

	if(ed1 > ed2*10) {	 #shifts back if first and last landmark are fragment margins
		index <- shiftm(as.matrix(1:nrow(configuration)), -1)
		configuration <- configuration[index,]
	} 

	indices <- matrix(NA,nrow(configuration),2)
	for(r in 1: (nrow(configuration) - 2)) { #minus 2 for last pair of distances
		ed1 <- sqrt(sum( (configuration[r,] - configuration[r+1,]) ^2 ) )
		ed2 <- sqrt(sum( (configuration[r+1,] - configuration[r+2,]) ^2 ) )
		if(ed1 > ed2*10) {indices[r,] <- c(r,r+1)} #fragment margins
	}

	indices <- indices[!is.na(indices[,1]),]
	if(length(indices) == 2) {indices <- t(indices)}
	return(list(configuration, indices))
}
