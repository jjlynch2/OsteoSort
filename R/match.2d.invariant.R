

match.2d.invariant <- function(specmatrix = NULL, min = 1e+15, iter = 1000, trans = "rigid", threads=8, testme = "test") {
	library(Morpho)
	library(pracma)
	library(shapes)

	homolog <<- array(NA,c(dim(specmatrix)[1], dim(specmatrix)[2], dim(specmatrix)[3]))
	namess <- dimnames(specmatrix)[[3]] #capture specimen names

	for(bb in 1:50) { #add a while mean is so far form the new mean instead of number of iterations!
		if(bb == 1) {mean <- specmatrix[,,1]; homolog <<- specmatrix}
		for(i in 1:dim(homolog)[3]) {
			target <<- mean
			moving <- homolog[,,i]
			temp <<- icpmat(moving, target, iterations = iter, mindist = min, type = trans, threads=threads)
			homolog[,,i]  <<- temp
		}
		mean <- apply(homolog, c(1,2), mean) #mean shape
	}

#shifts the landmarks to correspond with first coorespondance from mean
		shift <- function(d, k) rbind(tail(d,k), head(d,-k), deparse.level = 0)
	for(i in 1:dim(homolog)[3]) {
		meann <- apply(homolog, c(1,2), mean) #mean shape
		temp <- homolog[,,i]
		index <<- mcNNindex(target = meann, query = temp, cores = threads, k = 1)

		newindex <<- match(1,index)
		
		itt <<- 2 #start at 2
		while(is.na(newindex)) {
			newindex <<- match(itt,index)
			itt <<- itt + 1
		}
		newindex <<- newindex + itt


		if(newindex != 1) {
			newindex <- newindex - 1
			temp <- shift(temp, newindex)
			homolog[,,i] <- temp
		}
		if(newindex == 1) {
			homolog[,,i] <- temp
		}


	}

	dimnames(homolog)[[3]] <- namess #set specimen names again

	plot(apply(homolog, c(1,2), mean))
	for(a in 1:dim(homolog)[3]) {
		points(homolog[,,a], col=a)	
	}
#do these need to be global?
	matches <<- array(NA,c(dim(homolog)[3], 3))
	tempdistance <<- 9999999999999
	tempname <<- NA
		
	for(z in 1:dim(homolog)[3]) {
		homologtemp <- homolog[,,-z]
		for(x in 1:dim(homologtemp)[3]) {
			#distance <- abs(sqrt(rowSums((homolog[,,z] - homologtemp[,,x])^2)))
			#distance <- procdist(homolog[,,z], homologtemp[,,x], type = "Riemannian")
			distance <- segmented_hausdorff_dist(homolog[,,z], homologtemp[,,x], testme = testme)
			#dm1 <- distmat(homolog[,,z],homologtemp[,,x]))
			#dm2 <- dismat(homologtemp[,,x], homolog[,,z])
			#distance <- gromovdist(d1 = dm1, d2 = NULL, type = "l1", p=NULL)
			if(distance < tempdistance) {
				tempdistance <<- distance
				tempname <<- dimnames(homologtemp)[[3]][x]
			}

		}
		matches[z,] <- c(dimnames(homolog)[[3]][z], tempname, tempdistance)
		tempdistance <<- 9999999999999
		tempname <<- NA
	}

	return(list(homolog,matches))

}