#' two-dimensional pair-match function
#' 
#' This function takes input from the user to be pair-matched 
#' 
#' @param outlinedata The output from the outline.images() function
#' @param min The minmum number of points for matching in iterative closest point. This almost never needs to change
#' @param iter The number of iterations for iterative closest point
#' @param trans The type of transformation for iterative closest point. "Rigid", "Similarity", "Affine". If you need to remove size, use Similarity. Rigid is default 
#' @param threads The number of threads to use for processing. Default is 1
#' @param meanit The number of iterations to use around the mean. Default is 10.
#' @param testme The type of distance test to be utilized. "Regional", "Half", "Normal" Hausdorff distances
#' 
#' @keywords match.2d.invariant
#' @export
#' @examples
#' match.2d.invariant()

match.2d.invariant <- function(outlinedata = NULL, min = 1e+15, stdout = TRUE, sessiontempdir = NULL, oo = FALSE, iter = 10, trans = "rigid", threads=1, testme = "Segmented-Hausdorff", mspec = 1, meanit = 5, plotall = FALSE, nld = 1) {
	print("Two-dimensional pair match comparisons have started.")	
	library(Morpho)
	library(pracma)
	library(shapes)
	suppressMessages(library(compiler))
	enableJIT(3)

	workingdir = getwd()

	if(!stdout) { 
		if (!is.null(sessiontempdir)) {
			setwd(sessiontempdir)
		}
		direc <- randomstring(n = 1, length = 12)
		dir.create(direc)
		setwd(direc)
	}


	specmatrix <- outlinedata[[1]]

	homolog <<- array(NA,c(dim(specmatrix)[1], dim(specmatrix)[2], dim(specmatrix)[3]))
	namess <- dimnames(specmatrix)[[3]] #capture specimen names
	
	meann <<- specmatrix[,,mspec]
	homolog <<- specmatrix
	
	#shifts to long axis of specimens#
	D <- distmat(meann, meann)
	index <- apply(as.matrix(apply(D, 2, max)), 2, which.max)
	shiftm <- function(d, k) rbind(tail(d,k), head(d,-k), deparse.level = 0)
	index <- shiftm(as.matrix(1:nrow(meann)), -index)
	meann <<- meann[index,]
	#shifts to long axis of specimens#

	for(b in 1:meanit) {
		target <<- meann
		for(i in 1:dim(homolog)[3]) {
			moving <<- homolog[,,i]
			temp <<- icpmat(moving, target, iterations = iter, mindist = min, type = trans, threads=threads)
			homolog[,,i] <<- shiftmatrices(shape1 = temp, target = target, threads) #shifts matrices to match
			print(dimnames(homolog)[[3]][i])
		}
		
		meann <<- apply(homolog, c(1,2), mean)
	}



	
	dimnames(homolog)[[3]] <- namess #set specimen names again

#do these need to be global?
	matches <<- array(NA,c(dim(homolog)[3]*dim(homolog)[3], 3))
	nz <<- 1

	for(z in 1:length(outlinedata[[2]])) {
		for(x in length(outlinedata[[2]])+1:length(outlinedata[[3]])) {
			distance <- segmented_hausdorff_dist(homolog[,,z], homolog[,,x], testme = testme)
			matches[nz,] <- c(dimnames(homolog)[[3]][z], dimnames(homolog)[[3]][x], distance)
			print(distance)
			nz <<- nz + 1
		}
	}

	for(z in length(outlinedata[[2]])+1:length(outlinedata[[3]])) {
		for(x in 1:length(outlinedata[[2]])) {
			distance <- segmented_hausdorff_dist(homolog[,,z], homolog[,,x], testme = testme)
			matches[nz,] <- c(dimnames(homolog)[[3]][z], dimnames(homolog)[[3]][x], distance)
			print(distance)
			nz <<- nz + 1
		}
	}

	resmatches <- array()
	for(a in dimnames(homolog)[[3]]) {
		m <- matches[matches[,1] == a,]
		resmatches <- rbind(resmatches, m[order(m[,3], decreasing=FALSE),][1:nld,])
		
	}
	resmatches <- resmatches[-1,]

	if(plotall) {
		plot(meann, col="white", xlim=c(min(homolog),max(homolog)), ylim=c(max(homolog),min(homolog)), xlab="", ylab="")
		for(a in 1:dim(homolog)[3]) {
			points(homolog[,,a], col=a)	
		}
		points(meann, col="black", bg="blue", pch=23)
	}

	colnames(resmatches) <- c("ID", "Match-ID", "Distance")
	print("Two-dimensional pair match comparisons have completed.")	

	if(oo) {
		write.csv(matches, file = "potential-matches.csv", row.names=FALSE, col.names=TRUE)
		png(filename="registration.png")
		plot(meann, col="white", xlim=c(min(homolog),max(homolog)), ylim=c(max(homolog),min(homolog)), xlab="", ylab="")
		for(a in 1:dim(homolog)[3]) {
			points(homolog[,,a], col=a)	
		}
		points(meann, col="black", bg="blue", pch=23)
		dev.off()
	}
	gc()
	setwd(workingdir)
	enableJIT(0)
	return(list(homolog,resmatches,direc))

}