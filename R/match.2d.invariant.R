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

match.2d.invariant <- function(outlinedata = NULL, min = 1e+15, iter = 1000, trans = "rigid", threads=1, testme = "regional") {
	library(Morpho)
	library(pracma)
	library(shapes)

	specmatrix <- outlinedata[[1]]

	homolog <<- array(NA,c(dim(specmatrix)[1], dim(specmatrix)[2], dim(specmatrix)[3]))
	namess <- dimnames(specmatrix)[[3]] #capture specimen names



	mean <- specmatrix[,,1]
	homolog <<- specmatrix
	for(i in 1:dim(homolog)[3]) {
		target <<- mean
		moving <- homolog[,,i]
		temp <<- icpmat(moving, target, iterations = iter, mindist = min, type = trans, threads=threads)
		homolog[,,i]  <<- temp
	}



	dimnames(homolog)[[3]] <- namess #set specimen names again

	plot(homolog[,,1], col="white")
	for(a in 1:dim(homolog)[3]) {
		points(homolog[,,a], col=a)	
	}
#do these need to be global?
	matches <<- array(NA,c(dim(homolog)[3], 3))
	tempdistance <<- 9999999999999
	tempname <<- NA
		
	for(z in 1:length(outlinedata[[2]])) {
		#homologtemp <- homolog[,,-z]
		for(x in length(outlinedata[[2]])+1:length(outlinedata[[3]])) {
			distance <- segmented_hausdorff_dist(homolog[,,z], homolog[,,x], testme = testme)
			if(distance < tempdistance) {
				tempdistance <<- distance
				tempname <<- dimnames(homolog)[[3]][x]
			}

		}
		matches[z,] <- c(dimnames(homolog)[[3]][z], tempname, tempdistance)
		tempdistance <<- 9999999999999
		tempname <<- NA
	}

	return(list(homolog,matches))

}