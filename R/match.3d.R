#' match.3d Input Function
#' testing code
#' @param spec1 First specimen configuration
#' @param spec2 Second specimen configuration
#' @param min minimum distance between points in ICP
#' @param iter number of iterations for ICP
#' @param trans type of transformation (affine, rigid, similarity)
#' @param plot produce plot
#' @param aspect aspect of plot
#' @param GPA use GPA or just ICP
#' @param meanshape calculate mean shape
#' @keywords match.3d
#' @export
#' @examples 
#' match.3d()

match.3d <- function(spec1 = NULL, spec2 = NULL, min = 10, iter = 1000, trans = "rigid", plot = TRUE, aspect = c(4,1,1), GPA = TRUE, meanshape = FALSE, regression = FALSE) {
	options(warn = -1)
	if(is.null(spec1) || is.null(spec2)) { return(NULL)}

	#libraries
	require(Morpho)
	if(plot) {require(rgl)}
	if(regression) {require(TriDimRegression)}

	######transform to matrix######
	spec1 <- as.matrix(spec1)
	spec2 <- as.matrix(spec2)
	######transform to matrix######

	######mirrors######depricated since ICP does reflecting. Keeping this here as an example.
	#if(mirror) {
	#	for(i in 1:length(spec1)) {
	#		spec1[i] <- spec1[i] - (spec1[i] * 2)	
	#	}
	#}
	######mirrors######


	######transforms the specimen with lower number of points
	if(length(spec1) < length(spec2)) {
		moving <- spec1
		target <- spec2
	} 
	if(length(spec2) < length(spec1)) {
		moving <- spec2
		target <- spec1
	}
	######transforms the specimen with lower number of points

	moving <- icpmat(moving, target, iterations = iter, mindist = min, type = trans)
	index <- mcNNindex(target, moving, cores = 1, k = 1)
	target <- target[index,]

	if(length(target) != length(moving)) {
		print("You shouldn't be in this logic statement.... Something seriously went wrong with the code above me\n")
	}

	if(!GPA) {
		if(plot) {
			open3d()
			plot3d(moving, col = 4)
			points3d(target, col = 3)
			aspect3d(aspect)
		}
		dist <- (sqrt(sum((moving - target)^2)))
		size <- (cSize(moving) - cSize(target))
		array3d <- array(0,c(nrow(moving),ncol(moving),2))
		array3d[,,1] <- as.array(moving)
		array3d[,,2] <- as.array(target)
		if(meanshape) {
			consensus = apply(array3d, c(1,2), mean)
			points3d(consensus, col = 2)
		}
	}

	if(GPA) {
		array3d <- array(0,c(nrow(moving),ncol(moving),2))
		array3d[,,1] <- as.array(moving)
		array3d[,,2] <- as.array(target)

		Y.gpa <- ProcGPA(array3d, CSinit = TRUE)
	
		specimen1 <- Y.gpa$rotated[,,1]
		specimen2 <- Y.gpa$rotated[,,2]
		if(plot) {
			open3d()
			plot3d(specimen1, col = 4)
			points3d(specimen2, col = 3)
			aspect3d(aspect)
		}	
		dist <- (sqrt(sum((specimen1 - specimen2)^2)))
		size <- (cSize(specimen1) - cSize(specimen2))
		if(meanshape) {
			consensus = apply(Y.gpa$rotated, c(1,2), mean)
			points3d(consensus, col = 2)
		}
	}
	
	if(regression) {
	
	
	}
	
	if(meanshape) {
		return(list(dist,size,consensus))
	}
	else
		return(list(dist,size))

}