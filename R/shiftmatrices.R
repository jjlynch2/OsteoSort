#' Nearest neighbor matrices shifting function
#' 
#' 
#' @param shape1 The two-dimensional shape configuration to be moved
#' @param target The two-dimensional shape target to be shifted to
#' @param threads The number of threads to use
#' 
#' @keywords shiftmatrices
#' @export
#' @examples
#' shiftmatrices()

shiftmatrices <- function(shape1, target, threads=1) {

	index <- mcNNindex(shape1, target, k = 1, threads = threads)

	if(sum(diff(index) < 0) > nrow(target)/2) {A <-2}
	if(sum(diff(index) > 0) > nrow(target)/2) {A <-1}

	if(A == 1) {
		if(index[1] != 1) {
			index <- c(index[1]:nrow(target),1:(index[1]-1))
		}
		else {
			index <- 1:nrow(target)
		}
	}

	if(A == 2) {
		index <- c(index[1]:1,nrow(target):(index[1]+1))
	}

	shape1 <- shape1[index,]
	return(shape1)
}





