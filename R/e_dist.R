#' Euclidean distance calculation between two matrices
#'
#' @param first_configuration The first pointcloud
#' @param second_configuration The second pointcloud
#'
#' Modified from the distmat() function in the pracma package for R
#' 
#' @keywords e_dist
#' @export
#' examples
#' e_dist()

e_dist <- function(first_configuration, second_configuration) {
	m <- nrow(first_configuration)
	n <- nrow(second_configuration)

	XY <- first_configuration %*% t(second_configuration)
	XX <- matrix(rep(apply(first_configuration * first_configuration, 1, sum), n), m, n, byrow = F)
	YY <- matrix(rep(apply(second_configuration * second_configuration, 1, sum), m), m, n, byrow = T)

	results <- sqrt(pmax(XX + YY - 2 * XY, 0))
	return(results)
}