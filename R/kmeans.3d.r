#' k-means simplification wrapper
#' 
#'
#' @keywords kmeans.3d
#' @export
#' @examples
#' kmeans.3d()

kmeans.3d <- function(data = NULL, clusters = NULL, iter = 10) {
	if(!is.null(data) && !is.null(clusters)) {
		data <- data[,c(1:3)]
		nr <- nrow(data)
		nr <- nr * clusters
		if(nr < 0) {nr <- 2}
		nr <- round(nr, digits = 0)
		km <- KMeans_arma(data, clusters = nr, n_iter = iter, seed_mode = "random_subset", verbose = T, CENTROIDS = NULL)
	}
	return(km)
}