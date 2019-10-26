kmeans.3d <- function(data = NULL, clusters = NULL, threads = 1) {
	if(!is.null(data) && !is.null(clusters)) {
		data <- data[,c(1:3)]
		nr <- nrow(data)
		nr <- nr * clusters
		if(nr < 0) {nr <- 2}
		nr <- round(nr, digits = 0)
		km <- fastKmeans(data, k = nr, threads = threads)
	}
	return(km$centers)
}
