#' RGB coordinate extractor
#' 
#'
#' @keywords RGB.locator.3d()
#' @export
#' @examples
#' RGB.locator.3d()

RGB.locator.3d <- function(align_data, type = "landmark",r = c(255,0,0), g = c(0,255,0), b = c(0,0,255), f = c(0,0,0), f_threshold = 100, threads = 1) {
	setThreadOptions(threads)
	a <- 0
	b <- 0

	align_data <- as.matrix(align_data)

	if(type == "landmark" || type == "both") {
		r <- as.matrix(t(r))
		g <- as.matrix(t(g))
		b <- as.matrix(t(b))

		lr <- euclidean_distance_matrix_rcpp(align_data[,c(4:6)], r)
		lg <- euclidean_distance_matrix_rcpp(align_data[,c(4:6)], g)
		lb <- euclidean_distance_matrix_rcpp(align_data[,c(4:6)], b)

		red <- which.min(lr)

		green <- which.min(lg)

		blue <- which.min(lb)

		landmarks <- align_data[c(red,green,blue),c(1:3)]
		a <- 1
	}

	if(type == "fracture" || type == "both") {
		f <- as.matrix(t(f))
		lf <- euclidean_distance_matrix_rcpp(align_data[,c(4:6)], f)
		fracture <- which(lf <= f_threshold)
		b <- 1
	}

	if(a == 1 && b == 1) {return(list(landmarks, fracture))}
	if(a == 1 && b == 0) {return(list(landmarks))}
	if(a == 0 && b == 1) {return(list(fracture))}
}
