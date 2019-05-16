#' RGB coordinate extractor
#' 
#'
#' @keywords RGB.locator.3d()
#' @export
#' @examples
#' RGB.locator.3d()

RGB.locator.3d <- function(align_data, type = "landmark",r = c(255,255,255), g = c(0,255,0), b = c(0,0,255), f = c(255,0,0), f_threshold = 100) {	
	a <- 0
	aa <- 0

	align_data <- as.matrix(align_data)
	if(ncol(align_data) < 6) {return(NULL)}
	if(type == "landmark" || type == "both") {

		if(class(r) == "data.frame") {
			r <- as.matrix(r)
			g <- as.matrix(g)
			b <- as.matrix(b)
		}
		else {
			r <- as.matrix(t(r))
			g <- as.matrix(t(g))
			b <- as.matrix(t(b))
		}

		lr <- JuliaCall("AD3D", align_data[,c(4:6)], r)
		lg <- JuliaCall("AD3D", align_data[,c(4:6)], g)
		lb <- JuliaCall("AD3D", align_data[,c(4:6)], b)

		red <- which.min(lr)

		green <- which.min(lg)

		blue <- which.min(lb)

		landmarks <- align_data[c(red,green,blue),c(1:3)]
		a <- 1
	}

	if(type == "fracture" || type == "both") {
		if(class(f) == "data.frame") {
			f <- as.matrix(f)
		}
		else {
			f <- as.matrix(t(f))
		}

		lf <- JuliaCall("AD3D", align_data[,c(4:6)], f)
		fracture <- which(lf <= f_threshold)
		aa <- 1
	}

	if(a == 1 && aa == 1) {return(list(landmarks, fracture))}
	if(a == 1 && aa == 0) {return(list(landmarks))}
	if(a == 0 && aa == 1) {return(list(fracture))}
}
