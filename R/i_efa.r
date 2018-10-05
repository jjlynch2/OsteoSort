#' 2D and 3D inverse EFA function
#'
#' @param efar Elliptical Fourier coefficients 
#' @param points The number of points to approximate
#'
#' Modified from the efourier_i function in the Mmocs package for R and Hayley Fancourt's 3D EFA code from Honours thesis
#' 
#' @keywords i_efa
#' @export
#' examples
#' i_efa()

i_efa <- function(efar, points = 200) {
	ef <- efar[[1]]
	ao <- efar[[2]]
	co <- efar[[3]]
	if(ncol(ef) > 4) {
		eo <- efar[[4]]
	}

	harmonics <- length(ef$an)
	theta <- seq(0, 2 * pi, length = points * 1)[-(points + 1)]
	
	harmx <- harmy <- matrix(NA, harmonics, points)
	if(ncol(ef) > 4) {
		harmz <- matrix(NA, harmonics, points)
	}

	for (i in 1:harmonics) {
		harmx[i,] <- ef[i,1] * cos(i * theta) + ef[i,2] * sin(i * theta)
		harmy[i,] <- ef[i,3] * cos(i * theta) + ef[i,4] * sin(i * theta)
		if(ncol(ef) > 4) {
			harmz[i,] <- ef[i,5] * cos(i * theta) + ef[i,6] * sin(i * theta)
		}
	}

	if(ncol(ef) > 4) {
		results <- data.frame(x = (ao/2) + apply(harmx,2,sum), y = (co/2) + apply(harmy,2,sum), z = (eo/2) + apply(harmz,2,sum))
	}
	if(ncol(ef) == 4) {
		results <- data.frame(x = (ao/2) + apply(harmx,2,sum), y = (co/2) + apply(harmy,2,sum))
	}
	
	return(results)
}