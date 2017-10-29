#' 2D and 3D EFA function
#'
#' @param pointcloud The two-dimensional or three-dimensional point cloud
#' @param harmonics The number of harmonics
#'
#' Modified from the efourier function in the Mmocs package for R and Hayley Fancourt's 3D EFA code from Honours thesis
#' 
#' @keywords efa
#' @export
#' examples
#' efa()

efa <- function(pointcloud, harmonics = 40) {
	n_row <- nrow(pointcloud)

	Dx <- pointcloud[,1] - pointcloud[,1][c(n_row, (1:(n_row - 1)))]
	Dy <- pointcloud[,2] - pointcloud[,2][c(n_row, (1:(n_row - 1)))]

	if(ncol(pointcloud) == 3) {
		Dz <- pointcloud[,3] - pointcloud[,3][c(n_row, (1:(n_row - 1)))]
		Dt <- sqrt(Dx^2 + Dy^2 + Dz^2)
	}
	if(ncol(pointcloud) == 2) {
		Dt <- sqrt(Dx^2 + Dy^2)
	}

	Dt[Dt < 1e-10] <- 1e-10 #required fix when dealing with 2D stuff, taken from Momocs

	t1 <- cumsum(Dt)
	t1m1 <- c(0, t1[-n_row])
	T <- sum(Dt)
	an <- bn <- cn <- dn <- en <- fn <- harmonics
#make multithreading
#OR re-write this loop using inline c++ code... might be best option. Simple calculations. #Find rcpp book

	for (i in 1:harmonics) {
		Ti <- (T / (2 * pi^2 * i^2))
		r <- 2 * i * pi

		an[i] <- Ti * sum((Dx / Dt) * (cos(r * t1 / T) - cos(r * t1m1 / T)))
		bn[i] <- Ti * sum((Dx / Dt) * (sin(r * t1 / T) - sin(r * t1m1 / T)))
		cn[i] <- Ti * sum((Dy / Dt) * (cos(r * t1 / T) - cos(r * t1m1 / T)))
		dn[i] <- Ti * sum((Dy / Dt) * (sin(r * t1 / T) - sin(r * t1m1 / T)))
		if(ncol(pointcloud) == 3) {
			en[i] <- Ti * sum((Dz / Dt) * (cos(r * t1 / T) - cos(r * t1m1 / T)))
			fn[i] <- Ti * sum((Dz / Dt) * (sin(r * t1 / T) - sin(r * t1m1 / T)))
		}
	}


	ao <- 2 * sum(pointcloud[,1] * Dt  /  T)
	co <- 2 * sum(pointcloud[,2] * Dt  /  T)
	if(ncol(pointcloud) == 3) {
		eo <- 2 * sum(pointcloud[,3] * Dt  /  T)
	}

	if(ncol(pointcloud) == 3) {
		results <- list(data.frame(an, bn, cn, dn, en, fn), ao, co, eo)
	}
	if(ncol(pointcloud) == 2) {
		results <- list(data.frame(an, bn, cn, dn), ao, co)
	}

	return(results)
} 