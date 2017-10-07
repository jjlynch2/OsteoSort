#' outline.images function
#' 
#' @param imagelist1 List of image locations for first set
#' @param imagelist2 List of image locations for second set
#' @param threshold Threshold value between white(1) and black(0)
#' @param scale Sets if a scale should be extracted from pixel dimensions
#' @param mirror If true mirrors imagelist2 to imagelist2
#' @param npoints The number of points in the inverse elliptical fourier analysis transformation
#' @param smooth_iterations The number of smoothing iterations in the elliptical fourier analysis
#' @param nharmonics The number of harmonics in elliptical fourier analysis
#' @param fragment Currently under development 
#'
#' @keywords outline.images
#' @export
#' @examples
#' outline.images()

outline.images <- function (imagelist1, imagelist2, threshold = 0.8, scale = TRUE, mirror = TRUE, npoints = 200, smooth_iterations = 1, nharmonics = 400) {
	library(jpeg)
	library(pixmap)
	library(Momocs)
	
	nimages <- length(imagelist1) + length(imagelist2)
	imagelist <- c(imagelist1, imagelist2)

	array3d <- array(NA,c(npoints, 2, nimages))

	for(iii in 1:nimages) {
print(iii)
		M <- readJPEG(imagelist[iii])
		M <- suppressWarnings(pixmapGrey(M))

		M@grey[which(M@grey > threshold)] <- 1#white
		M@grey[which(M@grey <= threshold)] <- 0#black

		start = list(x = NA, y = NA)

		start$x = M@size[2]/2 #middle of image
		start$y = M@size[1]/2

		x <- c(round(start$x), round(start$y)) #start point
		I <- M@grey #b/w matrix

		x <- rev(x) #reverse X to work backwards

		x[1] <- dim(I)[1] - x[1]
		while (abs(I[x[1], x[2]] - I[x[1], (x[2] - 1)]) < 0.1) {
			x[2] <- x[2] - 1
		}
		a <- 1
		M <- matrix(c(0, -1, -1, -1, 0, 1, 1, 1, 1, 1, 0, -1, -1, -1, 0, 1), 2, 8, byrow = TRUE)
		M <- cbind(M[, 8], M, M[, 1])
		X <- 0
		Y <- 0
		x1 <- x[1]
		x2 <- x[2]
		SS <- NA
		S <- 6

		while ((any(c(X[a], Y[a]) != c(x1, x2)) | length(X) < 3)) {
			if (abs(I[x[1] + M[1, S + 1], x[2] + M[2, S + 1]] - I[x[1], x[2]]) < 0.1) {
				a <- a + 1
				X[a] <- x[1]
				Y[a] <- x[2]
				x <- x + M[, S + 1]
				SS[a] <- S + 1
				S <- (S + 7)%%8
			}
			else if (abs(I[x[1] + M[1, S + 2], x[2] + M[2, S + 2]] - I[x[1], x[2]]) < 0.1) {
				a <- a + 1
				X[a] <- x[1]
				Y[a] <- x[2]
				x <- x + M[, S + 2]
				SS[a] <- S + 2
				S <- (S + 7)%%8
			}
			else if (abs(I[x[1] + M[1, (S + 3)], x[2] + M[2, (S + 3)]] - I[x[1], x[2]]) < 0.1) {
				a <- a + 1
				X[a] <- x[1]
				Y[a] <- x[2]
				x <- x + M[, (S + 3)]
				SS[a] <- S + 3
				S <- (S + 7)%%8
			}
			else S <- (S + 1)%%8
			if (a > (dim(I)[1] + dim(I)[2]) * 100) {
				X[a] = x1
				Y[a] = x2
			}
		}
		spec1 <- list(X = (Y[-1]), Y = ((dim(I)[1] - X))[-1])
		spec1 <- as.matrix(data.frame(spec1))
		spec1 <- round(spec1) #round to whole numbers

		if(mirror) {
			if(imagelist[iii] %in% imagelist2) {
				spec1[,1] <- -spec1[,1] #swap X axis to mirror ### should this by multiply by -1????
			}
		}

		if(scale) { #scale comes after EFA duh! 
			centroid <- apply(spec1,2,mean)
			centroidsize <- sqrt(sum((t(t(spec1)-centroid))^2))
		}

		test1 <- efourier(spec1, smooth.it=smooth_iterations, verbose=FALSE, norm = TRUE, start = FALSE, nb.h = nharmonics)
		spec1 <- efourier_i(test1, nb.pts=npoints)
		spec1 <- as.matrix(data.frame(spec1))

		if(scale) {
			spec1 <- spec1 / centroidsize
		}

		array3d[,1,iii] <- spec1[,1]
		array3d[,2,iii] <- spec1[,2]
	}


	dimnames(array3d)[[3]] <- paste(gsub(".*/\\s*|.JPG.*","",imagelist), ".JPG", sep="")



	return(list(array3d, imagelist1, imagelist2))

}
