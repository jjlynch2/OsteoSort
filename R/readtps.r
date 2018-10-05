#' A function to import TPS data. 
#' 
#' Modified from the opensource readland.tps function from the Geomorph package for R
#'
#' @param file File path for TPS data
#'
#' @keywords random
#' @export
#' @examples
#' readtps()

readtps <- function (file) {
	specID = "ID"
	ignore.case = TRUE
	tpsfile <- scan(file = file, what = "char", sep = "\n", quiet = TRUE)
	lmdata <- grep("LM=", tpsfile, ignore.case)
    
	#2D
	if (length(lmdata != 0)) {
		nland <- as.numeric(sub("LM=", "", tpsfile[lmdata], ignore.case))
		k <- 2
	}
    
	#3D
	if (length(lmdata) == 0) {
		lmdata <- grep("LM3=", tpsfile, ignore.case)
		nland <- as.numeric(sub("LM3=", "", tpsfile[lmdata], ignore.case))
		k <- 3
	}
    
	#number of landmarks should be the same, even if the landmark is empty    
	n <- nspecs <- length(lmdata)
	if (max(nland) - min(nland) != 0) {
		stop("Number of landmarks not the same for all specimens.")
	}
    
	#scale
	p <- nland[1]
	imscale <- as.numeric(sub("SCALE=", "", tpsfile[grep("SCALE", tpsfile, ignore.case)], ignore.case))
    
	if (is.null(imscale)) {
		imscale = array(1, nspecs)
	}

	if (length(imscale) != nspecs) {
		imscale = array(1, nspecs)
	}

	tmp <- tpsfile[-(grep("=", tpsfile))]
	tmp <- matrix(as.numeric(unlist(strsplit(tmp, "\\s+"))), ncol = k, byrow = T)

	#puts coords together in array
	coords <- aperm(array(t(tmp), c(k, p, n)), c(2, 1, 3))
	imscale <- aperm(array(rep(imscale, p * k), c(n, k, p)), c(3, 2, 1))
	coords <- coords * imscale
	coords <- coords[1:nland, , ]
	if (n == 1) {
		coords <- array(coords, c(nland, k, n))
	}

	#extracts ID
	ID <- sub("ID=", "", tpsfile[grep("ID=", tpsfile, ignore.case)], ignore.case)
	if (length(ID) != 0) {
		dimnames(coords)[[3]] <- as.list(ID)
	}

	return(coords = coords)
}