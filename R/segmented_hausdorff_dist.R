segmented_hausdorff_dist <- function (P, Q, testme = TRUE) 
{
#Modified from the opensource code in hausdorff_dist() function from the
#pracma package for R
    stopifnot(is.numeric(P), is.numeric(Q))
    if (is.vector(P)) 
        P <- matrix(P, ncol = 1)
    if (is.vector(Q)) 
        Q <- matrix(Q, ncol = 1)
    if (ncol(P) != ncol(Q)) 
        stop("'P' and 'Q' must have the same number of columns.")
    D <- distmat(P, Q)
	
	if(testme == "test") {
		dhd_PQ <- apply(D, 1, min)
		dhd_QP <- apply(D, 2, min)

		#maxdist1 <- max(dhd_PQ)
		#maxdist2 <- max(dhd_QP)
	
		nums <- (nrow(D)/4)
	
		upper1 <- max(dhd_PQ[1:nums])
		upper2 <- max(dhd_QP[1:nums])

		half1 <- max(dhd_PQ[(nums):(nums*2)])
		half2 <- max(dhd_QP[(nums):(nums*2)])

		half11 <- max(dhd_PQ[(nums*2):(nums*3)])
		half22 <- max(dhd_QP[(nums*2):(nums*3)])

		lower1 <- max(dhd_PQ[(nums*3):(nums*4)])
		lower2 <- max(dhd_QP[(nums*3):(nums*4)])

		dhd_PQ <- sum(upper1, half1, half11, lower1)
		dhd_QP <- sum(upper2, half2, half22, lower2)
	}
	if(testme == "half") {
		dhd_PQ <- sort(apply(D, 1, min))
		dhd_QP <- sort(apply(D, 2, min))
		dhd_PQ <- sum(dhd_PQ[(nrow(D)/2):nrow(D)])
		dhd_QP <- sum(dhd_QP[(nrow(D)/2):nrow(D)])
	}

    return(max(dhd_PQ, dhd_QP))
}