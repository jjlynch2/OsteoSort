half_hausdorff_dist <- function (P, Q) 
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
    dhd_PQ <- sort(apply(D, 1, min))
    dhd_QP <- sort(apply(D, 2, min))

	dhd_PQ <- sum(dhd_PQ[(nrow(D)/2):nrow(D)])
	dhd_QP <- sum(dhd_QP[(nrow(D)/2):nrow(D)])

    return(max(dhd_PQ, dhd_QP))
}