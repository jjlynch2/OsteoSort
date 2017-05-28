#' segmented hausdorff distance function
#' 
#' This function takes input from the user to calculate variations of the hausdorff distance between two two-dimensional shapes
#' 
#' @param P First shape configuration
#' @param Q Second shape configuration
#' @param testme The type of distance test to be utilized. "Regional", "Half", "Normal" Hausdorff distances
#' 
#' Heavily modified from the opensource code in hausdorff_dist() function from the pracma package
#'
#' @keywords segmented_hausdorff_dist
#' @export
#' @examples
#' segmented_hausdorff_dist()

segmented_hausdorff_dist <- function (P, Q, testme = "segmented") 
{
    stopifnot(is.numeric(P), is.numeric(Q))
    if (is.vector(P)) 
        P <- matrix(P, ncol = 1)
    if (is.vector(Q)) 
        Q <- matrix(Q, ncol = 1)
    if (ncol(P) != ncol(Q)) 
        stop("'P' and 'Q' must have the same number of columns.")
    D <- distmat(P, Q)
	
	if(testme == "Segmented-Hausdorff") { #should number of regions be variable?
		dhd_PQ <- apply(D, 1, min)
		dhd_QP <- apply(D, 2, min)
	
		nums <- (nrow(D)/6)
	
		upper1 <- max(dhd_PQ[1:nums])
		upper2 <- max(dhd_QP[1:nums])

		half1 <- max(dhd_PQ[(nums):(nums*2)])
		half2 <- max(dhd_QP[(nums):(nums*2)])

		half11 <- max(dhd_PQ[(nums*2):(nums*3)])
		half22 <- max(dhd_QP[(nums*2):(nums*3)])

		lower1 <- max(dhd_PQ[(nums*3):(nums*4)])
		lower2 <- max(dhd_QP[(nums*3):(nums*4)])

		lower11 <- max(dhd_PQ[(nums*4):(nums*5)])
		lower22 <- max(dhd_QP[(nums*4):(nums*5)])

		lower111 <- max(dhd_PQ[(nums*5):(nums*6)])
		lower222 <- max(dhd_QP[(nums*5):(nums*6)])

		dhd_PQ <- sum(upper1, half1, half11, lower1, lower11, lower111)
		dhd_QP <- sum(upper2, half2, half22, lower2, lower22, lower222)
	}
	if(testme == "Hausdorff") {
		dhd_PQ <- max(apply(D, 1, min))
		dhd_QP <- max(apply(D, 2, min))
	}

    return(max(dhd_PQ, dhd_QP))
}