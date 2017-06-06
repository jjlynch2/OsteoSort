#' segmented hausdorff distance function
#' 
#' 
#' @param first_configuration The first two-dimensional configuration
#' @param second_configuration The second two-dimensional configuration
#' @param test Specifies the distance calculation ("Segmented-Hausdorff", "Hausdorff")
#' 
#' Heavily modified from the opensource code in hausdorff_dist() function from the pracma package
#'
#' @keywords segmented_hausdorff_dist
#' @export
#' @examples
#' segmented_hausdorff_dist()

segmented_hausdorff_dist <- function (first_configuration, second_configuration, test = "segmented") 
{
    stopifnot(is.numeric(first_configuration), is.numeric(second_configuration))
    if (is.vector(first_configuration)) 
        first_configuration <- matrix(first_configuration, ncol = 1)
    if (is.vector(second_configuration)) 
        second_configuration <- matrix(second_configuration, ncol = 1)
    if (ncol(first_configuration) != ncol(second_configuration)) 
        stop("'first_configuration' and 'second_configuration' must have the same number of columns.")
    D <- distmat(first_configuration, second_configuration)
	
	if(test == "Segmented-Hausdorff") { #should number of regions be variable?
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
	if(test == "Hausdorff") {
		dhd_PQ <- max(apply(D, 1, min))
		dhd_QP <- max(apply(D, 2, min))
	}

    return(max(dhd_PQ, dhd_QP))
}