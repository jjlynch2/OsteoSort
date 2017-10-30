#' segmented hausdorff distance function
#' 
#' 
#' @param first_configuration The first two-dimensional configuration
#' @param second_configuration The second two-dimensional configuration
#' @param test Specifies the distance calculation ("Segmented-Hausdorff", "Hausdorff")
#' @param n_regions Specifies the number of regions for Segmented-Hausdorff
#' @param dist Specifies distance per region, either maximum or average distance
#'
#' Heavily modified from the opensource code in hausdorff_dist() function from the pracma package
#'
#' @keywords segmented_hausdorff_dist
#' @export
#' @examples
#' segmented_hausdorff_dist()

segmented_hausdorff_dist <- function (first_configuration, second_configuration, test = "Segmented-Hausdorff", n_regions = NULL, dist = "maximum") {
	if(n_regions == 1) {test="Hausdorff"}

	stopifnot(is.numeric(first_configuration), is.numeric(second_configuration))
	if (is.vector(first_configuration)) 
		first_configuration <- matrix(first_configuration, ncol = 1)
	if (is.vector(second_configuration)) 
		second_configuration <- matrix(second_configuration, ncol = 1)
	if (ncol(first_configuration) != ncol(second_configuration)) 
		stop("'first_configuration' and 'second_configuration' must have the same number of columns.")
	D <- e_dist(first_configuration, second_configuration)
	
	if(test == "Segmented-Hausdorff") {
		dhd_PQ <- apply(D, 1, min)
		dhd_QP <- apply(D, 2, min)
		nums <- round(nrow(D)/n_regions)
		dhd_PQsum <- 0
		dhd_QPsum <- 0
		n1 <- 1
		n2 <- nums
		for(i in 1:n_regions) {
			if(n2 > nrow(D)) {
				rr <- n2-nrow(D)
				n1 <- n1-rr
				n2 <- n2-rr
			}
			if(dist == "average"){
				dhd_PQsum <- sum(dhd_PQsum, mean(dhd_PQ[n1:n2]))
				dhd_QPsum <- sum(dhd_QPsum, mean(dhd_QP[n1:n2]))
			}
			if(dist == "maximum"){
				dhd_PQsum <- sum(dhd_PQsum, max(dhd_PQ[n1:n2]))
				dhd_QPsum <- sum(dhd_QPsum, max(dhd_QP[n1:n2]))
			}

			n1 <- n2
			n2 <- n2 + nums
		}
		if(dist == "maximum"){distance_results <- max(dhd_PQsum, dhd_QPsum)}
		if(dist == "average"){distance_results <- mean(dhd_PQsum, dhd_QPsum)}
	}
	if(test == "Hausdorff") {
		if(dist == "average"){
			dhd_PQsum <- mean(apply(D, 1, min))
			dhd_QPsum <- mean(apply(D, 2, min))
			distance_results <- mean(dhd_PQsum, dhd_QPsum)
		}
		if(dist == "maximum"){
			dhd_PQsum <- max(apply(D, 1, min))
			dhd_QPsum <- max(apply(D, 2, min))
			distance_results <- max(dhd_PQsum, dhd_QPsum)
		}
	}
	if(test == "Procrustes") {
		distance_results <- procdist(first_configuration, second_configuration)
	}

    return(distance_results)
}

