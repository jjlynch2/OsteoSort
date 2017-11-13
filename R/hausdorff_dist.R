#' hausdorff distance function
#' 
#' 
#' @param first_configuration The first two-dimensional configuration
#' @param second_configuration The second two-dimensional configuration
#' @param test Specifies the distance calculation ("Segmented-Hausdorff", "Hausdorff", "Uni-Hausdorff")
#' @param n_regions Specifies the number of regions for Segmented-Hausdorff
#' @param dist Specifies distance per region, either maximum or average distance
#' @param cores The number of threads/cores to use with RcppParallel
#'
#' Heavily modified from the opensource code in hausdorff_dist() function from the pracma package
#'
#' @keywords segmented_hausdorff_dist
#' @export
#' @examples
#' hausdorff_dist()

hausdorff_dist <- function (first_configuration, second_configuration, test = "Segmented-Hausdorff", n_regions = 0, dist = "maximum", cores = 1) {
	if(n_regions == 1) {test="Hausdorff"}
	
	setThreadOptions(cores)

	if(test == "Segmented-Hausdorff") {
		nums <- round(nrow(first_configuration)/n_regions)
		distance_results <- 0
		n1 <- 1
		n2 <- nums
		for(i in 1:n_regions) {
			if(n2 > nrow(first_configuration)) {
				rr <- n2-nrow(first_configuration)
				n1 <- n1-rr
				n2 <- n2-rr
			}
			if(dist == "average"){
				distance_results <- sum(distance_results, mean(mean_directional_hausdorff_rcpp(first_configuration[n1:n2,], second_configuration[n1:n2,]), mean_directional_hausdorff_rcpp(second_configuration[n1:n2,], first_configuration[n1:n2,])))
			}
			if(dist == "maximum"){
				distance_results <- sum(distance_results, mean(max_directional_hausdorff_rcpp(first_configuration[n1:n2,], second_configuration[n1:n2,]), max_directional_hausdorff_rcpp(second_configuration[n1:n2,], first_configuration[n1:n2,])))
			}
			if(dist == "dilated"){
				distance_results <- sum(distance_results, mean(dilated_directional_hausdorff_rcpp(first_configuration[n1:n2,], second_configuration[n1:n2,]), dilated_directional_hausdorff_rcpp(second_configuration[n1:n2,], first_configuration[n1:n2,])))
			}
			n1 <- n2
			n2 <- n2 + nums
		}

		if(dist == "average"){distance_results <- distance_results / n_regions}
	}
	if(test == "Hausdorff") {
		if(dist == "average"){
			distance_results <- mean(mean_directional_hausdorff_rcpp(first_configuration, second_configuration),mean_directional_hausdorff_rcpp(second_configuration, first_configuration))
		}
		if(dist == "maximum"){
			distance_results <- max(max_directional_hausdorff_rcpp(first_configuration, second_configuration),max_directional_hausdorff_rcpp(second_configuration, first_configuration))
		}
		if(dist == "dilated"){
			distance_results <- mean(dilated_directional_hausdorff_rcpp(first_configuration, second_configuration),dilated_directional_hausdorff_rcpp(second_configuration, first_configuration))
		}
		
	}
	if(test == "Uni-Hausdorff") {
		if(dist == "average") {
			distance_results <- mean_directional_hausdorff_rcpp(first_configuration, second_configuration)
		}
		if(dist == "maximum") {
			distance_results <- max_directional_hausdorff_rcpp(first_configuration, second_configuration)
		}
		if(dist == "dilated") {
			distance_results <- dilated_directional_hausdorff_rcpp(first_configuration, second_configuration)
		}
	}

    return(distance_results)
}

