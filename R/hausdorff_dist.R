#' hausdorff distance function
#' 
#' @param first_configuration The first two-dimensional configuration
#' @param second_configuration The second two-dimensional configuration
#' @param test Specifies the distance calculation ("Segmented-Hausdorff", "Hausdorff", "Uni-Hausdorff")
#' @param n_regions Specifies the number of regions for Segmented-Hausdorff
#' @param dist Specifies distance type either maximum, average, or dilated
#' @param threads The number of threads to use
#'
#'
#' @keywords segmented_hausdorff_dist
#' @export
#' @examples
#' hausdorff_dist()

hausdorff_dist <- function (first_configuration, second_configuration, test = "Segmented-Hausdorff", n_regions = 0, dist = "average", threads = 1, indices = NULL) {
	julia_call("Set_Procs", threads,detectCores())

	#1 Region is the same as regular Hausdorff
	if(n_regions == 1 && test == "Segmented-Hausdorff") {test="Hausdorff"}

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
				distance_results <- sum(distance_results, julia_call("Average_Hausdorff", first_configuration, second_configuration))
			}
			if(dist == "maximum"){
				distance_results <- sum(distance_results, julia_call("Maximum_Hausdorff", first_configuration, second_configuration))
			}
			if(dist == "dilated"){
				distance_results <- sum(distance_results, julia_call("Dilated_Hausdorff", first_configuration, second_configuration))
			}
			n1 <- n2
			n2 <- n2 + nums
		}
		if(dist == "average"){distance_results <- distance_results / n_regions}
	}
	if(test == "Hausdorff") {
		if(dist == "average"){
				if(!is.null(indices)) {
					res <- remove_fragmented_margins(first_configuration, second_configuration, indices)
					distance_results <- mean( mean(res[[1]]), mean(res[[2]]) )
				 }
				else distance_results <- julia_call("Average_Hausdorff", first_configuration, second_configuration)

		}
		if(dist == "maximum"){
				if(!is.null(indices)) {
					res <- remove_fragmented_margins(first_configuration, second_configuration, indices)
					distance_results <- max( max(res[[1]]), max(res[[2]]) )
				 }
				else distance_results <- julia_call("Maximum_Hausdorff", first_configuration, second_configuration)
	
		}
		if(dist == "dilated"){
				if(!is.null(indices)) {
					res <- remove_fragmented_margins(first_configuration, second_configuration, indices)
					distance_results <- mean( (mean(res[[1]]) * sd(res[[1]])), (mean(res[[2]]) * sd(mean(res[[2]]))) )
				 }
				else distance_results <- julia_call("Dilated_Hausdorff", first_configuration, second_configuration)
		}
		
	}

    return(distance_results)
}

