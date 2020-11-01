hausdorff_dist <- function (first_configuration, second_configuration, dist = "average", indices = NULL, threads = 1) {
	if(dist == "average"){
			if(!is.null(indices)) {
				res <- remove_fragmented_margins(first_configuration, second_configuration, indices, threads = threads)
				distance_results <- max( mean(res[[1]]), mean(res[[2]]) )
			 }
			else distance_results <- Avg_HD_KDTree(first_configuration, second_configuration, threads = threads)
	}
	if(dist == "maximum"){
			if(!is.null(indices)) {
				res <- remove_fragmented_margins(first_configuration, second_configuration, indices, threads = threads)
				distance_results <- max( max(res[[1]]), max(res[[2]]) )
			 }
			else distance_results <- Max_HD_KDTree(first_configuration, second_configuration, threads = threads)
	}
	return(distance_results)
}

