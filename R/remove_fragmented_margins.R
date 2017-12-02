#' helper function to remove fragmented margins for Hausdorff
#' 
#' @param first_configuration first shape
#' @param second_configuration second shape
#' @param indices list of indices of fracture margins
#'
#' @keywords remove_fragmented_margins
#' @export
#' @examples
#' remove_fragmented_margins()

remove_fragmented_margins <- function(first_configuration, second_configuration, indices) {
	moving_indices <- indices[[1]]
	target_indices <- indices[[2]]
	t1 <- minimum_euclidean_distances_indices(first_configuration, second_configuration)
	t2 <- minimum_euclidean_distances_indices(second_configuration, first_configuration)
	if(length(target_indices) > 0) {
		for(i in 1:nrow(target_indices)) {
			t1 <- t1[t1[,2] != target_indices[i,1], ] #first margin
			t1 <- t1[t1[,2] != target_indices[i,2], ] #second margin
		}
	}
	if(length(moving_indices) > 0) {
		for(i in 1:nrow(moving_indices)) {
			t2 <- t2[t2[,2] != moving_indices[i,1], ] #first margin
			t2 <- t2[t2[,2] != moving_indices[i,2], ] #second margin
		}
	}
	return(list(t1[,1],t2[,1]))
}