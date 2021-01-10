remove_fragmented_margins <- function(first_configuration, second_configuration, indices, threads) {
	moving_indices <- indices[[1]]
	target_indices <- indices[[2]]

	t2 <- HD_KDTree_Ind(first_configuration, second_configuration, threads = threads)
	t1 <- HD_KDTree_Ind(second_configuration, first_configuration, threads = threads)

	if(length(moving_indices) > 0) {
		t1 <- t1[-moving_indices[,1], ]
	}
	if(length(target_indices) > 0) {
		t2 <- t2[-target_indices[,1], ]
	}
	if(length(target_indices) > 0) {
		for(i in 1:nrow(target_indices)) {
			t1 <- t1[t1[,2] != target_indices[i,1], ] #first margin
			if(ncol(target_indices) > 1) {
				t1 <- t1[t1[,2] != target_indices[i,2], ] #second margin
			}
		}
	}
	if(length(moving_indices) > 0) {
		for(i in 1:nrow(moving_indices)) {
			t2 <- t2[t2[,2] != moving_indices[i,1], ] #first margin
			if(ncol(moving_indices) > 1) {
				t2 <- t2[t2[,2] != moving_indices[i,2], ] #second margin
			}
		}
	}
	return(list(t1[,1],t2[,1]))
}
