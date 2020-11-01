shiftmatrices <- function(first_configuration, second_configuration, threads=1) {
	index <- mcNNindex(first_configuration, second_configuration, k = 1, threads = threads)
	if(sum(diff(index) < 0) > nrow(second_configuration)/2) {A <-2}
	if(sum(diff(index) > 0) > nrow(second_configuration)/2) {A <-1}

	if(A == 1) {
		if(index[1] != 1) {
			index <- c(index[1]:nrow(second_configuration),1:(index[1]-1))
		}
		else {
			index <- 1:nrow(second_configuration)
		}
	}

	if(A == 2) {
		index <- c(index[1]:1,nrow(second_configuration):(index[1]+1))
	}

	first_configuration <- first_configuration[index,]
	return(first_configuration)
}





