pca_align <- function(configuration = NULL) {
	results <- NULL #return NULL if no data

	if(!is.null(configuration)) {
		results <- scale(configuration, scale=F)%*%eigen(var(configuration))$vectors
	}

	return(results)
}
