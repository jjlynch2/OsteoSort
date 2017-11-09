#' Conducts PCA and returns scores
#'
#' @param first_configuration The first point cloud and the cloud to be mirroed
#'
#' @keywords pca_align
#' @export
#' @examples
#' pca_align()

pca_align <- function(configuration = NULL) {
	results <- NULL #return NULL if no data
	if(!is.null(configuration)) {
		results <- scale(configuration, scale=F)%*%eigen(var(configuration))$vectors
	}
	return(results)
}