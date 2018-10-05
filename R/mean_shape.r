#' Cartesian coordinate mean shape
#' 
#'
#' @keywords mean_shape
#' @export
#' @examples
#' mean_shape()  

mean_shape <- function(M) {
	apply(M, 2, mean)
}