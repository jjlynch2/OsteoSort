#' match.3d Input Function
#' testing code
#' @param spec1 matrix
#' @param spec2 matrix
#' @param plot TRUE
#' @keywords match.2d
#' @export
#' @examples 
#' match.2d()

match.2d <- function(spec1 = NULL, spec2 = NULL, plot = TRUE) {
	options(warn = -1)
	if(is.null(spec1) || is.null(spec2)) { return(NULL)}
	require(morpho)

	######transform to matrix######
	spec1 <- as.matrix(spec1)
	spec2 <- as.matrix(spec2)
	######transform to matrix######



	array3d <- array(0,c(nrow(spec1),ncol(spec1),2))
	array3d[,,1] <- as.array(spec1)
	array3d[,,2] <- as.array(spec2)

	Y.gpa <<- ProcGPA(array3d)	
	specimen1 <- Y.gpa$rotated[,,1]
	specimen2 <- Y.gpa$ro[,,2]
	
	if(plot) {
		plot(specimen1, col = 4)
		points(specimen2, col = 3)
	}
	print(sqrt(sum((specimen1 - specimen2)^2)))


}