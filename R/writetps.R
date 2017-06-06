#' A function to export TPS data. 
#' 
#' Modified from the opensource writeland.tps function from the Geomorph package for R
#'
#' @param shape_matrices matrices of shape configurations
#' @param file File path for saving output
#' @param scale Outputs scale if present
#'
#' @keywords random
#' @export
#' @examples
#' writetps()

writetps <- function (shape_matrices, file, scale = NULL) {
	n <- dim(shape_matrices)[3]
	k <- dim(shape_matrices)[2]
	p <- dim(shape_matrices)[1]
    
	lmline <- ifelse(k == 2, paste("LM=", p, sep = ""), paste("LM3=", p, sep = ""))
    
	file.create(file, showWarnings = TRUE)
    
	#writes if scale is present
	if (!is.null(scale)) {
		scaleline <- paste("SCALE", "=", scale, sep = "")
	}
    
	for (i in 1:n) {
		write(lmline, file, append = TRUE)
		write.table(shape_matrices[, , i], file, col.names = FALSE, row.names = FALSE, append = TRUE)
        
		if (!is.null(scale)) {
			if (length(scaleline) == 1) {
				write(scaleline, file, append = TRUE)
			}
			if (length(scaleline) > 1) {
				write(scaleline[i], file, append = TRUE)
			}
		}

		if (is.null(dimnames(shape_matrices)[[3]])) {
		dimnames(shape_matrices)[[3]] <- c(1:dim(shape_matrices)[3])
		}
		idline <- paste("ID=", dimnames(shape_matrices)[[3]][i], sep = "")
		write(idline, file, append = TRUE)
		write("", file, append = TRUE)
	}
}