#' A function to export TPS data. 
#' 
#' Modified from the opensource writeland.tps function from the Geomorph package for R
#' 
#'
#' @param file test
#' @keywords random
#' @export
#' @examples
#' writetps()

writetps <- function (A, file, scale = NULL) {
	n <- dim(A)[3]
	k <- dim(A)[2]
	p <- dim(A)[1]
    
	lmline <- ifelse(k == 2, paste("LM=", p, sep = ""), paste("LM3=", p, sep = ""))
    
	file.create(file, showWarnings = TRUE)
    
	#writes if scale is present
	if (!is.null(scale)) {
		scaleline <- paste("SCALE", "=", scale, sep = "")
	}
    
	for (i in 1:n) {
		write(lmline, file, append = TRUE)
		write.table(A[, , i], file, col.names = FALSE, row.names = FALSE, append = TRUE)
        
		if (!is.null(scale)) {
			if (length(scaleline) == 1) {
				write(scaleline, file, append = TRUE)
			}
			if (length(scaleline) > 1) {
				write(scaleline[i], file, append = TRUE)
			}
		}

		if (is.null(dimnames(A)[[3]])) {
		dimnames(A)[[3]] <- c(1:dim(A)[3])
		}
		idline <- paste("ID=", dimnames(A)[[3]][i], sep = "")
		write(idline, file, append = TRUE)
		write("", file, append = TRUE)
	}
}