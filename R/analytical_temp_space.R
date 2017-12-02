#' Internal function to deal with temporary analytical directory and session temp
#' 
#' @param output_options
#' @param sessiontempdir
#'
#' @examples
#' analytical_temp_space()


analytical_temp_space <- function(output_options, sessiontempdir) {
	if(any(output_options)) {
		if (!is.null(sessiontempdir)) {
			setwd(sessiontempdir)
		}
		direc <- OsteoSort:::randomstring(n = 1, length = 12)
		dir.create(direc)
		setwd(direc)
	}
	else {
		direc <- NULL
	}
	return(direc)
}