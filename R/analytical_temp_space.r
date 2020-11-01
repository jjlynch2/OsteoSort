analytical_temp_space <- function(output_options, sessiontempdir) {
	if(any(output_options)) {
		if (!is.null(sessiontempdir)) {
			dir.create(file.path(sessiontempdir), showWarnings = FALSE, recursive = TRUE)
		}
		direc <- OsteoSort:::randomstring(n = 1, length = 12)
		dir.create(paste(sessiontempdir, direc,sep="/"))
	}
	else {
		direc <- NULL
	}
	return(direc)
}
