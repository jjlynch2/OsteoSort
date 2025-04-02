analytical_temp_space <- function(sessiontempdir) {
	if (!is.null(sessiontempdir)) {
		dir.create(file.path(sessiontempdir), showWarnings = FALSE, recursive = TRUE)
	}
	direc <- randomstring(n = 1, length = 12)
	dir.create(paste(sessiontempdir, direc,sep="/"))
	return(direc)
}
