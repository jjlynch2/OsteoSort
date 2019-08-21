start_time <- function() {
	return(Sys.time())
}

end_time <- function(start_time) {
	return(round(as.numeric(gsub("*.of", "", (Sys.time() - start_time))), digits=2))
}