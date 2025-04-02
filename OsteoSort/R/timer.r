start_time <- function() {
	return(Sys.time())
}

end_time <- function(start_time) {
	return(round(as.numeric(gsub("*.of", "", difftime(Sys.time(), start_time, units="secs"))), digits=2))
}