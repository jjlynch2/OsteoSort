gh_update <- function() {
	local_version <- NULL
	remote_version <- NULL
	url <- paste0("https://raw.github.com/jjlynch2/OsteoSort/master/DESCRIPTION")
	tryCatch({
		x <- readLines(url)
	}, error=function(e){})

	if(exists("x")) {
		remote_version <- gsub("Version:\\s*", "", x[grep('Version:', x)])
		local_version <- packageVersion("OsteoSort")
		local_version <- unlist(local_version)
		local_version <- paste(local_version[1], local_version[2], local_version[3], sep=".")
	}

	return(list(remote_version, local_version))
}