session$onSessionEnded(function() {
	unlink(sessiontemp, recursive = TRUE)  #unlinks only the session temp use for deployment
})