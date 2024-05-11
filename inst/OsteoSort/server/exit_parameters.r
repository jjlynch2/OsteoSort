#stops the shiny app when closing session
session$onSessionEnded(function() {
	RJS(remove_cores = TRUE)
	unlink(tempdir(), recursive = TRUE) 
	stopApp()
})
