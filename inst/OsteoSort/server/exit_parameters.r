#stops the shiny app when closing session
session$onSessionEnded(function() {
	JuliaSetup(remove_cores = TRUE)
	unlink(tempdir(), recursive = TRUE) 
	stopApp()
})
