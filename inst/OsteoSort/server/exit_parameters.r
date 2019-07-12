#stops the shiny app when closing session
session$onSessionEnded(function() {
	unlink(sessiontemp, recursive = TRUE)
	JuliaSetup(remove_cores = TRUE)
	stopApp()
})