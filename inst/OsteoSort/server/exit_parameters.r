#stops the shiny app when closing session
session$onSessionEnded(function() {
	JuliaSetup(remove_cores = TRUE)
	unlink(sessiontemp, recursive = TRUE)
	stopApp()
})
