#stops the shiny app when closing session
session$onSessionEnded(function() { 
	JuliaSetup(remove_cores = TRUE)
	stopApp()
})

#delete session temp directory on session end
session$onSessionEnded(function() {
	unlink(sessiontemp, recursive = TRUE)    
})