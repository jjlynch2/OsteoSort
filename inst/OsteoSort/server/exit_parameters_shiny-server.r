#stops the shiny app when closing session
session$onSessionEnded(function() {
	unlink(sessiontemp, recursive = TRUE) 
})


