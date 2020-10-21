#stops the shiny app when closing session
session$onSessionEnded(function() {
	JuliaSetup(remove_cores = TRUE) #comment out for deployment in shiny-server
	unlink(tempdir(), recursive = TRUE)
	stopApp() #comment out for deployment in shiny-server
})
