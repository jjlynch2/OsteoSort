#stops the shiny app when closing session
session$onSessionEnded(function() {
	JuliaSetup(remove_cores = TRUE) #comment out for deployment in shiny-server
	unlink(tempdir(), recursive = TRUE) #comment out for deployment in shiny-server
	stopApp() #comment out for deployment in shiny-server
	#unlink(sessiontemp, recursive = TRUE) #uncomment for shiny-server
})
