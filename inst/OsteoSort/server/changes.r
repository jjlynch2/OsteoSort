output$changes <- renderUI({
	HTML(paste("<p><h4><b>Version Changes</b></h4></p>",
		"The Julia programming language is now integrated as the new analytical platform.",
		"<p></p>",
		"Reference data can be imported and exported.",
		"<p></p>",
		"The user interface is now dynamic and generates based on the reference data and user input.",
		"<p></p>",
		"The ability to check for updates on GitHub has been implemented.",
		"<p></p>",
		"Memory usage is provided in the navbar.",
		"<p></p>",
		"Z-transform method is now implemeneted.",
		"<br><br><br><br><br><br><p></p>"
		,sep=""))
})
