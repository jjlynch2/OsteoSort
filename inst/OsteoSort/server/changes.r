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
		"Z-transform method is now implemeneted.",
		"<p></p>",
		"Three-dimensional point cloud antimere sorting is now supported.",
		"<p></p>",
		"Two-dimensional photograph antimere sorting now uses pairwise registration.",
		"<br><br><br><br><br><br><p></p>"
		,sep=""))
})
