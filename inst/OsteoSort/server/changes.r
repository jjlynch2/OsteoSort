output$changes <- renderUI({
	HTML(paste("<p><h3>Version Changes</h3></p>",
		"Deprecated unidirectional Hausdorff distances",
		"<p></p>",
		"Deprecated Robject example data.tables",
		"<p></p>",
		"Integrated Julia environment",
		"<p></p>",
		"Check for updates",
		"<p></p>",
		"Monitor system resources",
		"<p></p>",
		"Import and data reference data",
		"<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><p></p>"
		,sep=""))
})
