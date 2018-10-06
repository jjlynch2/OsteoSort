output$changes <- renderUI({
	HTML(paste("<p><h3>Version Changes</h3></p>",
		"Deprecated unidirectional Hausdorff distances",
		"<p></p>",
		"Deprecated Robject example data.tables",
		"<p></p>",
		"Integrated Julia environment",
		"<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><p></p>"
		,sep=""))
})