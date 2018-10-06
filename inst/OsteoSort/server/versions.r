JV <- NULL
output$version_numbers <- renderUI({
	HTML(
		paste(
			"<p><h3>Version Details</h3></p>",
			"<strong>OsteoSort:  </strong>", gsub("'", "" , packageVersion("OsteoSort")),"<p></p>",
			"<strong>R: </strong>", gsub('R version', '', gsub('version.string R ', '', version['version.string'])),"<p></p>",
			"<strong>Julia: </strong>", JV, "<p></p>",
			"<strong>Measurements: </strong>", "0.0.1", 
		sep = "")
	)
})