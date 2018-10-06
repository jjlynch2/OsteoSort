#generate UI for version numbers
JV <- NULL
system_name <- Sys.info()[['sysname']]
if(system_name == "Linux") {
	system_name <- paste(icon = icon("linux", lib="font-awesome"), system_name, sep = " ")
}
if(system_name == "Windows") {
	system_name <- paste(icon = icon("windows", lib="font-awesome"), system_name, sep = " ")
}
if(system_name == "Darwin") {
	system_name <- paste(icon = icon("apple", lib="font-awesome"), system_name, sep = " ")
}

output$version_numbers <- renderUI({
	HTML(paste(
		"<p><h3>Version Details</h3></p>",
		"<hr><span style='font-family: 'Times New Roman', serif;'>",
		"<strong>OsteoSort:  </strong>", gsub("'", "" , packageVersion("OsteoSort")),"<p></p>",
		"<strong>R: </strong>", gsub('R version', '', gsub('version.string R ', '', version['version.string'])),"<p></p>",
		"<strong>Julia: </strong>", JV, "<p></p>",
		"<strong>Measurements: </strong>", "0.0.1", "<p></p>",
		"<strong>Platform:  </strong>", system_name, sep = "")
	)
})