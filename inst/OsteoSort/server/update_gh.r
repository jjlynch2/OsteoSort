output$update_gh <- renderUI({
	version_gh <- OsteoSort:::gh_update()
	if (is.null(version_gh[[1]])) {
		HTML(paste("<p><h3>Updates</h3></p>",
		"Unable to check for updates<p></p>",
		"Are you online?",
		"<br><br><br><br>",
		sep=""))
	}
	else if(version_gh[[1]] > version_gh[[2]]) {
		HTML(paste("<p><h3>Updates</h3></p>",
		"A newer version is available: ",
		version_gh[[1]],
		"<br><br><br><br><br><p></p>",
		sep=""))
	}
	else if(version_gh[[1]] == version_gh[[2]]) {
		HTML(paste("<p><h3>Updates</h3></p>",
		"Latest version is installed",
		"<br><br><br><br><br><p></p>",
		sep=""))
	}
	else if(version_gh[[1]] < version_gh[[2]]) {
		HTML(paste("<p><h3>Updates</h3></p>",
		"Development version is installed",
		"<br><br><br><br><br><p></p>",
		sep=""))
	}
})