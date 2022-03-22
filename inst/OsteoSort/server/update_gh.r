output$update_gh <- renderUI({
	version_gh <- OsteoSort:::gh_update()
	if (is.null(version_gh[[1]])) {
		HTML(paste(
		"Unable to check for updates<br>",
		"Are you online?",
		sep=""))
	}
	else if(version_gh[[1]] > version_gh[[2]]) {
		HTML(paste(
		"A newer version is available: ",
		version_gh[[1]],
		sep=""))
	}
	else if(version_gh[[1]] == version_gh[[2]]) {
		HTML(paste(
		"Latest version is installed",
		sep=""))
	}
	else if(version_gh[[1]] < version_gh[[2]]) {
		HTML(paste(
		"Development version is installed",
		sep=""))
	}
})
