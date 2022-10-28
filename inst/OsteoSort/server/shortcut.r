observeEvent(input$Create_Desktop_Icon, {
	if(Sys.info()[['sysname']] == "Windows") {
		target <- paste('"', file.path(R.home("bin"), "R.exe"), '"', sep="")
		arguments <- paste('"', "-e ", "library(OsteoSort);OsteoSort()", '"', sep="")
		icon <- paste('"', system.file("vbs/OsteoSort.ico", package = "OsteoSort"), '"', sep="")
		pathname <- paste(file.path(Sys.getenv("USERPROFILE"), "Desktop"), "/OsteoSort.lnk", sep="")
		vbs <- paste('"', system.file("vbs/createLink.vbs", package = "OsteoSort"), '"', sep="")
		system(paste("cscript", vbs, pathname, target, arguments, icon, sep=" "))
	}
	if(Sys.info()[['sysname']] == "Linux") {
		icon_name <- "OsteoSort.desktop"
		cat(
			paste(
				"[Desktop Entry]\nEncoding=UTF-8\nTerminal=true\nType=Application\nCategories=Application\nName=OsteoSort\n",
				"Version=",	packageVersion("OsteoSort"),"\n",
				"Icon=",		system.file("OsteoSort/www/OsteoSortIcon.png", package = "OsteoSort"),"\n",
				"Exec=",		paste(file.path(R.home("bin"), "R"), "-e", "library(OsteoSort);OsteoSort()", sep=" ")
			,sep=""),#paste
			file = paste(file.path(path.expand("~"), "Desktop"), "OsteoSort.desktop", sep = "/")
		)#cat
		Sys.chmod(paste(file.path(path.expand("~"), "Desktop"), "OsteoSort.desktop", sep="/"), mode = "0777", use_umask = TRUE)
	}
	if(Sys.info()[['sysname']] != "Linux" && Sys.info()[['sysname']] != "Windows") { #.command for mac
		icon_name <- "OsteoSort.sh"
		cat(paste(file.path(R.home("bin"), "R"), "-e", "'library(OsteoSort);OsteoSort()'", sep=" "), file = paste(file.path(path.expand("~"), "Desktop"), "OsteoSort.command", sep = "/"))
		Sys.chmod(paste(file.path(path.expand("~"), "Desktop"), "OsteoSort.sh", sep="/"), mode = "0777", use_umask = TRUE)
	}
})