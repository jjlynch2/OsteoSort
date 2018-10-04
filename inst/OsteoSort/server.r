#' Shiny server.r file
#' 
#' This is the server.r file for the interface that utilizes all previous functions. 
#' runApp("osteosort")
#' shinyServer()

library(shiny)
options(rgl.useNULL=TRUE) #required to avoid rgl device opening 
library(rgl)
options(shiny.maxRequestSize=200*1024^2) #increased file upload size to 30MB
options(warn = -1) #disables warnings

shinyServer(function(input, output, session) {
	################generates temporary directories for multiuser environment
	workingdd <- getwd()
	sessiontempd <- OsteoSort:::randomstring(n = 1, length = 12)
	dir.create('tmp') #new for package
	setwd('tmp')
	dir.create(sessiontempd)
	setwd(sessiontempd)
	sessiontemp <- getwd()

	showModal(modalDialog(title = "Loading Julia environment...", easyClose = FALSE, footer = NULL))
	JuliaSetup()
	removeModal()  



	#defines which modules to include
	source("../../server/reference.R", local=TRUE) ###imports single comparison server code
	source("../../server/single.R", local=TRUE) ###imports single comparison server code



	source("../../server/multiple.r", local=TRUE) ###imports multiple comparison server code
	source("../../server/metric.r", local=TRUE) ###imports metric comparison server code
	source("../../server/stature.r", local=TRUE) ###imports stature outlier comparison server code
	source("../../server/twod.r", local=TRUE) ###imports two-dimensional scomparison server code
	source("../../server/threed.r", local=TRUE) ###imports three-dimensional comparison server code
	source("../../server/threedalignment.r", local=TRUE) ###imports three-dimensional alignment tool
	source("../../server/antestat_single.r", local=TRUE) ###imports single comparison antemortem stature code
	source("../../server/antestat_multiple.r", local=TRUE) ###imports multiple comparison antemortem stature code
	################stops the shiny app when closing session
	session$onSessionEnded(function() { stopApp()})

	################delete session temp directory on session end
	session$onSessionEnded(function() {
		unlink(sessiontemp, recursive = TRUE)    
	})
	
	#download handlers for files on the help page
	output$postmortem_template <- downloadHandler(
		filename <- function() {
			"postmortem_template.csv"
		},
		content <- function(file) {
			file.copy(system.file("extdata", 'postmortem_template.csv', package = "OsteoSort"), file)                  
		},
	)  

	#download handlers for files on the help page
	output$antemortem_template <- downloadHandler(
		filename <- function() {
			"antemortem_template.csv"
		},
		content <- function(file) {
			file.copy(system.file("extdata", 'antemortem_template.csv', package = "OsteoSort"), file)                  
		},
	)  
	
	output$osteoguide <- downloadHandler(
		filename <- function() {
			"OsteoSort_User_Manual.pdf"
		},
		content <- function(file) {
			file.copy(system.file("extdata", 'OsteoSort_User_Manual.pdf', package = "OsteoSort"), file)                  
		},
	)  			

	output$example_data <- downloadHandler(
		filename <- function() {
			"Example_Data.zip"
		},
		content <- function(file) {
			file.copy(system.file("extdata", 'Example_Data.zip', package = "OsteoSort"), file)                  
		},
	)  	

	output$measurement_conversion_table <- DT::renderDataTable(read.table(system.file("extdata", "Standardized_Measurements.csv", package = "OsteoSort"), header = TRUE, sep=","), options = list(lengthMenu = c(10,20,30,40,50), pageLength = 20), rownames = FALSE)
	
	observeEvent(input$Create_Desktop_Icon, {
		if(Sys.info()[['sysname']] == "Windows") {
			target <- paste('"', file.path(R.home("bin"), "R.exe"), '"', sep="")
			arguments <- paste('"', "-e ", "library(OsteoSort);OsteoSort()", '"', sep="")
			icon <- paste('"', system.file("vbs/OsteoSort.ico", package = "OsteoSort"), '"', sep="")
			pathname <- paste('"', paste(gsub("/Documents", "", file.path(path.expand("~"), "Desktop") ), "OsteoSort.lnk", sep = "/"), '"', sep="")
			vbs <- paste('"', system.file("vbs/createLink.vbs", package = "OsteoSort"), '"', sep="")
			system(paste("cscript", vbs, pathname, target, arguments, icon, sep=" "))
		}
		if(Sys.info()[['sysname']] == "Linux") {
			icon_name <- "OsteoSort.desktop"
			cat(		
				paste(
					"[Desktop Entry]\nEncoding=UTF-8\nTerminal=true\nType=Application\nCategories=Application\nName=OsteoSort\n",
					"Version=",	packageVersion("OsteoSort"),"\n",
					"Icon=",		system.file("OsteoSort/www/OsteoSort.png", package = "OsteoSort"),"\n",
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
})