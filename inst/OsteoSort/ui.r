library(OsteoSort) #for deployment in shiny-server
source(system.file("OsteoSort/ui", 'dash.r', package = "OsteoSort"), local=TRUE) ###imports dashboard code
source(system.file("OsteoSort/ui", 'reference.r', package = "OsteoSort"), local=TRUE) ###imports reference code
source(system.file("OsteoSort/ui", 'single_osteometric_sorting.r', package = "OsteoSort"), local=TRUE) ###imports single osteometric sorting code
source(system.file("OsteoSort/ui", 'multiple_osteometric_sorting.r', package = "OsteoSort"), local=TRUE) ###imports multiple osteometric sorting code
source(system.file("OsteoSort/ui", 'outlier_metric.r', package = "OsteoSort"), local=TRUE) ###imports outlier metric code
source(system.file("OsteoSort/ui", 'outlier_stature.r', package = "OsteoSort"), local=TRUE) ###imports outlier stature code
#source(system.file("OsteoSort/ui", 'exit_button.r', package = "OsteoSort"), local=TRUE) ###imports exit button code
shinyUI(
navbarPage(theme = "css/flatly.min.css", windowTitle = "OsteoSort",
	header = tagList(
		useShinydashboard()
	),
	tags$script(HTML(paste("var header = $('.navbar > .container-fluid');header.append('<div style=\"float:left\"><img src=\"osteosort_new.png\" alt=\"alt\" style=\"float:right; width:200px;padding-top:0px;\"></div><div style=\"float:right; padding-top:15px\">", 
		uiOutput("memUsage"), "</div>');console.log(header)", sep=""))
	),
	navbarMenu("Help",icon = icon("info", lib="font-awesome"),
			dash#,
			#reference,
			#exit_button
		),
		navbarMenu("Osteometric",icon = icon("chart-column", lib="font-awesome"),
			single_osteometric_sorting,
			multiple_osteometric_sorting
		),
		navbarMenu("Outlier",icon = icon("arrow-down-short-wide", lib="font-awesome"),
			outlier_metric,
			outlier_stature
		)
	)
)
