source("./ui/single.r", local=TRUE) 
source("./ui/multiple.r", local=TRUE) 
source("./ui/files.r", local=TRUE) 

shinyUI(
	navbarPage(theme = "css/flatly.min.css", windowTitle = "OsteoSort",
		tags$script(HTML(paste("var header = $('.navbar > .container-fluid');header.append('<div style=\"float:left\"><img src=\"osteosort.png\" alt=\"alt\" style=\"float:right; width:200px;padding-top:0px;\"></div><div style=\"float:right; padding-top:15px\">", 
			"<font color=\"#FFFFFF\"><strong>Version: ", "1.4.0","</strong></font>","</div>');console.log(header)", sep=""))
		),
		single_osteometric_sorting,
		multiple_osteometric_sorting,
		files
	)
)