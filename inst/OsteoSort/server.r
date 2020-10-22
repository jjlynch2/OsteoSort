options(rgl.useNULL=TRUE) #required to avoid rgl device opening 
options(scipen = 999)
options(shiny.maxRequestSize=100*1024^2) #10MB file size limit
options(warn = -1) #disables warnings
options(as.is = TRUE)
options(stringsAsFactors = FALSE)
library(OsteoSort) #call for shiny-server deployment
shinyServer(function(input, output, session) {
	#defines which modules to include
	source(system.file("OsteoSort/server", 'temp_dir.r', package = "OsteoSort"), local=TRUE) ###imports temporary directory code
	source(system.file("OsteoSort/server", 'versions.r', package = "OsteoSort"), local=TRUE) ###imports versioning code
	source(system.file("OsteoSort/server", 'julia.r', package = "OsteoSort"), local=TRUE) ###imports julia environment
	source(system.file("OsteoSort/server", 'reference.r', package = "OsteoSort"), local=TRUE) ###imports single comparison server code
	source(system.file("OsteoSort/server", 'single.r', package = "OsteoSort"), local=TRUE) ###imports single comparison server code
	source(system.file("OsteoSort/server", 'multiple.r', package = "OsteoSort"), local=TRUE) ###imports multiple comparison server code
	source(system.file("OsteoSort/server", 'metric.r', package = "OsteoSort"), local=TRUE) ###imports metric comparison server code
	source(system.file("OsteoSort/server", 'stature.r', package = "OsteoSort"), local=TRUE) ###imports stature outlier comparison server code
	source(system.file("OsteoSort/server", 'twod.r', package = "OsteoSort"), local=TRUE) ###imports two-dimensional scomparison server code
	source(system.file("OsteoSort/server", 'threed.r', package = "OsteoSort"), local=TRUE) ###imports three-dimensional comparison server code
	source(system.file("OsteoSort/server", 'threedalignment.r', package = "OsteoSort"), local=TRUE) ###imports three-dimensional alignment tool
	source(system.file("OsteoSort/server", 'antestat_single.r', package = "OsteoSort"), local=TRUE) ###imports single comparison antemortem stature code
	source(system.file("OsteoSort/server", 'antestat_multiple.r', package = "OsteoSort"), local=TRUE) ###imports multiple comparison antemortem stature code
	source(system.file("OsteoSort/server", 'files.r', package = "OsteoSort"), local=TRUE) ###imports code to save supplemental files from help menu
	source(system.file("OsteoSort/server", 'shortcut.r', package = "OsteoSort"), local=TRUE) ###imports code to create shortcut from help menu
	source(system.file("OsteoSort/server", 'measurement_standards.r', package = "OsteoSort"), local=TRUE) ###imports code to create measurement conversion data table
	source(system.file("OsteoSort/server", 'system_info.r', package = "OsteoSort"), local=TRUE) ###imports code to display system information
	source(system.file("OsteoSort/server", 'URL.r', package = "OsteoSort"), local=TRUE) ###imports code to display URLs
	source(system.file("OsteoSort/server", 'update_gh.r', package = "OsteoSort"), local=TRUE) ###imports code to check for updates
	source(system.file("OsteoSort/server", 'about_refs.r', package = "OsteoSort"), local=TRUE) ###imports code to display references
	source(system.file("OsteoSort/server", 'changes.r', package = "OsteoSort"), local=TRUE) ###imports code to display version changes
	#source(system.file("OsteoSort/server", 'exit_parameters.r', package = "OsteoSort"), local=TRUE) ###imports code to execute when app closes. comment for shiny-server deployment
	source(system.file("OsteoSort/server", 'exit_parameters_shiny-server.r', package = "OsteoSort"), local=TRUE) #uncomment for shiny-server deployment
})
