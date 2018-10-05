#Imports reference data and the config file

reference_name_list <- reactiveValues(reference_name_list = list())
reference_list <- reactiveValues(reference_list = list())
config_df <- reactiveValues(config_df = data.frame())

observeEvent(reference_list$reference_list, {
	reference_name_list$reference_name_list <- list.files(system.file("extdata/data", '', package = "OsteoSort"), recursive = FALSE, full.names = FALSE)
	reference_name_list$reference_name_list <- reference_name_list$reference_name_list[grepl(".ref", reference_name_list$reference_name_list)]

	for (i in reference_name_list$reference_name_list) {
		reference_list$reference_list[[i]] <- read.csv(file = paste(system.file("extdata/data", '', package = "OsteoSort"), i, sep=""), header = TRUE, sep=",", stringsAsFactors=FALSE)
	}

	reference_name_list$reference_name_list <- gsub(".ref", "", reference_name_list$reference_name_list)
	names(reference_list$reference_list) <- reference_name_list$reference_name_list

	config_df$config_df <- read.csv(file = system.file("extdata/data", 'config', package = "OsteoSort"), header = TRUE, sep=",", stringsAsFactors=FALSE)
})


output$reference_data_interface <- renderUI({
	selectInput(inputId = "Reference_Sample", label = "Reference", choices = reference_name_list$reference_name_list)
})

output$importRefR <- renderUI({
	input$clearFileRef
	input$uploadFormatRef
	fileInput('importRef', '', accept=c('.ref'))
})

#clears session for multiple comparison
observeEvent(input$clearFileRef, {
	fileInput('importRef', '', accept=c('.ref'))
})

#work in progress
observeEvent(input$importRef, {
	for (i in input$importRef$name) {
		file.copy(input$importRef$datapath[i], paste(system.file("extdata/data", '', package = "OsteoSort"),input$importRef$name[i],sep=""))
	}

	reference_name_list$reference_name_list <- list.files(system.file("extdata/data", '', package = "OsteoSort"), recursive = FALSE, full.names = FALSE)
	reference_name_list$reference_name_list <- reference_name_list$reference_name_list[grepl(".ref", reference_name_list$reference_name_list)]

	for (i in reference_name_list$reference_name_list) {
		reference_list$reference_list[[i]] <- read.csv(file = paste(system.file("extdata/data", '', package = "OsteoSort"), i, sep=""), header = TRUE, sep=",", stringsAsFactors=FALSE)
	}

	reference_name_list$reference_name_list <- gsub(".ref", "", reference_name_list$reference_name_list)
	names(reference_list$reference_list) <- reference_name_list$reference_name_list

})