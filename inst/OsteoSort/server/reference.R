reference_name_list <- reactiveValues(reference_name_list = list.files(path = "../../../extdata/config/", recursive = FALSE, full.names = FALSE))
reference_list <- reactiveValues(reference_list = list())

config_name_list <- reactiveValues(config_name_list = list.files(path = "../../../extdata/config/", recursive = FALSE, full.names = FALSE))
config_list <- reactiveValues(config_list = list())

observeEvent(reference_list$reference_list, {
	reference_name_list$reference_name_list <- reference_name_list$reference_name_list[grepl(".ref", reference_name_list$reference_name_list)]
	config_name_list$config_name_list <- config_name_list$config_name_list[grepl(".conf", config_name_list$config_name_list)]

	for (i in reference_name_list$reference_name_list) {
		reference_list$reference_list[[i]] <- read.csv(file = paste("../../../extdata/config/", i, sep=""), header = TRUE, sep=",", stringsAsFactors=FALSE)
	}

	for (i in config_name_list$config_name_list) {
		config_list$config_list[[i]] <- read.csv(file = paste("../../../extdata/config/", i, sep=""), header = TRUE, sep=",", stringsAsFactors=FALSE)
	}


print(reference_name_list$reference_name_list)
print(config_name_list$config_name_list)
	reference_name_list$reference_name_list <- gsub(".ref", "", reference_name_list$reference_name_list)
	config_name_list$config_name_list <- gsub(".conf", "", config_name_list$config_name_list)

print(reference_name_list$reference_name_list)
print(config_name_list$config_name_list)

	names(reference_list$reference_list) <- reference_name_list$reference_name_list
	names(config_list$config_list) <- config_name_list$config_name_list

print(reference_name_list$reference_name_list)
print(config_name_list$config_name_list)
	output$reference_data_interface <- renderUI({
		selectInput(inputId = "Reference_Sample", label = "Reference", choices = reference_name_list$reference_name_list)
	})
global1 <<- reference_list$reference_list
global2 <<- config_list$config_list
global3 <<- reference_name_list$reference_name_list
global4 <<- config_name_list$config_name_list
})

#observeEvent(input$reference_import, {
#	for (i in input$reference_import$name) {
#		file.copy(input$reference_import$datapath[i], paste("../../extdata/config/",input$reference_import$name[i],sep=""))
#	}

#	config_name_list$config_name_list = list.files(path = "../../../extdata/config/", recursive = FALSE, full.names = FALSE)
#	config_list$config_list = list()

#	reference_name_list$reference_name_list = list.files(path = "../../../extdata/config/", recursive = FALSE, full.names = FALSE)
#	reference_list$reference_list = list()
#})

output$reference_import_upload <- renderUI({
	input$reference_import
	input$uploadFormat
	fileInput('reference_import', 'Import Reference Data', accept=c(".conf", "ref"), multiple = TRUE)
})