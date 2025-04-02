#Imports reference data and the config file
reference_name_list <- reactiveValues(reference_name_list = list.files("./extdata/data", recursive = FALSE, full.names = FALSE))
reference_list <- reactiveValues(reference_list = list())
config_df <- reactiveValues(config_df = data.frame())

observeEvent(TRUE, {
	reference_name_list$reference_name_list <- reference_name_list$reference_name_list[grepl(".ref", reference_name_list$reference_name_list)]
	for (i in reference_name_list$reference_name_list) {
		reference_list$reference_list[[i]] <- read.csv(file = paste("./extdata/data", i, sep="/"), header = TRUE, sep=",", stringsAsFactors=FALSE)
	}
	reference_name_list$reference_name_list <- gsub(".ref", "", reference_name_list$reference_name_list)
	names(reference_list$reference_list) <- reference_name_list$reference_name_list
	config_df$config_df <- read.csv(file = "./extdata/data/config", header = TRUE, sep=",", stringsAsFactors=FALSE)
})