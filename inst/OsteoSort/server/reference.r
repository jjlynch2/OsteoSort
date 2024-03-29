#Imports reference data and the config file
reference_name_list <- reactiveValues(reference_name_list = list.files(system.file("extdata/data", '', package = "OsteoSort"), recursive = FALSE, full.names = FALSE))
reference_list <- reactiveValues(reference_list = list())
config_df <- reactiveValues(config_df = data.frame())
units_df <- reactiveValues(units_df = data.frame())

observeEvent(TRUE, {
	reference_name_list$reference_name_list <- reference_name_list$reference_name_list[grepl(".ref", reference_name_list$reference_name_list)]
	for (i in reference_name_list$reference_name_list) {
		reference_list$reference_list[[i]] <- read.csv(file = paste(system.file("extdata/data", '', package = "OsteoSort"), i, sep="/"), header = TRUE, sep=",", stringsAsFactors=FALSE)
	}
	reference_name_list$reference_name_list <- gsub(".ref", "", reference_name_list$reference_name_list)
	names(reference_list$reference_list) <- reference_name_list$reference_name_list
	config_df$config_df <- read.csv(file = system.file("extdata/data", 'config', package = "OsteoSort"), header = TRUE, sep=",", stringsAsFactors=FALSE)
	units_df$units_df <- read.csv(file = system.file("extdata/data", 'units', package = "OsteoSort"), header = TRUE, sep=",", stringsAsFactors=FALSE)
})

output$importRefR <- renderUI({
	input$clearFileRef
	input$uploadFormat
	fileInput('importRef', '', accept=c('.ref'), multiple = TRUE)
})

observeEvent(input$clearFileRef, {
	if(!is.null(input$importRef$datapath)) {
		file.remove(input$importRef$datapath)
		file.remove(input$importRef$name)
	}
	fileInput('importRef', '', accept=c('.ref'), multiple = TRUE)
})

output$reference_data_interface <- renderUI({
	selectInput(inputId = "Reference_Sample", label = "", choices = reference_name_list$reference_name_list)
})

observeEvent(input$importRef, {
	for (i in length(input$importRef$name)) {
		file.copy(input$importRef$datapath[i], paste(system.file("extdata/data", '', package = "OsteoSort"),input$importRef$name[i],sep="/"))
		reference_name_list$reference_name_list[(length(reference_name_list$reference_name_list)+1)] <- input$importRef$name
		reference_list$reference_list[[(length(reference_list$reference_list)+1)]] <- read.csv(file = paste(system.file("extdata/data", '', package = "OsteoSort"),input$importRef$name[i],sep="/"), header = TRUE, sep=",", stringsAsFactors=FALSE)
	}
	reference_name_list$reference_name_list <- gsub(".ref", "", reference_name_list$reference_name_list)
	names(reference_list$reference_list) <- reference_name_list$reference_name_list
})

observeEvent(input$refdel, {
	rmf <- paste(system.file("extdata/data", '', package = "OsteoSort"),"/",input$Reference_Sample,".ref",sep="")
	if(file.exists(rmf)) { 
		file.remove(rmf) 
	}
	name_ind <- which(reference_name_list$reference_name_list == input$Reference_Sample)
	reference_list$reference_list <- reference_list$reference_list[-name_ind]
	reference_name_list$reference_name_list <- reference_name_list$reference_name_list[reference_name_list$reference_name_list != input$Reference_Sample]
})

observeEvent(input$Reference_Sample, {
	output$reference_table <- DT::renderDataTable ({
		DT::datatable(reference_list$reference_list[[input$Reference_Sample]],selection = list(mode="multiple"), options = list(lengthMenu = c(25,50,75,100), pageLength = 25, scrollX=TRUE), rownames = FALSE)
	})

	output$config_a <- renderUI({
		tempcona <- colnames(reference_list$reference_list[[input$Reference_Sample]][,-c(1:6)])
		selectInput(inputId = "config_a_input", label = "", choices = tempcona)
	})
	output$reference_config <- DT::renderDataTable ({
		DT::datatable(config_df$config_df, options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 20), rowname = FALSE)
	})
	output$reference_units <- DT::renderDataTable ({
		DT::datatable(units_df$units_df, options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 20), rowname = FALSE)
	})

})

observeEvent(input$config_a_input, {
	output$config_b <- renderUI({
		tempconb <- colnames(reference_list$reference_list[[input$Reference_Sample]][,-c(1:6)])
		tempconb <- tempconb[tempconb != input$config_a_input]
		selectInput(inputId = "config_b_input", label = "", choices = tempconb)
	})
})

output$config_render <- renderUI({
	radioButtons(inputId = "config_options", label = "", choices = c("Articulation_t-test","Stature"), selected = "Articulation_t-test")
})

output$units1 <- renderUI({
	textInput(inputId = "units1", label = "Stature", value = "cm")
})
output$units2 <- renderUI({
	textInput(inputId = "units2", label = "Measurement", value = "mm")
})

observeEvent(input$units_add, {
	skip = FALSE
	for(i in 1:nrow(units_df$units_df)) {
		if(units_df$units_df[i,1] == input$Reference_Sample && units_df$units_df[i,2] == input$units1 && units_df$units_df[i,3] == input$units2) {
			skip = TRUE
		}
	}
	if(!skip) {
		units_df$units_df <- rbind(units_df$units_df, data.frame(Reference = input$Reference_Sample, Stature = input$units1, Measurement = input$units2))
	}
	if(!skip) {
		write.csv(units_df$units_df, file = system.file("extdata/data", 'units', package = "OsteoSort"), col.names = TRUE, sep=",", row.names = FALSE)
	}
})

observeEvent(input$units_delete, {
	for(i in 1:nrow(units_df$units_df)) {
		if(units_df$units_df[i,1] == input$Reference_Sample && units_df$units_df[i,2] == input$units1 && units_df$units_df[i,3] == input$units2) {
			units_df$units_df <- units_df$units_df[-i,]
			write.csv(units_df$units_df, file = system.file("extdata/data", 'units', package = "OsteoSort"), col.names = TRUE, sep=",", row.names = FALSE)
			break
		}
	}
})

observeEvent(input$config_add, {
	skip = FALSE
	for(i in 1:nrow(config_df$config_df)) {
		if(config_df$config_df[i,1] == input$config_a_input && config_df$config_df[i,2] == input$config_b_input && config_df$config_df[i,3] == input$config_options && input$config_options == "Articulation_t-test" || config_df$config_df[i,1] == input$config_a_input && config_df$config_df[i,3] == input$config_options && input$config_options == "Stature") {
			skip = TRUE
		}
	}
	if(input$config_options == "Articulation_t-test" && !skip) {
		config_df$config_df <- rbind(config_df$config_df, data.frame(Measurementa = input$config_a_input, Measurementb = input$config_b_input, Method = input$config_options))
	}
	if(input$config_options == "Stature" && !skip) {
		config_df$config_df <- rbind(config_df$config_df, data.frame(Measurementa = input$config_a_input, Measurementb = "", Method = input$config_options))
	}
	if(!skip) {
		write.csv(config_df$config_df, file = system.file("extdata/data", 'config', package = "OsteoSort"), col.names = TRUE, sep=",", row.names = FALSE)
	}
})

observeEvent(input$config_delete, {
	for(i in 1:nrow(config_df$config_df)) {
		if(config_df$config_df[i,1] == input$config_a_input && config_df$config_df[i,2] == input$config_b_input && config_df$config_df[i,3] == input$config_options && input$config_options == "Articulation_t-test" || config_df$config_df[i,1] == input$config_a_input && config_df$config_df[i,3] == input$config_options && input$config_options == "Stature") {
			config_df$config_df <- config_df$config_df[-i,]
			write.csv(config_df$config_df, file = system.file("extdata/data", 'config', package = "OsteoSort"), col.names = TRUE, sep=",", row.names = FALSE)
			break
		}
	}
})

observeEvent(input$refsel, {
	if(is.numeric(input$reference_table_rows_selected)) {
		index = NULL
		for (i in 1:length(reference_name_list$reference_name_list)) {
			if(reference_name_list$reference_name_list[i] == "Custom_Selected") {
				index = i
			}
		}
		if(is.null(index)) {
			index <- length(reference_name_list$reference_name_list)+1
		}

		reference_name_list$reference_name_list[index] <- "Custom_Selected"
		reference_list$reference_list[[index]] <- reference_list$reference_list[[input$Reference_Sample]][input$reference_table_rows_selected,]
		reference_name_list$reference_name_list <- unique(reference_name_list$reference_name_list)
		names(reference_list$reference_list) <- reference_name_list$reference_name_list
	}
})

output$refdown <- downloadHandler(
	filename <- function() {
		"Reference.csv"
	},
	content <- function(file) {
		write.csv(reference_list$reference_list[[input$Reference_Sample]], file, row.names=FALSE, col.names=TRUE, quote = FALSE)
	}
)

