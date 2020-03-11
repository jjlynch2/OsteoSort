forcefunante <- function(hera1) {
	df1 <- as.data.frame(cbind(from_id = hera1[,1], to_id = hera1[,3], Probability = hera1[,7], Element = rep("Stature", nrow(hera1))))
	df2 <- as.data.frame(cbind(from_id = hera1[,3], to_id = hera1[,1], Probability = hera1[,7], Element = paste(hera1[,4], hera1[,5],sep='-')))
	df <- rbind(df1, df2)
	temp <- df[!duplicated(df[,1]),c(1,4)]
	colnames(temp) <- c("name", "group")
	nodes <- temp
	colnames(df) <- c("source", "target", "value", "group")
	df <- df[,c(1:3)]
	for(i in 1:nrow(nodes)) {
		df[df$source == nodes[i,1],1] <- i-1
		df[df$target == nodes[i,1],2] <- i-1
	}
	links <- df
	return(list(links,nodes))
}

forcante <- reactiveValues(forcante = TRUE) 
output$forcante <- renderUI({
	checkboxInput(inputId = "forcante", label = "Interactive network graph", value = TRUE)
})
observeEvent(input$forcante, {
	forcante$forcante <- input$forcante
})

fileoutputant1m <- reactiveValues(fileoutputant1m = TRUE)
output$fileoutputant1m <- renderUI({
	checkboxInput(inputId = "fileoutputant1m", label = "Output csv file", value = TRUE)
})
observeEvent(input$fileoutputant1m, {
	fileoutputant1m$fileoutputant1m <- input$fileoutputant1m
})

multiple_file_output_graph_ante <- reactiveValues(multiple_file_output_graph_ante = TRUE) 
output$multiple_file_output_graph_ante <- renderUI({
	checkboxInput(inputId = "multiple_file_output_graph_ante", label = "Output network graph", value = TRUE)
})
observeEvent(input$multiple_file_output_graph_ante, {
	multiple_file_output_graph_ante$multiple_file_output_graph_ante <- input$multiple_file_output_graph_ante
})

labtfa <- reactiveValues(labtfa = TRUE) 
output$labtfa <- renderUI({
	checkboxInput(inputId = "labtfa", label = "Network graph labels", value = TRUE)
})
observeEvent(input$labtfa, {
	labtfa$labtfa <- input$labtfa
})

alphalevelsantestatm <- reactiveValues(alphalevelsantestatm = 0.05) 
output$alphalevelsantestatm <- renderUI({
	sliderInput(inputId = "alphalevelsantestatm", label = "Alpha level", min=0.01, max=1, value=0.05, step = 0.01)
})
observeEvent(input$alphalevelsantestatm, {
	alphalevelsantestatm$alphalevelsantestatm <- input$alphalevelsantestatm
})

stature_reference_antem <- reactiveValues(stature_reference_antem = c("temp"))
observeEvent(input$stature_reference_antem, {
	stature_reference_antem$stature_reference_antem <- input$stature_reference_antem
})
output$stature_reference_antem <- renderUI({
	selectInput(inputId = "stature_reference_antem", label = "Reference", choices = reference_name_list$reference_name_list)
})

stature_reference_imported_antem <- reactiveValues(stature_reference_imported_antem = data.frame())
ante_elementsm <- reactiveValues(df = c())
ante_measurementsm <- reactiveValues(df = c())
observeEvent(input$stature_reference_antem, {
	stature_reference_imported_antem$stature_reference_imported_antem <- reference_list$reference_list[[stature_reference_antem$stature_reference_antem]]
	antem <- config_df$config_df[config_df$config_df$Method == 'Stature',]
	ref_col_namesm <- colnames(stature_reference_imported_antem$stature_reference_imported_antem)
	for(i in 1:nrow(antem)) {
		a = FALSE
		b = FALSE
		for(x in 1:length(ref_col_namesm)) {
			if(antem$Measurementa[i] == ref_col_namesm[x]) {
				ante_measurementsm$df <- c(ante_measurementsm$df, antem$Measurementa[i])
				temp1 <- na.omit(unique(stature_reference_imported_antem$stature_reference_imported_antem[!is.na(stature_reference_imported_antem$stature_reference_imported_antem[[antem$Measurementa[i]]]),]$Element))[1]
				if(!is.na(temp1)) {
					ante_elementsm$df <- unique(c(ante_elementsm$df, temp1))
				}
				break
			}
		}
	}
	output$multiple_ante_elements <- renderUI({
		selectInput(inputId = "multiple_ante_elements", label = "Elements", choices = ante_elementsm$df)
	})

	output$multiple_measurements_ante <- renderUI({
		selectizeInput("Measurement_ante_mm", label = "Measurement", choices = ante_measurementsm$df[which(ante_elementsm$df == input$multiple_ante_elements)], selected = ante_measurementsm$df[which(ante_elementsm$df == input$multiple_ante_elements)], multiple = TRUE)
	})

})

numbercoresglobalm <- reactiveValues(ncorem = 1)
observeEvent(input$numbercoresm, {
	numbercoresglobalm$ncorem <- input$numbercoresm
})
output$ncoresm <- renderUI({
	sliderInput(inputId = "numbercoresm", label = "Number of cores", min=1, max=detectCores(), value=1, step =1)
})

#file upload render for multiple comparison
output$resettableInputante1 <- renderUI({
	input$clearFile1ante
	input$uploadFormat
	fileInput('file1ante', 'Upload antemortem statures', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))  
})
#file upload render for multiple comparison
output$resettableInputante2 <- renderUI({
	input$clearFile1ante
	input$uploadFormat
	fileInput('file2ante', 'Upload postmortem measurements', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))  
})
#clears session for multiple comparison
observeEvent(input$clearFile1ante, {
	fileInput('file1ante', 'Upload antemortem statures', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))  
	fileInput('file2ante', 'Upload postmortem measurements', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))  
})

observeEvent(input$proantestatm, {
	showModal(modalDialog(title = "Calculation has started...Window will update when finished.", easyClose = FALSE, footer = NULL))
	withProgress(message = 'Calculation has started', detail = '', value = 0, min=0, max=3, {
		inFile1 <- input$file1ante
		inFile2 <- input$file2ante
		if (is.null(inFile1) || is.null(inFile2)){
			removeModal()
			shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss")
			return(NULL) 
		}

		tempdata1m <- read.csv(inFile1$datapath, header=TRUE, sep=",", na.strings=c("", " ", "NA"))
		tempdata2m <- read.csv(inFile2$datapath, header=TRUE, sep=",", na.strings=c("", " ", "NA"))

		incProgress(amount = 1, message = "Antemortem: sorting data")
		outtemp1m <- antestat.input(bone = input$multiple_ante_elements, 
								antemortem_stature = tempdata1m, 
								postmortem_measurement = tempdata2m, 
								side = input$state_reference_ante_sidem, 
								measurement = input$Measurement_ante_mm,
								ref = stature_reference_imported_antem$stature_reference_imported_antem
		)
		if(is.null(outtemp1m)) {removeModal();shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}
		if(is.null(input$Measurement_ante_mm)) {removeModal();shinyalert(title = "ERROR!", text="The measurement is missing.",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}
		incProgress(amount = 1, message = "Antemortem: running comparisons")
		outtemp2m <- antestat.regtest(labtfa = labtfa$labtfa,
									threads = numbercoresglobalm$ncorem, 
									antemortem = outtemp1m[[1]], 
									postmortem = outtemp1m[[2]], 
									ref = outtemp1m[[3]],
									alphalevel = alphalevelsantestatm$alphalevelsantestatm, 
									output_options = c(fileoutputant1m$fileoutputant1m, FALSE, multiple_file_output_graph_ante$multiple_file_output_graph_ante), 
									sessiontempdir = sessiontemp
		)

		if(!all(is.na(outtemp2m[[2]])) || !all(is.na(outtemp2m[[3]]))) {

			ll <- nrow(outtemp2m[[2]]) + nrow(outtemp2m[[3]])
			nmatch <- nrow(outtemp2m[[2]])
			pmsize <- length(unique(c(outtemp2m[[2]][,1], outtemp2m[[3]][,1]))) 
			amsize <- length(unique(c(outtemp2m[[2]][,4], outtemp2m[[3]][,4]))) 
			t_time <- outtemp2m[[4]]
			output$antestat_outputm <- renderUI({
				HTML(paste("<strong>",
							"Completed in: ", "<font color=\"#00688B\">", t_time, " minutes</font>", 
							"<br/>","Comparisons: ",   "<font color=\"#00688B\">", ll, "</font>", 
				              "<br/>", "Postmortem Specimens: ",           "<font color=\"#00688B\">",pmsize, "</font>", 
				              "<br/>", "Antemortem Statures: ",           "<font color=\"#00688B\">",amsize, "</font>", 
				              '<br/>', "Potential matches: ",  "<font color=\"#00688B\">",nmatch , "</font>",
				              '<br/>', "Exclusions: ",         "<font color=\"#00688B\">",ll - nmatch, " (", round((ll - nmatch) / ll, digits = 3) * 100, "%)",  "</font>",
							'</strong>'))
			})
		}

		output$antestat_table1m <- DT::renderDataTable({
			DT::datatable(outtemp2m[[2]], selection = list(mode="multiple"), options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
		})
		output$antestat_table2m <- DT::renderDataTable({
			DT::datatable(outtemp2m[[3]], selection = list(mode="multiple"), options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
		})

		if(forcante$forcante) {
				if(nrow(outtemp2m[[2]]) > 1){
					td <- forcefunante(outtemp2m[[2]])
					links <- td[[1]]
					nodes <- td[[2]]
					output$forceNetworkOSMante <- renderForceNetwork({
						forceNetwork(Links = links, Nodes = nodes,
								  Source = "source", Target = "target",
								  Value = "value", NodeID = "name",
								  Group = "group", opacity = 1,
									colourScale = JS('d3.scaleOrdinal().domain(["1", "2"]).range(["#ea6011","#126a8f"])'),
									zoom = TRUE
						)
					})
				}
		}

		if(fileoutputant1m$fileoutputant1m || multiple_file_output_graph_ante$multiple_file_output_graph_ante) {
			setwd(outtemp2m[[1]])
			nimages <- list.files()
			if(multiple_file_output_graph_ante$multiple_file_output_graph_ante && length(nimages[grep(".jpg", nimages)]) != 0) {
				nimages <- paste(sessiontemp, "/", outtemp2m[[1]], "/", nimages[grep(".jpg", nimages)], sep="")
			} else {
				nimages <- system.file("OsteoSort/www", 'blank.jpg', package = "OsteoSort")
			}
			output$multiple_plot_na_ante <- renderImage({
				list(src = nimages,
					contentType = 'image/jpg',
					height = 800,
					alt = "A"
				)
			}, deleteFile = FALSE)
			setwd(sessiontemp)
			output$downloadantestatm <- downloadHandler(
				filename <- function() {
					paste("results.zip")
				},
				content <- function(file) {
					setwd(outtemp2m[[1]])
					file.remove(paste(outtemp2m[[1]],'.zip',sep=''))
					if(is.numeric(input$antestat_table1m_rows_selected)) {
						no_return_value <- OsteoSort:::output_function(outtemp2m[[2]][input$antestat_table1m_rows_selected,], method="exclusion", type="csv2")
					} else {file.remove("excluded-selected-list.csv")}
					if(is.numeric(input$antestat_table2m_rows_selected)) {
						no_return_value <- OsteoSort:::output_function(outtemp2m[[3]][input$antestat_table2m_rows_selected,], method="exclusion", type="csv2")
					} else {file.remove("not-excluded-selected-list.csv")}
					setwd(sessiontemp)
					files <- list.files(outtemp2m[[1]], recursive = TRUE)
					setwd(outtemp2m[[1]])
					zip:::zipr(zipfile = paste(outtemp2m[[1]],'.zip',sep=''), files = files[1], compression = 1)
					for(file_na in files[-1]) {
						zip:::zipr_append(zipfile = paste(outtemp2m[[1]],'.zip',sep=''), files = file_na, compression = 1)
					}
					file.copy(paste(outtemp2m[[1]],'.zip',sep=''), file) 
					setwd(sessiontemp)  
				},
				contentType = "application/zip"
			)
		}
		setwd(sessiontemp) #restores session
		removeModal() #removes modal
		incProgress(amount = 1, message = "Completed")
	})
})
