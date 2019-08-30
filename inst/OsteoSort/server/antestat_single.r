
fileoutputant1 <- reactiveValues(fileoutputant1 = TRUE)
output$fileoutputant1 <- renderUI({
	checkboxInput(inputId = "fileoutputant1", label = "Output csv file", value = TRUE)
})
observeEvent(input$fileoutputant1, {
	fileoutputant1$fileoutputant1 <- input$fileoutputant1
})

fileoutputant2 <- reactiveValues(fileoutputant2 = TRUE)
output$fileoutputant2 <- renderUI({
	checkboxInput(inputId = "fileoutputant2", label = "Output plot", value = TRUE)
})
observeEvent(input$fileoutputant2, {
	fileoutputant2$fileoutputant2 <- input$fileoutputant2
})

alphalevelsantestat <- reactiveValues(alphalevelsantestat = 0.05) 
output$alphalevelsantestat <- renderUI({
	sliderInput(inputId = "alphalevelsantestat", label = "Alpha level", min=0.01, max=1, value=0.05, step = 0.01)
})
observeEvent(input$alphalevelsantestat, {
	alphalevelsantestat$alphalevelsantestat <- input$alphalevelsantestat
})

stature_reference_ante <- reactiveValues(stature_reference_ante = c("temp"))
observeEvent(input$stature_reference_ante, {
	stature_reference_ante$stature_reference_ante <- input$stature_reference_ante
})
output$stature_reference_ante <- renderUI({
	selectInput(inputId = "stature_reference_ante", label = "Reference", choices = reference_name_list$reference_name_list)
})

elements_ante <- reactiveValues(elements_ante = c("temp") )
stature_reference_imported_ante <- reactiveValues(stature_reference_imported_ante = data.frame())

ante_elements <- reactiveValues(df = c())
ante_measurements <- reactiveValues(df = c())
observeEvent(input$stature_reference_ante, {
	stature_reference_imported_ante$stature_reference_imported_ante <- reference_list$reference_list[[stature_reference_ante$stature_reference_ante]]
	ante <- config_df$config_df[config_df$config_df$Method == 'Stature',]
	ref_col_names <- colnames(stature_reference_imported_ante$stature_reference_imported_ante)
	for(i in 1:nrow(ante)) {
		a = FALSE
		b = FALSE
		for(x in 1:length(ref_col_names)) {
			if(ante$Measurementa[i] == ref_col_names[x]) {
				ante_measurements$df <- c(ante_measurements$df, ante$Measurementa[i])
				temp1 <- na.omit(unique(stature_reference_imported_ante$stature_reference_imported_ante[!is.na(stature_reference_imported_ante$stature_reference_imported_ante[[ante$Measurementa[i]]]),]$Element))[1]
				ante_elements$df <- c(ante_elements$df, temp1)
				break
			}
		}
	}
})

output$single_ante_elements <- renderUI({
	selectInput(inputId = "single_ante_elements", label = "Elements", choices = ante_elements$df)
})

output$single_measurements_ante <- renderUI({
	lapply(ante_measurements$df[which(ante_elements$df == input$single_ante_elements)], function(i) {
		numericInput(paste0(i,"_ante"), label = i, value = "", min=0,max=999,step=0.01)
	})
})

observeEvent(input$proantestat, {
	showModal(modalDialog(title = "Calculation has started...Window will update when finished.", easyClose = FALSE, footer = NULL))
	withProgress(message = 'Calculation has started',
		detail = '', value = 0, {
		for (i in 1:10) {
				incProgress(1/10)
				Sys.sleep(0.05)
		}
	})

	if(is.numeric(input$antestat_input) && is.numeric(pmm)) {
		#calls sorting function
		antemortem_data_frame <- cbind.data.frame(input$Antemortem_ID, input$antestat_input, stringsAsFactors = FALSE)
		colnames(antemortem_data_frame) <- c("id","Stature")
		postmortem_data_frame <- cbind.data.frame(input$Postmortem_ID, input$ante_side, input$antestat, pmm, stringsAsFactors = FALSE)
		colnames(postmortem_data_frame) <- c("id","Side","Element",bonemeasurementm)

		outtemp1 <- antestat.input(bone = input$antestat, metric = input$metric_type, antemortem_stature = antemortem_data_frame, postmortem_measurement = postmortem_data_frame, population = input$antestat_population)
		outtemp2 <- antestat.regtest(sort = outtemp1[[1]], ref = outtemp1[[2]], prediction_interval = input$predlevelantestat, alphalevel = input$alphalevelsantestat, alphatest = temptest, output_options = c(input$fileoutputant1, input$fileoutputant2), sessiontempdir = sessiontemp)
		#display output

		#Fix for using exclusion in multiple. Switches to which ever has a result for single
		if(nrow(outtemp2[[2]]) == 0) {table_out_single <- outtemp2[[3]]}
		if(nrow(outtemp2[[3]]) == 0) {table_out_single <- outtemp2[[2]]}
		output$antestat_table <- DT::renderDataTable({
			DT::datatable(table_out_single, options = list(lengthMenu = c(1), pageLength = 10), rownames = FALSE)
		})
		if(input$fileoutputant1 || input$fileoutputant2) {
			#Zip handler       
			direc6 <- outtemp2[[1]] #direc temp
			files <- list.files(direc6, recursive = TRUE)
			setwd(direc6)
			if(input$fileoutputant2) {
				nimages <- list.files()
				nimages <- paste(sessiontemp, "/", direc6, "/", nimages[grep(".jpg", nimages)], sep="")

				output$plotplotante <- renderImage({
					list(src = nimages,
						contentType = 'image/jpg',
						width = 400,
						height = 400,
						alt = "A"
					)
				}, deleteFile = FALSE)
			}
			zip:::zip(zipfile = paste(direc6,'.zip',sep=''), files = files)
			setwd(sessiontemp)  #restores session
			#Download handler
			output$downloadantestat <- downloadHandler(
				filename <- function() {
					paste("results.zip")
				},
				content <- function(file) {
					setwd(direc6)
					file.copy(paste(direc6,'.zip',sep=''), file) 
					setwd(sessiontemp)  
				},
				contentType = "application/zip"
			)
		}
	}
	setwd(sessiontemp) #restores session
	removeModal() #removes modal
})
