
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
				if(!is.na(temp1)) {
					ante_elements$df <- unique(c(ante_elements$df, temp1))
				}
				break
			}
		}
	}
	output$measurement_units_antes <- renderUI({
		if(any(units_df$units_df[,1] == input$stature_reference_ante)) {
			measurement_units <- paste(" ", units_df$units_df[units_df$units_df$Reference == input$stature_reference_ante,3], sep="")
			stature_units <- paste(" ", units_df$units_df[units_df$units_df$Reference == input$stature_reference_ante,2], sep="")
			HTML(paste("<strong>","Measurement units:",measurement_units, "</strong><br/>",
				   "<strong>","Stature units:",stature_units, "</strong><br/>"
			))
		} else {
			HTML(paste(""))
		}
	})
})

output$single_ante_elements <- renderUI({
	selectInput(inputId = "single_ante_elements", label = "Elements", choices = ante_elements$df)
})

output$single_measurements_ante <- renderUI({
	lapply(ante_measurements$df[which(ante_elements$df == input$single_ante_elements)], function(i) {
		numericInput(paste0(i,"_ante"), label = i, value = "", min=0,max=999,step=0.01)
	})
})
output$antestat_input_t <- renderUI({
	numericInput(inputId = 'antestat_input', label = 'Stature', value = '')
})

observeEvent(input$proantestat, {
	showModal(modalDialog(title = "Calculation has started...Window will update when finished.", easyClose = FALSE, footer = NULL))
	if(!any(unique(colnames(stature_reference_imported_ante$stature_reference_imported_ante)) == "Stature")) {removeModal();shinyalert(title = "ERROR!", text="Stature information is not available for this reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}
	withProgress(message = 'Calculation has started', detail = '', value = 0, min=0, max=3, {
		if(is.na(input$antestat_input) || is.na(input[[paste0(ante_measurements$df[which(ante_elements$df == input$single_ante_elements)], "_ante")]])) {removeModal();shinyalert(title = "ERROR!", text="Data input error.",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}

		ante <- data.frame(id = input$Antemortem_ID_ante, stature = input$antestat_input)
		post <- data.frame(id = input$Postmortem_ID_ante, side = input$state_reference_ante_side, element = input$single_ante_elements, input[[paste0(ante_measurements$df[which(ante_elements$df == input$single_ante_elements)], "_ante")]])
		colnames(ante) <- c("id", "Stature")
		colnames(post) <- c("id", "Side", "Element",ante_measurements$df[which(ante_elements$df == input$single_ante_elements)])


		incProgress(amount = 1, message = "Antemortem: sorting data")
		outtemp1 <- antestat.input(bone = input$single_ante_elements,
							  antemortem_stature = ante,
							  postmortem_measurement = post,
							  ref = stature_reference_imported_ante$stature_reference_imported_ante,
							  measurement = ante_measurements$df[which(ante_elements$df == input$single_ante_elements)],
							  side = input$state_reference_ante_side
		)
		if(is.null(outtemp1)) {removeModal();shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}
		incProgress(amount = 1, message = "Antemortem: running comparison")
		outtemp2 <- antestat.regtest(antemortem = outtemp1[[1]],
								postmortem = outtemp1[[2]],
								ref = outtemp1[[3]],
								alphalevel = alphalevelsantestat$alphalevelsantestat,
								output_options = c(fileoutputant1$fileoutputant1, fileoutputant2$fileoutputant2),
								sessiontempdir = sessiontemp
		)
		#display output

		tempDF <- rbind(outtemp2[[2]], outtemp2[[3]]) #combines excluded and not excluded for results
		output$antestat_table <- DT::renderDataTable({
			DT::datatable(tempDF, options = list(lengthMenu = c(1), pageLength = 1, dom = 't', ordering=F), rownames = FALSE)
		})
		if(fileoutputant1$fileoutputant1 || fileoutputant2$fileoutputant2) {
			#Zip handler       
			direc6 <- outtemp2[[1]] #direc temp
			sd <- paste(sessiontemp,direc6,sep="/")
			files <- list.files(sd, recursive = TRUE)
			if(fileoutputant2$fileoutputant2) {
				nimages <- list.files(sd)
				nimages <- paste(sessiontemp, "/", direc6, "/", nimages[grep(".jpg", nimages)], sep="")

				output$plotplotante <- renderImage({
					list(src = nimages,
						contentType = 'image/jpg',
						height = 400,
						alt = "A"
					)
				}, deleteFile = FALSE)
			}
			files <- list.files(sd, recursive = TRUE, full.names=TRUE)
			zip:::zipr(zipfile = paste(sd,"/",direc6,'.zip',sep=''), files = files)
			#Download handler
			output$downloadantestat <- downloadHandler(
				filename <- function() {
					paste("results.zip")
				},
				content <- function(file) {
					file.copy(paste(sd,"/",direc6,'.zip',sep=''), file)
				},
				contentType = "application/zip"
			)
		}
		removeModal() #removes modal
		incProgress(amount = 1, message = "Completed")
	})
})
