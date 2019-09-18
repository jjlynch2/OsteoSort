
fileoutputant1m <- reactiveValues(fileoutputant1m = TRUE)
output$fileoutputant1m <- renderUI({
	checkboxInput(inputId = "fileoutputant1m", label = "Output csv file", value = TRUE)
})
observeEvent(input$fileoutputant1m, {
	fileoutputant1m$fileoutputant1m <- input$fileoutputant1m
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
				ante_elementsm$df <- unique(c(ante_elementsm$df, temp1))
				break
			}
		}
	}
	output$multiple_ante_elements <- renderUI({
		selectInput(inputId = "multiple_ante_elements", label = "Elements", choices = ante_elementsm$df)
	})

	output$multiple_measurements_ante <- renderUI({
		selectInput("Measurement_ante_mm", label = "Measurement", choices = ante_measurementsm$df[which(ante_elementsm$df == input$multiple_ante_elements)])
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
	withProgress(message = 'Calculation has started',
		detail = '', value = 0, {
		for (i in 1:10) {
			incProgress(1/10)
			Sys.sleep(0.05)
		}
	})

	inFile1 <- input$file1ante
	inFile2 <- input$file2ante
	if (is.null(inFile1) || is.null(inFile2)){
		removeModal()                             
		return(NULL) 
	}

	tempdata1m <- read.csv(inFile1$datapath, header=TRUE, sep=",", na.strings=c("", " ", "NA"))
	tempdata2m <- read.csv(inFile2$datapath, header=TRUE, sep=",", na.strings=c("", " ", "NA"))

	outtemp1m <- antestat.input(bone = input$multiple_ante_elements, 
							antemortem_stature = tempdata1m, 
							postmortem_measurement = tempdata2m, 
							side = input$state_reference_ante_sidem, 
							measurement = input$Measurement_ante_mm,
							ref = stature_reference_imported_antem$stature_reference_imported_antem
	)

	outtemp2m <- antestat.regtest(threads = numbercoresglobalm$ncorem, 
								antemortem = outtemp1m[[1]], 
								postmortem = outtemp1m[[2]], 
								ref = outtemp1m[[3]],
								alphalevel = alphalevelsantestatm$alphalevelsantestatm, 
								output_options = c(fileoutputant1m$fileoutputant1m, FALSE), 
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
		                   '<br/>', "Exclusions: ",         "<font color=\"#00688B\">",ll - nmatch, " (", round((ll - nmatch) / ll, digits = 3) * 100, "%)",  "</font>",'</strong>'))
		})
	}

	output$antestat_table1m <- DT::renderDataTable({
		DT::datatable(outtemp2m[[2]], selection = list(mode="multiple"), options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
	})
	output$antestat_table2m <- DT::renderDataTable({
		DT::datatable(outtemp2m[[3]], selection = list(mode="multiple"), options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
	})

	observeEvent(input$antestat_table1m_rows_selected,{
		if(fileoutputant1m$fileoutputant1m) {
			setwd(outtemp2m[[1]])
			file.remove(paste(outtemp2m[[1]],'.zip',sep=''))
			no_return_value <- OsteoSort:::output_function(outtemp2m[[2]][input$antestat_table1m_rows_selected,], method="exclusion", type="csv2")
			setwd(sessiontemp)

			files <- list.files(outtemp2m[[1]], recursive = TRUE)
			setwd(outtemp2m[[1]])
			zip:::zipr(zipfile = paste(outtemp2m[[1]],'.zip',sep=''), files = files[1], compression = 1)
			for(file_na in files[-1]) {
				zip:::zipr_append(zipfile = paste(outtemp2m[[1]],'.zip',sep=''), files = file_na, compression = 1)
			}
			setwd(sessiontemp)
		}
	})
	observeEvent(input$antestat_table2m_rows_selected,{
		if(fileoutputant1m$fileoutputant1m) {
			setwd(outtemp2m[[1]])
			file.remove(paste(outtemp2m[[1]],'.zip',sep=''))
			no_return_value <- OsteoSort:::output_function(outtemp2m[[3]][input$antestat_table2m_rows_selected,], method="exclusion", type="csv2")
			setwd(sessiontemp)

			files <- list.files(outtemp2m[[1]], recursive = TRUE)
			setwd(outtemp2m[[1]])
			zip:::zipr(zipfile = paste(outtemp2m[[1]],'.zip',sep=''), files = files[1], compression = 1)
			for(file_na in files[-1]) {
				zip:::zipr_append(zipfile = paste(outtemp2m[[1]],'.zip',sep=''), files = file_na, compression = 1)
			}
			setwd(sessiontemp)
		}
	})

	if(fileoutputant1m$fileoutputant1m) {
		direc6 <- outtemp2m[[1]] #direc temp
		files <- list.files(direc6, recursive = TRUE)
		setwd(direc6)
		zip:::zipr(zipfile = paste(direc6,'.zip',sep=''), files = files)
		setwd(sessiontemp)  #restores session
		output$downloadantestatm <- downloadHandler(
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
	setwd(sessiontemp) #restores session
	removeModal() #removes modal
})
