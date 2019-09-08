
fileoutputant1m <- reactiveValues(fileoutputant1m = TRUE)
output$fileoutputant1m <- renderUI({
	checkboxInput(inputId = "fileoutputant1m", label = "Output csv file", value = TRUE)
})
observeEvent(input$fileoutputant1m, {
	fileoutputant1m$fileoutputant1m <- input$fileoutputant1m
})

fileoutputant2m <- reactiveValues(fileoutputant2m = TRUE)
output$fileoutputant2m <- renderUI({
	checkboxInput(inputId = "fileoutputant2m", label = "Output plot", value = TRUE)
})
observeEvent(input$fileoutputant2m, {
	fileoutputant2m$fileoutputant2m <- input$fileoutputant2m
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

elements_antem <- reactiveValues(elements_antem = c("temp") )
stature_reference_imported_antem <- reactiveValues(stature_reference_imported_antem = data.frame())

ante_elementsm <- reactiveValues(df = c())
ante_measurementsm <- reactiveValues(df = c())
observeEvent(input$stature_reference_antem, {
	stature_reference_imported_antem$stature_reference_imported_antem <- reference_list$reference_list[[stature_reference_antem$stature_reference_antem]]
	ante <- config_df$config_df[config_df$config_df$Method == 'Stature',]
	ref_col_names <- colnames(stature_reference_imported_antem$stature_reference_imported_antem)
	for(i in 1:nrow(ante)) {
		a = FALSE
		b = FALSE
		for(x in 1:length(ref_col_names)) {
			if(ante$Measurementa[i] == ref_col_names[x]) {
				ante_measurements$df <- c(ante_measurements$df, ante$Measurementa[i])
				temp1 <- na.omit(unique(stature_reference_imported_antem$stature_reference_imported_antem[!is.na(stature_reference_imported_antem$stature_reference_imported_antem[[ante$Measurementa[i]]]),]$Element))[1]
				ante_elements$df <- c(ante_elements$df, temp1)
				break
			}
		}
	}
})

output$multiple_ante_elements <- renderUI({
	selectInput(inputId = "multiple_ante_elements", label = "Elements", choices = ante_elementsm$df)
})

numbercoresglobalm <- reactiveValues(ncorem = detectCores()-1)
observeEvent(input$numbercoresm, {
	numbercoresglobalm$ncorem <- input$numbercoresm
})
output$ncoresm <- renderUI({
	sliderInput(inputId = "numbercoresm", label = "Number of threads", min=1, max=detectCores(), value=detectCores()-1, step =1)
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

	#Upload CSV file
	inFile1 <- input$file1ante
	inFile2 <- input$file2ante
	 #return null if not uploaded
	if (is.null(inFile1) || is.null(inFile2)){
		removeModal()                             
		return(NULL) 
	}

	tempdata1m <- read.csv(inFile1$datapath, header=TRUE, sep=",", na.strings=c("", " ", "NA"))## see na.strings forces NA for blanks, spaces, etc
	tempdata2m <- read.csv(inFile2$datapath, header=TRUE, sep=",", na.strings=c("", " ", "NA"))## see na.strings forces NA for blanks, spaces, etc		


	outtemp1m <- antestat.input(bone = input$antestatm, metric = input$metric_typem, antemortem_stature = tempdata1m, postmortem_measurement = tempdata2m, population = input$antestat_populationm)
	outtemp2m <- antestat.regtest(threads = numbercoresglobalm$ncorem, sort = outtemp1m[[1]], ref = outtemp1m[[2]], prediction_interval = input$predlevelantestatm, alphalevel = input$alphalevelsantestatm, alphatest = temptest, output_options = c(input$fileoutputant1m, input$fileoutputant2m), sessiontempdir = sessiontemp)
	  
	coo <- ""
	if(input$research_mm) {
		#used to assess accuracy of methodology
		globala <- as.matrix(outtemp2m[[3]][1])
		globalb <- as.matrix(outtemp2m[[3]][3])
		cn <- 0
		for(xx in 1:nrow(globala)) {
			if(globala[xx] == globalb[xx]) {cn <- cn +1}
		}
		global1 <- as.matrix(outtemp2m[[2]][1])
		global2 <- as.matrix(outtemp2m[[2]][3])
		co <- 0
		 for(i in 1:nrow(global1)) {
		 	if(global1[i] == global2[i]) {co <- co + 1}
		 }		


	temp1 <- outtemp2m[[2]][1]
	temp2 <- outtemp2m[[3]][1]
	ante_sample <- nrow(unique(rbind(temp1,temp2))) 
	temp3 <- outtemp2m[[2]][3]
	temp4 <- outtemp2m[[3]][3]
	post_sample <- nrow(unique(rbind(temp3,temp4))) 
	n_comp <- nrow(outtemp2m[[2]]) + nrow(outtemp2m[[3]])
	ex_rate <- nrow(outtemp2m[[3]]) / n_comp
		#display output
		output$antestat_outputm <- renderUI({
				HTML(paste("<strong>",'<br/>',"Comparisons: ", 					"<font color=\"#00688B\">",n_comp,"</font>", 
								  "<br/>", "Antemortem specimens: ", 			"<font color=\"#00688B\">",ante_sample, "</font>", 
								  "<br/>","Postmortem specimens: ", 			"<font color=\"#00688B\">",post_sample, "</font>", 
								  "<br/>", "Potential associations: ", 			"<font color=\"#00688B\">",nrow(outtemp2m[[2]]), "</font>", 
								  "<br/>", "Exclusions: ", 					"<font color=\"#00688B\">",nrow(outtemp2m[[3]]), " (", round(nrow(outtemp2m[[3]]) / n_comp, digits = 3) * 100, "%)", "</font>", 
								  "<br/>", coo, "<strong>"))
		}) 
		output$antestat_table1m <- DT::renderDataTable({
			DT::datatable(outtemp2m[[2]], options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
		})
		output$antestat_table2m <- DT::renderDataTable({
			DT::datatable(outtemp2m[[3]], options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
		})

		if(input$fileoutputant1m || input$fileoutputant2m) {
			#Zip handler       
			direc6 <- outtemp2m[[1]] #direc temp
			files <- list.files(direc6, recursive = TRUE)
			setwd(direc6)
			zip:::zip(zipfile = paste(direc6,'.zip',sep=''), files = files)

			setwd(sessiontemp)  #restores session
	
			#Download handler       
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
