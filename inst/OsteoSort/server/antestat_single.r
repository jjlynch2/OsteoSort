output$antestat_test <- renderUI({
	selectInput('antestat', 'Elements', c(Humerus='humerus', Ulna='ulna', Radius='radius', Femur='femur', Tibia='tibia', Fibula='fibula'),'humerus')
})

output$antestat_output <- renderUI({
   HTML(paste(""))
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

	if(input$antestat == "humerus") {pmm <- input$hu_antestat;bonemeasurementm <- "Hum_01"}
	if(input$antestat == "radius") {pmm <- input$ra_antestat;bonemeasurementm <- "Rad_01"}
	if(input$antestat == "ulna") {pmm <- input$ul_antestat;bonemeasurementm <- "Uln_01"}
	if(input$antestat == "femur") {pmm <- input$fe_antestat;bonemeasurementm <- "Fem_01"}
	if(input$antestat == "tibia") {pmm <- input$ti_antestat;bonemeasurementm <- "Tib_01"}
	if(input$antestat == "fibula") {pmm <- input$fi_antestat;bonemeasurementm <- "Fib_01"}

	if(is.numeric(input$antestat_input) && is.numeric(pmm)) {
		#calls sorting function
		antemortem_data_frame <- cbind.data.frame(input$Antemortem_ID, input$antestat_input, stringsAsFactors = FALSE)
		colnames(antemortem_data_frame) <- c("id","Stature")
		postmortem_data_frame <- cbind.data.frame(input$Postmortem_ID, input$ante_side, input$antestat, pmm, stringsAsFactors = FALSE)
		colnames(postmortem_data_frame) <- c("id","Side","Element",bonemeasurementm)
		if(input$alphatest1s == "Alpha") {temptest = TRUE}
		if(input$alphatest1s == "PI") {temptest = FALSE}
		outtemp1 <- antestat.input(bone = input$antestat, metric = input$metric_type, antemortem_stature = antemortem_data_frame, postmortem_measurement = postmortem_data_frame, population = input$antestat_population)
		outtemp2 <- antestat.regtest(sort = outtemp1[[1]], ref = outtemp1[[2]], prediction_interval = input$predlevelantestat, alphalevel = input$alphalevelsantestat, alphatest = temptest, output_options = c(input$fileoutputant1, input$fileoutputant2), sessiontempdir = sessiontemp)
		#display output
		output$antestat_output <- renderUI({
				HTML(paste(""))
		})   
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