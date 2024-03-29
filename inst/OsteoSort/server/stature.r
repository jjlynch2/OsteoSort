output$outliercontent4 <- renderUI({
	HTML(paste(""))
})

fileoutputstature1 <- reactiveValues(fileoutputstature1 = TRUE)
output$fileoutputstature1 <- renderUI({
	checkboxInput(inputId = "fileoutputstature1", label = "Output csv file", value = TRUE)
})
observeEvent(input$fileoutputstature1, {
	fileoutputstature1$fileoutputstature1 <- input$fileoutputstature1
})

fileoutputstature2 <- reactiveValues(fileoutputstature2 = TRUE)
output$fileoutputstature2 <- renderUI({
	checkboxInput(inputId = "fileoutputstature2", label = "Output plot", value = TRUE)
})
observeEvent(input$fileoutputstature2, {
	fileoutputstature2$fileoutputstature2 <- input$fileoutputstature2
})

OSmethod1 <- reactiveValues(OSmethod1 = "Standard_deviation")
observeEvent(input$method4, {
	OSmethod1$OSmethod1 <- input$method4
})

OSsd1 <- reactiveValues(OSsd1 = c(2.0, 2))
observeEvent(input$standard_dev4, {
	OSsd1$OSsd1 <- input$standard_dev4
})

OSqt1 <- reactiveValues(OSqt1 = c(1.5, 1.5))
observeEvent(input$Quartiles4, {
	OSqt1$OSqt1 <- input$Quartiles4
})

datafile4 <- reactiveValues(datafile4 = TRUE)
observeEvent(input$file4, {
	tempdata3 <- read.csv(input$file4$datapath, header=TRUE, sep=",", na.strings=c("", " ", "NA", "-","*"), stringsAsFactors = FALSE)## see na.strings forces NA for blanks, spaces, etc
	tempdataaa <- tempdata3[,1:3]
	tempdataba <- lapply(tempdata3[,-(1:3)], function(x) { as.numeric(as.character(x))})
	tempdata3 <- c(tempdataaa, tempdataba)
	tempdata3 <- as.data.frame(tempdata3) #combines first four columns with now numeric measurements
	datafile4$datafile4 <- tempdata3
})


observeEvent(input$custom, {
	if(input$custom) {
		output$testtypem1 <- renderUI({
			selectInput('zzm1', 'Measurements', c(colnames(datafile4$datafile4)[-c(1:3)]))
		})
	} else {
		output$testtypem1 <- renderUI({
			t1 <- unique(config_df$config_df[config_df$config_df$Method == "Stature",1])
			t2 <- unique(c(colnames(datafile4$datafile4)[-c(1:3)]))
			mlist <- c()
			for(x in t2) {
				for(i in t1) {
					if(x == i) {mlist <- c(mlist, x)}
				}
			}
			selectInput('zzm1', 'Measurements', mlist)
		})
	}
})


stature_reference <- reactiveValues(stature_reference = c("temp"))
observeEvent(input$stature_reference, {
	stature_reference$stature_reference <- input$stature_reference
})
output$stature_reference <- renderUI({
	selectInput(inputId = "stature_reference", label = "Reference", choices = reference_name_list$reference_name_list)
})

stature_reference_imported <- reactiveValues(stature_reference_imported = data.frame())
observeEvent(input$stature_reference, {
	if(input$stature_reference != "Custom") {
		stature_reference_imported$stature_reference_imported <- reference_list$reference_list[[stature_reference$stature_reference]]
	}

	output$measurement_units_stature <- renderUI({
		if(any(units_df$units_df[,1] == input$stature_reference)) {
			measurement_units <- paste(" ", units_df$units_df[units_df$units_df$Reference == input$stature_reference,3], sep="")
			stature_units <- paste(" ", units_df$units_df[units_df$units_df$Reference == input$stature_reference,2], sep="")
			HTML(paste("<strong>","Measurement units:",measurement_units, "</strong><br/>",
				   "<strong>","Stature units:",stature_units, "</strong><br/>"
			))
		} else {
			HTML(paste(""))
		}
	})
})

#clears session for multiple comparison
output$resettableInput4 <- renderUI({
	input$clearFile4
	input$uploadFormat
	fileInput('file4', 'Upload postmortem measurements', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))  
})

observeEvent(input$clearFile4, {
	fileInput('file4', 'Upload postmortem measurements', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))  
})

observeEvent(input$pro4, {
	showModal(modalDialog(title = "Calculation has started...Window will update when finished.", easyClose = FALSE, footer = NULL))
	if(!any(unique(colnames(stature_reference_imported$stature_reference_imported)) == "Stature")) {removeModal();shinyalert(title = "ERROR!", text="Stature information is not available for this reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}
	withProgress(message = 'Calculation has started', detail = '', value = 0, min=0, max=2, {
		if (is.null(input$file4)){
			removeModal()
			shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss")
			return(NULL)
		}
		if (!file.size(input$file4$datapath) > 1){
			removeModal()
			shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss")
			return(NULL)
		}

		if(OSmethod1$OSmethod1 == "Standard_deviation") {
			cutoffvalue <- OSsd1$OSsd1
		} else {
			cutoffvalue <- OSqt1$OSqt1
		}
		if(input$custom) {
			if(is.na(input$slope) || is.na(input$intercept)) {removeModal();	shinyalert(title = "ERROR!", text="y = a + bx",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}
			reference <- c("Custom", input$slope, input$intercept)
		} else {
			reference <- stature_reference_imported$stature_reference_imported
		}
		incProgress(amount = 1, message = "Calculating outliers")
		outtemp <- statsort(sort = datafile4$datafile4, ref = reference, method = OSmethod1$OSmethod1, measurements = input$zzm1, cutoff = cutoffvalue, sessiontempdir = sessiontemp, output_options = c(fileoutputstature1$fileoutputstature1, fileoutputstature2$fileoutputstature2))

		#counts number of outliers discovered
		outliercount <- 0
		if(!is.null(outtemp[[2]])) {outliercount <- nrow(outtemp[[2]])}
		if(!is.null(outtemp[[3]])) {outliercount <- outliercount + nrow(outtemp[[3]])}
		
		#display output
		output$outliercontent4 <- renderUI({
				HTML(paste("<strong>Outliers: ",            "<font color=\"#00688B\">",outliercount, "</font>",
						'<br/>',"Mean: ",                 "<font color=\"#00688B\">",outtemp[[5]], "</font>",
						"<br/>", "Standard Deviation: ",  "<font color=\"#00688B\">",outtemp[[6]], "</font>",
						"<br/>", "Median: ",              "<font color=\"#00688B\">",outtemp[[7]], "</font>",
						"<br/>", "Interquartile: ",       "<font color=\"#00688B\">",outtemp[[8]], "</font>","</strong>"))
		})

		output$tjbingworkb4 <- DT::renderDataTable({
			DT::datatable(outtemp[[2]], selection = list(mode="multiple"), options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
		})
		output$tjbingworka4 <- DT::renderDataTable({
			DT::datatable(outtemp[[3]], selection = list(mode="multiple"), options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
		})
		output$tjbingworkc4 <- DT::renderDataTable({
			DT::datatable(outtemp[[4]], selection = list(mode="multiple"), options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
		})

		if(fileoutputstature2$fileoutputstature2) {
			sd <- paste(sessiontemp,outtemp[[1]],sep="/")
			nimages <- list.files(sd)
			nimages <- paste(sessiontemp, "/", outtemp[[1]], "/", nimages[grep(".jpg", nimages)], sep="")
			output$plotoutlier4 <- renderImage({
				list(src = nimages,
					contentType = 'image/jpg',
					height = 400,
					alt = "A"
				)
			}, deleteFile = FALSE)
		}
		removeModal() #removes modal
		if(fileoutputstature1$fileoutputstature1 || fileoutputstature2$fileoutputstature2) {
			output$outlierdownload4 <- downloadHandler(
				filename <- function() {
					paste("results.zip")
				},
				content <- function(file) {
					file.remove(paste(sd,"/",outtemp[[1]],'.zip',sep=''))
					if(is.numeric(input$tjbingworkb4_rows_selected)) {
						no_return_value <- OsteoSort:::output_function(outtemp[[2]][input$tjbingworkb4_rows_selected,], method="exclusion", type="csv3", uln = "l", fpath=sd)
					} else {file.remove(paste(sd,"/lower-selected-list.csv",sep=""))}
					if(is.numeric(input$tjbingworka4_rows_selected)) {
						no_return_value <- OsteoSort:::output_function(outtemp[[3]][input$tjbingworka4_rows_selected,], method="exclusion", type="csv3", uln = "u", fpath=sd)
					} else {file.remove(paste(sd,"/upper-selected-list.csv",sep=""))}
					if(is.numeric(input$tjbingworkc4_rows_selected)) {
						no_return_value <- OsteoSort:::output_function(outtemp[[4]][input$tjbingworkc4_rows_selected,], method="exclusion", type="csv3", uln = "n", fpath=sd)
					} else {file.remove(paste(sd,"/non-selected-list.csv",sep=""))}
					files <- list.files(sd, recursive = TRUE, full.names=TRUE)
					zip:::zipr(zipfile = paste(sd,"/",outtemp[[1]],'.zip',sep=''), files = files[1], compression = 1)
					for(file_na in files[-1]) {
						zip:::zipr_append(zipfile = paste(sd,"/",outtemp[[1]],'.zip',sep=''), files = file_na, compression = 1)
					}
					file.copy(paste(sd,"/",outtemp[[1]],'.zip',sep=''), file) 
				},
				contentType = "application/zip"
			)
		}
		incProgress(amount = 1, message = "Completed")
	})
})
