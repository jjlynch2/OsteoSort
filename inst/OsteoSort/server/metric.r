#upload GUI for resettable input
output$resettableInput3 <- renderUI({
	input$clearFile3
	input$uploadFormat
	fileInput('file3', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))  
})

output$outliercontent <- renderUI({
	HTML(paste(""))
})	

#clears session for multiple comparison
observeEvent(input$clearFile3, {
	fileInput('file3', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))  
})

fileoutputl1 <- reactiveValues(fileoutputl1 = TRUE)
output$fileoutputl1 <- renderUI({
	checkboxInput(inputId = "fileoutputl1", label = "Output csv file", value = TRUE)
})
observeEvent(input$fileoutputl1, {
	fileoutputl1$fileoutputl1 <- input$fileoutputl1
})

fileoutputl2 <- reactiveValues(fileoutputl2 = TRUE)
output$fileoutputl2 <- renderUI({
	checkboxInput(inputId = "fileoutputl2", label = "Output plot", value = TRUE)
})
observeEvent(input$fileoutputl2, {
	fileoutputl2$fileoutputl2 <- input$fileoutputl2
})

OSmethod <- reactiveValues(OSmethod = "Standard_deviation")
observeEvent(input$method, {
	OSmethod$OSmethod <- input$method
})

OSsd <- reactiveValues(OSsd = c(2.0, 2))
observeEvent(input$standard_dev, {
	OSsd$OSsd <- input$standard_dev
})

OSqt <- reactiveValues(OSqt = c(1.5, 1.5))
observeEvent(input$Quartiles, {
	OSqt$OSqt <- input$Quartiles
})

datafile3 <- reactiveValues(datafile3 = TRUE)
observeEvent(input$file3, {
	tempdata3 <- read.csv(input$file3$datapath, header=TRUE, sep=",", na.strings=c("", " ", "NA", "-","*"), stringsAsFactors = FALSE)## see na.strings forces NA for blanks, spaces, etc
	tempdataaa <- tempdata3[,1:3]
	tempdataba <- lapply(tempdata3[,-(1:3)], function(x) { as.numeric(as.character(x))})
	tempdata3 <- c(tempdataaa, tempdataba)
	tempdata3 <- as.data.frame(tempdata3) #combines first four columns with now numeric measurements
	output$testtypem <- renderUI({
		selectInput('zzm', 'Measurements', c(colnames(tempdata3)[-c(1:3)]))
	})
	datafile3$datafile3 <- tempdata3
})

observeEvent(input$pro3, {
	showModal(modalDialog(title = "Calculation has started...Window will update when finished.", easyClose = FALSE, footer = NULL))
	withProgress(message = 'Calculation has started', detail = '', value = 0, min=0, max=2, {
		if (is.null(input$file3)){
			removeModal()
			shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss")
			return(NULL) 
		}
		if (!file.size(input$file3$datapath) > 1){
			removeModal()
			shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss")
			return(NULL)
		}


		if(OSmethod$OSmethod == "Standard_deviation") {
			cutoffvalue <- OSsd$OSsd
		} else {
			cutoffvalue <- OSqt$OSqt
		}

		#calls sorting function
		incProgress(amount = 1, message = "Calculating outliers")
		outtemp <- metricsort(sort = datafile3$datafile3, method = OSmethod$OSmethod, measurements = input$zzm, cutoff = cutoffvalue, sessiontempdir = sessiontemp, output_options = c(fileoutputl1$fileoutputl1, fileoutputl2$fileoutputl2))
		
		#counts number of outliers discovered
		outliercount <- 0
		if(!is.null(outtemp[[2]])) {outliercount <- nrow(outtemp[[2]])}
		if(!is.null(outtemp[[3]])) {outliercount <- outliercount + nrow(outtemp[[3]])}
		
		#display output
		output$outliercontent <- renderUI({
				HTML(paste("<strong>Outliers: ",            "<font color=\"#00688B\">",outliercount, "</font>",
						'<br/>',"Mean: ",                 "<font color=\"#00688B\">",outtemp[[5]], "</font>",
						"<br/>", "Standard Deviation: ",  "<font color=\"#00688B\">",outtemp[[6]], "</font>",
						"<br/>", "Median: ",              "<font color=\"#00688B\">",outtemp[[7]], "</font>",
						"<br/>", "Interquartile: ",       "<font color=\"#00688B\">",outtemp[[8]], "</font>","</strong>"))
		})
		
		
		output$tjbingworkb <- DT::renderDataTable({
			DT::datatable(outtemp[[2]],selection = list(mode="multiple"), options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
		})
		output$tjbingworka <- DT::renderDataTable({
			DT::datatable(outtemp[[3]],selection = list(mode="multiple"), options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
		})
		output$tjbingworkc <- DT::renderDataTable({
			DT::datatable(outtemp[[4]],selection = list(mode="multiple"), options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
		})

		if(fileoutputl2$fileoutputl2) {
			nimages <- list.files(outtemp[[1]])
			nimages <- paste(sessiontemp, "/", outtemp[[1]], "/", nimages[grep(".jpg", nimages)], sep="")

			output$plotoutlier <- renderImage({
				list(src = nimages,
					contentType = 'image/jpg',
					height = 400,
					alt = "A"
				)
			}, deleteFile = FALSE)
		}
		removeModal() #removes modal
		if(fileoutputl1$fileoutputl1 || fileoutputl2$fileoutputl2) {
			#Download handler       
			output$outlierdownload <- downloadHandler(
				filename <- function() {
					paste("results.zip")
				},      
				content <- function(file) {
					setwd(outtemp[[1]])
					file.remove(paste(outtemp[[1]],'.zip',sep=''))
					if(is.numeric(input$tjbingworka_rows_selected)) {
						no_return_value <- OsteoSort:::output_function(outtemp[[3]][input$tjbingworka_rows_selected,], method="exclusion", type="csv3", uln = "u")
					} else {file.remove("upper-selected-list.csv")}
					if(is.numeric(input$tjbingworkc_rows_selected)) {
						no_return_value <- OsteoSort:::output_function(outtemp[[4]][input$tjbingworkc_rows_selected,], method="exclusion", type="csv3", uln = "n")
					} else {file.remove("non-selected-list.csv")}
					if(is.numeric(input$tjbingworkb_rows_selected)) {
						no_return_value <- OsteoSort:::output_function(outtemp[[2]][input$tjbingworkb_rows_selected,], method="exclusion", type="csv3", uln = "l")
					} else {file.remove("lower-selected-list.csv")}
					setwd(sessiontemp)
					files <- list.files(outtemp[[1]], recursive = TRUE)
					setwd(outtemp[[1]])
					zip:::zipr(zipfile = paste(outtemp[[1]],'.zip',sep=''), files = files[1], compression = 1)
					for(file_na in files[-1]) {
						zip:::zipr_append(zipfile = paste(outtemp[[1]],'.zip',sep=''), files = file_na, compression = 1)
					}
					file.copy(paste(outtemp[[1]],'.zip',sep=''), file) 
					setwd(sessiontemp)  
				},
				contentType = "application/zip"
			)
			setwd(sessiontemp) #restores session
		}
		incProgress(amount = 1, message = "Completed")
	})
})
