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
	withProgress(message = 'Calculation has started',
	            detail = '', value = 0, {       
	            for (i in 1:10) {
	       incProgress(1/10)
	       Sys.sleep(0.05)
	     }
	})
	
	if (is.null(input$file3)){
		removeModal()
		return(NULL) 
	}
	if (!file.size(input$file3$datapath) > 1){
		removeModal()
		return(NULL)
	}


	if(OSmethod$OSmethod == "Standard_deviation") {
		cutoffvalue <- OSsd$OSsd
	} else {
		cutoffvalue <- OSqt$OSqt
	}

	#calls sorting function
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
		DT::datatable(outtemp[[2]], options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
	})
	output$tjbingworka <- DT::renderDataTable({
		DT::datatable(outtemp[[3]], options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
	})
	output$tjbingworkc <- DT::renderDataTable({
		DT::datatable(outtemp[[4]], options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
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
		#Zip handler
		direc6 <- outtemp[[1]] #direc temp
		files <- list.files(direc6, recursive = TRUE)
		setwd(direc6)
		zip:::zip(zipfile = paste(direc6,'.zip',sep=''), files = files)
		setwd(sessiontemp)  #restores session
		
		#Download handler       
		output$outlierdownload <- downloadHandler(
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
		setwd(sessiontemp) #restores session
	}
})
