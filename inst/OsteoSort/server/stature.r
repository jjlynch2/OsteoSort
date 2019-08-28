output$resettableInput4 <- renderUI({
	input$clearFile4
	input$uploadFormat
	fileInput('file4', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))  
})

output$outliercontent4 <- renderUI({
	HTML(paste(""))
})

#clears session for multiple comparison
observeEvent(input$clearFile4, {
	fileInput('file4', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))  
})

fileoutputstature1 <- reactiveValues(fileoutputstature1 = TRUE)
output$fileoutputstature1 <- renderUI({
	checkboxInput(inputId = "fileoutputstature1", label = "Output csv file", value = TRUE)
})

fileoutputstature2 <- reactiveValues(fileoutputstature2 = TRUE)
output$fileoutputstature2 <- renderUI({
	checkboxInput(inputId = "fileoutputstature2", label = "Output plot", value = TRUE)
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
})

observeEvent(input$pro4, {
	showModal(modalDialog(title = "Calculation has started...Window will update when finished.", easyClose = FALSE, footer = NULL))
	withProgress(message = 'Calculation has started',
		detail = '', value = 0, {
		for (i in 1:10) {
			incProgress(1/10)
			Sys.sleep(0.05)
		}
	})
	
	if (is.null(input$file4)){
		removeModal()
		return(NULL) 
	}
	if (!file.size(input$file4$datapath) > 1){
		removeModal()
		return(NULL)
	}

	if(OSmethod1$OSmethod1 == "Standard_deviation") {
		cutoffvalue <- OSsd1$OSsd1
	} else {
		cutoffvalue <- OSqt1$OSqt1
	}

	if(input$custom) {
		reference <- c("Custom", input$slope, input$intercept)
	} else {
		reference <- stature_reference_imported$stature_reference_imported
	}

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
		DT::datatable(outtemp[[2]], options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
	})
	output$tjbingworka4 <- DT::renderDataTable({
		DT::datatable(outtemp[[3]], options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
	})
	output$tjbingworkc4 <- DT::renderDataTable({
		DT::datatable(outtemp[[4]], options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
	})

	if(fileoutputstature2$fileoutputstature2) {
		nimages <- list.files(outtemp[[1]])
		nimages <- paste(sessiontemp, "/", outtemp[[1]], "/", nimages[grep(".jpg", nimages)], sep="")

		output$plotoutlier4 <- renderImage({
			list(src = nimages,
				contentType = 'image/jpg',
				alt = "A"
			)
		}, deleteFile = FALSE)
	}
	removeModal() #removes modal
	if(fileoutputstature1$fileoutputstature1 || fileoutputstature2$fileoutputstature2) {
		#Zip handler       
		direc6 <- outtemp[[1]] #direc temp
		files <- list.files(direc6, recursive = TRUE)
		setwd(direc6)
		zip:::zip(zipfile = paste(direc6,'.zip',sep=''), files = files)
		setwd(sessiontemp)  #restores session
		#Download handler
		output$outlierdownload4 <- downloadHandler(
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
