output$testtype4 <- renderUI({
	selectInput('zz4', 'Elements', c(Humerus='humerus', Ulna='ulna', Radius='radius', Femur='femur', Tibia='tibia', Fibula='fibula'),'humerus')
})

#upload GUI for resettable input
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

observeEvent(input$pro4, {
	showModal(modalDialog(title = "Calculation has started...Window will update when finished.", easyClose = FALSE, footer = NULL))
	withProgress(message = 'Calculation has started',
	            detail = '', value = 0, {       
	            for (i in 1:10) {
	       incProgress(1/10)
	       Sys.sleep(0.05)
	     }
	})
	
	#Upload CSV file
	inFile4 <- input$file4
   
	 #return null if not uploaded
	if (is.null(inFile4)){
		removeModal()                             
		return(NULL) 
	}
	#return null if empty file
	if (!file.size(inFile4$datapath) > 1)
		{
		removeModal()                             
		return(NULL)
	}
	tempdata3 <- read.csv(inFile4$datapath, header=TRUE, sep=",", na.strings=c("", " ", "NA", "-","*"))## see na.strings forces NA for blanks, spaces, etc

	#checks if measurements are numeric and converts alpha characters to numeric   
	tempdataaa <- tempdata3[,1:4]
	tempdataba <- lapply(tempdata3[,-(1:4)], function(x) { as.numeric(as.character(x))})
   
	tempdata3 <- c(tempdataaa, tempdataba)
	tempdata3 <- as.data.frame(tempdata3) #combines first four columns with now numeric measurements
	
	#defines measurements and methods from user input
	if(input$zz4 == "humerus") {outliermeasurements <- input$humerusmeasurements4; poppop <- input$population4}
	if(input$zz4 == "radius") {outliermeasurements <- input$radiusmeasurements4; poppop <- input$population4}
	if(input$zz4 == "ulna") {outliermeasurements <- input$ulnameasurements4; poppop <- input$population4}
	if(input$zz4 == "femur") {outliermeasurements <- input$femurmeasurements4; poppop <- input$population5G}
	if(input$zz4 == "tibia") {outliermeasurements <- input$tibiameasurements4; poppop <- input$population5G}
	if(input$zz4 == "fibula") {outliermeasurements <- input$fibulameasurements4; poppop <- input$population4}
	if(input$method4 == "Standard_deviation") {cutoffvalue <- input$standard_dev4}
	if(input$method4 == "Quartiles") {cutoffvalue <- input$Quartiles4}
	
	#calls sorting function
	outtemp <- statsort(metric = input$metric_type2, sort = tempdata3, side = input$outlierside4, bone = input$zz4, method = input$method4, measurements = outliermeasurements, cutoff = cutoffvalue, sessiontempdir = sessiontemp, population = poppop, output_options = c(input$fileoutputstature1, input$fileoutputstature2))
	
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

	if(input$fileoutputstature2) {
		nimages <- list.files(outtemp[[1]])
		nimages <- paste(sessiontemp, "/", outtemp[[1]], "/", nimages[grep(".jpg", nimages)], sep="")

		output$plotoutlier4 <- renderImage({
			list(src = nimages,
				contentType = 'image/jpg',
				width = 400,
				height = 400,
				alt = "A"
			)
		}, deleteFile = FALSE)
	}
	removeModal() #removes modal
	if(input$fileoutputstature1 || input$fileoutputstature2) {
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