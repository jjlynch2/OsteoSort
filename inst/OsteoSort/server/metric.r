output$testtype3 <- renderUI({
	selectInput('zz3', 'Elements', c(Humerus='humerus', Ulna='ulna', Radius='radius', Femur='femur', Tibia='tibia', Fibula='fibula', Scapula='scapula', Os_coxa='os_coxa', Clavicle='clavicle'),'humerus')
})

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

observeEvent(input$pro3, {
	showModal(modalDialog(title = "Calculation has started...Window will update when finished.", easyClose = FALSE, footer = NULL))
	withProgress(message = 'Calculation has started',
	            detail = '', value = 0, {       
	            for (i in 1:10) {
	       incProgress(1/10)
	       Sys.sleep(0.05)
	     }
	})
	
	#Upload CSV file
	inFile3 <- input$file3
   
	 #return null if not uploaded
	if (is.null(inFile3)){
		removeModal()                             
		return(NULL) 
	}
	#return null if empty file
	if (!file.size(inFile3$datapath) > 1)
		{
		removeModal()                             
		return(NULL)
	}
	tempdata3 <- read.csv(inFile3$datapath, header=TRUE, sep=",", na.strings=c("", " ", "NA", "-","*"))## see na.strings forces NA for blanks, spaces, etc

	#checks if measurements are numeric and converts alpha characters to numeric   
	tempdataaa <- tempdata3[,1:4]
	tempdataba <- lapply(tempdata3[,-(1:4)], function(x) { as.numeric(as.character(x))})
   
	tempdata3 <- c(tempdataaa, tempdataba)
	tempdata3 <- as.data.frame(tempdata3) #combines first four columns with now numeric measurements
	
	#defines measurements and methods from user input
	if(input$zz3 == "scapula") {outliermeasurements <- input$scapulameasurements}
	if(input$zz3 == "clavicle") {outliermeasurements <- input$claviclemeasurements}
	if(input$zz3 == "humerus") {outliermeasurements <- input$humerusmeasurements}
	if(input$zz3 == "radius") {outliermeasurements <- input$radiusmeasurements}
	if(input$zz3 == "ulna") {outliermeasurements <- input$ulnameasurements}
	if(input$zz3 == "os_coxa") {outliermeasurements <- input$os_coxameasurements}
	if(input$zz3 == "femur") {outliermeasurements <- input$femurmeasurements}
	if(input$zz3 == "tibia") {outliermeasurements <- input$tibiameasurements}
	if(input$zz3 == "fibula") {outliermeasurements <- input$fibulameasurements}
	if(input$method == "Standard_deviation") {cutoffvalue <- input$standard_dev}
	if(input$method == "Quartiles") {cutoffvalue <- input$Quartiles}
	
	#calls sorting function
	outtemp <- metricsort(sort = tempdata3, side = input$outlierside, bone = input$zz3, method = input$method, measurements = outliermeasurements, cutoff = cutoffvalue, sessiontempdir = sessiontemp, output_options = c(input$fileoutputl1, input$fileoutputl2))
	
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

	if(input$fileoutputl2) {
		nimages <- list.files(outtemp[[1]])
		nimages <- paste(sessiontemp, "/", outtemp[[1]], "/", nimages[grep(".jpg", nimages)], sep="")

		output$plotoutlier <- renderImage({
			list(src = nimages,
				contentType = 'image/jpg',
				width = 400,
				height = 400,
				alt = "A"
			)
		}, deleteFile = FALSE)
	}
	removeModal() #removes modal
	if(input$fileoutputl1 || input$fileoutputl2) {
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