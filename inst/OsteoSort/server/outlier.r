    ####This is the outlier server side code made for local import into server.r
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
		HTML(paste("Select the parameters and upload the file to begin.</br></br>"))
	})	
	
	#clears session for multiple comparison
	observeEvent(input$clearFile3, {
		fileInput('file3', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))  
	})


	observeEvent(input$pro3, {
		showModal(modalDialog(title = "Calculation has started...Window will update when finished.", easyClose = FALSE, footer = NULL))
		
		withProgress(message = 'Calculation has started',
		            detail = 'This may take a while...', value = 0, {       
		            for (i in 1:25) {
		       incProgress(1/25)
		       Sys.sleep(0.10)
		     
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
		tempdata3 <- read.csv(inFile3$datapath, header=TRUE, sep=input$sep3, na.strings=c("", " ", "NA", "-","*"))## see na.strings forces NA for blanks, spaces, etc

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
		outtemp <- lengthsort(file = tempdata3, side = input$outlierside, bone = input$zz3, method = input$method, measurement = outliermeasurements, cutoff = cutoffvalue, sessiontempdir = sessiontemp)
		
		
		#counts number of outliers discovered
		outliercount <- 0
		if(!is.null(outtemp[[2]])) {outliercount <- nrow(outtemp[[2]])}
		if(!is.null(outtemp[[3]])) {outliercount <- outliercount + nrow(outtemp[[3]])}
		
		
		#display output
		output$outliercontent <- renderUI({
				HTML(paste("Statistical analysis complete.", '<br/>',"Number of outliers: ",outliercount,'<br/>',"Mean: ", outtemp[[6]], "<br/>", "Standard Deviation: ", outtemp[[7]], "<br/>", "Median: ", outtemp[[8]], "<br/>", "Interquartile: ", outtemp[[9]]))
		})   
		
		
			output$tablejustfuckingworkb <- DT::renderDataTable({
				DT::datatable(outtemp[[2]], options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
			})
			output$tablejustfuckingworka <- DT::renderDataTable({
				DT::datatable(outtemp[[3]], options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
			})
			output$tablejustfuckingworkc <- DT::renderDataTable({
				DT::datatable(outtemp[[4]], options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
			})
		
		output$plotoutlier <- renderPlot({outtemp[[5]]})
		removeModal() #removes modal
		
		#Zip handler       
		direc6 <- outtemp[[1]] #direc temp
		files <- list.files(direc6, recursive = TRUE)
		setwd(direc6)
		zip(zipfile = direc6, files = files)
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
	})