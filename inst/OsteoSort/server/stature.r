    ####This is the outlier server side code made for local import into server.r
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
	   HTML(paste("Select the parameters and upload the file to begin.</br></br>"))
	})	
	
	#clears session for multiple comparison
	observeEvent(input$clearFile4, {
		output$tablejustfuckingworka4 <- renderUI({HTML(paste(""))}) 
		output$tablejustfuckingworkb4 <- renderUI({HTML(paste(""))}) 
		output$tablejustfuckingworkc4 <- renderUI({HTML(paste(""))}) 
		output$plotoutlier4 <- renderUI({HTML(paste(""))}) 
		output$outliercontent4 <- renderUI({HTML(paste("Select the parameters and upload the file to begin</br></br>"))})
	})
	#clears tables when running second stats
	observeEvent(input$pro4, {
		output$tablejustfuckingworka4 <- renderUI({HTML(paste(""))}) 
		output$tablejustfuckingworkd4 <- renderUI({HTML(paste(""))}) 
		output$tablejustfuckingworkc4 <- renderUI({HTML(paste(""))}) 
		output$plotoutlier4 <- renderUI({HTML(paste(""))}) 
		output$outliercontent4 <- renderUI({HTML(paste("Select the parameters and upload the file to begin.</br></br>"))})
	})

	observeEvent(input$pro4, {
		showModal(modalDialog(title = "Calculation has started...Window will update when finished.", easyClose = FALSE, footer = NULL))
		
		withProgress(message = 'Calculation has started',
		            detail = 'This may take a while...', value = 0, {       
		            for (i in 1:25) {
		       incProgress(1/25)
		       Sys.sleep(0.10)
		     
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
		tempdata3 <- read.csv(inFile4$datapath, header=TRUE, sep=input$sep4, na.strings=c("", " ", "NA", "-","*"))## see na.strings forces NA for blanks, spaces, etc

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
		outtemp <- statsort(file = tempdata3, side = input$outlierside4, bone = input$zz4, method = input$method4, measurement = outliermeasurements, cutoff = cutoffvalue, sessiontempdir = sessiontemp, population = poppop)
		
		
		#counts number of outliers discovered
		outliercount <- 0
		if(!is.null(outtemp[[2]])) {outliercount <- nrow(outtemp[[2]])}
		if(!is.null(outtemp[[3]])) {outliercount <- outliercount + nrow(outtemp[[3]])}
		
		
		#display output
		output$outliercontent4 <- renderUI({
				HTML(paste("Statistical analysis complete.", '<br/>',"Number of outliers: ",outliercount,'<br/>',"Mean: ", outtemp[[6]], "<br/>", "Standard Deviation: ", outtemp[[7]], "<br/>", "Median: ", outtemp[[8]], "<br/>", "Interquartile: ", outtemp[[9]]))
		})   

		
			output$tablejustfuckingworkb4 <- DT::renderDataTable({
				DT::datatable(outtemp[[2]], options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
			})
			output$tablejustfuckingworka4 <- DT::renderDataTable({
				DT::datatable(outtemp[[3]], options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
			})
			output$tablejustfuckingworkc4 <- DT::renderDataTable({
				DT::datatable(outtemp[[4]], options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
			})
		
		output$plotoutlier4 <- renderPlot({outtemp[[5]]})
		removeModal() #removes modal
		
		#Zip handler       
		direc6 <- outtemp[[1]] #direc temp
		files <- list.files(direc6, recursive = TRUE)
		setwd(direc6)
		zip(zipfile = direc6, files = files)
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
	})