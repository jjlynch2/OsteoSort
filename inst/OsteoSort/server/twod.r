	##########################testing for 2D interface##########################testing for 2D interface##########################testing for 2D interface##########################testing for 2D interface
	
	output$contents2d <- renderUI({
	   HTML(paste("Select the parameters and enter measurements to begin."))
	})

	getDataa <- reactive({
		if(is.null(input$file2)) return(NULL)
	})
	output$fileUploaded <- reactive ({

		return(!is.null(getDataa()))
	})
	outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)

	output$landmarks <- renderUI({      HTML(paste("Landmark Coordinates"))})                 
	output$scales <- renderUI({      HTML(paste("Scale Coordinates"))})                 

	filelist3 <- reactiveValues(list=data.frame())
	nfiles <- reactiveValues(files = 1)
	position <- reactiveValues(pos = 1)
	landmark <- reactiveValues(land = 1)
	landd <- reactiveValues(loser = 'test')
	row2d <- reactiveValues(row = 1)
	lockdown <- reactiveValues(lock = 0)
	lockdown2 <- reactiveValues(lock = 0)
	scale <- reactiveValues(scale = array(NA,c(2,2),dimnames=list(NULL,c('X','Y'))))
	array2D <- reactiveValues(twod = array(0,c(1,1,2)))
	array2Dside <- reactiveValues(twod = array('Left',c(1,1,2)))
	clear <- reactiveValues(clear = 1)

	observeEvent(input$file2, {
		filelist3$list = input$file2$datapath
		nfiles$files = length(input$file2$datapath)
		clear$clear = 0
	  			return(list(
	  				src = as.matrix(filelist3$list)[as.integer(position$pos)],
	  				contentType = "image/jpeg",
					alt = "jpg"
	  			))
	
		if(position$pos < nfiles$files) {
			position$pos = position$pos + 1
		}	
	})

	observeEvent(input$tpsfile, {
			tpstemp <- readtps(input$tpsfile$datapath)
			if(dim(tpstemp)[3] <= nfiles$files) {
			if(dim(tpstemp)[1] == row2d$row){
				lockdown$lock <- 1
				lockdown2$lock <- 0
				array2D$twod <- array(NA,c(row2d$row,2,nfiles$files), dimnames=list(NULL,c('X','Y'),NULL))
				array2Dside$twod <- array(NA,c(1,1,nfiles$files), dimnames=list('Left',c('Side'),NULL))
				for(i in 1:dim(tpstemp)[3]) {
					array2D$twod[,,i] = tpstemp[,,i]
				}
			}}
	})

	observeEvent(input$landmarks, {	 
		if(input$bonelandmarkconfig == 'humerus') {row2d$row = 10} 
	    	landmark$land = sapply(strsplit(input$landmarks, split = " "), "[", 2)
	    	if(input$landmarks == 'Scale') {lockdown2$lock <- 0}
	    	landd$loser = input$landmarks
	})

	output$variablelandmark <- renderUI({
		testme <- array(NA,c(input$variable,1))
		for (i in 1:input$variable) {testme[i,] <- paste('Landmark', i)}
		radioButtons(inputId = 'landmarks2', label = 'Landmarks', c(testme,'Scale'))
	})

	observeEvent(input$landmarks2, {	 
	    	landmark$land = sapply(strsplit(input$landmarks2, split = " "), "[", 2)
	    	if(input$landmarks2 == 'Scale') {lockdown2$lock <- 0}
	    	landd$loser = input$landmarks2
	    	print(landd$loser)
	})


	observeEvent(input$variable, {
		if(input$bonelandmarkconfig == 'variable') {
			row2d$row = input$variable
			lockdown$lock <- 0
		}
	})
	observeEvent(input$bonelandmarkconfigside, {
		if(!is.null(input$file2)) {
			array2Dside$twod[1,1,as.integer(position$pos)] = input$bonelandmarkconfigside
		}
	})

	 	 
	observeEvent(input$image_click, {
	if(clear$clear == 0) {
	  	if(!is.null(input$file2)) {
	  		if(landd$loser != 'Scale') {
	  			if(lockdown$lock == '0') {
	  				array2D$twod <- array(NA,c(row2d$row,2,nfiles$files), dimnames=list(NULL,c('X','Y'),NULL))
	  				array2Dside$twod <- array('Left',c(1,1,nfiles$files), dimnames=list(NULL,c('Side'),NULL))
	  				lockdown$lock <- 1
	  			}
	  			
	  			 if(as.integer(landmark$land) == 11) {
	 				landmark$land <- 1
	 			}
	  			
				scaletemp <- sqrt(abs(sum((scale$scale[1,1] - scale$scale[2,1]) + (scale$scale[1,2] - scale$scale[2,2]))))
				array2D$twod[as.integer(landmark$land),1,as.integer(position$pos)] = (scaletemp * input$image_click$x)
	 			array2D$twod[as.integer(landmark$land),2,as.integer(position$pos)] = (scaletemp * input$image_click$y)
	 		
	 			if(as.integer(landmark$land) <= as.integer(row2d$row)) {
	 				landmark$land <- as.integer(landmark$land) + 1
	 			}
	 		}
	 		if(landd$loser == 'Scale') {
	 			if(lockdown2$lock == 0) {
	 			scale$scale[1,1] = input$image_click$x
	 			scale$scale[1,2] = input$image_click$y
	 			lockdown2$lock <- 1
	 			}
	 			else if(lockdown2$lock == 1) {
	 			scale$scale[2,1] = input$image_click$x
	 			scale$scale[2,2] = input$image_click$y
	 			lockdown2$lock <- 0
	 			}
	 		}	
	 	}
	}
	})
	  

	output$clickinfo <- renderTable({
	  	if(!is.null(input$file2)) {
	  		if(lockdown$lock == '1') {
				array2D$twod[,,as.integer(position$pos)]
			}
		}					
	})
	  	
	output$scaleinfo <- renderTable({
	  	if(!is.null(input$file2)) {
				scale$scale
		}					
	})

	output$sideinfo <- renderTable({
	  	if(!is.null(input$file2)) {
				as.matrix(array2Dside$twod[1,1,as.integer(position$pos)])
		}					
	})

	output$savetps <- downloadHandler(
		filename <- function() {
			"sessionTPS.tps"
		},   
		content <- function(file) {
			writetps(array2D$twod, 'tpsfile')
			file.copy('tpsfile', file)                
		},
		contentType = "application/zip"
	)


	  



	observeEvent(input$nextimage, {
		if(position$pos < nfiles$files) {
			position$pos = position$pos + 1
	
			if(!is.null(input$file2)) {
	  			return(list(
	  				src = as.matrix(filelist3$list)[as.integer(position$pos)],
	  				contentType = "image/jpeg",
					alt = "jpg"
	  			))
			}
		}
	})
		

	observeEvent(input$previousimage, { 
	 	if(position$pos > 1) {
			position$pos = position$pos - 1
	
			if(!is.null(input$file2)) {
	  			return(list(
	  				src = as.matrix(filelist3$list)[as.integer(position$pos)],
	  				contentType = "image/jpeg",
					alt = "jpg"
	  			))
			}
		}
	})


	output$myimage <- renderImage(deleteFile = FALSE, { 

		if(!is.null(input$file2)) {
	  		return(list(
	  			src = as.matrix(filelist3$list)[as.integer(position$pos)],
	  			contentType = "image/jpeg",
				alt = "jpg",
				width = 1000
	  		))
		}
		else
			return(list(
				src = 'test.jpg',
				contentType = 'image/jpeg',
				alt = 'jpg',
				width = 1000
			))
	})


	 #file upload render for 2D comparison
	output$resettableInput2d <- renderUI({
		input$clearFile2
		fileInput('file2', 'Upload images', multiple = TRUE, accept=c('.jpg','.jpeg'))  
	})

	output$tpsupload <- renderUI({
		            input$clearFile2
		            fileInput('tpsfile', 'Upload existing TPS file', accept=c('.txt', '.tps','.TPS'))  
	})

	#clean session
	observeEvent(input$clearFile2, {
		nfiles$files = 1
		position$pos = 1 
		landmark$land = 1 
		row2d$row = 1 
		lockdown$lock = 0 
		lockdown2$lock = 0
		clear$clear = 1
		array2D$twod <- array(NA,c(1,1,2))
		scale$scale <- array(NA,c(2,2),dimnames=list(NULL,c('X','Y')))
		array2Dside <- reactiveValues(twod = array('Left',c(1,1,2)))
	
			return(list(
				src = 'test.jpg',
				contentType = 'image/jpeg',
				alt = 'jpg',
				width = 0
			))
	})