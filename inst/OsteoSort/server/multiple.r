	################multiple comparison  ################multiple comparison  ################multiple comparison  ################multiple comparison  ################multiple comparison  
		
	
	output$contents <- renderUI({
	   HTML(paste("Select the parameters and upload the file to begin.</br></br>"))
	})	

	
	#file upload render for multiple comparison
	output$resettableInput <- renderUI({
		            input$clearFile1
		            input$uploadFormat
		            fileInput('file1', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))  
	})

	#clears session for multiple comparison
	observeEvent(input$clearFile1, {
		   output$table <- renderUI({      HTML(paste(""))}) 
		   output$tablen <- renderUI({      HTML(paste(""))}) 
		   output$contents <- renderUI({HTML(paste("Select the parameters and upload the file to begin."))})
	})

	######standard measurement combinator
	measurementsglobal <- reactiveValues(clist = c(),blist = c())

	observeEvent(input$MeasurementsUsed1, {
		measurementsglobal$clist <- list(input$MeasurementsUsed1, input$MeasurementsUsed2,input$MeasurementsUsed3, input$MeasurementsUsed4,input$MeasurementsUsed5, input$MeasurementsUsed6, input$MeasurementsUsed7, input$MeasurementsUsed8,input$MeasurementsUsed9)
	})
	observeEvent(input$MeasurementsUsed2, {
		measurementsglobal$clist <- list(input$MeasurementsUsed1, input$MeasurementsUsed2,input$MeasurementsUsed3, input$MeasurementsUsed4,input$MeasurementsUsed5, input$MeasurementsUsed6, input$MeasurementsUsed7, input$MeasurementsUsed8,input$MeasurementsUsed9)
	})
	observeEvent(input$MeasurementsUsed3, {
		measurementsglobal$clist <- list(input$MeasurementsUsed1, input$MeasurementsUsed2,input$MeasurementsUsed3, input$MeasurementsUsed4,input$MeasurementsUsed5, input$MeasurementsUsed6, input$MeasurementsUsed7, input$MeasurementsUsed8,input$MeasurementsUsed9)
	})
	observeEvent(input$MeasurementsUsed4, {
		measurementsglobal$clist <- list(input$MeasurementsUsed1, input$MeasurementsUsed2,input$MeasurementsUsed3, input$MeasurementsUsed4,input$MeasurementsUsed5, input$MeasurementsUsed6, input$MeasurementsUsed7, input$MeasurementsUsed8,input$MeasurementsUsed9)
	})
	observeEvent(input$MeasurementsUsed5, {
		measurementsglobal$clist <- list(input$MeasurementsUsed1, input$MeasurementsUsed2,input$MeasurementsUsed3, input$MeasurementsUsed4,input$MeasurementsUsed5, input$MeasurementsUsed6, input$MeasurementsUsed7, input$MeasurementsUsed8,input$MeasurementsUsed9)
	})
	observeEvent(input$MeasurementsUsed6, {
		measurementsglobal$clist <- list(input$MeasurementsUsed1, input$MeasurementsUsed2,input$MeasurementsUsed3, input$MeasurementsUsed4,input$MeasurementsUsed5, input$MeasurementsUsed6, input$MeasurementsUsed7, input$MeasurementsUsed8,input$MeasurementsUsed9)
	})
	observeEvent(input$MeasurementsUsed7, {
		measurementsglobal$clist <- list(input$MeasurementsUsed1, input$MeasurementsUsed2,input$MeasurementsUsed3, input$MeasurementsUsed4,input$MeasurementsUsed5, input$MeasurementsUsed6, input$MeasurementsUsed7, input$MeasurementsUsed8,input$MeasurementsUsed9)
	})
	observeEvent(input$MeasurementsUsed8, {
		measurementsglobal$clist <- list(input$MeasurementsUsed1, input$MeasurementsUsed2,input$MeasurementsUsed3, input$MeasurementsUsed4,input$MeasurementsUsed5, input$MeasurementsUsed6, input$MeasurementsUsed7, input$MeasurementsUsed8,input$MeasurementsUsed9)
	})
	observeEvent(input$MeasurementsUsed9, {
		measurementsglobal$clist <- list(input$MeasurementsUsed1, input$MeasurementsUsed2,input$MeasurementsUsed3, input$MeasurementsUsed4,input$MeasurementsUsed5, input$MeasurementsUsed6, input$MeasurementsUsed7, input$MeasurementsUsed8,input$MeasurementsUsed9)
	})
	output$measurements1 <- renderUI({  
		sliderInput(inputId = "clavicle", label = "Clavicle", min=1, max=length(input$MeasurementsUsed1), value=1, step = 1)
	})  
	output$measurements2 <- renderUI({      
		sliderInput(inputId = "scapula", label = "Scapula", min=1, max=length(input$MeasurementsUsed2), value=1, step = 1)
	})
	output$measurements3 <- renderUI({      
		sliderInput(inputId = "humerus", label = "Humerus", min=1, max=length(input$MeasurementsUsed3), value=1, step = 1)
	})
	output$measurements4 <- renderUI({      
		sliderInput(inputId = "ulna", label = "Ulna", min=1, max=length(input$MeasurementsUsed4), value=1, step = 1)
	})
	output$measurements5 <- renderUI({      
		sliderInput(inputId = "radius", label = "Radius", min=1, max=length(input$MeasurementsUsed5), value=1, step = 1)
	})
	output$measurements6 <- renderUI({      
		sliderInput(inputId = "os_coxa", label = "Os_coxa", min=1, max=length(input$MeasurementsUsed6), value=1, step = 1)
	})
	output$measurements7 <- renderUI({      
		sliderInput(inputId = "femur", label = "Femur", min=1, max=length(input$MeasurementsUsed7), value=1, step = 1)
	})
	output$measurements8 <- renderUI({      
		sliderInput(inputId = "tibia", label = "Tibia", min=1, max=length(input$MeasurementsUsed8), value=1, step = 1)
	})
	output$measurements9 <- renderUI({      
		sliderInput(inputId = "fibula", label = "Fibula", min=1, max=length(input$MeasurementsUsed9), value=1, step = 1)
	})  
	######standard measurement combinator

	######supplemental measurement combinator
	observeEvent(input$MeasurementsUseda, {
		measurementsglobal$blist <- c(input$MeasurementsUseda, input$MeasurementsUsedb,input$MeasurementsUsedc, input$MeasurementsUsedd,input$MeasurementsUsede, input$MeasurementsUsedf, input$MeasurementsUsedg, input$MeasurementsUsedh,input$MeasurementsUsedi)
	})
	observeEvent(input$MeasurementsUsedb, {
		measurementsglobal$blist <- list(input$MeasurementsUseda, input$MeasurementsUsedb,input$MeasurementsUsedc, input$MeasurementsUsedd,input$MeasurementsUsede, input$MeasurementsUsedf, input$MeasurementsUsedg, input$MeasurementsUsedh,input$MeasurementsUsedi)
	})
	observeEvent(input$MeasurementsUsedc, {
		measurementsglobal$blist <- list(input$MeasurementsUseda, input$MeasurementsUsedb,input$MeasurementsUsedc, input$MeasurementsUsedd,input$MeasurementsUsede, input$MeasurementsUsedf, input$MeasurementsUsedg, input$MeasurementsUsedh,input$MeasurementsUsedi)
	})
	observeEvent(input$MeasurementsUsedd, {
		measurementsglobal$blist <- list(input$MeasurementsUseda, input$MeasurementsUsedb,input$MeasurementsUsedc, input$MeasurementsUsedd,input$MeasurementsUsede, input$MeasurementsUsedf, input$MeasurementsUsedg, input$MeasurementsUsedh,input$MeasurementsUsedi)
	})
	observeEvent(input$MeasurementsUsede, {
		measurementsglobal$blist <- list(input$MeasurementsUseda, input$MeasurementsUsedb,input$MeasurementsUsedc, input$MeasurementsUsedd,input$MeasurementsUsede, input$MeasurementsUsedf, input$MeasurementsUsedg, input$MeasurementsUsedh,input$MeasurementsUsedi)
	})
	observeEvent(input$MeasurementsUsedf, {
		measurementsglobal$blist <- list(input$MeasurementsUseda, input$MeasurementsUsedb,input$MeasurementsUsedc, input$MeasurementsUsedd,input$MeasurementsUsede, input$MeasurementsUsedf, input$MeasurementsUsedg, input$MeasurementsUsedh,input$MeasurementsUsedi)
	})
	observeEvent(input$MeasurementsUsedg, {
		measurementsglobal$blist <- list(input$MeasurementsUseda, input$MeasurementsUsedb,input$MeasurementsUsedc, input$MeasurementsUsedd,input$MeasurementsUsede, input$MeasurementsUsedf, input$MeasurementsUsedg, input$MeasurementsUsedh,input$MeasurementsUsedi)
	})
	observeEvent(input$MeasurementsUsedh, {
		measurementsglobal$blist <- list(input$MeasurementsUseda, input$MeasurementsUsedb,input$MeasurementsUsedc, input$MeasurementsUsedd,input$MeasurementsUsede, input$MeasurementsUsedf, input$MeasurementsUsedg, input$MeasurementsUsedh,input$MeasurementsUsedi)
	})
	observeEvent(input$MeasurementsUsedi, {
		measurementsglobal$blist <- list(input$MeasurementsUseda, input$MeasurementsUsedb,input$MeasurementsUsedc, input$MeasurementsUsedd,input$MeasurementsUsede, input$MeasurementsUsedf, input$MeasurementsUsedg, input$MeasurementsUsedh,input$MeasurementsUsedi)
	})
	output$measurementsa <- renderUI({  
		sliderInput(inputId = "claviclea", label = "Clavicle", min=1, max=length(input$MeasurementsUseda), value=1, step = 1)
	})  
	output$measurementsb <- renderUI({      
		sliderInput(inputId = "scapulab", label = "Scapula", min=1, max=length(input$MeasurementsUsedb), value=1, step = 1)
	})
	output$measurementsc <- renderUI({      
		sliderInput(inputId = "humerusc", label = "Humerus", min=1, max=length(input$MeasurementsUsedc), value=1, step = 1)
	})
	output$measurementsd <- renderUI({      
		sliderInput(inputId = "ulnad", label = "Ulna", min=1, max=length(input$MeasurementsUsedd), value=1, step = 1)
	})
	output$measurementse <- renderUI({      
		sliderInput(inputId = "radiuse", label = "Radius", min=1, max=length(input$MeasurementsUsede), value=1, step = 1)
	})
	output$measurementsf <- renderUI({      
		sliderInput(inputId = "os_coxaf", label = "Os_coxa", min=1, max=length(input$MeasurementsUsedf), value=1, step = 1)
	})
	output$measurementsg <- renderUI({      
		sliderInput(inputId = "femurg", label = "Femur", min=1, max=length(input$MeasurementsUsedg), value=1, step = 1)
	})
	output$measurementsh <- renderUI({      
		sliderInput(inputId = "tibiah", label = "Tibia", min=1, max=length(input$MeasurementsUsedh), value=1, step = 1)
	})
	output$measurementsi <- renderUI({      
		sliderInput(inputId = "fibulai", label = "Fibula", min=1, max=length(input$MeasurementsUsedi), value=1, step = 1)
	}) 
	######supplemental measurement combinator

	observeEvent(input$pro, {
	
			showModal(modalDialog(title = "Calculation has started...Window will update when finished.", easyClose = FALSE, footer = NULL))
	
	
		   withProgress(message = 'Calculation has started',
		            detail = 'This may take a while...', value = 0, {       
		            for (i in 1:25) {
		       incProgress(1/25)
		       Sys.sleep(0.10)
		     
		     }
		})
	
		#Upload CSV file
		inFile <- input$file1
	   

		 #return null if not uploaded
		if (is.null(inFile)){
			removeModal()                             
			return(NULL) 
		}
		#return null if empty file
		if (!file.size(inFile$datapath) > 1)
			{
			removeModal()                             
			return(NULL)
		}
		tempdata1 <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, na.strings=c("", " ", "NA"))## see na.strings forces NA for blanks, spaces, etc

		#checks if measurements are numeric and converts alpha characters to numeric   
		tempdataa <- tempdata1[,1:3]
		tempdatab <- lapply(tempdata1[,-(1:3)], function(x) { as.numeric(as.character(x))})
	   
		tempdata1 <- c(tempdataa, tempdatab)
		tempdata1 <- as.data.frame(tempdata1) #combines first four columns with now numeric measurements

	if(input$testtype2 != 'Regression_match') {
		#logic for calling input functions
		if(input$standard == 'Standard' & input$bone != 'altt' & input$bone != 'alttp' & input$bone != 'altta' & input$bone != 'hu' & input$bone != 'hr' & input$bone != 'hs' & input$bone != 'hss' & input$bone != 'fi' & input$bone != 'ft' & input$bone != 'ftt'){
			if(input$bone == 'clavicle') {threshold <- input$clavicle; measurements <- input$MeasurementsUsed1} 
			if(input$bone == 'scapula') {threshold <- input$scapula; measurements <- input$MeasurementsUsed2}   
			if(input$bone == 'humerus') {threshold <- input$humerus; measurements <- input$MeasurementsUsed3} 
			if(input$bone == 'ulna') {threshold <- input$ulna; measurements <- input$MeasurementsUsed4} 
			if(input$bone == 'radius') {threshold <- input$radius; measurements <- input$MeasurementsUsed5} 
			if(input$bone == 'os_coxa') {threshold <- input$os_coxa; measurements <- input$MeasurementsUsed6} 
			if(input$bone == 'femur') {threshold <- input$femur; measurements <- input$MeasurementsUsed7} 
			if(input$bone == 'tibia') {threshold <- input$tibia; measurements <- input$MeasurementsUsed8} 
			if(input$bone == 'fibula') {threshold <- input$fibula; measurements <- input$MeasurementsUsed9}        

			wtf <- pm.input(bone=toString(input$bone), sort=tempdata1, template='standard',tresh=threshold, measurements=measurements)
			direc2 <- pm.ttest(refdata = wtf[[2]], sortdata = wtf[[1]], stdout = FALSE, sessiontemp=sessiontemp, alphalevel = input$alphalevel, absolutevalue = input$absolutevalue, testagainst = input$testagainst, oo = c(input$fileoutput1, input$fileoutput2))
			ll <- nrow(direc2[[2]]) + nrow(direc2[[3]])       
		     	          
		}	
		if(input$standard == 'Supplemental' & input$bone != 'altt' & input$bone != 'alttp' & input$bone != 'altta' & input$bone != 'hu' & input$bone != 'hr' & input$bone != 'hs' & input$bone != 'hss' & input$bone != 'fi' & input$bone != 'ft' & input$bone != 'ftt'){
			if(input$bone == 'clavicle') {threshold <- input$claviclea; measurements <- input$MeasurementsUseda} 
			if(input$bone == 'scapula') {threshold <- input$scapulab; measurements <- input$MeasurementsUsedb}   
			if(input$bone == 'humerus') {threshold <- input$humerusc; measurements <- input$MeasurementsUsedc} 
			if(input$bone == 'ulna') {threshold <- input$ulnad; measurements <- input$MeasurementsUsedd} 
			if(input$bone == 'radius') {threshold <- input$radiuse; measurements <- input$MeasurementsUsede} 
			if(input$bone == 'os_coxa') {threshold <- input$os_coxaf; measurements <- input$MeasurementsUsedf} 
			if(input$bone == 'femur') {threshold <- input$femurg; measurements <- input$MeasurementsUsedg} 
			if(input$bone == 'tibia') {threshold <- input$tibiah; measurements <- input$MeasurementsUsedh} 
			if(input$bone == 'fibula') {threshold <- input$fibulai; measurements <- input$MeasurementsUsedi}                 

			wtf <- pm.input(bone=toString(input$bone), sort=tempdata1, template='supplemental',tresh=threshold, measurements=measurements)                                	      
			direc2 <- pm.ttest(refdata = wtf[[2]], sortdata = wtf[[1]], stdout = FALSE, sessiontemp=sessiontemp, alphalevel = input$alphalevel, absolutevalue = input$absolutevalue, testagainst = input$testagainst, oo = c(input$fileoutput1, input$fileoutput2))
			ll <- nrow(direc2[[2]]) + nrow(direc2[[3]])           
		}
		if(input$bone == 'altt' || input$bone == 'alttp' || input$bone == 'altta') {

			if(input$standard == "Standard") {
				treshlist <- c(input$clavicle, input$scapula, input$humerus, input$ulna, input$radius, input$os_coxa, input$femur, input$tibia, input$fibula)
				mlist <- measurementsglobal$clist
			}
			if(input$standard == "Supplemental") {
				treshlist <- c(input$claviclea, input$scapulab, input$humerusc, input$ulnad, input$radiuse, input$os_coxaf, input$femurg, input$tibiah, input$fibulai)
				mlist <- measurementsglobal$blist
			}
			if(input$bone == 'altt') {test <- "all"}
			if(input$bone == 'alttp'){test <- "all_pm"}
			if(input$bone == 'altta'){test <- "all_art"}
					       		
			direc2 <- all.ttest(sort = tempdata1, test = test, tresh = treshlist, measurements = mlist, template = input$standard, sessiontemp = sessiontemp, alphalevel = input$alphalevel, absolutevalue = input$absolutevalue, stdout = FALSE, testagainst = input$testagainst, oo = c(input$fileoutput1, input$fileoutput2))
			direc <- direc2[[1]]
			nmatch <- direc2[[3]]
			ll <- direc2[[4]]
			direc2 <- direc2[[2]]
		}                  
		if(input$bone == 'hu' | input$bone == 'hr' | input$bone == 'hs' | input$bone == "hss" | input$bone == 'fi' | input$bone == 'ft' | input$bone == 'ftt') {
			wtf <- art.input(bone=toString(input$bone), sort=tempdata1)
			direc2 <- art.ttest(refdata = wtf[[2]], sortdata = wtf[[1]], stdout = FALSE, sessiontemp=sessiontemp, alphalevel = input$alphalevel, absolutevalue = input$absolutevalue, testagainst = input$testagainst, oo = c(input$fileoutput1, input$fileoutput2))   
			ll <- nrow(wtf[[1]])                       
		}      
	}
		if(input$testtype2 == 'Regression_match') {
			if(input$standard == "Standard") {
				if(input$assbone1 == 'Clavicle') {threshold <- input$clavicle; measurements <- input$MeasurementsUsed1} 
				if(input$assbone1 == 'Scapula') {threshold <- input$scapula; measurements <- input$MeasurementsUsed2}   
				if(input$assbone1 == 'Humerus') {threshold <- input$humerus; measurements <- input$MeasurementsUsed3} 
				if(input$assbone1 == 'Ulna') {threshold <- input$ulna; measurements <- input$MeasurementsUsed4} 
				if(input$assbone1 == 'Radius') {threshold <- input$radius; measurements <- input$MeasurementsUsed5} 
				if(input$assbone1 == 'Os_coxa') {threshold <- input$os_coxa; measurements <- input$MeasurementsUsed6} 
				if(input$assbone1 == 'Femur') {threshold <- input$femur; measurements <- input$MeasurementsUsed7} 
				if(input$assbone1 == 'Tibia') {threshold <- input$tibia; measurements <- input$MeasurementsUsed8} 
				if(input$assbone1 == 'Fibula') {threshold <- input$fibula; measurements <- input$MeasurementsUsed9}  
				
				
				if(input$assbone2 == 'Clavicle') {threshold <- input$clavicle; measurements2 <- input$MeasurementsUsed1} 
				if(input$assbone2 == 'Scapula') {threshold <- input$scapula; measurements2 <- input$MeasurementsUsed2}   
				if(input$assbone2 == 'Humerus') {threshold <- input$humerus; measurements2 <- input$MeasurementsUsed3} 
				if(input$assbone2 == 'Ulna') {threshold <- input$ulna; measurements2 <- input$MeasurementsUsed4} 
				if(input$assbone2 == 'Radius') {threshold <- input$radius; measurements2 <- input$MeasurementsUsed5} 
				if(input$assbone2 == 'Os_coxa') {threshold <- input$os_coxa; measurements2 <- input$MeasurementsUsed6} 
				if(input$assbone2 == 'Femur') {threshold <- input$femur; measurements2 <- input$MeasurementsUsed7} 
				if(input$assbone2 == 'Tibia') {threshold <- input$tibia; measurements2 <- input$MeasurementsUsed8} 
				if(input$assbone2 == 'Fibula') {threshold <- input$fibula; measurements2 <- input$MeasurementsUsed9}        

			}
			if(input$standard == "Supplemental") {
				if(input$assbone1 == 'Clavicle') {threshold <- input$claviclea; measurements <- input$MeasurementsUseda} 
				if(input$assbone1 == 'Scapula') {threshold <- input$scapulab; measurements <- input$MeasurementsUsedb}   
				if(input$assbone1 == 'Humerus') {threshold <- input$humerusc; measurements <- input$MeasurementsUsedc} 
				if(input$assbone1 == 'Ulna') {threshold <- input$ulnad; measurements <- input$MeasurementsUsedd} 
				if(input$assbone1 == 'Radius') {threshold <- input$radiuse; measurements <- input$MeasurementsUsede} 
				if(input$assbone1 == 'Os_coxa') {threshold <- input$os_coxaf; measurements <- input$MeasurementsUsedf} 
				if(input$assbone1 == 'Femur') {threshold <- input$femurg; measurements <- input$MeasurementsUsedg} 
				if(input$assbone1 == 'Tibia') {threshold <- input$tibiah; measurements <- input$MeasurementsUsedh} 
				if(input$assbone1 == 'Fibula') {threshold <- input$fibulai; measurements <- input$MeasurementsUsedi}    
				
				if(input$assbone2 == 'Clavicle') {threshold <- input$claviclea; measurements2 <- input$MeasurementsUseda} 
				if(input$assbone2 == 'Scapula') {threshold <- input$scapulab; measurements2 <- input$MeasurementsUsedb}   
				if(input$assbone2 == 'Humerus') {threshold <- input$humerusc; measurements2 <- input$MeasurementsUsedc} 
				if(input$assbone2 == 'Ulna') {threshold <- input$ulnad; measurements2 <- input$MeasurementsUsedd} 
				if(input$assbone2 == 'Radius') {threshold <- input$radiuse; measurements2 <- input$MeasurementsUsede} 
				if(input$assbone2 == 'Os_coxa') {threshold <- input$os_coxaf; measurements2 <- input$MeasurementsUsedf} 
				if(input$assbone2 == 'Femur') {threshold <- input$femurg; measurements2 <- input$MeasurementsUsedg} 
				if(input$assbone2 == 'Tibia') {threshold <- input$tibiah; measurements2 <- input$MeasurementsUsedh} 
				if(input$assbone2 == 'Fibula') {threshold <- input$fibulai; measurements2 <- input$MeasurementsUsedi}              

			}
		
		
		
		
			wtf <- reg.input(sort = tempdata1, bone1 = input$assbone1, side1 = input$assside1, bone2 = input$assbone2, side2 = input$assside2, template = input$standard, measurements1 = measurements, measurements2 = measurements2)
			direc2 <- reg.multitest(sort = wtf[[1]], ref = wtf[[2]], splitn = wtf[[3]], predlevel = input$asspredlevel, stdout = FALSE, oo = c(input$fileoutput1, input$fileoutput2) )
			ll <- nrow(direc2[[2]]) + nrow(direc2[[3]])
		}
		#changes results to 0 if no possible combinations
		if(all(is.na(direc2[[2]])) && all(is.na(direc2[[3]]))) {ll <- 0; nmatch <- 0; lent <- 0}
		
		#if combinations exist, produces output
		if(!all(is.na(direc2[[2]])) || !all(is.na(direc2[[3]])) && any(oo=c(input$fileoutput1, input$fileoutput2))) {

			if (input$bone != 'altt' && input$bone != 'alttp' && input$bone != 'altta'){
				   direc <- direc2[[1]]
				   nmatch <- nrow(direc2[[2]])
			}

			files <- list.files(direc, recursive = TRUE)
			setwd(direc)
			nooutput <- lapply(files, function(x) {
				zip(zipfile = direc, files = x)
			})
			setwd(sessiontemp)

			lent <- length(unique(rbind(as.matrix(direc2[[2]][1]),as.matrix(direc2[[2]][4])))) #fix for number of specimens matched

		}
		
		temp1 <- direc2[[2]][1]
		temp2 <- direc2[[2]][4]
		temp3 <- direc2[[3]][1]
		temp4 <- direc2[[3]][4]

		co <- ""
		if(input$research) {
			#used to assess accuracy of methodology
			global1 <- as.matrix(direc2[[2]][1])
			global2 <- as.matrix(direc2[[2]][4])
			co <- 0
			 for(i in 1:nrow(global1)) {
			 	if(as.character(global1[i]) == as.character(global2[i])) {co <- co + 1}
			 }		
			#used to assess accuracy of methodology
			 co <- paste("Percent correct: ", co, " (", round(co/nrow(unique(rbind(temp1,temp2,temp3,temp4))),digits = 2) * 100, "%)", "<br/>", "Percent incorrect: ", nrow(unique(rbind(temp1,temp2,temp3,temp4))) - co, " (", round(1 - co/nrow(unique(rbind(temp1,temp2,temp3,temp4))),digits = 2) * 100, "%)",  "<br/>", sep="")
		}
		
		#
		potential_matches <- rbind(temp1, temp2)
		potential_matches <- as.data.frame(unlist(potential_matches))
		colnames(potential_matches) <- "a"
		
		#controls for when no matches exist
		if(nrow(potential_matches) != 0) {
			mean_potential_matches <- mean(ddply(potential_matches,.(a),nrow)[,2])
			std_potential_matches <- sd(ddply(potential_matches,.(a),nrow)[,2])
		}
		if(nrow(potential_matches) == 0) {
			mean_potential_matches <- 0
			std_potential_matches <- 0
		}
		#
		
		
		samplesize <- nrow(unique(rbind(temp1,temp2,temp3,temp4)))
		
		#Output results                  
			output$contents <- renderUI({
				HTML(paste(co,"Statistical analysis complete.",'<br/>',"Number of comparisons conducted: ",ll, "<br/>", "Total specimens tested: ", samplesize,'<br/>',"Number of specimens with 1 or more potential matches: ",lent, '<br/>', "Total number of potential matches: ", nmatch, '<br/>', "Mean number of potential matches per specimen: ", round(mean_potential_matches, digits = 2) , "<br/>", "Standard deviation of potential matches: ", round(std_potential_matches, digits = 2) ,"<br/>", "Total number of exclusions: ", ll - nmatch, " (", round((ll - nmatch) / ll, digits = 2) * 100, "%)", '<br/><br/>'))
			})   

			output$table <- DT::renderDataTable({
				DT::datatable(direc2[[2]], options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
			})
			output$tablen <- DT::renderDataTable({
				DT::datatable(direc2[[3]], options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
			})

		#Output results                  


		#Download handler       
			output$downloadData <- downloadHandler(
				filename <- function() {
				paste("results.zip")
				},      
				content <- function(file) {
					setwd(direc)
					file.copy(paste(direc,'.zip',sep=''), file)  
					setwd(sessiontemp)    
				},
				contentType = "application/zip"
			)
			setwd(sessiontemp)


			removeModal()                             	
		})
		     


		#######################generates options for multiple
		observeEvent(input$testtype2, {
			if(input$testtype2 == 'Pair_match') {
				output$testtype2 <- renderUI({
					selectInput('bone', 'Elements', c(All_pair_tests = 'alttp', Humerus='humerus', Ulna='ulna', Radius='radius', Femur='femur', Tibia='tibia', Fibula='fibula', Scapula='scapula', Os_coxa='os_coxa', Clavicle='clavicle'),'humerus')
				})
			}
			if(input$testtype2 == 'Articulation_match') {
				output$testtype2 <- renderUI({
					selectInput('bone', 'Elements', c(All_articulation_tests = 'altta', Humerus_Ulna='hu',Humerus_Radius='hr', Humerus_Scapula1='hs', Humerus_Scapula2='hss', Femur_Os_coxa='fi', Femur_Tibia='ft', Fibula_Tibia='ftt'),'fi')
				})
			}
			if(input$testtype2 == 'Regression_match') {
				output$testtype2 <- renderUI({
					#bone and side predictor predicted
					fluidRow(
						column(4,
							h4("Predictor"),
							selectInput("assside1", "Side", c(Left='Left', Right='Right')),
							selectInput("assbone1", "Element", c(Clavicle="Clavicle", Scapula="Scapula", Humerus="Humerus", Radius="Radius", Ulna="Ulna", Os_coxa="os_coxa", Femur="Femur", Tibia="Tibia", Fibula="Fibula"))
						),
						column(4,
							h4("Predicted"),
							selectInput("assside2", "Side", c(Left='Left', Right='Right')),
							selectInput("assbone2", "Element", c(Clavicle="Clavicle", Scapula="Scapula", Humerus="Humerus", Radius="Radius", Ulna="Ulna", Os_coxa="os_coxa", Femur="Femur", Tibia="Tibia", Fibula="Fibula"))
						)
					)
				})
			}
			if(input$testtype2 == 'All_tests') {
				output$testtype2 <- renderUI({
					selectInput('bone', 'Elements', c(All_tests='altt'))
				})
			}

	})
	