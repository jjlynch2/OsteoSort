output$contents <- renderUI({
   HTML(paste(""))
})	

#file upload render for multiple comparison
output$resettableInput <- renderUI({
	input$clearFile1
	input$uploadFormat
	fileInput('file1', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
})

#clears session for multiple comparison
observeEvent(input$clearFile1, {
	fileInput('file1', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
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

numbercoresglobal <- reactiveValues(ncore = detectCores()-1)

observeEvent(input$numbercores, {
	numbercoresglobal$ncore <- input$numbercores
})
output$ncores <- renderUI({
	sliderInput(inputId = "numbercores", label = "Number of threads", min=1, max=detectCores(), value=detectCores()-1, step =1)
})
		
observeEvent(input$pro, {
	showModal(modalDialog(title = "Calculation has started...Window will update when finished.", easyClose = FALSE, footer = NULL))
	withProgress(message = 'Calculation has started',
	            detail = '', value = 0, {       
	            for (i in 1:10) {
	       incProgress(1/10)
	       Sys.sleep(0.05)
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

	tempdata1 <- read.csv(inFile$datapath, header=TRUE, sep=",", na.strings=c("", " ", "NA"))## see na.strings forces NA for blanks, spaces, etc

	#checks if measurements are numeric and converts alpha characters to numeric   
	tempdataa <- tempdata1[,1:3]
	tempdatab <- lapply(tempdata1[,-(1:3)], function(x) { as.numeric(as.character(x))})

	tempdata1 <- c(tempdataa, tempdatab)
	tempdata1 <- as.data.frame(tempdata1) #combines first four columns with now numeric measurements

	if(input$testtype2 != 'Regression_match') {
		#logic for calling input functions
		if(input$standard == 'Standard' & input$bone != 'hu' & input$bone != 'hr' & input$bone != 'hs' & input$bone != 'hss' & input$bone != 'fi' & input$bone != 'ft' & input$bone != 'ftt'){
			if(input$bone == 'clavicle') {threshold <- input$clavicle; measurements <- input$MeasurementsUsed1} 
			if(input$bone == 'scapula') {threshold <- input$scapula; measurements <- input$MeasurementsUsed2}   
			if(input$bone == 'humerus') {threshold <- input$humerus; measurements <- input$MeasurementsUsed3} 
			if(input$bone == 'ulna') {threshold <- input$ulna; measurements <- input$MeasurementsUsed4} 
			if(input$bone == 'radius') {threshold <- input$radius; measurements <- input$MeasurementsUsed5} 
			if(input$bone == 'os_coxa') {threshold <- input$os_coxa; measurements <- input$MeasurementsUsed6} 
			if(input$bone == 'femur') {threshold <- input$femur; measurements <- input$MeasurementsUsed7} 
			if(input$bone == 'tibia') {threshold <- input$tibia; measurements <- input$MeasurementsUsed8} 
			if(input$bone == 'fibula') {threshold <- input$fibula; measurements <- input$MeasurementsUsed9}
			if(is.null(threshold)) {threshold <- 1} 

			wtf <- pm.input(bone=toString(input$bone), sort=tempdata1, measurement_standard='standard',threshold=threshold, measurements=measurements)
			direc2 <- pm.ttest(tails = input$tails2, ref = wtf[[2]], sort = wtf[[1]], sessiontempdir=sessiontemp, alphalevel = input$alphalevel, absolutevalue = input$absolutevalue, testagainstzero = input$testagainst, output_options =  c(input$fileoutput1, input$fileoutput1plot), threads = numbercoresglobal$ncore, boxcox = input$power2)
			ll <- nrow(direc2[[2]]) + nrow(direc2[[3]])       
		}	
		if(input$standard == 'Supplemental' & input$bone != 'hu' & input$bone != 'hr' & input$bone != 'hs' & input$bone != 'hss' & input$bone != 'fi' & input$bone != 'ft' & input$bone != 'ftt'){
			if(input$bone == 'clavicle') {threshold <- input$claviclea; measurements <- input$MeasurementsUseda} 
			if(input$bone == 'scapula') {threshold <- input$scapulab; measurements <- input$MeasurementsUsedb}   
			if(input$bone == 'humerus') {threshold <- input$humerusc; measurements <- input$MeasurementsUsedc} 
			if(input$bone == 'ulna') {threshold <- input$ulnad; measurements <- input$MeasurementsUsedd} 
			if(input$bone == 'radius') {threshold <- input$radiuse; measurements <- input$MeasurementsUsede} 
			if(input$bone == 'os_coxa') {threshold <- input$os_coxaf; measurements <- input$MeasurementsUsedf} 
			if(input$bone == 'femur') {threshold <- input$femurg; measurements <- input$MeasurementsUsedg} 
			if(input$bone == 'tibia') {threshold <- input$tibiah; measurements <- input$MeasurementsUsedh} 
			if(input$bone == 'fibula') {threshold <- input$fibulai; measurements <- input$MeasurementsUsedi}                 
			if(is.null(threshold)) {threshold <- 1}   

			wtf <- pm.input(bone=toString(input$bone), sort=tempdata1, measurement_standard='supplemental',threshold=threshold, measurements=measurements)                                	      
			direc2 <- pm.ttest(tails = input$tails2, ref = wtf[[2]], sort = wtf[[1]], sessiontempdir=sessiontemp, alphalevel = input$alphalevel, absolutevalue = input$absolutevalue, testagainstzero = input$testagainst, output_options =  c(input$fileoutput1, input$fileoutput1plot), threads = numbercoresglobal$ncore, boxcox = input$power2)
			ll <- nrow(direc2[[2]]) + nrow(direc2[[3]])           
		}
		if(input$bone == 'hu' | input$bone == 'hr' | input$bone == 'hs' | input$bone == "hss" | input$bone == 'fi' | input$bone == 'ft' | input$bone == 'ftt') {
			wtf <- art.input(bone=toString(input$bone), sort=tempdata1)
			direc2 <- art.ttest(tails = input$tails22, boxcox = input$power22, ref = wtf[[2]], sort = wtf[[1]], sessiontempdir=sessiontemp, alphalevel = input$alphalevel, absolutevalue = input$absolutevalue2, testagainstzero = input$testagainst2, output_options =  c(input$fileoutput1, input$fileoutput1plot), threads = numbercoresglobal$ncore)   
			ll <- nrow(direc2[[2]]) + nrow(direc2[[3]])                    
		}      
	}
	if(input$testtype2 == 'Regression_match') {
		if(input$standard == "Standard") {
			if(input$ab1 == 'Clavicle') {threshold <- input$clavicle; measurements <- input$MeasurementsUsed1} 
			if(input$ab1 == 'Scapula') {threshold <- input$scapula; measurements <- input$MeasurementsUsed2}   
			if(input$ab1 == 'Humerus') {threshold <- input$humerus; measurements <- input$MeasurementsUsed3} 
			if(input$ab1 == 'Ulna') {threshold <- input$ulna; measurements <- input$MeasurementsUsed4} 
			if(input$ab1 == 'Radius') {threshold <- input$radius; measurements <- input$MeasurementsUsed5} 
			if(input$ab1 == 'Os_coxa') {threshold <- input$os_coxa; measurements <- input$MeasurementsUsed6} 
			if(input$ab1 == 'Femur') {threshold <- input$femur; measurements <- input$MeasurementsUsed7} 
			if(input$ab1 == 'Tibia') {threshold <- input$tibia; measurements <- input$MeasurementsUsed8} 
			if(input$ab1 == 'Fibula') {threshold <- input$fibula; measurements <- input$MeasurementsUsed9}  
			
			if(input$ab2 == 'Clavicle') {threshold2 <- input$clavicle; measurements2 <- input$MeasurementsUsed1} 
			if(input$ab2 == 'Scapula') {threshold2 <- input$scapula; measurements2 <- input$MeasurementsUsed2}   
			if(input$ab2 == 'Humerus') {threshold2 <- input$humerus; measurements2 <- input$MeasurementsUsed3} 
			if(input$ab2 == 'Ulna') {threshold2 <- input$ulna; measurements2 <- input$MeasurementsUsed4} 
			if(input$ab2 == 'Radius') {threshold2 <- input$radius; measurements2 <- input$MeasurementsUsed5} 
			if(input$ab2 == 'Os_coxa') {threshold2 <- input$os_coxa; measurements2 <- input$MeasurementsUsed6} 
			if(input$ab2 == 'Femur') {threshold2 <- input$femur; measurements2 <- input$MeasurementsUsed7} 
			if(input$ab2 == 'Tibia') {threshold2 <- input$tibia; measurements2 <- input$MeasurementsUsed8} 
			if(input$ab2 == 'Fibula') {threshold2 <- input$fibula; measurements2 <- input$MeasurementsUsed9}   
			if(is.null(threshold)) {threshold <- 1}  
			if(is.null(threshold2)) {threshold2 <- 1}   
		}
		if(input$standard == "Supplemental") {
			if(input$ab1 == 'Clavicle') {threshold <- input$claviclea; measurements <- input$MeasurementsUseda} 
			if(input$ab1 == 'Scapula') {threshold <- input$scapulab; measurements <- input$MeasurementsUsedb}   
			if(input$ab1 == 'Humerus') {threshold <- input$humerusc; measurements <- input$MeasurementsUsedc} 
			if(input$ab1 == 'Ulna') {threshold <- input$ulnad; measurements <- input$MeasurementsUsedd} 
			if(input$ab1 == 'Radius') {threshold <- input$radiuse; measurements <- input$MeasurementsUsede} 
			if(input$ab1 == 'Os_coxa') {threshold <- input$os_coxaf; measurements <- input$MeasurementsUsedf} 
			if(input$ab1 == 'Femur') {threshold <- input$femurg; measurements <- input$MeasurementsUsedg} 
			if(input$ab1 == 'Tibia') {threshold <- input$tibiah; measurements <- input$MeasurementsUsedh} 
			if(input$ab1 == 'Fibula') {threshold <- input$fibulai; measurements <- input$MeasurementsUsedi}    
			
			if(input$ab2 == 'Clavicle') {threshold2 <- input$claviclea; measurements2 <- input$MeasurementsUseda} 
			if(input$ab2 == 'Scapula') {threshold2 <- input$scapulab; measurements2 <- input$MeasurementsUsedb}   
			if(input$ab2 == 'Humerus') {threshold2 <- input$humerusc; measurements2 <- input$MeasurementsUsedc} 
			if(input$ab2 == 'Ulna') {threshold2 <- input$ulnad; measurements2 <- input$MeasurementsUsedd} 
			if(input$ab2 == 'Radius') {threshold2 <- input$radiuse; measurements2 <- input$MeasurementsUsede} 
			if(input$ab2 == 'Os_coxa') {threshold2 <- input$os_coxaf; measurements2 <- input$MeasurementsUsedf} 
			if(input$ab2 == 'Femur') {threshold2 <- input$femurg; measurements2 <- input$MeasurementsUsedg} 
			if(input$ab2 == 'Tibia') {threshold2 <- input$tibiah; measurements2 <- input$MeasurementsUsedh} 
			if(input$ab2 == 'Fibula') {threshold2 <- input$fibulai; measurements2 <- input$MeasurementsUsedi}              
			if(is.null(threshold)) {threshold <- 1}   
			if(is.null(threshold2)) {threshold2 <- 1}
		}
		if(input$regtesttypem == "PCA-CCA") {regtypee <- TRUE}
		if(input$regtesttypem == "Simple") {regtypee <- FALSE}

		if(input$pcamultipleuse == "All") {
			pcan <- NULL
		}
		if(is.null(input$pcamultipleuse)) {
			pcan <- NULL
		}
		if(input$pcamultipleuse == "Select") {
			pcan <- input$pcamultiple1
		}
		if(input$pcamultipleuse == "Variance") {
			pcan <- input$pcamultiple2
		}


		wtf <- reg.input(threshold=c(threshold, threshold2),sort = tempdata1, bone1 = input$ab1, side1 = input$assside1, bone2 = input$ab2, side2 = input$assside2, measurement_standard = input$standard, measurements1 = measurements, measurements2 = measurements2)
		direc2 <- reg.multitest(pca = pcan, sort = wtf[[1]], ref = wtf[[2]], splitn = wtf[[3]], prediction_interval = input$asspredlevel, alphatest = input$alphapred2, output_options = c(input$fileoutput1, input$fileoutput1plot), threads = numbercoresglobal$ncore, test = regtypee, alphalevel = input$alphalevel)
		ll <- nrow(direc2[[2]]) + nrow(direc2[[3]])
	}
	#changes results to 0 if no possible combinations
	if(all(is.na(direc2[[2]])) && all(is.na(direc2[[3]]))) {ll <- 0; nmatch <- 0; lent <- 0}
	
	#if combinations exist, produces output
	if(!all(is.na(direc2[[2]])) || !all(is.na(direc2[[3]]))) {

		direc <- direc2[[1]]
		nmatch <- nrow(direc2[[2]])
		if(input$fileoutput1 || input$fileoutput1plot) {
			files <- list.files(direc, recursive = TRUE)
			setwd(direc)
			zip:::zip(zipfile = paste(direc,'.zip',sep=''), files = files[1], compression = 1)
			for(file_na in files[-1]) {
				zip:::zip_append(zipfile = paste(direc,'.zip',sep=''), files = file_na, compression = 1)
			}
			setwd(sessiontemp)
		}
		lent <- length(unique(rbind(as.matrix(direc2[[2]][1]),as.matrix(direc2[[2]][4])))) #fix for number of specimens matched
		if(input$research) { lent <- lent * 2} ################FIX for statistics having same id.
	}
	temp1 <- direc2[[2]][1]
	temp2 <- direc2[[2]][4]
	temp3 <- direc2[[3]][1]
	temp4 <- direc2[[3]][4]
	#Forces id name 
	names(temp1) <- "id"
	names(temp2) <- "id"
	names(temp3) <- "id"
	names(temp4) <- "id"
	co <- ""

	if(input$research) {
		#used to assess accuracy of methodology
		
		globala <- as.matrix(direc2[[3]][1])
		globalb <- as.matrix(direc2[[3]][4])
		cn <- 0
		for(xx in 1:nrow(globala)) {
			if(globala[xx] == globalb[xx]) {cn <- cn +1}
		}
		
		global1 <- as.matrix(direc2[[2]][1])
		global2 <- as.matrix(direc2[[2]][4])
		co <- 0
		 for(i in 1:nrow(global1)) {
		 	if(global1[i] == global2[i]) {co <- co + 1}
		 }		
		#used to assess accuracy of methodology
		 
		TP <- (ll - cn) - nmatch
		FP <- cn
		FN <- nmatch - co
		TN <- co
		co <- paste("TP: ", 				"<font color=\"#00688B\">",TP, "</font>", 
				"<br/>","FP: ", 			"<font color=\"#00688B\">",FP, "</font>", 
				"<br/>","FN: ", 			"<font color=\"#00688B\">",FN, "</font>", 
				"<br/>","TN: ", 			"<font color=\"#00688B\">", TN, "</font>", 
				"<br/>","FPR: ", 			"<font color=\"#00688B\">",1 - round(TN/(TN+FP), digits = 3) ,"</font>", 
				"<br/>","Sens: ", 			"<font color=\"#00688B\">",round(TP/(TP+FN), digits = 3), "</font>", 
				"<br/>","Spec: ", 			"<font color=\"#00688B\">",round(TN/(TN+FP), digits = 3),"</font>", 
				"<br/>","PPV: ", 			"<font color=\"#00688B\">",round(TP/(TP+FP), digits = 3), "</font>", 
				"<br/>","NPV: ", 			"<font color=\"#00688B\">",round(TN/(TN+FN), digits = 3),"</font>", 
				"<br/>","FDR: ", 			"<font color=\"#00688B\">",round(FP/(FP+TP), digits = 3), "</font>", 
				"<br/>","EFF: ", 			"<font color=\"#00688B\">",round((TP+TN) / (TP+TN+FN+FP), digits = 3),"</font>", 
				"<br/>", sep = "")

	}

	samplesize <- nrow(unique(rbind(temp1,temp2,temp3,temp4))) 
	if(input$research) { samplesize <- samplesize * 2} ################FIX for statistics having same id.
	#Output results                  
		output$contents <- renderUI({
			HTML(paste("<strong>","<br/>","Comparisons: ",   "<font color=\"#00688B\">", ll, "</font>", 
                             "<br/>", "Specimens: ",           "<font color=\"#00688B\">",samplesize, "</font>", 
                             '<br/>', "Potential matches: ",  "<font color=\"#00688B\">",nmatch , "</font>",
                             '<br/>', "Exclusions: ",         "<font color=\"#00688B\">",ll - nmatch, " (", round((ll - nmatch) / ll, digits = 3) * 100, "%)",  "</font>",
						'<br/>',co,'<br/></strong>'))
		})   

		output$table <- DT::renderDataTable({
			DT::datatable(direc2[[2]], options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
		})
		output$tablen <- DT::renderDataTable({
			DT::datatable(direc2[[3]], options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
		})

	#Output results                  

		if(input$fileoutput1 || input$fileoutput1plot) {
		#Download handler       
			output$downloadData <- downloadHandler(
				filename <- function() {
				paste("results.zip")
				},      
				content = function(file) {
					setwd(direc)
					file.copy(paste(direc,'.zip',sep=''), file)  
					setwd(sessiontemp)    
				},
				contentType = "application/zip"
			)
			setwd(sessiontemp)
		}

		gc()
		removeModal()                             	
})
	     
observeEvent(input$testtype2, {
	if(input$testtype2 == 'Pair_match') {
		output$testtype2 <- renderUI({
			selectInput('bone', 'Elements', c(Humerus='humerus', Ulna='ulna', Radius='radius', Femur='femur', Tibia='tibia', Fibula='fibula', Scapula='scapula', Os_coxa='os_coxa', Clavicle='clavicle'),'humerus')
		})
	}
	if(input$testtype2 == 'Articulation_match') {
		output$testtype2 <- renderUI({
			selectInput('bone', 'Elements', c(Humerus_Ulna='hu',Humerus_Radius='hr', Humerus_Scapula1='hs', Humerus_Scapula2='hss', Femur_Os_coxa='fi', Femur_Tibia='ft', Fibula_Tibia='ftt'),'fi')
		})
	}
	if(input$testtype2 == 'Regression_match') {
		output$testtype2 <- renderUI({
			#bone and side predictor predicted
			fluidRow(
				column(6,
					h4("Predictor"),
					selectInput("assside1", "Side", c(Left='Left', Right='Right')),
					selectInput("ab1", "Element", c(Clavicle="Clavicle", Scapula="Scapula", Humerus="Humerus", Radius="Radius", Ulna="Ulna", Os_coxa="Os_coxa", Femur="Femur", Tibia="Tibia", Fibula="Fibula"), 'Radius')
				),
				column(6,
					h4("Predicted"),
					selectInput("assside2", "Side", c(Left='Left', Right='Right')),
					selectInput("ab2", "Element", c(Clavicle="Clavicle", Scapula="Scapula", Humerus="Humerus", Radius="Radius", Ulna="Ulna", Os_coxa="Os_coxa", Femur="Femur", Tibia="Tibia", Fibula="Fibula"), 'Ulna')
				)
			)
		})
	}
})
	