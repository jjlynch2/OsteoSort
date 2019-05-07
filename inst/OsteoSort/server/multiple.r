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

##default blank output
output$multiple_contents <- renderUI({
	HTML(paste(""))
})
##default blank output

##output settings
multiple_file_output1 <- reactiveValues(multiple_file_output1 = TRUE) #default option
output$multiple_file_output1 <- renderUI({
	checkboxInput(inputId = "multiple_file_output1", label = "Output excel file", value = TRUE)
})
observeEvent(input$multiple_file_output1, {
	multiple_file_output1$multiple_file_output1 <- input$multiple_file_output1
})
##output settings

##association type settings
multiple_association_types <- reactiveValues(multiple_association_types = "PCA-CCA") #default option
output$multiple_association_types <- renderUI({
	radioButtons(inputId ="multiple_association_types", label = "Regression type", choices = c("PCA-CCA", "Simple"), selected = "Simple")
})
observeEvent(input$multiple_association_types, {
	multiple_association_types$multiple_association_types <- input$multiple_association_types
})
##association type settings

##association alpha test settings
multiple_association_alpha_prediction <- reactiveValues(multiple_association_alpha_prediction = TRUE) #default option
output$multiple_association_alpha_prediction <- renderUI({
	checkboxInput(inputId = "multiple_association_alpha_prediction", label = "Use alpha level hypothesis", value = TRUE)
})
observeEvent(input$multiple_association_alpha_prediction, {
	multiple_association_alpha_prediction$multiple_association_alpha_prediction <- input$multiple_association_alpha_prediction
})
##association alpha test settings

##association prediction test settings
multiple_association_prediction <- reactiveValues(multiple_association_prediction = 0.95) #default option
output$multiple_association_prediction <- renderUI({
	sliderInput(inputId = "multiple_association_prediction", label = "Prediction interval level", min=0.01, max=1, value=0.95, step = 0.01)
})
observeEvent(input$multiple_association_prediction, {
	multiple_association_prediction$multiple_association_prediction <- input$multiple_association_prediction
})
##association prediction test settings

##association pca-cca test settings
multiple_association_pca <- reactiveValues(multiple_association_pca = "Select") #default option
output$multiple_association_pca <- renderUI({
	radioButtons(inputId ="multiple_association_pca", label = "Principal Component Analysis", choices = c("All", "Select", "Variance"), selected = "Variance")
})
observeEvent(input$multiple_association_pca, {
	multiple_association_pca$multiple_association_pca <- input$multiple_association_pca
})
##association pca-cca test settings

##association pca-cca test settings
multiple_association_pca_select <- reactiveValues(multiple_association_pca_select = 1) #default option
output$multiple_association_pca_select <- renderUI({
	sliderInput(inputId = "multiple_association_pca_select", label = "Principal Components", min=1, max = 10, value = 1)
})
observeEvent(input$multiple_association_pca_select, {
	multiple_association_pca_select$multiple_association_pca_select <- input$multiple_association_pca_select
})
##association pca-cca test settings

##association pca-cca test settings
multiple_association_pca_variance <- reactiveValues(multiple_association_pca_variance = 0.99) #default option
output$multiple_association_pca_variance <- renderUI({
	sliderInput(inputId = "multiple_association_pca_variance", label = "Cumulative Variance", min=0.1, max = 0.99, value = 0.99)
})
observeEvent(input$multiple_association_pca_variance, {
	multiple_association_pca_variance$multiple_association_pca_variance <- input$multiple_association_pca_variance
})
##association pca-cca test settings

##common alpha level
multiple_common_alpha_level <- reactiveValues(multiple_common_alpha_level = 0.05) #default option
output$multiple_common_alpha_level <- renderUI({
	sliderInput(inputId = "multiple_common_alpha_level", label = "Alpha level", min=0.01, max=1, value=0.05, step = 0.01)
})
observeEvent(input$multiple_common_alpha_level, {
	multiple_common_alpha_level$multiple_common_alpha_level <- input$multiple_common_alpha_level
})
##common alpha level

##pair-match articulation absolute value
multiple_absolute_value <- reactiveValues(multiple_absolute_value = FALSE) #default option
output$multiple_absolute_value <- renderUI({
	checkboxInput(inputId = "multiple_absolute_value", label = "Absolute D-value |a-b|", value = FALSE)
})
observeEvent(input$multiple_absolute_value, {
	multiple_absolute_value$multiple_absolute_value <- input$multiple_absolute_value
})
##pair-match articulation absolute value

##pair-match articulation boxcox
multiple_boxcox <- reactiveValues(multiple_boxcox = FALSE) #default option
output$multiple_boxcox <- renderUI({
	checkboxInput(inputId = "multiple_boxcox", label = "Boxcox transformation", value = FALSE)
})
observeEvent(input$multiple_boxcox, {
	multiple_boxcox$multiple_boxcox <- input$multiple_boxcox
})
##pair-match articulation boxcox

##pair-match articulation mean
multiple_mean <- reactiveValues(multiple_mean = FALSE) #default option
output$multiple_mean <- renderUI({
	checkboxInput(inputId = "multiple_mean", label = "Zero mean", value = FALSE)					
})
observeEvent(input$multiple_mean, {
	multiple_mean$multiple_mean <- input$multiple_mean
})
##pair-match articulation mean

##pair-match articulation tails
multiple_tails <- reactiveValues(multiple_tails = TRUE) #default option
output$multiple_tails <- renderUI({
	sliderInput(inputId = "multiple_tails", label = "Tails", min=1, max=2, value=2, step=1)
})
observeEvent(input$multiple_tails, {
	multiple_tails$multiple_tails <- input$multiple_tails
})
##pair-match articulation tails

numbercoresglobal <- reactiveValues(ncore = detectCores()-1)
observeEvent(input$numbercores, {
	numbercoresglobal$ncore <- input$numbercores
})
output$ncores <- renderUI({
	sliderInput(inputId = "numbercores", label = "Number of threads", min=1, max=detectCores(), value=1, step =1)
})

multiple_analysis <- reactiveValues(multiple_analysis = "Antimere t-test")
observeEvent(input$multiple_analysis, {
	multiple_analysis$multiple_analysis <- input$multiple_analysis
})
output$multiple_analysis <- renderUI({
	selectInput(inputId = "multiple_analysis", label = "Analysis", choices = c("Antimere t-test","Non-Antimere t-test","Non-Antimere regression"), selected = "Antimere t-test")
})

output$multiple_element_pair_match <- renderUI({
	selectInput(inputId = "multiple_elements_pairmatch", label = "Element", choices = elements$elements)
})

multiple_reference <- reactiveValues(multiple_reference = c("temp"))
observeEvent(input$multiple_reference, {
	multiple_reference$multiple_reference <- input$multiple_reference
})
output$multiple_reference <- renderUI({
	selectInput(inputId = "multiple_reference", label = "Reference", choices = reference_name_list$reference_name_list)
})

multiple_reference_imported <- reactiveValues(multiple_reference_imported = data.frame())
elements <- reactiveValues(elements = c("temp") )

observeEvent(input$multiple_reference, {
	multiple_reference_imported$multiple_reference_imported <- reference_list$reference_list[[multiple_reference$multiple_reference]]
	elements$elements <- unique(multiple_reference_imported$multiple_reference_imported$Element)
})

multiple_ML <- reactiveValues(multiple_ML = c("temp"))
observeEvent(input$multiple_elements_pairmatch, {
	temp <- multiple_reference_imported$multiple_reference_imported[multiple_reference_imported$multiple_reference_imported$Element == input$multiple_elements_pairmatch,]
	t1 <- temp[,c(1:6)]
	t2 <- temp[,-c(1:6)]
	t2 <- t2[,colSums(is.na(t2)) < nrow(t2)]
	multiple_ML$multiple_ML <- names(t2)
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
	if (is.null(inFile)) {
		removeModal()
		return(NULL)
	}
	#return null if empty file
	if (!file.size(inFile$datapath) > 1) {
		removeModal()
		return(NULL)
	}

	tempdata1 <- read.csv(inFile$datapath, header=TRUE, sep=",", na.strings=c("", " ", "NA"))## see na.strings forces NA for blanks, spaces, etc
	#checks if measurements are numeric and converts alpha characters to numeric   
	tempdataa <- tempdata1[,1:3]
	tempdatab <- lapply(tempdata1[,-(1:3)], function(x) { as.numeric(as.character(x))})
	tempdata1 <- c(tempdataa, tempdatab)
	tempdata1 <- as.data.frame(tempdata1) #combines first four columns with now numeric measurements

	if(input$multiple_analysis == "Antimere t-test") {
		pm.d1 <- pm.input(sort = tempdata1, bone = input$multiple_elements_pairmatch, measurements = multiple_ML$multiple_ML, ref = multiple_reference_imported$multiple_reference_imported)
		pm.d2 <- pm.ttest(sortleft = pm.d1[[3]], sortright = pm.d1[[4]], refleft = pm.d1[[1]], refright = pm.d1[[2]], sessiontempdir = sessiontemp, alphalevel = multiple_common_alpha_level$multiple_common_alpha_level, absolute = multiple_absolute_value$multiple_absolute_value, zmean = multiple_mean$multiple_mean, boxcox = multiple_boxcox$multiple_boxcox, tails = multiple_tails$multiple_tails, output_options = multiple_file_output1$multiple_file_output1, threads = numbercoresglobal$ncore)
		tempDF <- rbind(pm.d2[[2]], pm.d2[[3]]) #combines excluded and not excluded for results
	}

	#if combinations exist, produces output
	if(!all(is.na(pm.d2[[2]])) || !all(is.na(pm.d2[[3]]))) {
		direc <- pm.d2[[1]]
		if(multiple_file_output1$multiple_file_output1) {
			files <- list.files(direc, recursive = TRUE)
			setwd(direc)
			zip:::zip(zipfile = paste(direc,'.zip',sep=''), files = files[1], compression = 1)
			for(file_na in files[-1]) {
				zip:::zip_append(zipfile = paste(direc,'.zip',sep=''), files = file_na, compression = 1)
			}
			setwd(sessiontemp)
		}
		lent <- length(unique(rbind(as.matrix(pm.d2[[2]][1]),as.matrix(pm.d2[[2]][4])))) #fix for number of specimens matched
		ll <- nrow(pm.d2[[2]]) + nrow(pm.d2[[3]])
		nmatch <- nrow(pm.d2[[2]])
		samplesize <- length(unique(c(pm.d2[[2]]$id_1, pm.d2[[2]]$id_2, pm.d2[[3]]$id_1, pm.d2[[3]]$id_2)))
	}
	output$multiple_contents <- renderUI({
		HTML(paste("<strong>","<br/>","Comparisons: ",   "<font color=\"#00688B\">", ll, "</font>", 
                        "<br/>", "Specimens: ",           "<font color=\"#00688B\">",samplesize, "</font>", 
                        '<br/>', "Potential matches: ",  "<font color=\"#00688B\">",nmatch , "</font>",
                        '<br/>', "Exclusions: ",         "<font color=\"#00688B\">",ll - nmatch, " (", round((ll - nmatch) / ll, digits = 3) * 100, "%)",  "</font>",'</strong>'))
	})

	output$table <- DT::renderDataTable({
		DT::datatable(pm.d2[[2]], options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
	})
	output$tablen <- DT::renderDataTable({
		DT::datatable(pm.d2[[3]], options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
	})

	if(multiple_file_output1$multiple_file_output1) {
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