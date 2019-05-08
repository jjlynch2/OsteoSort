##default blank output
output$single_contents <- renderUI({
	HTML(paste(""))
})
##default blank output

##output settings
single_file_output1 <- reactiveValues(single_file_output1 = TRUE) #default option
output$single_file_output1 <- renderUI({
	checkboxInput(inputId = "single_file_output1", label = "Output excel file", value = TRUE)
})
observeEvent(input$single_file_output1, {
	single_file_output1$single_file_output1 <- input$single_file_output1
})
##output settings

##output settings
single_file_output2 <- reactiveValues(single_file_output2 = TRUE) #default option
output$single_file_output2 <- renderUI({
	checkboxInput(inputId = "single_file_output2", label = "Output plot", value = TRUE)
})
observeEvent(input$single_file_output2, {
	single_file_output2$single_file_output2 <- input$single_file_output2
})
##output settings

##association type settings
association_types <- reactiveValues(association_types = "PCA-CCA") #default option
output$association_types <- renderUI({
	radioButtons(inputId ="association_types", label = "Regression type", choices = c("PCA-CCA", "Simple"), selected = "Simple")
})
observeEvent(input$association_types, {
	association_types$association_types <- input$association_types
})
##association type settings

##association alpha test settings
association_alpha_prediction <- reactiveValues(association_alpha_prediction = TRUE) #default option
output$association_alpha_prediction <- renderUI({
	checkboxInput(inputId = "association_alpha_prediction", label = "Use alpha level hypothesis", value = TRUE)
})
observeEvent(input$association_alpha_prediction, {
	association_alpha_prediction$association_alpha_prediction <- input$association_alpha_prediction
})
##association alpha test settings

##association prediction test settings
association_prediction <- reactiveValues(association_prediction = 0.95) #default option
output$association_prediction <- renderUI({
	sliderInput(inputId = "association_prediction", label = "Prediction interval level", min=0.01, max=1, value=0.95, step = 0.01)
})
observeEvent(input$association_prediction, {
	association_prediction$association_prediction <- input$association_prediction
})
##association prediction test settings

##association pca-cca test settings
association_pca <- reactiveValues(association_pca = "Select") #default option
output$association_pca <- renderUI({
	radioButtons(inputId ="association_pca", label = "Principal Component Analysis", choices = c("All", "Select", "Variance"), selected = "Variance")
})
observeEvent(input$association_pca, {
	association_pca$association_pca <- input$association_pca
})
##association pca-cca test settings

##association pca-cca test settings
association_pca_select <- reactiveValues(association_pca_select = 1) #default option
output$association_pca_select <- renderUI({
	sliderInput(inputId = "association_pca_select", label = "Principal Components", min=1, max = 10, value = 1)
})
observeEvent(input$association_pca_select, {
	association_pca_select$association_pca_select <- input$association_pca_select
})
##association pca-cca test settings

##association pca-cca test settings
association_pca_variance <- reactiveValues(association_pca_variance = 0.99) #default option
output$association_pca_variance <- renderUI({
	sliderInput(inputId = "association_pca_variance", label = "Cumulative Variance", min=0.1, max = 0.99, value = 0.99)
})
observeEvent(input$association_pca_variance, {
	association_pca_variance$association_pca_variance <- input$association_pca_variance
})
##association pca-cca test settings

##common alpha level
common_alpha_level <- reactiveValues(common_alpha_level = 0.05) #default option
output$common_alpha_level <- renderUI({
	sliderInput(inputId = "common_alpha_level", label = "Alpha level", min=0.01, max=1, value=0.05, step = 0.01)
})
observeEvent(input$common_alpha_level, {
	common_alpha_level$common_alpha_level <- input$common_alpha_level
})
##common alpha level

##pair-match articulation absolute value
single_absolute_value <- reactiveValues(single_absolute_value = FALSE) #default option
output$single_absolute_value <- renderUI({
	checkboxInput(inputId = "single_absolute_value", label = "Absolute D-value |a-b|", value = FALSE)
})
observeEvent(input$single_absolute_value, {
	single_absolute_value$single_absolute_value <- input$single_absolute_value
})
##pair-match articulation absolute value

##pair-match articulation boxcox
single_boxcox <- reactiveValues(single_boxcox = FALSE) #default option
output$single_boxcox <- renderUI({
	checkboxInput(inputId = "single_boxcox", label = "Boxcox transformation", value = FALSE)
})
observeEvent(input$single_boxcox, {
	single_boxcox$single_boxcox <- input$single_boxcox
})
##pair-match articulation boxcox

##pair-match articulation mean
single_mean <- reactiveValues(single_mean = FALSE) #default option
output$single_mean <- renderUI({
	checkboxInput(inputId = "single_mean", label = "Zero mean", value = FALSE)					
})
observeEvent(input$single_mean, {
	single_mean$single_mean <- input$single_mean
})
##pair-match articulation mean

##pair-match articulation tails
single_tails <- reactiveValues(single_tails = TRUE) #default option
output$single_tails <- renderUI({
	sliderInput(inputId = "single_tails", label = "Tails", min=1, max=2, value=2, step=1)
})
observeEvent(input$single_tails, {
	single_tails$single_tails <- input$single_tails
})
##pair-match articulation tails

single_analysis <- reactiveValues(single_analysis = "Antimere t-test")
observeEvent(input$single_analysis, {
	single_analysis$single_analysis <- input$single_analysis
})
output$single_analysis <- renderUI({
	selectInput(inputId = "single_analysis", label = "Analysis", choices = c("Antimere t-test","Non-Antimere t-test","Non-Antimere regression"), selected = "Antimere t-test")
})

single_reference <- reactiveValues(single_reference = c("temp"))
observeEvent(input$single_reference, {
	single_reference$single_reference <- input$single_reference
})
output$single_reference <- renderUI({
	selectInput(inputId = "single_reference", label = "Reference", choices = reference_name_list$reference_name_list)
})

single_reference_imported <- reactiveValues(single_reference_imported = data.frame())
elements <- reactiveValues(elements = c("temp") )

art_elements <- reactiveValues(df = c())
art_measurements_a <- reactiveValues(df = c())
art_measurements_b <- reactiveValues(df = c())

observeEvent(input$single_reference, {
	single_reference_imported$single_reference_imported <- reference_list$reference_list[[single_reference$single_reference]]
	elements$elements <- unique(single_reference_imported$single_reference_imported$Element)

	art <- config_df$config_df[config_df$config_df$Method == 'Articulation',]
	ref_col_names <- colnames(single_reference_imported$single_reference_imported)
	for(i in 1:nrow(art)) {
		a = FALSE
		b = FALSE
		for(x in 1:length(ref_col_names)) {
			if(art$Measurementa[i] == ref_col_names[x]) {a=TRUE}
			if(art$Measurementb[i] == ref_col_names[x]) {b=TRUE}
			if(a && b) {
				art_measurements_a$df <- c(art_measurements_a$df, art$Measurementa[i])
				art_measurements_b$df <- c(art_measurements_b$df, art$Measurementb[i])
				temp1 <- na.omit(unique(single_reference_imported$single_reference_imported[!is.na(single_reference_imported$single_reference_imported[[art$Measurementa[i]]]),]$Element))[1]
				temp2 <- na.omit(unique(single_reference_imported$single_reference_imported[!is.na(single_reference_imported$single_reference_imported[[art$Measurementb[i]]]),]$Element))[1]
				art_elements$df <- c(art_elements$df, paste(temp1, temp2, sep="_"))
				break
			}
		}
	}
})

output$single_element_articulation <- renderUI({
	selectInput(inputId = "single_element_articulation", label = "Elements", choices = art_elements$df)
})

observeEvent(input$single_element_articulation, {
	temp1 <- which(art_elements$df == input$single_element_articulation)
	tempa <- art_measurements_a$df[temp1][!duplicated(art_measurements_a$df[temp1])]
	tempb <- art_measurements_b$df[temp1][!duplicated(art_measurements_b$df[temp1])]
	output$single_measurement_articulation_a <- renderUI({
		lapply(tempa, function(i) {
			numericInput(paste0(i,"_art_a"), label = i, value = "", min=0,max=999,step=0.01)
		})
	})
	output$single_measurement_articulation_b <- renderUI({
		lapply(tempb, function(i) {
			numericInput(paste0(i,"_art_b"), label = i, value = "", min=0,max=999,step=0.01)
		})
	})
})

output$single_element_pair_match <- renderUI({
	selectInput(inputId = "single_elements_pairmatch", label = "Element", choices = elements$elements)
})

single_ML <- reactiveValues(single_ML = c("temp"))
observeEvent(input$single_elements_pairmatch, {
	temp <- single_reference_imported$single_reference_imported[single_reference_imported$single_reference_imported$Element == input$single_elements_pairmatch,]
	t1 <- temp[,c(1:6)]
	t2 <- temp[,-c(1:6)]
	t2 <- t2[,colSums(is.na(t2)) < nrow(t2)]
	single_ML$single_ML <- names(t2)
})

output$list_numeric_inputs_single_left <- renderUI ({
	lapply(single_ML$single_ML, function(i) {
		numericInput(paste0(i,"_left"), label = i, value = "", min=0,max=999,step=0.01)
	})
})

output$list_numeric_inputs_single_right <- renderUI ({
	lapply(single_ML$single_ML, function(i) {
		numericInput(paste0(i,"_right"), label = i, value = "", min=0,max=999,step=0.01)
	})
})

output$single_elements_association_a <- renderUI({
	selectInput(inputId = "single_elements_association_a", label = "Dependent", choices = elements$elements)
})

observeEvent(input$single_elements_association_a, {
	output$single_elements_association_b <- renderUI({
		selectInput(inputId = "single_elements_association_b", label = "Independent", choices = elements$elements[elements$elements != input$single_elements_association_a])
	})
})

single_MLA <- reactiveValues(single_ML = c("temp"))
observeEvent(input$single_elements_association_a, {
	temp <- single_reference_imported$single_reference_imported[single_reference_imported$single_reference_imported$Element == input$single_elements_association_a,]
	t1 <- temp[,c(1:6)]
	t2 <- temp[,-c(1:6)]
	t2 <- t2[,colSums(is.na(t2)) < nrow(t2)]
	single_MLA$single_ML <- names(t2)
})

single_MLB <- reactiveValues(single_ML = c("temp"))
observeEvent(input$single_elements_association_b, {
	temp <- single_reference_imported$single_reference_imported[single_reference_imported$single_reference_imported$Element == input$single_elements_association_b,]
	t1 <- temp[,c(1:6)]
	t2 <- temp[,-c(1:6)]
	t2 <- t2[,colSums(is.na(t2)) < nrow(t2)]
	single_MLB$single_ML <- names(t2)
})

output$list_numeric_inputs_single_A <- renderUI ({
	lapply(single_MLA$single_ML, function(i) {
		numericInput(paste0(i,"_A"), label = i, value = "", min=0,max=999,step=0.01)
	})
})

output$list_numeric_inputs_single_B <- renderUI ({
	lapply(single_MLB$single_ML, function(i) {
		numericInput(paste0(i,"_B"), label = i, value = "", min=0,max=999,step=0.01)
	})
})

observeEvent(input$proc, {
	showModal(modalDialog(title = "Calculation has started...Window will update when finished.", easyClose = FALSE, footer = NULL))
	withProgress(message = 'Calculation has started', detail = '', value = 0, {       
		for (i in 1:10) {
			incProgress(1/10)
			Sys.sleep(0.05)
		}
	})
	if(input$single_analysis == "Non-Antimere t-test") {
		temp1 <- which(art_elements$df == input$single_element_articulation)
		tempa <- art_measurements_a$df[temp1][!duplicated(art_measurements_a$df[temp1])]
		tempb <- art_measurements_b$df[temp1][!duplicated(art_measurements_b$df[temp1])]

		single_input_art_a <- reactiveValues(single_input_art_a = c())
		lapply(tempa, function(i) {
			single_input_art_a$single_input_art_a <- c(single_input_art_a$single_input_art_a, input[[paste0(i,"_art_a")]])
		})

		single_input_art_b <- reactiveValues(single_input_art_b = c())
		lapply(tempb, function(i) {
			single_input_art_b$single_input_art_b <- c(single_input_art_b$single_input_art_b, input[[paste0(i,"_art_b")]])
		})

		single_input_art_a$single_input_art_a <- t(data.frame(single_input_art_a$single_input_art_a))
		colnames(single_input_art_a$single_input_art_a) <- tempa
		single_input_art_b$single_input_art_b <- t(data.frame(single_input_art_b$single_input_art_b))
		colnames(single_input_art_b$single_input_art_b) <- tempb
		
		false_df_a <- as.data.frame(matrix(NA,nrow = 1, ncol = length(tempa)))
		colnames(false_df_a) <- tempa
		false_df_b <- as.data.frame(matrix(NA,nrow = 1, ncol = length(tempb)))
		colnames(false_df_b) <- tempb

		sorta <- data.frame(id = input$ID1, Side = input$single_articulation_side, Element = strsplit(input$single_element_articulation, split = "_")[[1]][1], single_input_art_a$single_input_art_a, false_df_b, stringsAsFactors = FALSE)
		sortb <- data.frame(id = input$ID2, Side = input$single_articulation_side, Element = strsplit(input$single_element_articulation, split = "_")[[1]][2], false_df_a, single_input_art_b$single_input_art_b, stringsAsFactors = FALSE)
		sort <- rbind(sorta, sortb)
		pm.d1 <- art.input(side = input$single_articulation_side, ref = single_reference_imported$single_reference_imported, sort = sort, bones = c(strsplit(input$single_element_articulation, split = "_")[[1]][1], strsplit(input$single_element_articulation, split = "_")[[1]][2]), measurementsa = tempa, measurementsb = tempb)
g1 <<- pm.d1
	}
	if(input$single_analysis == "Antimere t-test") {
		#concat left values
		single_input_list_left <- reactiveValues(single_input_list_left = c())
		lapply(single_ML$single_ML, function(i) {
			single_input_list_left$single_input_list_left <- c(single_input_list_left$single_input_list_left, input[[paste0(i,"_left")]])
		})
		#concat right values
		single_input_list_right <- reactiveValues(single_input_list_right = c())
		lapply(single_ML$single_ML, function(i) {
			single_input_list_right$single_input_list_right <- c(single_input_list_right$single_input_list_right, input[[paste0(i,"_right")]])
		})

		#transform into dataframe and name columns
		single_input_list_left$single_input_list_left <- t(data.frame(single_input_list_left$single_input_list_left))
		colnames(single_input_list_left$single_input_list_left) <- single_ML$single_ML
		single_input_list_right$single_input_list_right <- t(data.frame(single_input_list_right$single_input_list_right))
		colnames(single_input_list_right$single_input_list_right) <- single_ML$single_ML

		#combine with id, bone, and side
		sortleft <- data.frame(id = input$ID1, Side = "left", Element = input$single_elements_pairmatch, single_input_list_left$single_input_list_left, stringsAsFactors = FALSE)
		sortright <- data.frame(id = input$ID2, Side = "right", Element = input$single_elements_pairmatch, single_input_list_right$single_input_list_right, stringsAsFactors = FALSE)
		pm.d1 <- pm.input(sort = rbind(sortleft, sortright), bone = input$single_elements_pairmatch, measurements = single_ML$single_ML, ref = single_reference_imported$single_reference_imported)
		pm.d2 <- pm.ttest(sortleft = pm.d1[[3]], sortright = pm.d1[[4]], refleft = pm.d1[[1]], refright = pm.d1[[2]], sessiontempdir = sessiontemp, alphalevel = common_alpha_level$common_alpha_level, absolute = single_absolute_value$single_absolute_value, zmean = single_mean$single_mean, boxcox = single_boxcox$single_boxcox, tails = single_tails$single_tails, output_options = c(single_file_output1$single_file_output1, single_file_output2$single_file_output2))
		tempDF <- rbind(pm.d2[[2]], pm.d2[[3]]) #combines excluded and not excluded for results
	}

	#output table
	output$table2 <- DT::renderDataTable({
		DT::datatable(tempDF, options = list(lengthMenu = c(1), pageLength = 1), rownames = FALSE)
	})

	if(single_file_output1$single_file_output1 || single_file_output2$single_file_output2) {  
		#Zip and download handler
		direc <- pm.d2[[1]]
		setwd(sessiontemp)
		setwd(direc)
		if(single_file_output2$single_file_output2) {
			nimages <- list.files()
			nimages <- paste(sessiontemp, "/", direc, "/", nimages[grep(".jpg", nimages)], sep="")

			output$single_plot <- renderImage({
				list(src = nimages,
					contentType = 'image/jpg',
					width = 400,
					height = 400,
					alt = "A"
				)
			}, deleteFile = FALSE)
		}
		files <- list.files(recursive = TRUE)
		zip:::zip(zipfile = paste(direc,'.zip',sep=''), files = files)
		output$downloadData2 <- downloadHandler(
			filename = function() {
				paste("results.zip")
			},
			content = function(file) {
				setwd(direc)
				file.copy(paste(direc,'.zip',sep=''), file)  
			},
			contentType = "application/zip"
		)
	}
	removeModal()
	setwd(sessiontemp)
	gc() #clean up 
})