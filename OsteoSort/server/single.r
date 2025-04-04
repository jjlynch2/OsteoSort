common_alpha_level <- reactiveValues(common_alpha_level = 0.1) 
output$common_alpha_level <- renderUI({
	sliderInput(inputId = "common_alpha_level", label = "Alpha level", min=0.01, max=1, value=0.1, step = 0.01)
})
observeEvent(input$common_alpha_level, {
	common_alpha_level$common_alpha_level <- input$common_alpha_level
})

single_absolute_value <- reactiveValues(single_absolute_value = FALSE) 
output$single_absolute_value <- renderUI({
	checkboxInput(inputId = "single_absolute_value", label = "Absolute D-value |a-b|", value = FALSE)
})
observeEvent(input$single_absolute_value, {
	single_absolute_value$single_absolute_value <- input$single_absolute_value
})

single_yeojohnson <- reactiveValues(single_yeojohnson = FALSE) 
output$single_yeojohnson <- renderUI({
	checkboxInput(inputId = "single_yeojohnson", label = "YeoJohnson transformation", value = FALSE)
})
observeEvent(input$single_yeojohnson, {
	single_yeojohnson$single_yeojohnson <- input$single_yeojohnson
})

single_mean <- reactiveValues(single_mean = FALSE) 
output$single_mean <- renderUI({
	checkboxInput(inputId = "single_mean", label = "Zero mean", value = FALSE)
})
observeEvent(input$single_mean, {
	single_mean$single_mean <- input$single_mean
})

single_tails <- reactiveValues(single_tails = 2) 
output$single_tails <- renderUI({
	radioButtons(inputId = "single_tails", label = "Tails", choices = list(1, 2), selected = 2, inline = TRUE)
})
observeEvent(input$single_tails, {
	single_tails$single_tails <- as.numeric(input$single_tails)
})

single_analysis <- reactiveValues(single_analysis = "pair-match")
observeEvent(input$single_analysis, {
	single_analysis$single_analysis <- input$single_analysis
})
output$single_analysis <- renderUI({
	selectInput(inputId = "single_analysis", label = "Analysis", choices = c("pair-match","articulation","osr"), selected = "pair-match")
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
single_MLB <- reactiveValues(single_ML = c("temp"))
single_MLA <- reactiveValues(single_ML = c("temp"))
single_ML <- reactiveValues(single_ML = c("temp"))

observeEvent(input$single_reference, {
	single_reference_imported$single_reference_imported <- reference_list$reference_list[[single_reference$single_reference]]
	elements$elements <- unique(single_reference_imported$single_reference_imported$Element)

	art <- config_df$config_df[config_df$config_df$Method == 'articulation',]
	ref_col_names <- colnames(single_reference_imported$single_reference_imported)
	art_elements$df <- NULL
	art_measurements_a$df <- NULL
	art_measurements_b$df <- NULL
	for(i in 1:nrow(art)) {
		a = FALSE
		b = FALSE
		for(x in 1:length(ref_col_names)) {
			if(art$Measurementa[i] == ref_col_names[x]) {a=TRUE}
			if(art$Measurementb[i] == ref_col_names[x]) {b=TRUE}
			if(a && b) {
				art_measurements_a$df <- na.omit(c(art_measurements_a$df, art$Measurementa[i]))
				art_measurements_b$df <- na.omit(c(art_measurements_b$df, art$Measurementb[i]))
				temp1 <- na.omit(unique(single_reference_imported$single_reference_imported[!is.na(single_reference_imported$single_reference_imported[[art$Measurementa[i]]]),]$Element))[1]
				temp2 <- na.omit(unique(single_reference_imported$single_reference_imported[!is.na(single_reference_imported$single_reference_imported[[art$Measurementb[i]]]),]$Element))[1]
				if(!is.na(temp1) && !is.na(temp2)) {
					cf <- function(a, b) {
						for(t in 1:length(a)) {
							if(a[t] == b) {return(TRUE)}
						}
						return(FALSE)
					}
					n <- 0
					temp3 <- paste(temp1, temp2, sep="-")
					if(!is.null(art_elements$df)) {
						while(cf(art_elements$df, temp3)) {
							n <- n + 1
							temp3 <- paste(temp1, temp2,n+1, sep="-")
						}
					}
					art_elements$df <- c(art_elements$df, temp3)
				}
				break
			}
		}
	}

	output$single_element_osr <- renderUI({
		selectInput(inputId = "single_element_osr", label = "Elements", choices = art_elements$df)
	})

	output$single_element_pair_match <- renderUI({
		selectInput(inputId = "single_elements_pairmatch", label = "Element", choices = elements$elements)
	})

	output$single_elements_association_a <- renderUI({
		selectInput(inputId = "single_elements_association_a", label = "Independent", choices = elements$elements)
	})

	observeEvent(input$single_elements_association_a, {
		output$single_elements_association_b <- renderUI({
			selectInput(inputId = "single_elements_association_b", label = "Dependent", choices = elements$elements[elements$elements != input$single_elements_association_a])
		})
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

	output$single_measurement_osr_a <- renderUI({
		lapply(art_measurements_a$df[which(art_elements$df == input$single_element_osr)], function(i) {
			numericInput(paste0(i,"_art_a"), label = i, value = "", min=0,max=999,step=0.01)
		})
	})
	output$single_measurement_osr_b <- renderUI({
		lapply(art_measurements_b$df[which(art_elements$df == input$single_element_osr)], function(i) {
			numericInput(paste0(i,"_art_b"), label = i, value = "", min=0,max=999,step=0.01)
		})
	})

	observeEvent(input$single_elements_pairmatch, {
		temp <- single_reference_imported$single_reference_imported[single_reference_imported$single_reference_imported$Element == input$single_elements_pairmatch,]
		t1 <- temp[,c(1:6)]
		t2 <- temp[,-c(1:6)]
		single_ML$single_ML <- names(which(colSums(is.na(t2)) < nrow(t2)))
	})

	observeEvent(input$single_elements_association_a, {
		temp <- single_reference_imported$single_reference_imported[single_reference_imported$single_reference_imported$Element == input$single_elements_association_a,]
		t1 <- temp[,c(1:6)]
		t2 <- temp[,-c(1:6)]
		single_MLA$single_ML <- names(which(colSums(is.na(t2)) < nrow(t2)))
	})

	observeEvent(input$single_elements_association_b, {
		temp <- single_reference_imported$single_reference_imported[single_reference_imported$single_reference_imported$Element == input$single_elements_association_b,]
		t1 <- temp[,c(1:6)]
		t2 <- temp[,-c(1:6)]
		single_MLB$single_ML <- names(which(colSums(is.na(t2)) < nrow(t2)))
	})

})

observeEvent(input$proc, {
	showModal(modalDialog(title = "Window will update when finished.", easyClose = FALSE, footer = NULL))
	withProgress(message = 'Calculation has started', detail = '', value = 0, min=0, max=3, {
		if(input$single_analysis == "articulation") {
			temp1 <- which(art_elements$df == input$single_element_osr)
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

			if(is.na(single_input_art_a$single_input_art_a[1]) || is.na(single_input_art_b$single_input_art_b[1])) {removeModal();shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}

			sorta <- data.frame(id = input$ID1, Side = input$single_osr_side, Element = strsplit(input$single_element_osr, split = "-")[[1]][1], single_input_art_a$single_input_art_a, stringsAsFactors = FALSE)
			sortb <- data.frame(id = input$ID2, Side = input$single_osr_side, Element = strsplit(input$single_element_osr, split = "-")[[1]][2], single_input_art_b$single_input_art_b, stringsAsFactors = FALSE)
			incProgress(amount = 1, message = "sorting data")
			art.d1 <- art.input(side = input$single_osr_side, ref = single_reference_imported$single_reference_imported, sorta = sorta, sortb = sortb, bonea = strsplit(input$single_element_osr, split = "-")[[1]][1], boneb = strsplit(input$single_element_osr, split = "-")[[1]][2], measurementsa = tempa, measurementsb = tempb)
			incProgress(amount = 1, message = "running comparison")
			if(is.null(art.d1)) {removeModal();shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}
			d2 <- ttest(sorta = art.d1[[3]], sortb = art.d1[[4]], refa = art.d1[[1]], refb = art.d1[[2]], sessiontempdir = sessiontemp, alphalevel = common_alpha_level$common_alpha_level, reference = single_reference$single_reference, absolute = single_absolute_value$single_absolute_value, zmean = single_mean$single_mean, yeojohnson = single_yeojohnson$single_yeojohnson, tails = single_tails$single_tails)
			tempDF <- rbind(d2[[2]], d2[[3]])
		} else if(input$single_analysis == "pair-match") {
			single_input_list_left <- reactiveValues(single_input_list_left = c())
			lapply(single_ML$single_ML, function(i) {
				single_input_list_left$single_input_list_left <- c(single_input_list_left$single_input_list_left, input[[paste0(i,"_left")]])
			})
			single_input_list_right <- reactiveValues(single_input_list_right = c())
			lapply(single_ML$single_ML, function(i) {
				single_input_list_right$single_input_list_right <- c(single_input_list_right$single_input_list_right, input[[paste0(i,"_right")]])
			})
			single_input_list_left$single_input_list_left <- t(data.frame(single_input_list_left$single_input_list_left))
			colnames(single_input_list_left$single_input_list_left) <- single_ML$single_ML
			single_input_list_right$single_input_list_right <- t(data.frame(single_input_list_right$single_input_list_right))
			colnames(single_input_list_right$single_input_list_right) <- single_ML$single_ML

			for(x in 1:length(single_input_list_left$single_input_list_left)) {
				if(!is.na(single_input_list_left$single_input_list_left[x]) && !is.na(single_input_list_right$single_input_list_right[x])) {
					break #break if at least 1 pair is present
				} else if(x == length(single_input_list_left$single_input_list_left)) {
					removeModal();shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)
				}
			}

			sortleft <- data.frame(id = input$ID1, Side = "left", Element = input$single_elements_pairmatch, single_input_list_left$single_input_list_left, stringsAsFactors = FALSE)
			sortright <- data.frame(id = input$ID2, Side = "right", Element = input$single_elements_pairmatch, single_input_list_right$single_input_list_right, stringsAsFactors = FALSE)
			incProgress(amount = 1, message = "sorting data")
			pm.d1 <- pm.input(sort = rbind(sortleft, sortright), bone = input$single_elements_pairmatch, measurements = single_ML$single_ML, ref = single_reference_imported$single_reference_imported)
			if(is.null(pm.d1)) {removeModal();shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}
			incProgress(amount = 1, message = "running comparison")
			d2 <- ttest(sorta = pm.d1[[3]], sortb = pm.d1[[4]], refa = pm.d1[[1]], refb = pm.d1[[2]], sessiontempdir = sessiontemp, alphalevel = common_alpha_level$common_alpha_level, reference = single_reference$single_reference, absolute = single_absolute_value$single_absolute_value, zmean = single_mean$single_mean, yeojohnson = single_yeojohnson$single_yeojohnson, tails = single_tails$single_tails)
			tempDF <- rbind(d2[[2]], d2[[3]])
		} else if(input$single_analysis == "osr") {
			single_input_list_A <- reactiveValues(single_input_list_A = c())
			lapply(single_MLA$single_ML, function(i) {
				single_input_list_A$single_input_list_A <- c(single_input_list_A$single_input_list_A, input[[paste0(i,"_A")]])
			})

			single_input_list_B <- reactiveValues(single_input_listB = c())
			lapply(single_MLB$single_ML, function(i) {
				single_input_list_B$single_input_list_B <- c(single_input_list_B$single_input_list_B, input[[paste0(i,"_B")]])
			})
			single_input_list_A$single_input_list_A <- t(data.frame(single_input_list_A$single_input_list_A))
			single_input_list_B$single_input_list_B <- t(data.frame(single_input_list_B$single_input_list_B))
			colnames(single_input_list_A$single_input_list_A) <- single_MLA$single_ML
			colnames(single_input_list_B$single_input_list_B) <- single_MLB$single_ML

			if(all(is.na(single_input_list_A$single_input_list_A)) || all(is.na(single_input_list_B$single_input_list_B))) {removeModal();shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}

			sorta <- data.frame(id = input$ID1, Side = input$single_association_side_a, Element = input$single_elements_association_a, single_input_list_A$single_input_list_A, stringsAsFactors = FALSE)
			sortb <- data.frame(id = input$ID2, Side = input$single_association_side_b, Element = input$single_elements_association_b, single_input_list_B$single_input_list_B, stringsAsFactors = FALSE)
			incProgress(amount = 1, message = "sorting data")
			reg.d1 <- reg.input(sorta = sorta, sortb = sortb, sidea = input$single_association_side_a, sideb = input$single_association_side_b, bonea = input$single_elements_association_a, boneb = input$single_elements_association_b, measurementsa = single_MLA$single_ML, measurementsb = single_MLB$single_ML, ref = single_reference_imported$single_reference_imported)
			if(is.null(reg.d1)) {removeModal();shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}
			incProgress(amount = 1, message = "running comparison")
			d2 <- reg.test(refa = reg.d1[[1]], refb = reg.d1[[2]], sorta = reg.d1[[3]], sortb = reg.d1[[4]], sessiontempdir = sessiontemp, alphalevel = common_alpha_level$common_alpha_level, reference = single_reference$single_reference)
			tempDF <- rbind(d2[[2]], d2[[3]])
		}

		output$table2 <- DT::renderDataTable({
			DT::datatable(tempDF, selection = 'none', options = list(lengthMenu = c(1), pageLength = 1, dom = 't', ordering=F), rownames = FALSE)
		})

		direc <- d2[[1]]
		sd <- paste(sessiontemp,direc,sep="/")

		nimages <- list.files(sd)
		nimages <- paste(sessiontemp, "/", direc, "/", nimages[grep(".jpg", nimages)], sep="")

		output$single_plot <- renderImage({
			list(src = nimages,
				contentType = 'image/jpg',
				height = 400,
				alt = ""
			)
		}, deleteFile = FALSE)

		files <- list.files(sd, recursive = TRUE, full.names=TRUE)
		zip:::zipr(zipfile = paste(sd,"/",direc,'.zip',sep=''), files = files)
		output$downloadData2 <- downloadHandler(
			filename = function() {
				paste("results.zip")
			},
			content = function(file) {
				file.copy(paste(sd,"/",direc,'.zip',sep=''), file)  
			},
			contentType = "application/zip"
		)
		
		gc()
		removeModal()
		incProgress(amount = 1, message = "Completed")
	})
})
