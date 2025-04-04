output$resettableInput <- renderUI({
	input$clearFile1
	input$uploadFormat
	fileInput('file1', 'Upload measurements', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
})

observeEvent(input$clearFile1, {
	fileInput('file1', 'Upload measurements', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
})

output$multiple_contents <- renderUI({
	HTML(paste(""))
})

multiple_common_alpha_level <- reactiveValues(multiple_common_alpha_level = 0.1) 
output$multiple_common_alpha_level <- renderUI({
	sliderInput(inputId = "multiple_common_alpha_level", label = "Alpha level", min=0.01, max=1, value=0.1, step = 0.01)
})
observeEvent(input$multiple_common_alpha_level, {
	multiple_common_alpha_level$multiple_common_alpha_level <- input$multiple_common_alpha_level
})

multiple_absolute_value <- reactiveValues(multiple_absolute_value = FALSE) 
output$multiple_absolute_value <- renderUI({
	checkboxInput(inputId = "multiple_absolute_value", label = "Absolute D-value |a-b|", value = FALSE)
})
observeEvent(input$multiple_absolute_value, {
	multiple_absolute_value$multiple_absolute_value <- input$multiple_absolute_value
})

multiple_yeojohnson <- reactiveValues(multiple_yeojohnson = FALSE) 
output$multiple_yeojohnson <- renderUI({
	checkboxInput(inputId = "multiple_yeojohnson", label = "Yeojohnson transformation", value = FALSE)
})
observeEvent(input$multiple_yeojohnson, {
	multiple_yeojohnson$multiple_yeojohnson <- input$multiple_yeojohnson
})

multiple_mean <- reactiveValues(multiple_mean = FALSE) 
output$multiple_mean <- renderUI({
	checkboxInput(inputId = "multiple_mean", label = "Zero mean", value = FALSE)
})
observeEvent(input$multiple_mean, {
	multiple_mean$multiple_mean <- input$multiple_mean
})

multiple_tails <- reactiveValues(multiple_tails = 2) 
output$multiple_tails <- renderUI({
	radioButtons(inputId = "multiple_tails", label = "Tails", choices = list(1, 2), selected = 2, inline = TRUE)
})
observeEvent(input$multiple_tails, {
	multiple_tails$multiple_tails <- as.numeric(input$multiple_tails)
})

multiple_analysis <- reactiveValues(multiple_analysis = "pair-match")
observeEvent(input$multiple_analysis, {
	multiple_analysis$multiple_analysis <- input$multiple_analysis
})
output$multiple_analysis <- renderUI({
	selectInput(inputId = "multiple_analysis", label = "Analysis", choices = c("pair-match","articulation","osr"), selected = "pair-match")
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

multiple_art_elements <- reactiveValues(df = c())
multiple_art_measurements_a <- reactiveValues(df = c())
multiple_art_measurements_b <- reactiveValues(df = c())
multiple_ML <- reactiveValues(multiple_ML = c("temp"))
multiple_MLB <- reactiveValues(multiple_ML = c("temp"))
multiple_MLA <- reactiveValues(multiple_ML = c("temp"))

observeEvent(input$multiple_reference, {
	multiple_reference_imported$multiple_reference_imported <- reference_list$reference_list[[multiple_reference$multiple_reference]]
	elements$elements <- unique(multiple_reference_imported$multiple_reference_imported$Element)
	art <- config_df$config_df[config_df$config_df$Method == 'articulation',]
	ref_col_names <- colnames(multiple_reference_imported$multiple_reference_imported)
	multiple_art_elements$df <- NULL
	multiple_art_measurements_a$df <- NULL
	multiple_art_measurements_b$df <- NULL
	for(i in 1:nrow(art)) {
		a = FALSE
		b = FALSE
		for(x in 1:length(ref_col_names)) {
			if(art$Measurementa[i] == ref_col_names[x]) {a=TRUE}
			if(art$Measurementb[i] == ref_col_names[x]) {b=TRUE}
			if(a && b) {
				multiple_art_measurements_a$df <- na.omit(c(multiple_art_measurements_a$df, art$Measurementa[i]))
				multiple_art_measurements_b$df <- na.omit(c(multiple_art_measurements_b$df, art$Measurementb[i]))
				temp1 <- na.omit(unique(multiple_reference_imported$multiple_reference_imported[!is.na(multiple_reference_imported$multiple_reference_imported[[art$Measurementa[i]]]),]$Element))[1]
				temp2 <- na.omit(unique(multiple_reference_imported$multiple_reference_imported[!is.na(multiple_reference_imported$multiple_reference_imported[[art$Measurementb[i]]]),]$Element))[1]
				if(!is.na(temp1) && !is.na(temp2)) {
					cf <- function(a, b) {
						for(t in 1:length(a)) {
							if(a[t] == b) {return(TRUE)}
						}
						return(FALSE)
					}
					n <- 0
					temp3 <- paste(temp1, temp2, sep="-")
					if(!is.null(multiple_art_elements$df)) {
						while(cf(multiple_art_elements$df, temp3)) {
							n <- n + 1
							temp3 <- paste(temp1, temp2,n+1, sep="-")
						}
					}
					multiple_art_elements$df <- c(multiple_art_elements$df, temp3)
				}
				break
			}
		}
	}

	output$multiple_measurements_non_antimere_a <- renderUI({
		temp1 <- which(multiple_art_elements$df == input$multiple_element_non_antimere)
		tempa <- multiple_art_measurements_a$df[temp1][!duplicated(multiple_art_measurements_a$df[temp1])]
		selectizeInput(inputId = "multiple_measurements_non_antimere_a", label = "", choices = c(tempa), multiple = TRUE, selected = c(tempa))
	})
	
	output$multiple_measurements_non_antimere_b <- renderUI({
		temp1 <- which(multiple_art_elements$df == input$multiple_element_non_antimere)
		tempa <- multiple_art_measurements_b$df[temp1][!duplicated(multiple_art_measurements_b$df[temp1])]
		selectizeInput(inputId = "multiple_measurements_non_antimere_b", label = "", choices = c(tempa), multiple = TRUE, selected = c(tempa))
	})

	output$multiple_measurement_antimere <- renderUI({
		selectizeInput(inputId = "multiple_measurement_antimere", label = "Measurements", choices = c(multiple_ML$multiple_ML), multiple = TRUE, selected = c(multiple_ML$multiple_ML))
	})

	output$multiple_measurement_association_a <- renderUI({
		selectizeInput(inputId = "multiple_measurement_association_a", label = "", choices = c(multiple_MLA$multiple_ML), multiple = TRUE, selected = c(multiple_MLA$multiple_ML))
	})
	output$multiple_measurement_association_b <- renderUI({
		selectizeInput(inputId = "multiple_measurement_association_b", label = "", choices = c(multiple_MLB$multiple_ML), multiple = TRUE, selected = c(multiple_MLB$multiple_ML))
	})

	output$multiple_element_non_antimere <- renderUI({
		selectInput(inputId = "multiple_element_non_antimere", label = "Elements", choices = multiple_art_elements$df)
	})

	output$multiple_elements_association_a <- renderUI({
		selectInput(inputId = "multiple_elements_association_a", label = "Independent", choices = elements$elements)
	})

	observeEvent(input$multiple_elements_association_a, {
		output$multiple_elements_association_b <- renderUI({
			selectInput(inputId = "multiple_elements_association_b", label = "Dependent", choices = elements$elements[elements$elements != input$multiple_elements_association_a])
		})
	})

	observeEvent(input$multiple_elements_association_a, {
		temp <- multiple_reference_imported$multiple_reference_imported[multiple_reference_imported$multiple_reference_imported$Element == input$multiple_elements_association_a,]
		t1 <- temp[,c(1:6)]
		t2 <- temp[,-c(1:6)]
		multiple_MLA$multiple_ML <- names(which(colSums(is.na(t2)) < nrow(t2)))
	})

	observeEvent(input$multiple_elements_association_b, {
		temp <- multiple_reference_imported$multiple_reference_imported[multiple_reference_imported$multiple_reference_imported$Element == input$multiple_elements_association_b,]
		t1 <- temp[,c(1:6)]
		t2 <- temp[,-c(1:6)]
		multiple_MLB$multiple_ML <- names(which(colSums(is.na(t2)) < nrow(t2)))
	})

	observeEvent(input$multiple_elements_pairmatch, {
		temp <- multiple_reference_imported$multiple_reference_imported[multiple_reference_imported$multiple_reference_imported$Element == input$multiple_elements_pairmatch,]
		t1 <- temp[,c(1:6)]
		t2 <- temp[,-c(1:6)]
		multiple_ML$multiple_ML <- names(which(colSums(is.na(t2)) < nrow(t2)))
	})
})

observeEvent(input$pro, {
	showModal(modalDialog(title = "Window will update when finished.", easyClose = FALSE, footer = NULL))
	withProgress(message = 'Calculation has started', detail = '', value = 0, min = 0, max = 3, {

		#Upload CSV file
		inFile <- input$file1

		 #return null if not uploaded
		if (is.null(inFile)) {
			removeModal()
			shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss")
			return(NULL)
		}
		#return null if empty file
		if (!file.size(inFile$datapath) > 1) {
			removeModal()
			shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss")
			return(NULL)
		}
		#added quote
		tempdata1 <- read.csv(inFile$datapath, header=TRUE, sep=",", na.strings=c("", " ", "NA"), quote="\"")## see na.strings forces NA for blanks, spaces, etc

		cnam <- colnames(tempdata1)
		if(cnam[1] == "se_id") { #detect cora input
			cora_data <- tempdata1 #save data for later cora output
			tempdata1 <- tempdata1[,-c(1,3,4,5,6)] #Remove excess columns
			tempdata1 <- tempdata1[,c(1,3,2,4:ncol(tempdata1))] #rearrange column order
			colnames(tempdata1) <- c("id","Side","Element",cnam[-c(1:8)]) #rename columns
		}

		#checks if measurements are numeric and converts alpha characters to numeric   
		tempdataa <- tempdata1[,1:3]
		tempdatab <- lapply(tempdata1[,-(1:3)], function(x) { as.numeric(as.character(x))})
		tempdata1 <- c(tempdataa, tempdatab)
		tempdata1 <- as.data.frame(tempdata1) #combines first three columns with now numeric measurements


		if(input$multiple_analysis == "articulation") {
			if(is.null(input$multiple_measurements_non_antimere_a) || is.null(input$multiple_measurements_non_antimere_b)) {removeModal();shinyalert(title = "ERROR!", text="The measurement data is missing",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}
			temp1 <- which(multiple_art_elements$df == input$multiple_element_non_antimere)
			tempa <- multiple_art_measurements_a$df[temp1][!duplicated(multiple_art_measurements_a$df[temp1])]
			tempb <- multiple_art_measurements_b$df[temp1][!duplicated(multiple_art_measurements_b$df[temp1])]
			tempdata1$Element <- tolower(tempdata1$Element)
			sorta = tempdata1[tempdata1$Element == strsplit(input$multiple_element_non_antimere, split = "-")[[1]][1],]
			sortb = tempdata1[tempdata1$Element == strsplit(input$multiple_element_non_antimere, split = "-")[[1]][2],]
			measa <- input$multiple_measurements_non_antimere_a
			measb <- input$multiple_measurements_non_antimere_b
			incProgress(amount = 1, message = "sorting data")
			art.d1 <- art.input(side = input$multiple_non_antimere_side, ref = multiple_reference_imported$multiple_reference_imported, sorta = sorta, sortb = sortb, bonea = strsplit(input$multiple_element_non_antimere, split = "-")[[1]][1], boneb = strsplit(input$multiple_element_non_antimere, split = "-")[[1]][2], measurementsa = measa, measurementsb = measb)
			if(is.null(art.d1)) {removeModal();shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}
			incProgress(amount = 1, message = "running comparisons")
			d2 <- ttest(sorta = art.d1[[3]], sortb = art.d1[[4]], refa = art.d1[[1]], refb = art.d1[[2]], sessiontempdir = sessiontemp, alphalevel = multiple_common_alpha_level$multiple_common_alpha_level, absolute = multiple_absolute_value$multiple_absolute_value, zmean = multiple_mean$multiple_mean, yeojohnson = multiple_yeojohnson$multiple_yeojohnson, tails = multiple_tails$multiple_tails, reference = multiple_reference$multiple_reference)
			tempDF <- rbind(d2[[2]], d2[[3]]) #combines excluded and not excluded for results	
		} else if(input$multiple_analysis == "pair-match") {
			if(is.null(input$multiple_measurement_antimere)) {removeModal();shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}
			meas <- input$multiple_measurement_antimere
			incProgress(amount = 1, message = "sorting data")			
			pm.d1 <- pm.input(sort = tempdata1, bone = input$multiple_elements_pairmatch, measurements = meas, ref = multiple_reference_imported$multiple_reference_imported)
			if(is.null(pm.d1)) {removeModal();shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}
			incProgress(amount = 1, message = "running comparisons")
			d2 <- ttest(sorta = pm.d1[[3]], sortb = pm.d1[[4]], refa = pm.d1[[1]], refb = pm.d1[[2]], sessiontempdir = sessiontemp, alphalevel = multiple_common_alpha_level$multiple_common_alpha_level, absolute = multiple_absolute_value$multiple_absolute_value, zmean = multiple_mean$multiple_mean, yeojohnson = multiple_yeojohnson$multiple_yeojohnson, tails = multiple_tails$multiple_tails, reference = multiple_reference$multiple_reference)
			tempDF <- rbind(d2[[2]], d2[[3]]) #combines excluded and not excluded for results
		} else if(input$multiple_analysis == "osr") {
			if(is.null(input$multiple_measurement_association_a) || is.null(input$multiple_measurement_association_b)) {removeModal();shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}
			tempdata1$Element <- tolower(tempdata1$Element)
			sorta = tempdata1[tempdata1$Element == input$multiple_elements_association_a,]
			sortb = tempdata1[tempdata1$Element == input$multiple_elements_association_b,]
			measa <- input$multiple_measurement_association_a
			measb <- input$multiple_measurement_association_b
			incProgress(amount = 1, message = "sorting data")
			reg.d1 <- reg.input(sorta = sorta, sortb = sortb, sidea = input$multiple_association_side_a, sideb = input$multiple_association_side_b, bonea = input$multiple_elements_association_a, boneb = input$multiple_elements_association_b, measurementsa = measa, measurementsb = measb, ref = multiple_reference_imported$multiple_reference_imported)
			if(is.null(reg.d1)) {removeModal();shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}
			incProgress(amount = 1, message = "running comparisons")
			d2 <- reg.test(refa = reg.d1[[1]], refb = reg.d1[[2]], sorta = reg.d1[[3]], sortb = reg.d1[[4]], sessiontempdir = sessiontemp, alphalevel = multiple_common_alpha_level$multiple_common_alpha_level, reference = multiple_reference$multiple_reference)
			tempDF <- rbind(d2[[2]], d2[[3]]) #combines excluded and not excluded for results
		}
		#if combinations exist, produces output
		if(!all(is.na(d2[[2]])) || !all(is.na(d2[[3]]))) {
			direc <- d2[[1]]
			ll <- nrow(d2[[2]]) + nrow(d2[[3]])
			nmatch <- nrow(d2[[2]])
			samplesize <- length(unique(c(d2[[2]][,1], d2[[2]][,4], d2[[3]][,1], d2[[3]][,4])))
			t_time <- d2[[4]]
			output$multiple_contents <- renderUI({
				HTML(paste("<strong>",
							"Completed in: ", "<font color=\"#00688B\">", t_time, " seconds</font>", 
							"<br/>","Comparisons: ","<font color=\"#00688B\">", ll, "</font>", 
							"<br/>", "Specimens: ","<font color=\"#00688B\">",samplesize, "</font>", 
							'<br/>', "Potential matches: ","<font color=\"#00688B\">",nmatch , "</font>",
							'<br/>', "Exclusions: ","<font color=\"#00688B\">",ll - nmatch, " (", round((ll - nmatch) / ll, digits = 3) * 100, "%)",  "</font>",
							'<br/>', "Rejected: ","<font color=\"#00688B\">",nrow(d2[[5]]),"</font>",
							'</strong>'))
			})
		}
		output$table <- DT::renderDataTable({
			DT::datatable(d2[[2]], selection = list(mode="multiple"), options = list(lengthMenu = c(5,10,15,20,25,30), search = list(regex = TRUE, caseInsensitive = FALSE), pageLength = 10), rownames = FALSE)
		})
		output$tablen <- DT::renderDataTable({
			DT::datatable(d2[[3]], selection = list(mode="multiple"), options = list(lengthMenu = c(5,10,15,20,25,30), search = list(regex = TRUE, caseInsensitive = FALSE), pageLength = 10), rownames = FALSE)
		})
		output$tablenr <- DT::renderDataTable({
			DT::datatable(d2[[5]], selection = list(mode="multiple"), options = list(lengthMenu = c(5,10,15,20,25,30), search = list(regex = TRUE, caseInsensitive = FALSE), pageLength = 10), rownames = FALSE)
		})

		sd <- paste(sessiontemp,direc,sep="/")
		nimages <- list.files(sd)
		output$downloadData <- downloadHandler(
			filename <- function() {
			paste("results.zip")
			},
			content = function(file) {
				file.remove(paste(sd,"/",direc,'.zip',sep=''))
				if(is.numeric(input$tablen_rows_selected)) {
					no_return_value <- output_function(d2[[3]][input$tablen_rows_selected,], method="exclusion", type="csv2", fpath=sd)
				} else {file.remove(paste(sd,"excluded-selected-list.csv",sep="/"))}
				if(is.numeric(input$table_rows_selected)) {
					no_return_value <- output_function(d2[[2]][input$table_rows_selected,], method="exclusion", type="csv2", fpath=sd)
				} else {file.remove(paste(sd,"not-excluded-selected-list.csv",sep="/"))}
				if(is.numeric(input$tablenr_rows_selected)) {
					no_return_value <- output_function(rejected = d2[[5]][input$tablenr_rows_selected,], method="exclusion", type="csv2", fpath=sd)
				} else {file.remove(paste(sd,"rejected-selected-list.csv",sep="/"))}
				if(is.numeric(input$table_rows_selected) && is.numeric(input$tablen_rows_selected) && cnam[1] == "se_id") {
					hera1 <- list(rbind(d2[[2]][input$table_rows_selected,], d2[[3]][input$tablen_rows_selected,]),d2[[6]],d2[[7]])
					no_return_value <- output_function(hera1, method="exclusion", type="cora", cora_data = cora_data, options = c(multiple_reference$multiple_reference, alphalevel = multiple_common_alpha_level$multiple_common_alpha_level, absolute = multiple_absolute_value$multiple_absolute_value, zmean = multiple_mean$multiple_mean, yeojohnson = multiple_yeojohnson$multiple_yeojohnson, tails = multiple_tails$multiple_tails), fpath=sd)
				}
				if(is.numeric(input$table_rows_selected) && !is.numeric(input$tablen_rows_selected) && cnam[1] == "se_id") {
					hera1 <- list(rbind(d2[[2]][input$table_rows_selected,], d2[[3]]),d2[[6]],d2[[7]])
					no_return_value <- output_function(hera1, method="exclusion", type="cora", cora_data = cora_data, options = c(multiple_reference$multiple_reference, alphalevel = multiple_common_alpha_level$multiple_common_alpha_level, absolute = multiple_absolute_value$multiple_absolute_value, zmean = multiple_mean$multiple_mean, yeojohnson = multiple_yeojohnson$multiple_yeojohnson, tails = multiple_tails$multiple_tails), fpath=sd)
				}
				if(!is.numeric(input$table_rows_selected) && is.numeric(input$tablen_rows_selected) && cnam[1] == "se_id") {
					hera1 <- list(rbind(d2[[2]], d2[[3]][input$tablen_rows_selected,]),d2[[6]],d2[[7]])
					no_return_value <- output_function(hera1, method="exclusion", type="cora", cora_data = cora_data, options = c(multiple_reference$multiple_reference, alphalevel = multiple_common_alpha_level$multiple_common_alpha_level, absolute = multiple_absolute_value$multiple_absolute_value, zmean = multiple_mean$multiple_mean, yeojohnson = multiple_yeojohnson$multiple_yeojohnson, tails = multiple_tails$multiple_tails), fpath=sd)
				}
				if(!is.numeric(input$table_rows_selected) && !is.numeric(input$tablen_rows_selected) && cnam[1] == "se_id") {
					hera1 <- list(rbind(d2[[2]], d2[[3]]),d2[[6]],d2[[7]])
					no_return_value <- output_function(hera1, method="exclusion", type="cora", cora_data = cora_data, options = c(multiple_reference$multiple_reference, alphalevel = multiple_common_alpha_level$multiple_common_alpha_level, absolute = multiple_absolute_value$multiple_absolute_value, zmean = multiple_mean$multiple_mean, yeojohnson = multiple_yeojohnson$multiple_yeojohnson, tails = multiple_tails$multiple_tails), fpath=sd)
				}
				files <- list.files(sd, recursive = TRUE, full.names=TRUE)
				zip:::zipr(zipfile = paste(sd,"/",direc,'.zip',sep=''), files = files[1], compression = 1)
				for(file_na in files[-1]) {
					zip:::zipr_append(zipfile = paste(sd,"/",direc,'.zip',sep=''), files = file_na, compression = 1)
				}
				file.copy(paste(sd,"/",direc,'.zip',sep=''), file)
			},
			contentType = "application/zip"
		)
		gc()
		removeModal()
		incProgress(amount = 1, message = "Completed")
	})
})
