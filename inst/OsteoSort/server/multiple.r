forcefun <- function(hera1) {
	df1 <- as.data.frame(cbind(from_id = hera1[,1], to_id = hera1[,4], Probability = hera1[,8], Element = paste(hera1[,3], hera1[,2],sep='-')))
	df2 <- as.data.frame(cbind(from_id = hera1[,4], to_id = hera1[,1], Probability = hera1[,8], Element = paste(hera1[,6], hera1[,5],sep='-')))
	df <- rbind(df1, df2)
	temp <- df[!duplicated(df[,1]),c(1,4)]
	colnames(temp) <- c("name", "group")
	nodes <- temp
	colnames(df) <- c("source", "target", "value", "group")
	df <- df[,c(1:3)]
	for(i in 1:nrow(nodes)) {
		df[df$source == nodes[i,1],1] <- i-1
		df[df$target == nodes[i,1],2] <- i-1
	}
	links <- df
	return(list(links,nodes))
}

output$resettableInput <- renderUI({
	input$clearFile1
	input$uploadFormat
	fileInput('file1', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
})

observeEvent(input$clearFile1, {
	fileInput('file1', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
})

output$multiple_contents <- renderUI({
	HTML(paste(""))
})

multiple_file_output1 <- reactiveValues(multiple_file_output1 = TRUE) 
output$multiple_file_output1 <- renderUI({
	checkboxInput(inputId = "multiple_file_output1", label = "Output csv file", value = TRUE)
})
observeEvent(input$multiple_file_output1, {
	multiple_file_output1$multiple_file_output1 <- input$multiple_file_output1
})

multiple_file_output_graph <- reactiveValues(multiple_file_output_graph = TRUE) 
output$multiple_file_output_graph <- renderUI({
	checkboxInput(inputId = "multiple_file_output_graph", label = "Output network graph", value = TRUE)
})
observeEvent(input$multiple_file_output_graph, {
	multiple_file_output_graph$multiple_file_output_graph <- input$multiple_file_output_graph
})

labtf <- reactiveValues(labtf = TRUE) 
output$labtf <- renderUI({
	checkboxInput(inputId = "labtf", label = "Network graph labels", value = TRUE)
})
observeEvent(input$labtf, {
	labtf$labtf <- input$labtf
})

forc <- reactiveValues(forc = TRUE) 
output$forc <- renderUI({
	checkboxInput(inputId = "forc", label = "Interactive network graph", value = TRUE)
})
observeEvent(input$forc, {
	forc$forc <- input$forc
})

multiple_common_alpha_level <- reactiveValues(multiple_common_alpha_level = 0.05) 
output$multiple_common_alpha_level <- renderUI({
	sliderInput(inputId = "multiple_common_alpha_level", label = "Alpha level", min=0.01, max=1, value=0.05, step = 0.01)
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

multiple_boxcox <- reactiveValues(multiple_boxcox = FALSE) 
output$multiple_boxcox <- renderUI({
	checkboxInput(inputId = "multiple_boxcox", label = "Boxcox transformation", value = FALSE)
})
observeEvent(input$multiple_boxcox, {
	multiple_boxcox$multiple_boxcox <- input$multiple_boxcox
})

multiple_mean <- reactiveValues(multiple_mean = FALSE) 
output$multiple_mean <- renderUI({
	checkboxInput(inputId = "multiple_mean", label = "Zero mean", value = FALSE)
})
observeEvent(input$multiple_mean, {
	multiple_mean$multiple_mean <- input$multiple_mean
})

multiple_ztransform <- reactiveValues(multiple_ztransform = FALSE) 
output$multiple_ztransform <- renderUI({
	checkboxInput(inputId = "multiple_ztransform", label = "Z-transform", value = FALSE)
})
observeEvent(input$multiple_ztransform, {
	multiple_ztransform$multiple_ztransform <- input$multiple_ztransform
})

multiple_tails <- reactiveValues(multiple_tails = 2) 
output$multiple_tails <- renderUI({
	sliderInput(inputId = "multiple_tails", label = "Tails", min=1, max=2, value=2, step=1)
})
observeEvent(input$multiple_tails, {
	multiple_tails$multiple_tails <- input$multiple_tails
})

numbercoresglobal <- reactiveValues(ncore = 1)
observeEvent(input$numbercores, {
	numbercoresglobal$ncore <- input$numbercores
})
output$ncores <- renderUI({
	sliderInput(inputId = "numbercores", label = "Number of cores", min=1, max=detectCores(), value=1, step =1)
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

multiple_art_elements <- reactiveValues(df = c())
multiple_art_measurements_a <- reactiveValues(df = c())
multiple_art_measurements_b <- reactiveValues(df = c())
multiple_ML <- reactiveValues(multiple_ML = c("temp"))
multiple_MLB <- reactiveValues(multiple_ML = c("temp"))
multiple_MLA <- reactiveValues(multiple_ML = c("temp"))

observeEvent(input$multiple_reference, {
	multiple_reference_imported$multiple_reference_imported <- reference_list$reference_list[[multiple_reference$multiple_reference]]
	elements$elements <- unique(multiple_reference_imported$multiple_reference_imported$Element)
	art <- config_df$config_df[config_df$config_df$Method == 'Non_antimere_t-test',]
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
	showModal(modalDialog(title = "Calculation has started...Window will update when finished.", easyClose = FALSE, footer = NULL))
	withProgress(message = 'Calculation has started', detail = '', value = 0, min = 0, max = 3, {

		#Upload CSV file
		inFile <- input$file1

		 #return null if not uploaded
		if (is.null(inFile)) {
			removeModal()
			shinyalert(title = "ERROR!", text="Task failed successfully",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss")
			return(NULL)
		}
		#return null if empty file
		if (!file.size(inFile$datapath) > 1) {
			removeModal()
			shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss")
			return(NULL)
		}

		tempdata1 <- read.csv(inFile$datapath, header=TRUE, sep=",", na.strings=c("", " ", "NA"))## see na.strings forces NA for blanks, spaces, etc
		#checks if measurements are numeric and converts alpha characters to numeric   
		tempdataa <- tempdata1[,1:3]
		tempdatab <- lapply(tempdata1[,-(1:3)], function(x) { as.numeric(as.character(x))})
		tempdata1 <- c(tempdataa, tempdatab)
		tempdata1 <- as.data.frame(tempdata1) #combines first four columns with now numeric measurements
		if(input$multiple_analysis == "Non-Antimere t-test") {
			if(is.null(input$multiple_measurements_non_antimere_a) || is.null(input$multiple_measurements_non_antimere_b)) {removeModal();shinyalert(title = "ERROR!", text="You forgot to enter measurement data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}
			temp1 <- which(multiple_art_elements$df == input$multiple_element_non_antimere)
			tempa <- multiple_art_measurements_a$df[temp1][!duplicated(multiple_art_measurements_a$df[temp1])]
			tempb <- multiple_art_measurements_b$df[temp1][!duplicated(multiple_art_measurements_b$df[temp1])]
			tempdata1$Element <- tolower(tempdata1$Element)
			sorta = tempdata1[tempdata1$Element == strsplit(input$multiple_element_non_antimere, split = "-")[[1]][1],]
			sortb = tempdata1[tempdata1$Element == strsplit(input$multiple_element_non_antimere, split = "-")[[1]][2],]
			measa <- input$multiple_measurements_non_antimere_a
			measb <- input$multiple_measurements_non_antimere_b
			incProgress(amount = 1, message = "Non-antimere t-test: sorting data")
			art.d1 <- art.input(side = input$multiple_non_antimere_side, ref = multiple_reference_imported$multiple_reference_imported, sorta = sorta, sortb = sortb, bonea = strsplit(input$multiple_element_non_antimere, split = "-")[[1]][1], boneb = strsplit(input$multiple_element_non_antimere, split = "-")[[1]][2], measurementsa = measa, measurementsb = measb)
			if(is.null(art.d1)) {removeModal();shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}
			incProgress(amount = 1, message = "Non-antimere t-test: running comparisons")
			d2 <- ttest(labtf = labtf$labtf, ztest = FALSE, sorta = art.d1[[3]], sortb = art.d1[[4]], refa = art.d1[[1]], refb = art.d1[[2]], sessiontempdir = sessiontemp, alphalevel = multiple_common_alpha_level$multiple_common_alpha_level, absolute = multiple_absolute_value$multiple_absolute_value, zmean = multiple_mean$multiple_mean, boxcox = multiple_boxcox$multiple_boxcox, tails = multiple_tails$multiple_tails, output_options = c(multiple_file_output1$multiple_file_output1, FALSE, multiple_file_output_graph$multiple_file_output_graph), threads = numbercoresglobal$ncore)
			tempDF <- rbind(d2[[2]], d2[[3]]) #combines excluded and not excluded for results	
		} else if(input$multiple_analysis == "Antimere t-test") {
			if(is.null(input$multiple_measurement_antimere)) {removeModal();shinyalert(title = "ERROR!", text="Sorry.",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}
			meas <- input$multiple_measurement_antimere
			incProgress(amount = 1, message = "Antimere t-test: sorting data")			
			pm.d1 <- pm.input(sort = tempdata1, bone = input$multiple_elements_pairmatch, measurements = meas, ref = multiple_reference_imported$multiple_reference_imported)
			if(is.null(pm.d1)) {removeModal();shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}
			incProgress(amount = 1, message = "Antimere t-test: running comparisons")
			d2 <- ttest(labtf = labtf$labtf, ztest = multiple_ztransform$multiple_ztransform, sorta = pm.d1[[3]], sortb = pm.d1[[4]], refa = pm.d1[[1]], refb = pm.d1[[2]], sessiontempdir = sessiontemp, alphalevel = multiple_common_alpha_level$multiple_common_alpha_level, absolute = multiple_absolute_value$multiple_absolute_value, zmean = multiple_mean$multiple_mean, boxcox = multiple_boxcox$multiple_boxcox, tails = multiple_tails$multiple_tails, output_options = c(multiple_file_output1$multiple_file_output1, FALSE, multiple_file_output_graph$multiple_file_output_graph), threads = numbercoresglobal$ncore)
			tempDF <- rbind(d2[[2]], d2[[3]]) #combines excluded and not excluded for results
		} else if(input$multiple_analysis == "Non-Antimere regression") {
			if(is.null(input$multiple_measurement_association_a) || is.null(input$multiple_measurement_association_b)) {removeModal();shinyalert(title = "ERROR!", text="Nope.",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}
			tempdata1$Element <- tolower(tempdata1$Element)
			sorta = tempdata1[tempdata1$Element == input$multiple_elements_association_a,]
			sortb = tempdata1[tempdata1$Element == input$multiple_elements_association_b,]
			measa <- input$multiple_measurement_association_a
			measb <- input$multiple_measurement_association_b
			incProgress(amount = 1, message = "Non-antimere regression: sorting data")
			reg.d1 <- reg.input(sorta = sorta, sortb = sortb, sidea = input$multiple_association_side_a, sideb = input$multiple_association_side_b, bonea = input$multiple_elements_association_a, boneb = input$multiple_elements_association_b, measurementsa = measa, measurementsb = measb, ref = multiple_reference_imported$multiple_reference_imported)
			if(is.null(reg.d1)) {removeModal();shinyalert(title = "ERROR!", text="There was an error with the input and/or reference data",type = "error", closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText="Dismiss");return(NULL)}
			incProgress(amount = 1, message = "Non-antimere regression: running comparisons")
			d2 <- reg.test(labtf = labtf$labtf, threads = numbercoresglobal$ncore, ztest = multiple_ztransform$multiple_ztransform, refa = reg.d1[[1]], refb = reg.d1[[2]], sorta = reg.d1[[3]], sortb = reg.d1[[4]], sessiontempdir = sessiontemp, alphalevel = multiple_common_alpha_level$multiple_common_alpha_level, output_options = c(multiple_file_output1$multiple_file_output1, FALSE, multiple_file_output_graph$multiple_file_output_graph))
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
							"Completed in: ", "<font color=\"#00688B\">", t_time, " minutes</font>", 
							"<br/>","Comparisons: ","<font color=\"#00688B\">", ll, "</font>", 
							"<br/>", "Specimens: ","<font color=\"#00688B\">",samplesize, "</font>", 
							'<br/>', "Potential matches: ","<font color=\"#00688B\">",nmatch , "</font>",
							'<br/>', "Exclusions: ","<font color=\"#00688B\">",ll - nmatch, " (", round((ll - nmatch) / ll, digits = 3) * 100, "%)",  "</font>",
							'<br/>', "Rejected: ","<font color=\"#00688B\">",nrow(d2[[5]]),"</font>",
							'</strong>'))
			})
		}
		output$table <- DT::renderDataTable({
			DT::datatable(d2[[2]], selection = list(mode="multiple"), options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
		})
		output$tablen <- DT::renderDataTable({
			DT::datatable(d2[[3]], selection = list(mode="multiple"), options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
		})
		output$tablenr <- DT::renderDataTable({
			DT::datatable(d2[[5]], selection = list(mode="multiple"), options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
		})
		if(multiple_file_output1$multiple_file_output1 || multiple_file_output_graph$multiple_file_output_graph) {
			setwd(direc)
			nimages <- list.files()
			if(forc$forc) {
				if(nrow(d2[[2]]) > 1){
					td <- forcefun(d2[[2]])
					links <- td[[1]]
					nodes <- td[[2]]
					output$forceNetworkOSM <- renderForceNetwork({
						forceNetwork(Links = links, Nodes = nodes,
								  Source = "source", Target = "target",
								  Value = "value", NodeID = "name",
								  Group = "group", opacity = 1,
									colourScale = JS('d3.scaleOrdinal().domain(["1", "2"]).range(["#ea6011","#126a8f"])'),
									zoom = TRUE
						)
					})
				}
			}

			if(multiple_file_output_graph$multiple_file_output_graph && length(nimages[grep(".jpg", nimages)]) != 0) {
				nimages <- paste(sessiontemp, "/", direc, "/", nimages[grep(".jpg", nimages)], sep="")
			} else {
				nimages <- system.file("OsteoSort/www", 'blank.jpg', package = "OsteoSort")
			}
			output$multiple_plot_na <- renderImage({
				list(src = nimages,
					contentType = 'image/jpg',
					height = 800,
					alt = "A"
				)
			}, deleteFile = FALSE)
			setwd(sessiontemp)
			output$downloadData <- downloadHandler(
				filename <- function() {
				paste("results.zip")
				},
				content = function(file) {
					setwd(direc)
					file.remove(paste(direc,'.zip',sep=''))
					if(is.numeric(input$tablen_rows_selected)) {
						no_return_value <- OsteoSort:::output_function(d2[[3]][input$tablen_rows_selected,], method="exclusion", type="csv2")
					} else {file.remove("excluded-selected-list.csv")}
					if(is.numeric(input$table_rows_selected)) {
						no_return_value <- OsteoSort:::output_function(d2[[2]][input$table_rows_selected,], method="exclusion", type="csv2")
					} else {file.remove("not-excluded-selected-list.csv")}
					if(is.numeric(input$tablenr_rows_selected)) {
						no_return_value <- OsteoSort:::output_function(rejected = d2[[5]][input$tablenr_rows_selected,], method="exclusion", type="csv2")
					} else {file.remove("rejected-selected-list.csv")}
					setwd(sessiontemp)
					files <- list.files(direc, recursive = TRUE)
					setwd(direc)
					zip:::zipr(zipfile = paste(direc,'.zip',sep=''), files = files[1], compression = 1)
					for(file_na in files[-1]) {
						zip:::zipr_append(zipfile = paste(direc,'.zip',sep=''), files = file_na, compression = 1)
					}
					file.copy(paste(direc,'.zip',sep=''), file)
					setwd(sessiontemp)
				},
				contentType = "application/zip"
			)
		}
		setwd(sessiontemp)
		gc()
		removeModal()
		incProgress(amount = 1, message = "Completed")
	})
})
