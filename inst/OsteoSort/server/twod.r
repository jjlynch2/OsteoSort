forc2d <- reactiveValues(forc2d = TRUE) 
output$forc2d <- renderUI({
	checkboxInput(inputId = "forc2d", label = "Interactive network graph", value = TRUE)
})
observeEvent(input$forc2d, {
	forc2d$forc2d <- input$forc2d
})

output$tabpanpan <- renderUI({
	panels2 <- list(
		tabPanel("Results ",DT::dataTableOutput('table2D')),
		tabPanel("Registration",imageOutput('pwspeci')),
		tabPanel("Graph", imageOutput('multiple_plot_na_2d')),
		tabPanel("Interactive", forceNetworkOutput("forceNetworkOSM2d"))
	)
	do.call(tabsetPanel, panels2)
})

output$contents2D <- renderUI({
	HTML(paste(""))
})	

output$resettableInput2D <- renderUI({
	input$clearFile2D
	input$uploadFormat
	fileInput('leftimages', 'Upload first image set', accept=c('jpeg', "jpg"), multiple = TRUE)
})

output$resettableInput2DD <- renderUI({
	input$clearFile2D
	input$uploadFormat
	fileInput('rightimages', 'Upload second image set', accept=c('jpeg', "jpg"), multiple = TRUE)
})

observeEvent(input$clearFile2D, {
	if(!is.null(input$leftimages$datapath) && !is.null(input$leftimages$datapath)) { #prevents crashing
		file.remove(input$leftimages$datapath)
		file.remove(input$leftimages$name)
		file.remove(input$rightimages$datapath)
		file.remove(input$rightimages$name)
	}
	file.remove(list.files(full.names=TRUE, recursive = TRUE))
	fileInput('leftimages', 'Upload first image set', accept=c('jpeg', "jpg"), multiple = TRUE)
	fileInput('rightimages', 'Upload second image set', accept=c('jpeg', "jpg"), multiple = TRUE)
})

fileoutput2Dexcel1 <- reactiveValues(fileoutput2Dexcel1 = TRUE)
observeEvent(input$fileoutput2Dexcel1, {
	fileoutput2Dexcel1$fileoutput2Dexcel1 <- input$fileoutput2Dexcel1
})
output$fileoutput2Dexcel1 <- renderUI({
	checkboxInput(inputId = "fileoutput2Dexcel1", label = "Output match distances to excel file ", value = TRUE)
})

fileoutput2Dexcel2 <- reactiveValues(fileoutput2Dexcel2 = TRUE)
observeEvent(input$fileoutput2Dexcel2, {
	fileoutput2Dexcel2$fileoutput2Dexcel2 <- input$fileoutput2Dexcel2
})
output$fileoutput2Dexcel2 <- renderUI({
	checkboxInput(inputId = "fileoutput2Dexcel2", label = "Output all distances to excel file ", value = TRUE)
})

fileoutput2Dplot <- reactiveValues(fileoutput2Dplot = TRUE)
observeEvent(input$fileoutput2Dplot, {
	fileoutput2Dplot$fileoutput2Dplot <- input$fileoutput2Dplot
})
output$fileoutput2Dplot <- renderUI({
	checkboxInput(inputId = "fileoutput2Dplot", label = "Output registered plot", value = TRUE)
})

fileoutput2Dtps <- reactiveValues(fileoutput2Dtps = FALSE)
observeEvent(input$fileoutput2Dtps, {
	fileoutput2Dtps$fileoutput2Dtps <- input$fileoutput2Dtps
})
output$fileoutput2Dtps <- renderUI({
	checkboxInput(inputId = "fileoutput2Dtps", label = "Output TPS registered coordinates", value = FALSE)
})

multiple_file_output_graph_2d <- reactiveValues(multiple_file_output_graph_2d = TRUE) 
output$multiple_file_output_graph_2d <- renderUI({
	checkboxInput(inputId = "multiple_file_output_graph_2d", label = "Output network graph", value = TRUE)
})
observeEvent(input$multiple_file_output_graph_2d, {
	multiple_file_output_graph_2d$multiple_file_output_graph_2d <- input$multiple_file_output_graph_2d
})


labtf2d <- reactiveValues(labtf2d = TRUE) 
output$labtf2d <- renderUI({
	checkboxInput(inputId = "labtf2d", label = "Network graph labels", value = TRUE)
})
observeEvent(input$labtf2d, {
	labtf2d$labtf2d <- input$labtf2d
})


nthreshold <- reactiveValues(nthreshold = 0.8)
observeEvent(input$nthreshold, {
	nthreshold$nthreshold <- input$nthreshold
})
output$nthreshold <- renderUI({
	sliderInput(inputId = "nthreshold", label = "Threshold for binary matrices", min=0.01, max=1, value=0.8, step=0.01)
})

mirror2D <- reactiveValues(mirror2D = TRUE)
observeEvent(input$mirror2D, {
	mirror2D$mirror2D <- input$mirror2D
})
output$mirror2D <- renderUI({
	checkboxInput(inputId = "mirror2D", label = "Mirror left images to right", value = TRUE)
})

ncores2D <- reactiveValues(ncores2D = 1)
observeEvent(input$ncores2D, {
	ncores2D$ncores2D <- input$ncores2D
})
output$ncores2D <- renderUI({
	sliderInput(inputId = "ncores2D", label = "Number of cores", min=1, max=detectCores(), value=1, step =1)
})


scale2D <- reactiveValues(scale2D = FALSE)
observeEvent(input$scale2D, {
	scale2D$scale2D <- input$scale2D
})
output$efa_options3 <- renderUI({
	checkboxInput(inputId = "scale2D", label = "Scale to Centroid Size", value = FALSE)
})


max_avg_distance <- reactiveValues(max_avg_distance = "average")
observeEvent(input$max_avg_distance, {
	max_avg_distance$max_avg_distance <- input$max_avg_distance
})
output$max_avg_distance <- renderUI({
	radioButtons(inputId = "max_avg_distance", label = "Distance type:", choices = c("maximum",  "average"), selected = "average")
})

icp2D <- reactiveValues(icp2D = 20)
observeEvent(input$icp2D, {
	icp2D$icp2D <- input$icp2D
})
output$icp2D <- renderUI({
	sliderInput(inputId = "icp2D", label = "ICP iterations", min=1, max=1000, value=20, step=1)
})


shortlistn <- reactiveValues(shortlistn = 1)
observeEvent(input$shortlistn, {
	shortlistn$shortlistn <- input$shortlistn
})
output$shortlistn <- renderUI({
	sliderInput(inputId = "shortlistn", label = "Shorest distance matches", min = 1, max = 100, value = 1, step = 1)
})

hidedist <- reactiveValues(hidedist = FALSE)
observeEvent(input$hidedist, {
	hidedist$hidedist <- input$hidedist
})
output$hidedist <- renderUI({
	checkboxInput(inputId = "hidedist", label = "Hide distance from results", value = FALSE)
})


observeEvent(input$pro2D, {
	output$contents2D <- renderUI({
	   HTML(paste(""))
	})	
	showModal(modalDialog(title = "Calculation has started...Window will update when finished.", easyClose = FALSE, footer = NULL))
	withProgress(message = 'Calculation has started', detail = '', value = 1, min=0, max=3, {
		if(!is.null(input$leftimages$datapath) && !is.null(input$leftimages$datapath)) { #prevents crashing
			leftimages <- input$leftimages$datapath
			rightimages <- input$rightimages$datapath
			file.copy(input$leftimages$datapath, input$leftimages$name)
			file.copy(input$rightimages$datapath, input$rightimages$name)
			setProgress(value = 1, message = "Tracing outlines", detail = '')
			out1 <- outline.images(imagelist1 = input$rightimages$name, imagelist2 = input$leftimages$name, threshold =nthreshold$nthreshold, scale = scale2D$scale2D, mirror = mirror2D$mirror2D)
			setProgress(value = 2, message = "Running comparisons", detail = '')
			out2 <- match.2d(outlinedata = out1, hide_distances = hidedist$hidedist, iteration = icp2D$icp2D, dist = max_avg_distance$max_avg_distance, n_lowest_distances = shortlistn$shortlistn, labtf2d = labtf2d$labtf2d, output_options = c(fileoutput2Dexcel1$fileoutput2Dexcel1, fileoutput2Dexcel2$fileoutput2Dexcel2, fileoutput2Dplot$fileoutput2Dplot, fileoutput2Dtps$fileoutput2Dtps, multiple_file_output_graph_2d$multiple_file_output_graph_2d), sessiontempdir = sessiontemp, threads = ncores2D$ncores2D)
			direc <- out2[[3]]
			sd <- paste(sessiontemp,direc,sep="/")
			if(forc2d$forc2d) {
					if(nrow(out2[[2]]) > 1){
						td <- forcefun3d(out2[[2]])
						links <- td[[1]]
						nodes <- td[[2]]
						output$forceNetworkOSM2d <- renderForceNetwork({
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
			if(multiple_file_output_graph_2d$multiple_file_output_graph_2d) {
				imagetemp2 <- paste(sessiontemp, "/", direc, "/", "network.jpg", sep="")
				output$multiple_plot_na_2d <- renderImage({
					list(src = imagetemp2,
						contentType = 'image/jpg',
						height = 800,
						alt = "A"
					)
				}, deleteFile = FALSE)
			}
			if(fileoutput2Dplot$fileoutput2Dplot) {
				pwspec <- list.files(sd)
				pwspec <- pwspec[grep(".jpg", pwspec)]
				pwspec <- pwspec[pwspec != "network.jpg"]
				pwspec = pwspec[pwspec != "Registration.jpg"]
				output$pwspec <- renderUI({
					selectInput(inputId = "pwspec", label = "Choose pairwise comparison", choices=pwspec, selected = pwspec[1])
				})
				observeEvent(input$pwspec, {
					output$pwspeci <- renderImage({
						if(fileoutput2Dplot$fileoutput2Dplot) {
							tempni <- paste(sessiontemp, "/", direc, "/", input$pwspec, sep="")
							list(src = tempni,
								contentType = 'image/jpg',
								height = 800,
								alt = "A"
							)
						}
					}, deleteFile = FALSE)
				})
			}

			if(is.null(nrow(out2[[2]]))) {pm <- 1; out2[[2]] <- rbind(out2[[2]],c(NA,NA,NA)) }
			if(!is.null(nrow(out2[[2]]))) {pm <- nrow(as.matrix(out2[[2]][,1]))}
			output$table2D <- DT::renderDataTable({
				DT::datatable(out2[[2]], selection = list(mode="multiple"), options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
			})

			output$contents2D <- renderUI({
				HTML(paste("<strong>Completed in: ", "<font color=\"#00688B\">", out2[[6]], " minutes</font></strong><br>","<strong>Potential matches: ", "<font color=\"#00688B\">", pm, "</font></strong>"))
			})
			if(multiple_file_output_graph_2d$multiple_file_output_graph_2d || fileoutput2Dexcel1$fileoutput2Dexcel1 || fileoutput2Dexcel2$fileoutput2Dexcel2 || fileoutput2Dplot$fileoutput2Dplot || fileoutput2Dtps$fileoutput2Dtps) {
				output$downloadData2D <- downloadHandler(
					filename <- function() {
						paste("results.zip")
					},
					content <- function(file) {
						file.remove(paste(sd,"/",direc,'.zip',sep=''))
						if(is.numeric(input$table2D_rows_selected)) {
							no_return_value <- OsteoSort:::output_function(out2[[2]][input$table2D_rows_selected,], method="exclusion", type="csv4", fpath=sd)
					} else {file.remove(paste(sd,"/selected-list.csv",sep=""))}

						files <- list.files(sd, recursive = TRUE, full.names=TRUE)

						zip:::zipr(zipfile = paste(sd,"/",direc,'.zip',sep=''), files = files[1], compression = 1)
						for(file_na in files[-1]) {
							zip:::zipr_append(zipfile = paste(sd,"/",direc,'.zip',sep=''), files = file_na, compression = 1)
						}
						file.copy(paste(sd,"/",direc,'.zip',sep=''), file)  
					},
					contentType = "application/zip"
				)
			}
		}
		gc()
		removeModal()
		setProgress(value = 3, message = "Running comparisons", detail = '')
	})
})
	
