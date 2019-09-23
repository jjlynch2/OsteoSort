output$tabpanpan <- renderUI({
	panels1 <- list(
		tabPanel("Starting Mean",imageOutput('meanImage')),
		tabPanel("Registration",imageOutput('plotplottd')),
		tabPanel("Results",DT::dataTableOutput('table2D'))
	)
	panels2 <- list(
		tabPanel("Results ",DT::dataTableOutput('table2D'))
	)
	if(input$fragcomp == "Complete") {panel <- panels1}
	if(input$fragcomp == "Fragmented") {panel <- panels2}
	do.call(tabsetPanel, panel)
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
	checkboxInput(inputId = "fileoutput2Dplot", label = "Output registered plot (WARNING: fragmented analysis will generate a plot for every comparison)", value = TRUE)
})

fileoutput2Dtps <- reactiveValues(fileoutput2Dtps = TRUE)
observeEvent(input$fileoutput2Dtps, {
	fileoutput2Dtps$fileoutput2Dtps <- input$fileoutput2Dtps
})
output$fileoutput2Dtps <- renderUI({
	checkboxInput(inputId = "fileoutput2Dtps", label = "Output TPS registered coordinates", value = TRUE)
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

meanit2D <- reactiveValues(meanit2D = 2)
observeEvent(input$meanit2D, {
	meanit2D$meanit2D <- input$meanit2D
})
output$comp_options <- renderUI({
	sliderInput(inputId = "meanit2D", label = "Mean iterations", min=1, max=100, value=2, step=1)
})

efaH2D <- reactiveValues(efaH2D = 40)
observeEvent(input$efaH2D, {
	efaH2D$efaH2D <- input$efaH2D
})
output$efa_options1 <- renderUI({
	sliderInput(inputId = "efaH2D", label = "EFA harmonics", min=1, max=1000, value=40, step=1)
})

npoints2D <- reactiveValues(npoints2D = 200)
observeEvent(input$npoints2D, {
	npoints2D$npoints2D <- input$npoints2D
})
output$efa_options2 <- renderUI({
	sliderInput(inputId = "npoints2D", label = "Landmarks for inverse EFA", min=20, max=1000, value=200, step=1)
})


scale2D <- reactiveValues(scale2D = FALSE)
observeEvent(input$scale2D, {
	scale2D$scale2D <- input$scale2D
})
output$efa_options3 <- renderUI({
	checkboxInput(inputId = "scale2D", label = "Scale to Centroid Size", value = FALSE)
})

n_regions <- reactiveValues(n_regions = 6)
observeEvent(input$n_regions, {
	n_regions$n_regions <- input$n_regions
})
output$n_regions <- renderUI({			
	sliderInput(inputId = "n_regions", label = "Segmented regions", min = 2, max = input$npoints2D, value = 6, step = 1)										
})

max_avg_distance <- reactiveValues(max_avg_distance = "average")
observeEvent(input$max_avg_distance, {
	max_avg_distance$max_avg_distance <- input$max_avg_distance
})
output$max_avg_distance <- renderUI({
	radioButtons(inputId = "max_avg_distance", label = "Distance type:", choices = c("maximum",  "average", "dilated"), selected = "average")
})

icp2D <- reactiveValues(icp2D = 20)
observeEvent(input$icp2D, {
	icp2D$icp2D <- input$icp2D
})
output$icp2D <- renderUI({
	sliderInput(inputId = "icp2D", label = "ICP iterations", min=1, max=1000, value=20, step=1)
})


distance2D <- reactiveValues(distance2D = "Hausdorff")
observeEvent(input$distance2D, {
	distance2D$distance2D <- input$distance2D
})
output$distance2D <- renderUI({
	radioButtons(inputId = "distance2D", label = "Distance calculation:", choices = c("Segmented-Hausdorff", "Hausdorff"), selected = "Hausdorff")
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

#renders temporary mean
observeEvent(input$rightimages, {
	output$mspec <- renderUI({
		sliderInput(inputId = "mspec", label = "Choose specimen # for temporary mean", min=1, max=nrow(input$leftimages) + nrow(input$rightimages), value = 1, step = 1)
	})
})

observeEvent(input$mspec, {
	nimages <- rbind(input$leftimages$datapath, input$rightimages$datapath)
	nimages <- nimages[input$mspec]
	output$meanImage <- renderImage({
		list(src = nimages,
			contentType = 'image/jpg',
			width = 400,
			height = 400,
			alt = "A"
		)
	}, deleteFile = FALSE)
})

observeEvent(input$pro2D, {
	output$contents2D <- renderUI({
	   HTML(paste(""))
	})	
	showModal(modalDialog(title = "Calculation has started...Window will update when finished.", easyClose = FALSE, footer = NULL))
	withProgress(message = 'Calculation has started',
	            detail = '', value = 0, {       
	            for (i in 1:10) {
	       incProgress(1/10)
	       Sys.sleep(0.05)
	     }
	})
	if(!is.null(input$leftimages$datapath) && !is.null(input$leftimages$datapath)) { #prevents crashing
		leftimages <- input$leftimages$datapath
		rightimages <- input$rightimages$datapath
		file.copy(input$leftimages$datapath, input$leftimages$name)
		file.copy(input$rightimages$datapath, input$rightimages$name)
		if(input$fragcomp == "Complete") {fragment <- FALSE}
		if(input$fragcomp == "Fragmented") {fragment <- TRUE}

		out1 <- outline.images(imagelist1 = input$rightimages$name, imagelist2 = input$leftimages$name, fragment = fragment, threshold =nthreshold$nthreshold, scale = scale2D$scale2D, mirror = mirror2D$mirror2D, npoints = npoints2D$npoints2D, nharmonics = efaH2D$efaH2D)
		out2 <- match.2d(outlinedata = out1, hide_distances = hidedist$hidedist, iteration = icp2D$icp2D, fragment = fragment, dist = max_avg_distance$max_avg_distance, n_regions = n_regions$n_regions, n_lowest_distances = shortlistn$shortlistn, output_options = c(fileoutput2Dexcel1$fileoutput2Dexcel1, fileoutput2Dexcel2$fileoutput2Dexcel2, fileoutput2Dplot$fileoutput2Dplot, fileoutput2Dtps$fileoutput2Dtps), sessiontempdir = sessiontemp, threads = ncores2D$ncores2D, test = distance2D$distance2D, temporary_mean_specimen = input$mspec, mean_iterations = meanit2D$meanit2D)
		direc <- out2[[3]]
		if(fileoutput2Dplot$fileoutput2Dplot && input$fragcomp == "Complete") {
			setwd(sessiontemp)
			setwd(direc)
			nimages <- list.files()
			nimages <- paste(sessiontemp, "/", direc, "/", nimages[grep(".jpg", nimages)], sep="")
			output$plotplottd <- renderImage({
				list(src = nimages,
					contentType = 'image/jpg',
					height = 600,
					alt = "A"
				)
			}, deleteFile = FALSE)
		}
		if(!fileoutput2Dplot$fileoutput2Dplot && input$fragcomp == "Complete") {
			output$plotplottd <- renderImage({
				list(src = "",
					contentType = 'image/jpg',
					alt = ""
				)})
		}
		if(is.null(nrow(out2[[2]]))) {pm <- 1; out2[[2]] <- rbind(out2[[2]],c(NA,NA,NA)) }
		if(!is.null(nrow(out2[[2]]))) {pm <- nrow(as.matrix(out2[[2]][,1]))}
		output$table2D <- DT::renderDataTable({
			DT::datatable(out2[[2]], selection = list(mode="multiple"), options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
		})

		output$contents2D <- renderUI({
			HTML(paste("<strong>Completed in: ", "<font color=\"#00688B\">", out2[[6]], " minutes</font></strong><br>","<strong>Potential matches: ", "<font color=\"#00688B\">", pm, "</font></strong>"))
		})
		if(fileoutput2Dexcel1$fileoutput2Dexcel1 || fileoutput2Dexcel2$fileoutput2Dexcel2 || fileoutput2Dplot$fileoutput2Dplot || fileoutput2Dtps$fileoutput2Dtps) {
			output$downloadData2D <- downloadHandler(
				filename <- function() {
					paste("results.zip")
				},
				content <- function(file) {
					setwd(direc)
					file.remove(paste(direc,'.zip',sep=''))
					if(is.numeric(input$table2D_rows_selected)) {
						no_return_value <- OsteoSort:::output_function(out2[[2]][input$table2D_rows_selected,], method="exclusion", type="csv4")
				} else {file.remove("selected-list.csv.csv")}
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
			setwd(sessiontemp)
		}
	}
	gc()
	removeModal()  
})
	
