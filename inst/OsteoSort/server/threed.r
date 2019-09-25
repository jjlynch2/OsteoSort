forcefun3d <- function(hera1) {
	hera1 <- as.data.frame(hera1)
	df1 <- as.data.frame(cbind(from_id = hera1[,1], to_id = hera1[,2], Distance = hera1[,3]))
	df2 <- as.data.frame(cbind(from_id = hera1[,2], to_id = hera1[,1], Distance = hera1[,3]))
	df <- rbind(df1, df2)
	df$Distance <- (max(as.numeric(df$Distance))+1) - as.numeric(df$Distance)
	group <- c()
	skip <- TRUE
	for(i in 1:nrow(df)) {
		if(length(grep("_L_", df[i,1])) == 1) {
			group <- c(group, "left")
		} else if(length(grep("_R_", df[i,1])) == 1) {
			group <- c(group, "right")
		} else {
			skip <- TRUE
			break
		}
		skip <- FALSE
	}
	if(!skip) {
		df <- cbind(df, group)
	} else {
		df <- cbind(df, rep("group", nrow(df)))
	}
	nodes <- df[!duplicated(df[,1]),c(1,4)]
	colnames(nodes) <- c("name", "group")
	colnames(df) <- c("source", "target", "value", "group")
	df <- df[,c(1:3)]
	for(i in 1:nrow(nodes)) {
		df[df$source == nodes[i,1],1] <- i-1
		df[df$target == nodes[i,1],2] <- i-1
	}
	links <- df
	return(list(links,nodes))
}

forcd <- reactiveValues(forcd = TRUE) 
output$forcd <- renderUI({
	checkboxInput(inputId = "forcd", label = "Interactive network graph", value = TRUE)
})
observeEvent(input$forcd, {
	forcd$forcd <- input$forcd
})
output$contents3D <- renderUI({
   HTML(paste(""))
})	

output$resettableInput3D <- renderUI({
	input$clearFile3D
	input$uploadFormat
	fileInput('leftimages3D', 'Upload first data set', accept=c("xyz"), multiple = TRUE)
})

output$resettableInput3DD <- renderUI({
	input$clearFile3D
	input$uploadFormat
	fileInput('rightimages3D', 'Upload second data set', accept=c("xyz"), multiple = TRUE)
})

observeEvent(input$clearFile3D, {
	if(!is.null(input$rightimages3D$datapath)) { #prevents crashing
		file.remove(input$rightimages3D$datapath)
		file.remove(input$rightimages3D$name)
	}
	if(!is.null(input$leftimages3D$datapath)) {
		file.remove(input$leftimages3D$datapath)
		file.remove(input$leftimages3D$name)
	}
	fileInput('leftimages3D', 'Upload first data set', accept=c("xyz"), multiple = TRUE)
	fileInput('rightimages3D', 'Upload second data set', accept=c("xyz"), multiple = TRUE)
})

fileoutput3Dexcel1 <- reactiveValues(fileoutput3Dexcel1 = TRUE)
observeEvent(input$fileoutput3Dexcel1, {
	fileoutput3Dexcel1$fileoutput3Dexcel1 <- input$fileoutput3Dexcel1
})
output$fileoutput3Dexcel1 <- renderUI({
	checkboxInput(inputId = "fileoutput3Dexcel1", label = "Output match distances to excel file ", value = TRUE)
})

fileoutput3Dexcel2 <- reactiveValues(fileoutput3Dexcel2 = TRUE)
observeEvent(input$fileoutput3Dexcel2, {
	fileoutput3Dexcel2$fileoutput3Dexcel2 <- input$fileoutput3Dexcel2
})
output$fileoutput3Dexcel2 <- renderUI({
	checkboxInput(inputId = "fileoutput3Dexcel2", label = "Output all distances to excel file ", value = TRUE)
})



fileoutput3Dtps <- reactiveValues(fileoutput3Dtps = FALSE)
observeEvent(input$fileoutput3Dtps, {
	fileoutput3Dtps$fileoutput3Dtps <- input$fileoutput3Dtps
})
output$fileoutput3Dtps <- renderUI({
	checkboxInput(inputId = "fileoutput3Dtps", label = "Output TPS registered coordinates", value = FALSE)
})



multiple_file_output_graph_3d <- reactiveValues(multiple_file_output_graph_3d = TRUE) 
output$multiple_file_output_graph_3d <- renderUI({
	checkboxInput(inputId = "multiple_file_output_graph_3d", label = "Output network graph", value = TRUE)
})
observeEvent(input$multiple_file_output_graph_3d, {
	multiple_file_output_graph_3d$multiple_file_output_graph_3d <- input$multiple_file_output_graph_3d
})


labtf3d <- reactiveValues(labtf3d = TRUE) 
output$labtf3d <- renderUI({
	checkboxInput(inputId = "labtf3d", label = "Network graph labels", value = TRUE)
})
observeEvent(input$labtf3d, {
	labtf3d$labtf3d <- input$labtf3d
})



nthreshold3D <- reactiveValues(nthreshold3D = 4)
observeEvent(input$nthreshold3D, {
	nthreshold3D$nthreshold3D <- input$nthreshold3D
})
output$nthreshold3D <- renderUI({
	sliderInput(inputId = "nthreshold3D", label = "Threshold for principal bands", min=0.5, max=100, value=4, step=0.5)
})



shortlistn3D <- reactiveValues(shortlistn3D = 1)
observeEvent(input$shortlistn3D, {
	shortlistn3D$shortlistn3D <- input$shortlistn3D
})
output$shortlistn3D <- renderUI({
	sliderInput(inputId = "shortlistn3D", label = "Shortest distance matches", min = 1, max = 100, value = 1, step = 1)
})



hidedist3D <- reactiveValues(hidedist3D = FALSE)
observeEvent(input$hidedist3D, {
	hidedist3D$hidedist3D <- input$hidedist3D
})
output$hidedist3D <- renderUI({
	checkboxInput(inputId = "hidedist3D", label = "Hide distance from results", value = FALSE)
})


banding <- reactiveValues(banding = TRUE)
observeEvent(input$banding, {
	banding$banding <- input$banding
})
output$banding <- renderUI({
	checkboxInput(inputId = "banding", label = "Principal bands", value = TRUE)
})



ncores3D <- reactiveValues(ncores3D = detectCores()-1)
observeEvent(input$ncores3D, {
	ncores2D$ncores3D <- input$ncores3D
})
output$ncores3D <- renderUI({
	sliderInput(inputId = "ncores3D", label = "Number of cores", min=1, max=detectCores(), value=detectCores()-1, step =1)
})


max_avg_distance3D <- reactiveValues(max_avg_distance3D = "average")
observeEvent(input$max_avg_distance3D, {
	max_avg_distance3D$max_avg_distance3D <- input$max_avg_distance3D
})
output$max_avg_distance3D <- renderUI({
	radioButtons(inputId = "max_avg_distance3D", label = "Distance type:", choices = c("maximum",  "average", "dilated"), selected = "average")
})

icp3D <- reactiveValues(icp3D = 1)
observeEvent(input$icp3D, {
	icp3D$icp3D <- input$icp3D
})
output$icp3D <- renderUI({
	sliderInput(inputId = "icp3D", label = "ICP iterations", min=1, max=1000, value=10, step=1)
})

				
observeEvent(input$leftimages3D$datapath, {
	file.copy(input$leftimages3D$datapath, input$leftimages3D$name)
})
observeEvent(input$rightimages3D$datapath, {
	file.copy(input$rightimages3D$datapath, input$rightimages3D$name)
})

observeEvent(input$pro3D, {
	output$contents3D <- renderUI({
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

	if(!is.null(input$leftimages3D$datapath) && !is.null(input$leftimages3D$datapath)) { #prevents crashing

		out1 <- input.3d(list1 = input$rightimages3D$name, list2 = input$leftimages3D$name)
		out2 <- match.3d(data = out1, hide_distances = hidedist3D$hidedist3D, iteration = icp3D$icp3D, dist = max_avg_distance3D$max_avg_distance3D, n_lowest_distances = shortlistn3D$shortlistn3D, output_options = c(fileoutput3Dexcel1$fileoutput3Dexcel1, fileoutput3Dexcel2$fileoutput3Dexcel2, fileoutput3Dtps$fileoutput3Dtps, multiple_file_output_graph_3d$multiple_file_output_graph_3d), labtf3d = labtf3d$labtf3d, sessiontempdir = sessiontemp, threads = ncores3D$ncores3D, band_threshold = nthreshold3D$nthreshold3D/2, band = banding$banding, fragment = input$fragcomp3d)
		direc <- out2[[3]]

		if(is.null(nrow(out2[[2]]))) {pm <- 1; out2[[2]] <- rbind(out2[[2]],c(NA,NA,NA)) }
		if(!is.null(nrow(out2[[2]]))) {pm <- nrow(as.matrix(out2[[2]][,1]))}
		output$table3D <- DT::renderDataTable({
			DT::datatable(out2[[2]], selection = list(mode="multiple"), options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
		})

		output$contents3D <- renderUI({
			HTML(paste("<strong>Completed in: ", "<font color=\"#00688B\">", out2[[7]], " minutes</font></strong><br>","<strong>Potential matches: ", "<font color=\"#00688B\">", pm, "</font></strong>"))
		})
		output$mspec3D <- renderUI({
			selectInput(inputId = "mspec3D", label = "Choose comparison", choices = c(out2[[6]][,3]))
		})

		if(forcd$forcd) {
				if(nrow(out2[[2]]) > 1){
					td <- forcefun3d(out2[[2]])
					links <- td[[1]]
					nodes <- td[[2]]
					output$forceNetworkOSMd <- renderForceNetwork({
						forceNetwork(Links = links, Nodes = nodes,
								  Source = "source", Target = "target",
								  Value = "value", NodeID = "name",
								  Group = "group", opacity = 1,
									colourScale = JS('d3.scaleOrdinal().domain(["1", "2", "3"]).range(["#ea6011","#126a8f"])'),
									zoom = TRUE
						)
					})
				}
		}

		if(fileoutput3Dexcel1$fileoutput3Dexcel1 || fileoutput3Dexcel2$fileoutput3Dexcel2 || fileoutput3Dtps$fileoutput3Dtps || multiple_file_output_graph_3d$multiple_file_output_graph_3d) {
			setwd(direc)
			if(multiple_file_output_graph_3d$multiple_file_output_graph_3d) {
				nimages <- paste(sessiontemp, "/", direc, "/", "network.jpg", sep="")
			} else {
				nimages <- system.file("OsteoSort/www", 'blank.jpg', package = "OsteoSort")
			}
			output$multiple_plot_na_3d <- renderImage({
				list(src = nimages,
					contentType = 'image/jpg',
					height = 800,
					alt = "A"
				)
			}, deleteFile = FALSE)
			setwd(sessiontemp)

			output$downloadData3D <- downloadHandler(
				filename <- function() {
					paste("results.zip")
				},      
				content <- function(file) {
					setwd(direc)
					file.remove(paste(direc,'.zip',sep=''))
					if(is.numeric(input$table3D_rows_selected)) {
						no_return_value <- OsteoSort:::output_function(out2[[2]][input$table3D_rows_selected,], method="exclusion", type="csv4")
					} else {file.remove("selected-list.csv")}
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
	observeEvent(input$mspec3D, {
			tt1 <- out2[[6]][out2[[6]][,3] == input$mspec3D, ][1]
			tt1 <- as.numeric(tt1)

			tt2 <- out2[[6]][out2[[6]][,3] == input$mspec3D, ][2]
			tt2 <- as.numeric(tt2)

			tt1 <- out2[[1]][[tt1]]
			tt2 <- out2[[1]][[tt2]]

			output$webgl3D <- renderRglwidget ({
				try(rgl.close())

				points3d(tt1, size=3, col="dimgray", box=FALSE)
				points3d(tt2, size=3, col="dodgerblue", box=FALSE)
				axes3d(c('x++', 'y++', 'z++'))

				rglwidget()
			})
	})

	gc()
	removeModal()  
})
	
