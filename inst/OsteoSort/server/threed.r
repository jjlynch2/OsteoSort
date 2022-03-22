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

forcd <- reactiveValues(forcd = FALSE) 
output$forcd <- renderUI({
	checkboxInput(inputId = "forcd", label = "Interactive network graph", value = FALSE)
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

dirdel <- reactiveValues(dirdel = NULL)

observeEvent(input$clearFile3D, {
	if(!is.null(input$rightimages3D$datapath)) { #prevents crashing
		file.remove(input$rightimages3D$datapath)
		file.remove(input$rightimages3D$name)
	}
	if(!is.null(input$leftimages3D$datapath)) {
		file.remove(input$leftimages3D$datapath)
		file.remove(input$leftimages3D$name)
	}
	delete.tmp.data(dirdel$dirdel, sessiontemp)
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

shortlistn3D <- reactiveValues(shortlistn3D = 1)
observeEvent(input$shortlistn3D, {
	shortlistn3D$shortlistn3D <- input$shortlistn3D
})
output$shortlistn3D <- renderUI({
	sliderInput(inputId = "shortlistn3D", label = "Shortest distance matches", min = 1, max = 100, value = 1, step = 1)
})

ncores3D <- reactiveValues(ncores3D = detectCores()-1)
observeEvent(input$ncores3D, {
	ncores2D$ncores3D <- input$ncores3D
})
output$ncores3D <- renderUI({
	sliderInput(inputId = "ncores3D", label = "Number of cores", min=1, max=detectCores(), value=detectCores()-1, step =1)
})
				
observeEvent(input$pro3D, {
	output$contents3D <- renderUI({
	   HTML(paste(""))
	})
	showModal(modalDialog(title = "Calculation has started...Window will update when finished.", easyClose = FALSE, footer = NULL))
	withProgress(message = 'Calculation has started', detail = '', value = 1, min=0, max=4, {
		if(!is.null(input$leftimages3D$datapath) && !is.null(input$leftimages3D$datapath)) { #prevents crashing
			setProgress(value = 3, message = "Running comparisons", detail = '')

			out2 <- match.3d(path1 = input$leftimages3D$datapath, path2 = input$rightimages3D$datapath, names1 = input$leftimages3D$name, names2 = input$rightimages3D$name, n_lowest_distances = shortlistn3D$shortlistn3D, output_options = c(fileoutput3Dexcel1$fileoutput3Dexcel1, fileoutput3Dexcel2$fileoutput3Dexcel2), sessiontempdir = sessiontemp, cores = ncores3D$ncores3D)
			
			direc <- out2[[2]]
			sd <- paste(sessiontemp, direc, sep="/")
			dirdel$dirdel <- direc
			if(is.null(nrow(out2[[1]]))) {pm <- 1; out2[[1]] <- rbind(out2[[1]],c(NA,NA,NA)) }
			if(!is.null(nrow(out2[[5]]))) {pm <- nrow(as.matrix(out2[[1]][,1]))}
			output$table3D <- DT::renderDataTable({
				DT::datatable(out2[[1]], selection = list(mode="multiple"), options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
			})

			output$contents3D <- renderUI({
				HTML(paste("<strong>Completed in: ", "<font color=\"#00688B\">", out2[[5]], " minutes</font></strong><br>","<strong>Potential matches: ", "<font color=\"#00688B\">", pm, "</font></strong>"))
			})
			if(forcd$forcd) {
					if(nrow(out2[[1]]) > 1){
						td <- forcefun3d(out2[[1]])
						links <- td[[1]]
						nodes <- td[[2]]
						output$forceNetworkOSMd <- renderForceNetwork({
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

			if(fileoutput3Dexcel1$fileoutput3Dexcel1 || fileoutput3Dexcel2$fileoutput3Dexcel2) {
				output$downloadData3D <- downloadHandler(
					filename <- function() {
						paste("results.zip")
					},      
					content <- function(file) {
						file.remove(paste(sd,"/", direc,'.zip',sep=''))
						if(is.numeric(input$table3D_rows_selected)) {
							no_return_value <- OsteoSort:::output_function(out2[[1]][input$table3D_rows_selected,], method="exclusion", type="csv4", fpath=sd)
						} else {file.remove(paste(sd,"/selected-list.csv",sep=""))}

						files <- list.files(sd, recursive = TRUE, full.names = TRUE)

						zip:::zipr(zipfile = paste(sd,"/", direc,'.zip',sep=''), files = files[1], compression = 1)
						for(file_na in files[-1]) {
							zip:::zipr_append(zipfile = paste(sd,"/", direc,'.zip',sep=''), files = file_na, compression = 1)
						}
						file.copy(paste(sd,"/", direc,'.zip',sep=''), file)  
					},
					contentType = "application/zip"
				)
			}
		}
		gc()
		removeModal()
		setProgress(value = 4, message = "Completed", detail = '')
	})
})
	
