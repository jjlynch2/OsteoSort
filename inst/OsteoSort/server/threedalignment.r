filelist3 <- reactiveValues(list=list())
position <- reactiveValues(pos = 1)
landmarks <- reactiveValues(landmarks=list())

output$ncorespc <- renderUI({
	sliderInput(inputId = "ncorespc", label = "Number of cores", min=1, max=detectCores(), value=detectCores()-1, step =1)
})

output$vara <- renderUI({
	sliderInput(inputId = "vara", label = "K-means", min=0.01, max=1, value=0.01, step = 0.01)
})

output$sft <- renderUI({
	sliderInput(inputId = "sft", label = "Simplify fracture", min=0.1, max=500, value=5, step = 0.1)
})

output$fmt <- renderUI({
	sliderInput(inputId = "fmt", label = "Margin threshold", min=0.1, max=500, value=1, step = 0.1)
})

output$fr <- renderUI({
	sliderInput(inputId = "fr", label = "Fracture radius", min=0.1, max=500, value=1, step = 0.1)
})

output$resettableInput3Da <- renderUI({
	input$clearFile3Da
	input$uploadFormat
	fileInput('aligndata', 'Upload data set', accept=c("xyz"), multiple = TRUE)
})

observeEvent(input$clearFile3Da, {
	if(!is.null(input$aligndata$datapath)) {
		file.remove(input$aligndata$datapath)
		file.remove(input$aligndata$name)
	}
	try(rgl.close())
	filelist3$list = list()
	position$pos = 1
	landmarks$landmarks = list()
	fileInput('aligndata', 'Upload data set', accept=c("xyz"), multiple = TRUE)
})

observeEvent(input$aligndata$datapath, {
	file.copy(input$aligndata$datapath, input$aligndata$name)
	filelist3$list <- input.3d(input$aligndata$name) #imports 3D xyzrbg data
	landmarks$landmarks <- rep(list(NULL), length(filelist3$list)) #populate as NULL x file length on upload


######broken code to reimport landmarks
#	for(i in 1:length(filelist3$list)) {
#		if(ncol(filelist3$list[[i]]) > 6) {
#			if(!is.na(filelist3$list[[i]]$ml[1])) {
#				temp4 <- as.numeric(na.omit(filelist3$list[[i]]$ml))
#				landmarks$landmarks[[i]] <- temp4
#			}
#		}
#	}
######broken code to reimport landmarks

	observeEvent(position$pos, {

		if(is.null(filelist3$list[[position$pos]])) {nn <- 0}
		else {nn <- nrow(filelist3$list[[position$pos]]); mm <- input$aligndata$name[position$pos]}

		output$coordinates <- renderUI({
			HTML(paste("<strong>","<br/>","Coordinates: ",   "<font color=\"#00688B\">", nn, "</font>",
					 "<strong>","<br/>","Specimen: ",   "<font color=\"#00688B\">", mm, "</font>"

			))
		})   
		showModal(modalDialog(title = "Rendering...", easyClose = FALSE, footer = NULL))
		output$webgl3Dalign <- renderRglwidget ({
   			try(rgl.close())

			if(length(filelist3$list) != 0) {
				tt1 <- filelist3$list[[position$pos]]
			}
			else {
				tt1 = matrix(0)
			}

			if(ncol(tt1) >5) {
				if(any(is.na(tt1[,c(4:6)]))) { 
					cc <- "dimgrey"
				}
				else {
					cc <- rgb(tt1[,c(4:6)], max=255)
				}
			}
			else {cc <- "dimgrey"}
			if(length(filelist3$list) != 0) {
				plot3d(tt1, size=3, col=cc, box=FALSE, axes=FALSE, aspect = "iso", xlab="",ylab="",zlab="")


				if(!is.null(landmarks$landmarks[[position$pos]]))  {
					mp <- filelist3$list[[position$pos]][landmarks$landmarks[[position$pos]],]
					points3d(mp, size=20, col="black", box=FALSE)
				}
			}
			else {
				points3d(c(0,0,0), size=3, col="white", box=FALSE, aspect = "iso")
			}
			title3d(main = input$aligndata$name[position$pos], col = "DODGERBLUE")
			rglwidget()
		})
		removeModal()  
	})

})

observeEvent(input$simplify, {
	if(length(input$aligndata$datapath) > 0) {
		showModal(modalDialog(title = "Point cloud K-means simplification started...", easyClose = FALSE, footer = NULL))
		if(input$alln == "Present") {		
			ttt <- filelist3$list[[position$pos]]

			filelist3$list[[position$pos]] <- kmeans.3d(filelist3$list[[position$pos]], cluster = input$vara, threads = input$ncorespc)
			if(!is.null(landmarks$landmarks[[position$pos]])) {



				tempp <- HD_KDTree_Ind(as.matrix(filelist3$list[[position$pos]]), as.matrix(ttt[landmarks$landmarks[[position$pos]],]), threads = input$ncorespc, k = input$sft)
				landmarks$landmarks[[position$pos]] <- unique(tempp[which(tempp[,1] <= input$sft),2])
				if(length(landmarks$landmarks[[position$pos]]) == 0) { landmarks$landmarks[[position$pos]] <- NULL }
			}
		}
		if(input$alln == "All") {	
			ll <- length(filelist3$list)
			for (i in 1:ll) {	
				ttt <- filelist3$list[[i]]
				filelist3$list[[i]] <- kmeans.3d(filelist3$list[[i]], cluster = input$vara, threads = input$ncorespc)

				if(!is.null(landmarks$landmarks[[i]])) {
					tempp <- HD_KDTree_Ind(as.matrix(filelist3$list[[i]]), as.matrix(ttt[landmarks$landmarks[[i]],]), threads = input$ncorespc, k = input$sft)
					landmarks$landmarks[[i]] <- unique(tempp[which(tempp[,1] <= input$sft),2])
					if(length(landmarks$landmarks[[i]]) == 0) { landmarks$landmarks[[i]] <- NULL }
				}
			}
		}
		removeModal()  
	}
})

observeEvent(input$nnext, {
	if(length(input$aligndata$datapath) > 0) {
		if(position$pos < length(filelist3$list)) {
			position$pos = position$pos + 1
		}
	}
})

observeEvent(input$previous, {
	if(length(input$aligndata$datapath) > 0) {
		if(position$pos > 1) {
			position$pos = position$pos - 1
		}
	}
})


observeEvent(input$start2, {
	if(length(input$aligndata$datapath) > 0) {
		showModal(modalDialog(title = "Digitization has started...Please check the RGL window.", easyClose = FALSE, footer = NULL))
		if(input$alln == "Present") {
			temp_p <- filelist3$list[[position$pos]]
			landmarks$landmarks[[position$pos]] <- digitize.3d(temp_p)
		}
		else if(input$alln == "All") {
			ll <- length(filelist3$list)
			for (i in 1:ll) {
				ttt <- filelist3$list[[i]]
				landmarks$landmarks[[i]] <- digitize.3d(ttt)
   				try(rgl.close())
			}
		}
	removeModal()  
	}
})


#this code isnt working yet... something about the returned indices
observeEvent(input$start1, {
	if(length(input$aligndata$datapath) > 0) {
		showModal(modalDialog(title = "Fracture margin identification has started...", easyClose = FALSE, footer = NULL))
		if(input$alln == "Present") {
			ttt <- filelist3$list[[position$pos]]
			results <<- julia_call("radius_search", as.matrix(ttt), input$fr)
			landmarks$landmarks[[position$pos]] <- ttt[which(results <= input$fmt),]
			if(length(landmarks$landmarks[[position$pos]]) == 0) {
				landmarks$landmarks[[position$pos]] <- NULL
			}
		}
		else if(input$alln == "All") {
			ll <- length(filelist3$list)
			for (i in 1:ll) {
				ttt <- filelist3$list[[i]]
				results <- julia_call("radius_search", as.matrix(ttt), input$fr)
				landmarks$landmarks[[i]] <- ttt[which(results <= input$fmt),]
				if(length(landmarks$landmarks[[i]]) == 0) {
					landmarks$landmarks[[i]] <- NULL
				}
			}
		}
		removeModal()
	}
})



#download code needs to be adjusted for the new saving type.. no more landmarks eh
output$savedata <- downloadHandler(
	filename <- function() {
		paste("aligned.zip")
	},      
	content <- function(file) {
		#eventually move this to OsteoSort in output_functions. Here as proof-of-concept
		direc <- OsteoSort:::analytical_temp_space(output_options <- TRUE, sessiontempdir = sessiontemp)
		setwd(sessiontemp)
		setwd(direc)
		for(i in 1:length(filelist3$list)) {
			if(!is.null(landmarks$landmarks[[i]])) {
				r1 <- length(landmarks$landmarks[[i]])
				saveme <- cbind(x = filelist3$list[[i]][,1], 
							 y = filelist3$list[[i]][,2], 
							 z = filelist3$list[[i]][,3], 
							 ml = c(landmarks$landmarks[[i]], rep(NA, nrow(filelist3$list[[i]]) - r1))
				)
			}
			if(is.null(landmarks$landmarks[[i]])) {
				saveme <- cbind(x = filelist3$list[[i]][,1], y = filelist3$list[[i]][,2], z = filelist3$list[[i]][,3])
			}
			write.table(saveme, sep = ' ', file = input$aligndata$name[i], row.names = FALSE)
		}
		setwd(sessiontemp)
		files <- list.files(direc, recursive = TRUE)
		setwd(direc)
		zip:::zipr(zipfile = paste(direc,'.zip',sep=''), files = files)
		file.copy(paste(direc,'.zip',sep=''), file)

	},
	contentType = "application/zip"
)
setwd(sessiontemp)
	
