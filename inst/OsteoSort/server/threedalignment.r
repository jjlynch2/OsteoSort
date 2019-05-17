
filelist3 <- reactiveValues(list=list())
position <- reactiveValues(pos = 1)
landmarks <- reactiveValues(landmarks=list())

red <- reactiveValues(red=c(255,255,255))
green <- reactiveValues(green=c(0,255,0))
blue <- reactiveValues(blue=c(0,0,255))
fra <- reactiveValues(fra=c(255,0,0))

ncorespc <- reactiveValues(ncorespc = detectCores()-1)
observeEvent(input$ncorespc, {
	ncorespc$ncorespc <- input$ncorespc
})
output$ncorespc <- renderUI({
	sliderInput(inputId = "ncorespc", label = "Number of threads", min=1, max=detectCores(), value=detectCores()-1, step =1)
})


vara <- reactiveValues(vara = 0.01)
observeEvent(input$vara, {
	vara$vara <- input$vara
})
output$vara <- renderUI({
	sliderInput(inputId = "vara", label = "K-means", min=0.01, max=1, value=0.01, step = 0.01)
})


tva <- reactiveValues(tva = 1)
observeEvent(input$vara, {
	tva$tva <- input$tva
})
output$tva <- renderUI({
	sliderInput(inputId = "tva", label = "Simplify fracture threshold", min=0.1, max=10, value=1, step = 0.1)
})



fracturet <- reactiveValues(fracturet = 100)
observeEvent(input$fracturet, {
	fracturet$fracturet <- input$fracturet
})
output$fracturet <- renderUI({
	sliderInput(inputId = "fracturet", label = "Fracture distance threshold", min=1, max=255, value=100, step =1)
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
	landmarks$landmarks <- landmarks$landmarks[1:length(filelist3$list)] #populate as NULL x file length on upload

	for(i in 1:length(landmarks$landmarks)) {
		landmarks$landmarks[[i]] <- list(NULL,NULL)
	}

######broken code to reimport landmarks
	for(i in 1:length(filelist3$list)) {
		if(ncol(filelist3$list[[i]]) > 6) {
			if(!is.na(filelist3$list[[i]]$l1x[1])) {
				temp1 <- c(as.numeric(na.omit(filelist3$list[[i]]$l1x)),as.numeric(na.omit(filelist3$list[[i]]$l1y)),as.numeric(na.omit(filelist3$list[[i]]$l1z)))
				temp2 <- c(as.numeric(na.omit(filelist3$list[[i]]$l2x)),as.numeric(na.omit(filelist3$list[[i]]$l2y)),as.numeric(na.omit(filelist3$list[[i]]$l2z)))
				temp3 <- c(as.numeric(na.omit(filelist3$list[[i]]$l3x)),as.numeric(na.omit(filelist3$list[[i]]$l3y)),as.numeric(na.omit(filelist3$list[[i]]$l3z)))
				names(temp1) <- c("lx","ly","lz")
				names(temp2) <- c("lx","ly","lz")
				names(temp3) <- c("lx","ly","lz")
				landmarks$landmarks[[i]][[1]] <- rbind(temp1, temp2, temp3)

			}
		
			if(!is.na(filelist3$list[[i]]$ml[1])) {
				temp4 <- as.numeric(na.omit(filelist3$list[[i]]$ml))
				landmarks$landmarks[[i]][[2]] <- temp4
			}
		}
	}
######broken code to reimport landmarks

	pos_destroy_me <- observeEvent(position$pos, {

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
				if(!is.null(landmarks$landmarks[[position$pos]][[1]]))  {

					p1 <- t(matrix(landmarks$landmarks[[position$pos]][[1]][1,c(1:3)]))
					p2 <- t(matrix(landmarks$landmarks[[position$pos]][[1]][2,c(1:3)]))
					p3 <- t(matrix(landmarks$landmarks[[position$pos]][[1]][3,c(1:3)]))

					points3d(p1, size=20, col="white", box=FALSE)
					points3d(p2, size=20, col="green", box=FALSE)
					points3d(p3, size=20, col="blue", box=FALSE)
				}

				if(!is.null(landmarks$landmarks[[position$pos]][[2]]))  {
					mp <- filelist3$list[[position$pos]][landmarks$landmarks[[position$pos]][[2]],]
					points3d(mp, size=20, col="black", box=FALSE)
				}
			}
			else {
				points3d(c(0,0,0), size=3, col="white", box=FALSE, aspect = "iso")
			}


			#axes3d(c('x++', 'y++', 'z++'))

			title3d(main = input$aligndata$name[position$pos], col = "DODGERBLUE")
			rglwidget()
		})
		removeModal()  
	})

})

observeEvent(input$simplify, {
	if(length(input$aligndata$datapath) > 0) {
		JuliaSetup(add_cores = ncorespc$ncorespc, source = TRUE, recall_libraries = TRUE)
		showModal(modalDialog(title = "Point cloud K-means simplification started...", easyClose = FALSE, footer = NULL))
		if(input$alln == "Present") {		
			ttt <- filelist3$list[[position$pos]]
			filelist3$list[[position$pos]] <- kmeans.3d(filelist3$list[[position$pos]], cluster = vara$vara)
			if(!is.null(landmarks$landmarks[[position$pos]][[2]])) {
				tempp <- julia_call("AD3D", as.matrix(ttt[landmarks$landmarks[[position$pos]][[2]],]), as.matrix(filelist3$list[[position$pos]]))
				landmarks$landmarks[[position$pos]][[2]] <- unique(which(tempp < tva$tva, arr.ind = TRUE)[,2])
				if(length(landmarks$landmarks[[position$pos]][[2]]) == 0) { landmarks$landmarks[[position$pos]][[2]] <- NULL }
			}
		}
		if(input$alln == "All") {	
			ll <- length(filelist3$list)
			for (i in 1:ll) {	
				ttt <- filelist3$list[[i]]
				filelist3$list[[i]] <- kmeans.3d(filelist3$list[[i]], cluster = vara$vara)

				if(!is.null(landmarks$landmarks[[i]][[2]])) {
					tempp <- julia_call("AD3D", as.matrix(ttt[landmarks$landmarks[[i]][[2]],]), as.matrix(filelist3$list[[i]]))
					landmarks$landmarks[[i]][[2]] <- unique(which(tempp < tva$tva, arr.ind = TRUE)[,2])
					if(length(landmarks$landmarks[[i]][[2]]) == 0) { landmarks$landmarks[[i]][[2]] <- NULL }
				}
			}
		}
				JuliaSetup(remove_cores = TRUE) #clean up workers
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

observeEvent(input$start, {
	if(length(input$aligndata$datapath) > 0) {
		showModal(modalDialog(title = "Digitization has started...Please check the RGL window.", easyClose = FALSE, footer = NULL))
		temp_p <- filelist3$list[[position$pos]]
		landmarks$landmarks[[position$pos]][[1]] <- digitize.3d(temp_p, type = "single")
		removeModal()  
	}
})
observeEvent(input$start2, {
	if(length(input$aligndata$datapath) > 0) {
		showModal(modalDialog(title = "Digitization has started...Please check the RGL window.", easyClose = FALSE, footer = NULL))
		temp_p <- filelist3$list[[position$pos]]
		landmarks$landmarks[[position$pos]][[2]] <- digitize.3d(temp_p, type = "multiple")
		removeModal()  
	}
})

observeEvent(input$RGB1, {
	if(length(input$aligndata$datapath) > 0) {
		JuliaSetup(add_cores = ncorespc$ncorespc, source = TRUE, recall_libraries = TRUE)
		showModal(modalDialog(title = "RGB landmark extraction has started...", easyClose = FALSE, footer = NULL))
		if(input$alln == "Present") {		
			temp_p <- filelist3$list[[position$pos]]
			landmarks$landmarks[[position$pos]][[1]] <- RGB.locator.3d(temp_p, r = red$red, g = green$green, b = blue$blue,type = "landmark")[[1]]
		}
		if(input$alln == "All") {		
			ll <- length(filelist3$list)
			for (i in 1:ll) {	
				landmarks$landmarks[[i]][[1]] <- RGB.locator.3d(filelist3$list[[i]], r = red$red, g = green$green, b = blue$blue, type = "landmark")[[1]]
			}
		}
		JuliaSetup(remove_cores = TRUE) #clean up workers
		removeModal() 
	} 
})
observeEvent(input$RGB2, {
	if(length(input$aligndata$datapath) > 0) {
		JuliaSetup(add_cores = ncorespc$ncorespc, source = TRUE, recall_libraries = TRUE)
		showModal(modalDialog(title = "RGB fracture extraction has started...", easyClose = FALSE, footer = NULL))
		if(input$alln == "Present") {		
			temp_p <- filelist3$list[[position$pos]]
			landmarks$landmarks[[position$pos]][[2]] <- RGB.locator.3d(temp_p, f = fra$fra,type = "fracture",f_threshold = fracturet$fracturet)[[1]]
		}
		if(input$alln == "All") {		
			ll <- length(filelist3$list)
			for (i in 1:ll) {	
				landmarks$landmarks[[i]][[2]] <- RGB.locator.3d(filelist3$list[[i]], f = fra$fra, type = "fracture", f_threshold = fracturet$fracturet)[[1]]
			}
		}
		JuliaSetup(remove_cores = TRUE) #clean up workers
		removeModal()  
	}
})


observeEvent(input$RGB3, {
	if(length(input$aligndata$datapath) > 0) {
		showModal(modalDialog(title = "RGB calibration has started...Please select colors for landmarks 1-3 and fracture margin...", easyClose = FALSE, footer = NULL))

		temp_p <- filelist3$list[[position$pos]]
		RGBtemp <- RGB.calibrate.3d(temp_p)

		red$red = RGBtemp[1,]
		green$green = RGBtemp[2,]
		blue$blue = RGBtemp[3,]
		fra$fra = RGBtemp[4,]
		
		removeModal()  
	}
})


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
				r1 <- length(landmarks$landmarks[[i]][[2]])
				saveme <- cbind(x = filelist3$list[[i]][,1], 
							 y = filelist3$list[[i]][,2], 
							 z = filelist3$list[[i]][,3], 
							 l1x = c(landmarks$landmarks[[i]][[1]][1,1], rep(NA, nrow(filelist3$list[[i]]) - 1)),  
							 l1y = c(landmarks$landmarks[[i]][[1]][1,2], rep(NA, nrow(filelist3$list[[i]]) - 1)),  
							 l1z = c(landmarks$landmarks[[i]][[1]][1,3], rep(NA, nrow(filelist3$list[[i]]) - 1)), 
				  			 l2x = c(landmarks$landmarks[[i]][[1]][2,1], rep(NA, nrow(filelist3$list[[i]]) - 1)),
							 l2y = c(landmarks$landmarks[[i]][[1]][2,2], rep(NA, nrow(filelist3$list[[i]]) - 1)),
							 l2z = c(landmarks$landmarks[[i]][[1]][2,3], rep(NA, nrow(filelist3$list[[i]]) - 1)), 
						  	 l3x = c(landmarks$landmarks[[i]][[1]][3,1], rep(NA, nrow(filelist3$list[[i]]) - 1)),
							 l3y = c(landmarks$landmarks[[i]][[1]][3,2], rep(NA, nrow(filelist3$list[[i]]) - 1)),
							 l3z = c(landmarks$landmarks[[i]][[1]][3,3], rep(NA, nrow(filelist3$list[[i]]) - 1)),
							 ml = c(landmarks$landmarks[[i]][[2]], rep(NA, nrow(filelist3$list[[i]]) - r1))
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
	