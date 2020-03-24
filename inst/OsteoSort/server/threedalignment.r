filelist3 <- reactiveValues(list=list())
position <- reactiveValues(pos = 1)
landmarks <- reactiveValues(landmarks=list())
landmarks_align <- reactiveValues(landmarks_align=list())
tt1 <- reactiveValues(tt1 = matrix(0))
direc <- reactiveValues(direc1 = c())
output$ncorespc <- renderUI({
	sliderInput(inputId = "ncorespc", label = "Number of cores", min=1, max=detectCores(), value=detectCores()-1, step =1)
})

output$vara <- renderUI({
	sliderInput(inputId = "vara", label = "K-means", min=0.01, max=1, value=0.01, step = 0.01)
})

output$sft <- renderUI({
	sliderInput(inputId = "sft", label = "Simplify fracture", min=0.1, max=500, value=1, step = 0.1)
})

output$fmt <- renderUI({
	sliderInput(inputId = "fmt", label = "Margin threshold", min=1, max=500, value=125, step = 1)
})

output$fr <- renderUI({
	sliderInput(inputId = "fr", label = "Fracture radius", min=0.1, max=500, value=1, step = 0.1)
})


output$landmark_switch <- renderUI({
	checkboxGroupInput(inputId = "landmark_switch", label = "landmarks", choices = c(1,2,3,4,5,6,7,8,9,10), selected = c(1,2,3), inline=TRUE)
})


observeEvent(input$landmarks_dig, {
	if(length(input$aligndata$datapath) > 0) {
		showModal(modalDialog(title = "Digitization has started...Please check the RGL window.", easyClose = FALSE, footer = NULL))
		if(input$alln == "Present") {
			landmarks_align$landmarks_align[[position$pos]] <- digitize.3d(tt1$tt1, landmarks=input$landmark_switch)
		}
		else if(input$alln == "All") {
			ll <- length(filelist3$list)
			for (i in 1:ll) {
				ttt <- import.tmp.data.pct(filelist3$list[[i]], sessiontemp)
				landmarks_align$landmarks_align[[i]] <- digitize.3d(ttt, landmarks=input$landmark_switch)
   				try(rgl.close())
			}
		}
	removeModal()  
	}
})




output$resettableInput3Da <- renderUI({
	input$clearFile3Da
	input$uploadFormat
	fileInput('aligndata', 'Upload data set', accept=c("xyz"), multiple = TRUE)
})

observeEvent(input$clearFile3Da, {
	setwd(sessiontemp)
	delete.tmp.data.pct(filelist3$list, sessiontemp, direc$direc1)
	if(!is.null(input$aligndata$datapath)) {
		file.remove(input$aligndata$datapath)
		file.remove(input$aligndata$name)
	}
	try(rgl.close())
	filelist3$list = list()
	position$pos = 1
	landmarks$landmarks = list()
	landmarks_align$landmarks_align = list()
	fileInput('aligndata', 'Upload data set', accept=c("xyz"), multiple = TRUE)
	tt1$tt1 <- matrix(0,1,2)
})


observeEvent(input$reimport, {
		if(input$alln == "Present") {
			ttt <- import.tmp.data.pct(filelist3$list[[position$pos]], sessiontemp)
			if(ncol(ttt) == 5) {
				landmarks$landmarks[[position$pos]] <- which(ttt[,4] != 0)
				ttt_temp <- unique(ttt[ttt[,5] != 0,5])
				p_temp <- rep(0,10)
				for(p in ttt_temp) {
					 p_temp[p] <- which(ttt[,5] == p)
				}
				landmarks_align$landmarks_align[[position$pos]] <- p_temp
			} else if (ncol(ttt) == 4) {
				landmarks$landmarks[[position$pos]] <- which(ttt[,4] != 0)
				landmarks_align$landmarks_align[[position$pos]] <- rep(0,10)
			}
		} 
		else if(input$alln == "All") {
			ll <- length(filelist3$list)
			for (i in 1:ll) {
				ttt <- import.tmp.data.pct(filelist3$list[[i]], sessiontemp)
				if(ncol(ttt) == 5) {
					landmarks$landmarks[[i]] <- which(ttt[,4] != 0)
					ttt_temp <- unique(ttt[ttt[,5] != 0,5])
					p_temp <- rep(0,10)
					for(p in ttt_temp) {
						 p_temp[p] <- which(ttt[,5] == p)
					}
					landmarks_align$landmarks_align[[i]] <- p_temp
				} else if (ncol(ttt) == 4) {
					landmarks$landmarks[[i]] <- which(ttt[,4] != 0)
					landmarks_align$landmarks_align[[i]] <- rep(0,10)
				}
			}
		}
})

observeEvent(input$aligndata$datapath, {
	file.copy(input$aligndata$datapath, input$aligndata$name)
	filelist3$list <- input$aligndata$name
	landmarks$landmarks <- rep(list(NULL), length(filelist3$list)) #populate as NULL x file length on upload
	landmarks_align$landmarks_align <- rep(list(NULL), length(filelist3$list)) #populate as NULL x file length on upload

	observeEvent(position$pos, {

		showModal(modalDialog(title = "Rendering...", easyClose = FALSE, footer = NULL))
		output$webgl3Dalign <- renderRglwidget ({
   			try(rgl.close())

			if(length(filelist3$list) != 0) {
				tt1$tt1 <- import.tmp.data.pct(filelist3$list[[position$pos]], sessiontemp)
			}
			else {
				tt1$tt1 = matrix(0)
			}

			if(ncol(tt1$tt1) >5) {
				if(any(is.na(tt1$tt1[,c(4:6)]))) { 
					cc <- "dimgrey"
				}
				else {
					cc <- rgb(tt1$tt1[,c(4:6)], max=255)
				}
			}
			else {cc <- "dimgrey"}
			if(length(tt1$tt1) != 0) {
				plot3d(tt1$tt1, size=3, col=cc, box=FALSE, axes=FALSE, aspect = "iso", xlab="",ylab="",zlab="")
				if(!is.null(landmarks$landmarks[[position$pos]]))  {
					mp <- tt1$tt1[landmarks$landmarks[[position$pos]],]
					spheres3d(mp, size=10, col="DODGERBLUE")
				}
				if(!is.null(landmarks_align$landmarks_align[[position$pos]]))  {
					mp <- tt1$tt1[landmarks_align$landmarks_align[[position$pos]][landmarks_align$landmarks_align[[position$pos]] != 0],]
					spheres3d(mp, size=10, col="red")
				}
				output$coordinates <- renderUI({
					HTML(paste("<strong>","<br/>","Coordinates: ",   "<font color=\"#00688B\">", nrow(tt1$tt1), "</font>",
							 "<strong>","<br/>","Specimen: ",   "<font color=\"#00688B\">", input$aligndata$name[position$pos], "</font>"

					))
				})   
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
			ttt <- tt1$tt1
			te <- kmeans.3d(ttt, cluster = input$vara, threads = input$ncorespc)
			tt1$tt1 <- te
			if(!is.null(landmarks$landmarks[[position$pos]])) {
				tempp <- HD_KDTree_Ind(as.matrix(te[,1:3]), as.matrix(ttt[landmarks$landmarks[[position$pos]],c(1:3)]), threads = input$ncorespc, k = input$sft)
				landmarks$landmarks[[position$pos]] <- unique(tempp[which(tempp[,1] <= input$sft),2])
				if(length(landmarks$landmarks[[position$pos]]) == 0) { landmarks$landmarks[position$pos] <- list(NULL) }
			}
			write.table(te, sep = ' ', file = filelist3$list[[position$pos]], row.names = FALSE)
		}
		if(input$alln == "All") {	
			ll <- length(filelist3$list)
			for (i in 1:ll) {	
				ttt <- import.tmp.data.pct(filelist3$list[[i]], sessiontemp)
				te <- kmeans.3d(ttt, cluster = input$vara, threads = input$ncorespc)
				if(!is.null(landmarks$landmarks[[i]])) {
					tempp <- HD_KDTree_Ind(as.matrix(te[,1:3]), as.matrix(ttt[landmarks$landmarks[[i]],c(1:3)]), threads = input$ncorespc, k = input$sft)
					landmarks$landmarks[[i]] <- unique(tempp[which(tempp[,1] <= input$sft),2])
					if(length(landmarks$landmarks[[i]]) == 0) { landmarks$landmarks[i] <- list(NULL) }
				}
				if(i == position$pos) {
					tt1$tt1 <- te
				}
				write.table(te, sep = ' ', file = filelist3$list[[i]], row.names = FALSE)
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
			landmarks$landmarks[[position$pos]] <- digitize.3d(tt1$tt1)
		}
		else if(input$alln == "All") {
			ll <- length(filelist3$list)
			for (i in 1:ll) {
				ttt <- import.tmp.data.pct(filelist3$list[[i]], sessiontemp)
				landmarks$landmarks[[i]] <- digitize.3d(ttt)
   				try(rgl.close())
			}
		}
	removeModal()  
	}
})


observeEvent(input$start1, {
	if(length(input$aligndata$datapath) > 0) {
		showModal(modalDialog(title = "Fracture margin identification has started...", easyClose = FALSE, footer = NULL))
		if(input$alln == "Present") {
			results <- julia_call("radius_search", as.matrix(tt1$tt1), input$fr)
			landmarks$landmarks[[position$pos]] <- which(results <= input$fmt)
			if(length(landmarks$landmarks[[position$pos]]) == 0) {
				landmarks$landmarks[position$pos] <- list(NULL)
			}
		}
		else if(input$alln == "All") {
			ll <- length(filelist3$list)
			for (i in 1:ll) {
				ttt <- import.tmp.data.pct(filelist3$list[[i]], sessiontemp)
				results <- julia_call("radius_search", as.matrix(ttt), input$fr)
				landmarks$landmarks[[i]] <- which(results <= input$fmt)
				if(length(landmarks$landmarks[[i]]) == 0) {
					landmarks$landmarks[i] <- list(NULL)
				}
			}
		}
		removeModal()
	}
})


output$savedata <- downloadHandler(
	filename <- function() {
		paste("aligned.zip")
	},
	content <- function(file) {
		direc$direc1 <- OsteoSort:::analytical_temp_space(output_options <- TRUE, sessiontempdir = sessiontemp)
		setwd(sessiontemp)
		setwd(direc$direc1)
		for(i in 1:length(filelist3$list)) {
			saveme <- import.tmp.data.pct(filelist3$list[[i]], sessiontemp)
			if(ncol(saveme) == 3) {
				saveme <- cbind(saveme, 0,0)
			} else if(ncol(saveme) == 4) {
				saveme <- cbind(saveme, 0)
			}
			colnames(saveme) <- c("x","y","z","f","a")
			if(!is.null(landmarks$landmarks[[i]])) {
				saveme[landmarks$landmarks[[i]],4] <- 1
			}
			if(!is.null(landmarks_align$landmarks_align[[i]])) {
				for(la in 1:10) {
					lat <- landmarks_align$landmarks_align[[i]][la]
					if(lat != 0) {
						saveme[lat,5] <- la
					}
				}
			}
			write.table(saveme, sep = ' ', file = input$aligndata$name[i], row.names = FALSE)
		}
		setwd(sessiontemp)
		files <- list.files(direc$direc1, recursive = TRUE)
		setwd(direc$direc1)
		zip:::zipr(zipfile = paste(direc$direc1,'.zip',sep=''), files = files)
		file.copy(paste(direc$direc1,'.zip',sep=''), file)
		setwd(sessiontemp)
	},
	contentType = "application/zip"
)
	
