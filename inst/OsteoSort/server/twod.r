
	output$contents2D <- renderUI({
	   HTML(paste("Select the parameters and upload images to begin"))
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
		fileInput('leftimages', 'Upload first image set', accept=c('jpeg', "jpg"), multiple = TRUE)
		fileInput('rightimages', 'Upload second image set', accept=c('jpeg', "jpg"), multiple = TRUE)
	})

	
	ncores2D <- reactiveValues(ncores2D = 1)
	
	observeEvent(input$ncores2D, {
		ncores2D$ncores2D <- input$ncores2D
	})
	output$ncores2D <- renderUI({
		sliderInput(inputId = "ncores2D", label = "Number of cores", min=1, max=detectCores(), value=1, step =1)
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
				width = 600,
				height = 600,
				alt = "A"
			)
		}, deleteFile = FALSE)
	})
	#renders temporary mean


	observeEvent(input$pro2D, {
		output$contents2D <- renderUI({
		   HTML(paste(""))
		})	

		showModal(modalDialog(title = "Calculation has started...Window will update when finished.", easyClose = FALSE, footer = NULL))
	
	
		withProgress(message = 'Calculation has started',
			detail = 'This may take a while...', value = 0, {       
				for (i in 1:25) {
				incProgress(1/25)
				Sys.sleep(0.10)
		     
			}
		})
		 	
		leftimages <- input$leftimages$datapath
		rightimages <- input$rightimages$datapath

		file.copy(input$leftimages$datapath, input$leftimages$name)
		file.copy(input$rightimages$datapath, input$rightimages$name)

		

		#if(nrow(leftimages) > limit1) {} #Do not run if limit
		#if(nrow(rightimages) > limit2) {} #Do not run if limit

		out1 <- outline.images(imagelist1 = input$rightimages$name, imagelist2 = input$leftimages$name, threshold =input$nthreshold, scale = input$scale2D, mirror = input$mirror2D, npoints = input$npoints2D, smooth = input$nsmooth2D, nb.h = input$efaH2D)
		out2 <- match.2d.invariant(outlinedata = out1,  oo = input$fileoutput2D, sessiontempdir = sessiontemp, stdout = FALSE, trans = input$trans2D, threads = ncores2D$ncores2D, testme = input$distance2D, mspec =, meanit = input$meanit2D)
		direc <- out2[[3]]
		jpeg(paste("graph", ".jpeg", sep=""), height = 1200, width = 1200)
		dev.control('enable')
		plot(meann, col="white", xlim=c(min(homolog),max(homolog)), ylim=c(max(homolog),min(homolog)))
		for(a in 1:dim(homolog)[3]) {
			points(homolog[,,a], col=a)	
		}
		points(meann, col="black", bg="blue", pch=23, cex=2)
		plotted <- recordPlot()
		dev.off()

		output$regplot <- renderPlot(plotted)
		output$table2D <- DT::renderDataTable({
			DT::datatable(out2[[2]], options = list(lengthMenu = c(5,10,15,20,25,30), pageLength = 10), rownames = FALSE)
		})



		files <- list.files(direc, recursive = TRUE)
		setwd(direc)
		nooutput <- lapply(files, function(x) {
			zip(zipfile = direc, files = x)
		})
		setwd(sessiontemp)
			#Download handler       
		output$downloadData2D <- downloadHandler(
			filename <- function() {
				paste("results.zip")
			},      
			content <- function(file) {
				setwd(direc)
				file.copy(paste(direc,'.zip',sep=''), file)  
				setwd(sessiontemp)    
			},
			contentType = "application/zip"
		)
		setwd(sessiontemp)



		for(i in 10) { gc() } #clean up 
		removeModal()  
	})
	