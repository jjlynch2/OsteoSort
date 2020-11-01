outlier_metric <- tabPanel("Metric",icon = icon("line-chart", lib="font-awesome"),
	sidebarLayout(
		sidebarPanel(
			uiOutput('resettableInput3'),
			uiOutput("testtypem"),
			fluidRow(
				column(6,
					actionButton("settings3","Settings", icon=icon("keyboard-o"))
				),
				column(6,
					actionButton("pro3","Process ", icon = icon("cog"))
				)
			),
			fluidRow(br()),
			fluidRow(
				column(6,
					actionButton("clearFile3", "Clear   ", icon = icon("window-close"))
				),
				column(6,
					downloadButton("outlierdownload", "Save    ")
				)
			),
			tags$style(type = "text/css", "#settings3 { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#pro3 { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#clearFile3 { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#outlierdownload { width:100%; font-size:85%; background-color:#126a8f }"),
			width=3
		),
		mainPanel(
			htmlOutput('outliercontent'),
			imageOutput('plotoutlier'),
			tabsetPanel(id="tabSelectedoutlier",
				tabPanel("Upper outliers",
					DT::dataTableOutput('tjbingworka')
				),		
				tabPanel("Lower outliers",
			 		DT::dataTableOutput('tjbingworkb')
			 	),
			 	tabPanel("Non-outliers",
			 		DT::dataTableOutput('tjbingworkc')
			 	)
		 	),
		 	bsModal("settingsoutlier", title = "Settings", trigger = "settings3", size = "medium", 
		 		tabsetPanel(id="tabSelected2",
					tabPanel("Output Paramters",
						uiOutput("fileoutputl1"),
						uiOutput("fileoutputl2")
					),
					tabPanel("Statistical Parameters",
						fluidRow(
							column(6,
								radioButtons('method', '', c(standard_deviation='Standard_deviation', quartiles='Quartiles'),'Standard_deviation')
							),
							column(6,
								conditionalPanel(condition = "input.method == 'Standard_deviation'",
									sliderInput(inputId = "standard_dev", label = "Standard Deviation Cutoff", min=0.5, max=10, value=c(2.0,2), step = 0.1)
								),
								conditionalPanel(condition = "input.method == 'Quartiles'",
									sliderInput(inputId = "Quartiles", label = "Interquartile Cutoff", min=0.5, max=10, value=c(1.5,1.5), step = 0.1)
								)
							)
						)
					)
				)
			)
		)
	)
)
