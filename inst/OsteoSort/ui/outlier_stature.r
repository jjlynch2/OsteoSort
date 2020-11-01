outlier_stature <- tabPanel("Stature",icon = icon("user", lib="font-awesome"),
	sidebarLayout(
		sidebarPanel(
			uiOutput('resettableInput4'),
			checkboxInput("custom", "Specify Slope and Intercept", FALSE),
			conditionalPanel(condition = "input.custom",
				numericInput("slope", label = "Slope", value = "", min=0,max=999,step=0.01),
				numericInput("intercept", label = "Intercept", value = "", min=0,max=999,step=0.01)
			),
			conditionalPanel(condition = "!input.custom",
				uiOutput("stature_reference")
			),
			uiOutput("testtypem1"),
			fluidRow(
				column(6,
					actionButton("settings4","Settings", icon=icon("keyboard-o"))
				),
				column(6,
					actionButton("pro4","Process ", icon = icon("cog"))
				)
			),
			fluidRow(br()),
			fluidRow(
				column(6,
					actionButton("clearFile4", "Clear   ", icon = icon("window-close"))
				),
				column(6,
					downloadButton("outlierdownload4", "Save    ")
				)
			),
			tags$style(type = "text/css", "#settings4 { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#pro4 { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#clearFile4 { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#outlierdownload4 { width:100%; font-size:85%; background-color:#126a8f }"),
			width=3
		),
		mainPanel(
			htmlOutput('outliercontent4'),
			imageOutput('plotoutlier4'),
			tabsetPanel(id="tabSelectedoutlier",
				tabPanel("Upper outliers",
					DT::dataTableOutput('tjbingworka4')
				),		
				tabPanel("Lower outliers",
					DT::dataTableOutput('tjbingworkb4')
			 	),
			 	tabPanel("Non-outliers",
		 			DT::dataTableOutput('tjbingworkc4')
			 	)
		 	),
		 	bsModal("settingsoutlier4", title = "Settings", trigger = "settings4", size = "medium", 
		 		tabsetPanel(id="tabSelected2",
					tabPanel("Output Paramters",
						uiOutput("fileoutputstature1"),
						uiOutput("fileoutputstature2")
					),
					tabPanel("Statistical Parameters",
						fluidRow(
							column(6,
								radioButtons('method4', '', c(standard_deviation='Standard_deviation', quartiles='Quartiles'),'Standard_deviation')
							),
							column(6,
								conditionalPanel(condition = "input.method4 == 'Standard_deviation'",
									sliderInput(inputId = "standard_dev4", label = "Standard Deviation Cutoff", min=0.5, max=10, value=c(2,2), step = 0.1)
								),
								conditionalPanel(condition = "input.method4 == 'Quartiles'",
									sliderInput(inputId = "Quartiles4", label = "Interquartile Cutoff", min=0.5, max=10, value=c(1.5,1.5), step = 0.1)
								)
							)
						)
					)
				)
			)
		)
	)
)
