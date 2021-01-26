two_dimensional <- tabPanel("2D Antimere",icon = icon("picture", lib="glyphicon"),
	titlePanel(""),
	sidebarLayout(
		sidebarPanel(
			uiOutput('resettableInput2D'),
			uiOutput('resettableInput2DD'),
			uiOutput('pwspec'),
			fluidRow(
				column(6,
					actionButton("settings2D","Settings", icon=icon("keyboard-o"))
				),
				column(6,
					actionButton("pro2D","Process ", icon = icon("cog"))
				)
			),
			fluidRow(br()),
			fluidRow(
				column(6,
					actionButton("clearFile2D", "Clear   ", icon = icon("window-close"))
				),
				column(6,
					downloadButton("downloadData2D", "Save    ")
				)
			),
			tags$style(type = "text/css", "#settings2D { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#pro2D { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#clearFile2D { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#downloadData2D { width:100%; font-size:85%; background-color:#126a8f }"),
			width = 3
		),
		mainPanel(
			uiOutput("contents2D"),
			uiOutput("tabpanpan"),
			bsModal("settings2DD", title = "Settings", trigger = "settings2D", size = "large", 
				tabsetPanel(id="tabSelected2",
					tabPanel("Output Parameters",
						uiOutput('fileoutput2Dexcel1'),
						uiOutput('fileoutput2Dexcel2'),
						uiOutput('fileoutput2Dplot'),
						uiOutput('fileoutput2Dtps'),
						uiOutput("forc2d")
					),
					tabPanel("Statistical Parameters",
						fluidRow(
							column(4,
								h4("Outline"),
								uiOutput('nthreshold'),
								uiOutput('mirror2D'),
								uiOutput('efa_options3'),
				 				conditionalPanel(condition = "input.fragcomp == 'Complete'",
									uiOutput('efa_options1'),
									uiOutput('efa_options2')
								)
							),
							column(4, 
								h4("Registration"),
								uiOutput('icp2D')
							),
							column(4,
								h4("Distance"),
								uiOutput('max_avg_distance'),
								uiOutput('shortlistn'),
								uiOutput('hidedist')
							)
						)
					),
					tabPanel("Computational Parameters",
						uiOutput('ncores2D')
					)
				)
			)
		)
	)
)
