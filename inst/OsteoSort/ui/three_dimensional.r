three_dimensional <- tabPanel("3D Antimere",icon = icon("sort-by-order", lib="glyphicon"),
	titlePanel(""),
	sidebarLayout(
		sidebarPanel(
			uiOutput('resettableInput3D'),	
			uiOutput('resettableInput3DD'),
			fluidRow(
				column(6,
					actionButton("settings3D","Settings", icon=icon("keyboard-o"))
				),
				column(6,
					actionButton("pro3D","Process ", icon = icon("cog"))
				)
			),
			fluidRow(br()),
			fluidRow(
				column(6,
					actionButton("clearFile3D", "Clear   ", icon = icon("window-close"))
				),
				column(6,
					downloadButton("downloadData3D", "Save    ")
				)
			),
			tags$style(type = "text/css", "#settings3D { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#pro3D { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#clearFile3D { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#downloadData3D { width:100%; font-size:85%; background-color:#126a8f }"),
			width = 3
		),
		mainPanel(
			uiOutput("contents3D"),
			tabsetPanel(id="tabSelected3D",
				tabPanel("Results ",
					DT::dataTableOutput('table3D')
				),
				tabPanel("Interactive",
					forceNetworkOutput("forceNetworkOSMd")
				)
		 	),
			bsModal("settings3DD", title = "Settings", trigger = "settings3D", size = "large", 
				tabsetPanel(id="tabSelected33",
					tabPanel("Output Parameters",
						uiOutput('fileoutput3Dexcel1'),
						uiOutput('fileoutput3Dexcel2'),
						uiOutput("forcd")
					),
					tabPanel("Statistical Parameters",
						fluidRow(
							column(4,
								h4("Distance"),
								uiOutput('shortlistn3D')
							)
						)
					),
					tabPanel("Computational Parameters",
						uiOutput('ncores3D')
					)
				)
			)
		)
	)
)
