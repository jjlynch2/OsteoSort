antemortem_multiple <- tabPanel("Multiple",icon = icon("gears", lib="font-awesome"),
	sidebarLayout(
		sidebarPanel(
			uiOutput("stature_reference_antem"),
			htmlOutput('measurement_units_ante'),
			br(),
			fluidRow(
				column(12,
					selectInput("state_reference_ante_sidem", "Side", c(Left='Left', Right='Right'))
				),
				column(12,
					uiOutput("multiple_ante_elements"),
					uiOutput("multiple_measurements_ante")
				)
			),
			uiOutput('resettableInputante1'),
			uiOutput('resettableInputante2'),
			fluidRow(
				column(6,
					actionButton("settingsantem","Settings", icon=icon("sliders"))
				),
				column(6,
					actionButton("proantestatm","Process ", icon = icon("gear"))
				)
			),
			fluidRow(br()),
			fluidRow(
				column(6,
					actionButton("clearFile1ante", "Clear   ", icon = icon("rectangle-xmark"))
				),
				column(6,
					downloadButton("downloadantestatm", "Save    ")
				)
			),
			tags$style(type = "text/css", "#settingsantem { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#proantestatm { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#clearFile1ante { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#downloadantestatm { width:100%; font-size:85%; background-color:#126a8f }"),
			width=3
		),
		mainPanel(
			htmlOutput('antestat_outputm'),
			tabsetPanel(id="tabSelected",
				tabPanel("Not excluded",
		 			DT::dataTableOutput('antestat_table1m')
				),
		 		tabPanel("Excluded",
		 			DT::dataTableOutput('antestat_table2m')
				),
				tabPanel("Interactive",
					forceNetworkOutput("forceNetworkOSMante")
				),
		 		bsModal("settingsante2m", title = "Settings", trigger = "settingsantem", size = "medium", 
			 		tabsetPanel(id="tabSelected2m",
						tabPanel("Output Paramters",
							uiOutput('fileoutputant1m'),
							uiOutput("forcante")
						),	
						tabPanel("Statistical Parameters",
							fluidRow(
								column(6,
									uiOutput("alphalevelsantestatm")
								)
							)
						)
					)
				)
			)
		)
	)
)
