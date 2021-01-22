antemortem_single <- tabPanel("Single",icon = icon("gear", lib="font-awesome"),
	sidebarLayout(
		sidebarPanel(
			uiOutput("stature_reference_ante"),
			fluidRow(
				column(12,
					selectInput("state_reference_ante_side", "Side", c(Left='Left', Right='Right'))
				),
				column(12,
					uiOutput("single_ante_elements")
				),
				column(6,
					uiOutput("antestat_input_t"),
					textInput(inputId = 'Antemortem_ID_ante', label = 'Antemortem ID', value = 'X1')
				),
				column(6,
					uiOutput("single_measurements_ante"),
					textInput(inputId = 'Postmortem_ID_ante', label = 'Postmortem ID', value = 'Y1')
				)
			),
			fluidRow(
				column(6,
					actionButton("settingsante","Settings", icon=icon("keyboard-o"))
				),
				column(6,
					actionButton("proantestat","Process ", icon = icon("cog"))
				)
			),
			fluidRow(br()),
			fluidRow(
				column(6),
				column(6,
					downloadButton("downloadantestat", "Save    ")
				)
			),
			tags$style(type = "text/css", "#settingsante { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#proantestat { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#downloadantestat { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(".well {border-width:1px; border-color:#126a8f;}"),
			width=3
		),
		mainPanel(
			htmlOutput('antestat_output'),
			imageOutput('plotplotante'),
			DT::dataTableOutput('antestat_table'),
		 	bsModal("settingsante2", title = "Settings", trigger = "settingsante", size = "medium", 
		 		tabsetPanel(id="tabSelected2s",
					tabPanel("Output Paramters",
						uiOutput("fileoutputant1"),
						uiOutput("fileoutputant2")
					),	
					tabPanel("Statistical Parameters",
						fluidRow(column(6,
								uiOutput("alphalevelsantestat")
							)
						)
					)
				)
			)
		)
	)
)
