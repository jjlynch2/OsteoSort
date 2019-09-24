antemortem_multiple <- tabPanel("Multiple",icon = icon("gears", lib="font-awesome"),
	sidebarLayout(
		sidebarPanel(
			uiOutput("stature_reference_antem"),
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
					actionButton("settingsantem","Settings", icon=icon("keyboard-o"))
				),
				column(6,
					actionButton("proantestatm","Process ", icon = icon("cog"))
				)
			),
			fluidRow(br()),
			fluidRow(
				column(6,
					actionButton("clearFile1ante", "Clear   ", icon = icon("window-close"))
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
				tabPanel("Graph",
					imageOutput('multiple_plot_na_ante')
				),
		 		bsModal("settingsante2m", title = "Settings", trigger = "settingsantem", size = "medium", 
			 		tabsetPanel(id="tabSelected2m",
						tabPanel("Output Paramters",
							uiOutput('fileoutputant1m'),
							uiOutput('multiple_file_output_graph_ante'),
							conditionalPanel(condition = "input.multiple_file_output_graph_ante", 
								uiOutput('labtfa')
							)
						),	
						tabPanel("Statistical Parameters",
							uiOutput("alphalevelsantestatm")
						),
						tabPanel("Computational Parameters",
							uiOutput('ncoresm')
						)
					)
				)
			)
		)
	)
)
