multiple_osteometric_sorting <- tabPanel("Multiple",icon = icon("gears", lib="font-awesome"),
	titlePanel(""),
	sidebarLayout(
		sidebarPanel(
			uiOutput("multiple_reference"),
			uiOutput("multiple_analysis"),
			uiOutput("testtype2"),
			uiOutput('resettableInput'),
			conditionalPanel(condition = "input.multiple_analysis == 'Antimere t-test'",
				uiOutput("multiple_element_pair_match"),
				uiOutput("multiple_measurement_antimere")
			),
			conditionalPanel(condition = "input.multiple_analysis == 'Non-Antimere t-test'",
				selectInput("multiple_non_antimere_side", "Side", c(Left='Left', Right='Right')),
				uiOutput("multiple_element_non_antimere"),
				fluidRow(
					column(6,
						uiOutput("multiple_measurements_non_antimere_a")
					),
					column(6,
						uiOutput("multiple_measurements_non_antimere_b")
					)
				)
			),
			conditionalPanel(condition = "input.multiple_analysis == 'Non-Antimere regression'",
				fluidRow(
					column(6,
						selectInput("multiple_association_side_a", "Side", c(Left='Left', Right='Right')),
						uiOutput("multiple_elements_association_a"),
						uiOutput("multiple_measurement_association_a")
					),
					column(6,
						selectInput("multiple_association_side_b", "Side", c(Left='Left', Right='Right')),
						uiOutput("multiple_elements_association_b"),
						uiOutput("multiple_measurement_association_b")
					)
				)
			),
			fluidRow(
				column(6,
					actionButton("settings1","Settings", icon=icon("keyboard-o"))
				),
				column(6,
					actionButton("pro","Process ", icon = icon("cog"))
				)
			),
			fluidRow(br()),
			fluidRow(
				column(6,
					actionButton("clearFile1", "Clear   ", icon = icon("window-close"))
				),
				column(6,
					downloadButton("downloadData", "Save    ")
				)
			),
			tags$style(type = "text/css", "#settings1 { width:100%; font-size:85%; background-color:#126a8f  }"),
			tags$style(type = "text/css", "#pro { width:100%; font-size:85%; background-color:#126a8f  }"),
			tags$style(type = "text/css", "#clearFile1 { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#downloadData { width:100%; font-size:85%; background-color:#126a8f }"),
			width=3
		),
		mainPanel(
			htmlOutput('multiple_contents'),
			tabsetPanel(id="tabSelected",
				tabPanel("Not excluded",
					DT::dataTableOutput('table')
				),
				tabPanel("Excluded",
					DT::dataTableOutput('tablen')
				),
				tabPanel("Rejected",
					DT::dataTableOutput('tablenr')
				),
				tabPanel("Graph",
					imageOutput('multiple_plot_na')
				),
				tabPanel("Interactive",
					forceNetworkOutput("forceNetworkOSM")
				),
				bsModal("settingsmultiple", title = "Settings", trigger = "settings1", size = "medium", 
					tabsetPanel(id="multiple_tab",
						tabPanel("Output Parameters",
							uiOutput("multiple_file_output1"),
							uiOutput("multiple_file_output_graph"),
							conditionalPanel(condition = "input.multiple_file_output_graph", 
								uiOutput("labtf")
							),
							uiOutput("forc")
						),
						tabPanel("Statistical Parameters",
							fluidRow(
								column(8,
								conditionalPanel(condition = "input.multiple_analysis == 'Non-Antimere regression'", 
									uiOutput("multiple_association_types")
								),
								conditionalPanel(condition = "input.multiple_analysis == 'Antimere t-test'", 
									uiOutput("multiple_ztransform")
								),
								conditionalPanel(condition = "!input.multiple_ztransform && input.multiple_analysis != 'Non-Antimere regression'",
									uiOutput("multiple_absolute_value"),
									uiOutput("multiple_boxcox"),
									uiOutput("multiple_mean"),
									uiOutput("multiple_tails")
								),
								uiOutput("multiple_common_alpha_level")
								)
							)
						),
						tabPanel("Computational Parameters",
							uiOutput('ncores')
						)
					)
				)
			)
		)
	)
)
