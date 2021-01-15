single_osteometric_sorting <- tabPanel("Single",icon = icon("gear", lib="font-awesome"),
	titlePanel(""),
	sidebarLayout(
		sidebarPanel(
			tags$style(type='text/css', ".selectize-input { font-size: 14px; line-height: 14px;} .selectize-dropdown { font-size: 14px; line-height: 14px; }"),
			tags$style(".irs-bar, .irs-bar-edge, .irs-single, irs.grid-pol {background: #126a8f; border-color: #126a8f;}"),
			uiOutput("single_reference"),
			uiOutput("single_analysis"),
			conditionalPanel(condition = "input.single_analysis == 'Antimere t-test'",
				uiOutput("single_element_pair_match"),
				fluidRow(
					column(6,
						h4("Left"),
						uiOutput("list_numeric_inputs_single_left")
					),
					column(6,
						h4("Right"),
						uiOutput("list_numeric_inputs_single_right")
					)
				)
			),
			conditionalPanel(condition = "input.single_analysis == 'Non_antimere regression'",
				fluidRow(
					column(6,
						selectInput("single_association_side_a", "Side", c(Left='Left', Right='Right')),
						uiOutput("single_elements_association_a"),
						uiOutput("list_numeric_inputs_single_A")
					),
					column(6,
						selectInput("single_association_side_b", "Side", c(Left='Left', Right='Right')),
						uiOutput("single_elements_association_b"),
						uiOutput("list_numeric_inputs_single_B")
					)
				)
			),
			conditionalPanel(condition = "input.single_analysis == 'Articulation t-test'",
				fluidRow(
					column(12,
						selectInput("single_non_antimere_side", "Side", c(Left='Left', Right='Right'))
					),
					column(12,
						uiOutput("single_element_non_antimere")
					),
					column(6,
						uiOutput("single_measurement_non_antimere_a")
					),
					column(6,
						uiOutput("single_measurement_non_antimere_b")
					)
				)
			),
			fluidRow(
				column(6,
					textInput(inputId = 'ID1', label = '1st ID #', value = 'X1')
				),
				column(6,
					textInput(inputId = 'ID2', label = '2nd ID #', value = 'Y1')
				)
			),
			fluidRow(
				column(6,
					actionButton("settings2","Settings", icon=icon("keyboard-o"))
				),
				column(6,
					actionButton("proc","Process ", icon = icon("cog"))
				)
			),
			fluidRow(br()),
			fluidRow(
				column(6),
				column(6,
					downloadButton("downloadData2", "Save")
				)
			),
			tags$style(type = "text/css", "#downloadData2 { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#settings2 { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#proc { width:100%; font-size:85%; background-color:#126a8f }"),
			width=3
		),
		mainPanel(
			htmlOutput('single_contents'),
			imageOutput('single_plot'),
			DT::dataTableOutput('table2'),
			bsModal("settingssingle", title = "Settings", trigger = "settings2", size = "medium", 
		 		tabsetPanel(id="single_tab",
					tabPanel("Output Parameters",
						uiOutput("single_file_output1"),
						conditionalPanel(condition = "!input.single_ztransform || input.single_analysis == 'Articulation t-test'", 
							uiOutput("single_file_output2")
						)
			 		),
			 		tabPanel("Statistical Parameters",
						fluidRow(
							column(8, 
								conditionalPanel(condition = "input.single_analysis == 'Non_antimere regression'", 
									uiOutput("association_types")
								),
								conditionalPanel(condition = "input.single_analysis == 'Antimere t-test'", 
									uiOutput("single_ztransform")
								),
								conditionalPanel(condition = "!input.single_ztransform && input.single_analysis != 'Non_antimere regression' || input.single_analysis == 'Articulation t-test'",
									uiOutput("single_absolute_value"),
									uiOutput("single_yeojohnson"),
									uiOutput("single_mean"),
									uiOutput("single_tails")
								),
								uiOutput("common_alpha_level")
							)
						)
					)
				)
			)
		)
	)
)
