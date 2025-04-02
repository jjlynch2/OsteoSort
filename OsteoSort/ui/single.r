single_osteometric_sorting <- tabPanel("Single",icon = icon("gear", lib="font-awesome"),
	sidebarLayout(
		sidebarPanel(
			tags$style(type='text/css', ".selectize-input { font-size: 14px; line-height: 14px;} .selectize-dropdown { font-size: 14px; line-height: 14px; }"),
			tags$style(".irs-bar, .irs-bar-edge, .irs-single, irs.grid-pol {background: #126a8f; border-color: #126a8f;}"),
			tags$style(".well {background-color: #f6f6f6;}"),
			uiOutput("single_reference"),
			uiOutput("single_analysis"),
			conditionalPanel(condition = "input.single_analysis == 'pair-match'",
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
			conditionalPanel(condition = "input.single_analysis == 'osr'",
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
			conditionalPanel(condition = "input.single_analysis == 'articulation'",
				fluidRow(
					column(12,
						selectInput("single_osr_side", "Side", c(Left='Left', Right='Right'))
					),
					column(12,
						uiOutput("single_element_osr")
					),
					column(6,
						uiOutput("single_measurement_osr_a")
					),
					column(6,
						uiOutput("single_measurement_osr_b")
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
					actionButton("settings2","Settings", icon=icon("sliders"))
				),
				column(6,
					actionButton("proc","Process ", icon = icon("gear"))
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
			tags$style(type = "text/css", "#template { color:#FFFFFF }"),
			tags$style(type = "text/css", "#example { color:#FFFFFF }"),
			width=2
		),
		mainPanel(
			imageOutput('single_plot'),
			br(),
			DT::dataTableOutput('table2'),
			bsModal("settingssingle", title = "Settings", trigger = "settings2", size = "medium", 
		 		tabsetPanel(id="single_tab",
					fluidRow(
						column(8, 
							conditionalPanel(condition = "input.single_analysis == 'osr'", 
								uiOutput("association_types")
							),
							conditionalPanel(condition = "input.single_analysis == 'pair-match'", 
								uiOutput("single_ztransform")
							),
							conditionalPanel(condition = "!input.single_ztransform && input.single_analysis != 'osr' || input.single_analysis == 'articulation'",
								uiOutput("single_absolute_value"),
								uiOutput("single_yeojohnson"),
								uiOutput("single_mean"),
								uiOutput("single_tails")
							),
							uiOutput("common_alpha_level")
						)
					)
				)
			),
			width=10
		)
	)
)
