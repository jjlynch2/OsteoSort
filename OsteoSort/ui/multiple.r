multiple_osteometric_sorting <- tabPanel("Multiple",icon = icon("gears", lib="font-awesome"),
	sidebarLayout(
		sidebarPanel(
			uiOutput("multiple_reference"),
			uiOutput("multiple_analysis"),
			uiOutput("testtype2"),
			uiOutput('resettableInput'),
			conditionalPanel(condition = "input.multiple_analysis == 'pair-match'",
				uiOutput("multiple_element_pair_match"),
				uiOutput("multiple_measurement_antimere")
			),
			conditionalPanel(condition = "input.multiple_analysis == 'articulation'",
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
			conditionalPanel(condition = "input.multiple_analysis == 'osr'",
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
					actionButton("settings1","Settings", icon=icon("sliders"))
				),
				column(6,
					actionButton("pro","Process ", icon = icon("gear"))
				)
			),
			fluidRow(br()),
			fluidRow(
				column(6,
					actionButton("clearFile1", "Clear   ", icon = icon("rectangle-xmark"))
				),
				column(6,
					downloadButton("downloadData", "Save    ")
				)
			),
			tags$style(type = "text/css", "#settings1 { width:100%; font-size:85%; background-color:#126a8f  }"),
			tags$style(type = "text/css", "#pro { width:100%; font-size:85%; background-color:#126a8f  }"),
			tags$style(type = "text/css", "#clearFile1 { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#downloadData { width:100%; font-size:85%; background-color:#126a8f }"),
            tags$style(HTML('
                table.dataTable tbody tr.selected td,
                table.dataTable tbody td.selected {
                    border-top-color: white !important;
                    box-shadow: inset 0 0 0 9999px #126a8f !important;
                }

                table.dataTable tbody tr:active td {
                    background-color: #126a8f !important;
                }

                :root {
                    --dt-row-selected: transparent !important;
                }

                table.dataTable tbody tr:hover, table.dataTable tbody tr:hover td {
                    background-color: #126a8f !important;
                }
            ')),
			width=2
		),
		mainPanel(
			htmlOutput('multiple_contents'),
			br(),
			tabsetPanel(id="tabSelected",
				tabPanel("Not excluded",
					br(),
					DT::dataTableOutput('table')
				),
				tabPanel("Excluded",
					br(),
					DT::dataTableOutput('tablen')
				),
				tabPanel("Rejected",
					br(),
					DT::dataTableOutput('tablenr')
				),
			),
			bsModal("settingsmultiple", title = "Settings", trigger = "settings1", size = "medium", 
				fluidRow(
					column(8,
						conditionalPanel(condition = "input.multiple_analysis == 'osr'", 
							uiOutput("multiple_association_types")
						),
						conditionalPanel(condition = "input.multiple_analysis != 'osr'",
							uiOutput("multiple_absolute_value"),
							uiOutput("multiple_yeojohnson"),
							uiOutput("multiple_mean"),
							uiOutput("multiple_tails")
						),
						uiOutput("multiple_common_alpha_level")
					)
				)
			),
			width=10
		)
	)
)
