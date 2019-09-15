reference <- tabPanel("Reference",icon = icon("server", lib="font-awesome"),
	sidebarLayout(
		sidebarPanel(
			HTML("<p><h3>Import Reference</h3></p>"),
			uiOutput("importRefR"),
			actionButton("clearFileRef", "clear   ", icon = icon("window-close")),
			HTML("<p><h3>Delete Reference</h3></p>"),
			uiOutput("reference_data_interface"),
			actionButton("refdel", "delete   ", icon = icon("window-close")),
			HTML("<p><h3>Configuration</h3></p>"),
			fluidRow(
				uiOutput("config_render"),
				column(6,
					uiOutput("config_a")
				),
				column(6,
					conditionalPanel(condition = "input.config_options == 'Non_antimere_t-test'",
						uiOutput("config_b")
					)
				)
			),
			fluidRow(
				column(6,
					actionButton('config_add', 'Add', icon = icon("plus-square")),
					tags$style(type = "text/css", "#config_add { width:100%; font-size:85%; background-color:#126a8f }")
				),
				column(6,
					actionButton('config_delete', 'Delete', icon = icon("minus-square")),
					tags$style(type = "text/css", "#config_delete { width:100%; font-size:85%; background-color:#126a8f }")
				)
			)
		,width = 3),
		mainPanel(
			tabsetPanel(id="tabSelectedreference",
				tabPanel("Reference",
					fluidRow(
						column(12, 
							DT::dataTableOutput('reference_table')
						)
					,width = 12)
				),
			 	tabPanel("Configuration",
					fluidRow(
						column(12,
							DT::dataTableOutput('reference_config')
						)
					,width = 12)
				)
			)
		)
	)
)
