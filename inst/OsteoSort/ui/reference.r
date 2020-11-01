reference <- tabPanel("Reference",icon = icon("server", lib="font-awesome"),
	sidebarLayout(
		sidebarPanel(
			fluidRow(
				column(12,
					HTML("<h5><b>Import Reference</b></h5>"),
					uiOutput("importRefR")
				)
			),
			fluidRow(
				column(12,
					actionButton("clearFileRef", "Clear Import  ", icon = icon("window-close")),
					tags$style(type = "text/css", "#clearFileRef { width:100%; font-size:85%; background-color:#126a8f }")
				)
			),
			fluidRow(
				column(12,
					uiOutput("reference_data_interface")
				)
			),
			br(),
			fluidRow(
				column(12,
					actionButton("refdel", "Delete Reference  ", icon = icon("window-close")),
					tags$style(type = "text/css", "#refdel { width:100%; font-size:85%; background-color:#126a8f }")
				)
			),
			br(),
			fluidRow(
				column(12,
					downloadButton("refdown", "Download Reference"),
					tags$style(type = "text/css", "#refdown { width:100%; font-size:85%; background-color:#126a8f }")
				)
			),
			br(),
			fluidRow(
				column(12,
					actionButton("refsel", "Add Selected Rows  ", icon = icon("plus-square")),
					tags$style(type = "text/css", "#refsel { width:100%; font-size:85%; background-color:#126a8f }")
				)
			),
			fluidRow(
				column(12,
					HTML("<h5><b>Configuration</h5></b>"),
					uiOutput("config_render")
				)
			),
			fluidRow(
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
			),
		width = 3),
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
