dash <- tabPanel("About",icon = icon("question", lib="font-awesome"),
	sidebarLayout(
		sidebarPanel(
			uiOutput("version_numbers"),
			br(),
			uiOutput("update_gh"),
			br(),
			downloadButton('postmortem_template', 'Postmortem template'),
			br(),
			downloadButton('example_data', "Example data")
		),
		mainPanel(
			DT::dataTableOutput('measurement_conversion_table')
		)
	),
	tags$style(type = "text/css", "#postmortem_template { width:100%; font-size:85%; background-color:#126a8f }"),
	tags$style(type = "text/css", "#example_data { width:100%; font-size:85%; background-color:#126a8f }")
)