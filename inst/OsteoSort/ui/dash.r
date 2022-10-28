dash <- tabPanel("About",icon = icon("question", lib="font-awesome"),
	tags$style(HTML("
		.box.box-solid.box-primary>.box-header {
			  color:#ffffff;
			  background:#2c3e50
		}

		.box.box-solid.box-primary{
			border-bottom-color:#2c3e50;
			border-left-color:#2c3e50;
			border-right-color:#2c3e50;
			border-top-color:#2c3e50;
			background:#f5f5f5;
		}

		.box.box-primary>.box-header {
			color:#000000;
			background:#f5f5f5
		}

		.box.box-primary{
			border-bottom-color:#2c3e50;
			border-left-color:#2c3e50;
			border-right-color:#2c3e50;
			border-top-color:#2c3e50;
			background:#f5f5f5;
		}
	")),
	fluidRow(
		column(3,
			box(
				title = "Version Details",
				solidHeader=TRUE,
				width = 12,
				height = 210,
				status="primary",
				uiOutput("version_numbers")
			)
		),
		column(3,
			box(
				title = "Updates",
				solidHeader=TRUE,
				width = 12,
				height = 210,
				status="primary",
				uiOutput("update_gh")
			)
		),
		column(3,
			box(
				title = "System Information",
				solidHeader=TRUE,
				width = 12,
				height = 210,
				status="primary",
				uiOutput("system_info")
			)
		),
		column(3,
			box(
				title = "Web Information",
				solidHeader=TRUE,
				width = 12,
				height = 210,
				status="primary",
				uiOutput("URL")
			)
		)
	),
	fluidRow(
	
		column(2,
			box(
				title = "Files",
				solidHeader=TRUE,
				width = 12,
				height = 370,
				status="primary",
				actionButton('Create_Desktop_Icon', 'Desktop Shortcut', icon = icon("gears")),
				br(),
				br(),
				downloadButton('antemortem_template', 'Antemortem template'),
				br(),
				br(),
				downloadButton('postmortem_template', 'Postmortem template'),
				br(),
				br(),
				downloadButton('example_data', "Example data")
			)
		),
		column(10,
			box(
				title = "Version Changes",
				solidHeader=TRUE,
				width = 12,
				height = 370,
				status="primary",
				uiOutput("changes")
			)
		)
	),
	fluidRow(
		column(6,
			box(
				title = "References",
				solidHeader=TRUE,
				width = 12,
				height = 800,
				status="primary",
				uiOutput("about_refs")
			)
		),
		column(6,
			box(
				title = "Measurement Conversion",
				solidHeader=TRUE,
				width = 12,
				height = 800,
				status="primary",
				DT::dataTableOutput('measurement_conversion_table')
			)
		)
	),
	tags$style(type = "text/css", "#Create_Desktop_Icon { width:100%; font-size:85%; background-color:#126a8f }"),
	tags$style(type = "text/css", "#postmortem_template { width:100%; font-size:85%; background-color:#126a8f }"),
	tags$style(type = "text/css", "#antemortem_template { width:100%; font-size:85%; background-color:#126a8f }"),
	tags$style(type = "text/css", "#osteoguide { width:100%; font-size:85%; background-color:#126a8f }"),
	tags$style(type = "text/css", "#example_data { width:100%; font-size:85%; background-color:#126a8f }")
)

