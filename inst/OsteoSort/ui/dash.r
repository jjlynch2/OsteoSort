dash <- tabPanel("About",icon = icon("question", lib="font-awesome"),
	fluidRow(
		sidebarPanel(
			uiOutput("version_numbers")
		,width = 3),
		sidebarPanel(
			uiOutput("update_gh")
		,width = 3),
		sidebarPanel(
			uiOutput("system_info")
		,width=3),
		sidebarPanel(
			uiOutput("URL")
		,width=3)
	),
	fluidRow(br()),
	fluidRow(
		sidebarPanel(
			fluidRow(
				HTML("<p><h3>Files</h3></p>")
			),
			fluidRow(
				actionButton('Create_Desktop_Icon', 'Desktop Shortcut', icon = icon("gears"))
			),
			fluidRow(br()),
			fluidRow(
				downloadButton('osteoguide', 'Help guide')
			),
			fluidRow(br()),
			fluidRow(
				downloadButton('antemortem_template', 'Antemortem template')
			),
			fluidRow(br()),
			fluidRow(
				downloadButton('postmortem_template', 'Postmortem template')
			),
			fluidRow(br()),
			fluidRow(
				downloadButton('example_data', "Example data")
			)
		,width = 3),
		sidebarPanel(
			uiOutput("changes")
		,width = 9)
	),
	fluidRow(br()),
	fluidRow(
		sidebarPanel(
			uiOutput("about_refs")
		,width = 12)
	),
	tags$style(type = "text/css", "#Create_Desktop_Icon { width:100%; font-size:85%; background-color:#126a8f }"),
	tags$style(type = "text/css", "#postmortem_template { width:100%; font-size:85%; background-color:#126a8f }"),
	tags$style(type = "text/css", "#antemortem_template { width:100%; font-size:85%; background-color:#126a8f }"),
	tags$style(type = "text/css", "#osteoguide { width:100%; font-size:85%; background-color:#126a8f }"),
	tags$style(type = "text/css", "#example_data { width:100%; font-size:85%; background-color:#126a8f }")
)

