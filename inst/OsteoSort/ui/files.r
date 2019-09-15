files <- tabPanel("Files",icon = icon("folder", lib="font-awesome"),
	sidebarPanel(
		fluidRow(
			HTML("<p><h3>Help Files</h3></p>")
		),
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
	tags$style(type = "text/css", "#Create_Desktop_Icon { width:100%; font-size:85%; background-color:#126a8f }"),
	tags$style(type = "text/css", "#postmortem_template { width:100%; font-size:85%; background-color:#126a8f }"),
	tags$style(type = "text/css", "#antemortem_template { width:100%; font-size:85%; background-color:#126a8f }"),
	tags$style(type = "text/css", "#osteoguide { width:100%; font-size:85%; background-color:#126a8f }"),
	tags$style(type = "text/css", "#example_data { width:100%; font-size:85%; background-color:#126a8f }")
)
