dash <- tabPanel("About",icon = icon("question", lib="font-awesome"),
	fluidRow(
		sidebarPanel(
			uiOutput("version_numbers")
		,width = 3),
		sidebarPanel(
			uiOutput("update_gh")
		,width = 3),
		sidebarPanel(
			uiOutput("system_info"),
			actionButton('Create_Desktop_Icon', 'Desktop Shortcut', icon = icon("gears"))
		,width=3),
		sidebarPanel(
			uiOutput("URL")
		,width=3)
	),
	fluidRow(br()),
	fluidRow(
		sidebarPanel(
			uiOutput("changes")
		,width = 6),
		sidebarPanel(
			uiOutput("about_refs")
		,width = 6)
	)
)
