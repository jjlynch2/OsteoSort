three_dimensional_pointcloud <- tabPanel("3D Point Cloud Tools",icon = icon("cloud-download", lib="glyphicon"),
	titlePanel(""),
	sidebarLayout(
		sidebarPanel(
			uiOutput('resettableInput3Da'),
			fluidRow(
				column(6,
					actionButton("previous"," Previous", icon=icon("arrow-left"))
				),
				column(6,
					actionButton("nnext"," Next    ", icon = icon("arrow-right"))
				)
			),
			fluidRow(br()),
			fluidRow(
				column(6,
					actionButton("start"," Digitize", icon=icon("edit"))
				),
				column(6,
					actionButton("start2"," Fracture", icon=icon("scissors"))
				)
			),	
			fluidRow(br()),
			fluidRow(
				column(6,
					actionButton("RGB1"," RGB landmark", icon=icon("tint"))
				),
				column(6,
					actionButton("RGB2"," RGB fracture", icon=icon("tint"))
				)
			),	
			fluidRow(br()),
			fluidRow(
				column(12,
					actionButton("RGB3"," RGB calibration", icon=icon("tint"))
				)
			),
			fluidRow(br()),
			fluidRow(
				column(6,
					actionButton("pcset","Settings", icon=icon("keyboard-o"))
				),
				column(6,
					actionButton("simplify","Simplify", icon=icon("cloud-download"))
				)
			),
			fluidRow(br()),
			fluidRow(
				column(6,
					actionButton("clearFile3Da", " Clear   ", icon = icon("window-close"))
				),
				column(6,
					downloadButton("savedata", " Save    ")
				)
			),
			fluidRow(br()),
			fluidRow(
				column(12,
					radioButtons(inputId ="alln", label = "Specimens", choices = c("All", "Present"), selected = "Present")
				)
			),
			fluidRow(br()),
			fluidRow(
				column(12,
					uiOutput("coordinates")
				)
			),
			tags$style(type = "text/css", "#pcset { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#simplify { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#start2 { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#RGB1 { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#RGB2 { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#RGB3 { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#start { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#previous { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#nnext { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#clearFile3Da { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#savedata { width:100%; font-size:85%; background-color:#126a8f }"),
			width = 3
		),
		mainPanel(
			rglwidgetOutput('webgl3Dalign', width = "1200px", height = "1200px"),
			bsModal("pcset3D", title = "Settings", trigger = "pcset", size = "medium", 
				tabsetPanel(id="pcset33",
					tabPanel("Statistical Parameters",
						fluidRow(
							column(4,
								h4("Simplification"),
								uiOutput('vara'),
								uiOutput('tva')
							),
							column(4,
								h4("RGB"),
								uiOutput('fracturet')
							)
						)
					),
					tabPanel("Computational Parameters",
						uiOutput('ncorespc')
					)
				)
			)
		)
	)
)
