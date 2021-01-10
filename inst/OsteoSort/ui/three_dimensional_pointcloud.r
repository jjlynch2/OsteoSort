three_dimensional_pointcloud <- tabPanel("3D Point Cloud Tools",icon = icon("cloud-download", lib="glyphicon"),
	titlePanel(""),
	sidebarLayout(
		sidebarPanel(
			uiOutput('resettableInput3Da'),
			fluidRow(
				column(6,
					uiOutput('ncorespc')
				),
				column(6,
					radioButtons(inputId ="alln", label = "Specimens", choices = c("All", "Present"), selected = "Present")
				)
			),
			fluidRow(
				column(6,
					uiOutput('sft')
				),
				column(6,
					uiOutput('fmt')
				)
			),
			fluidRow(br()),
			fluidRow(
				column(6,
					uiOutput('fr')
				),
				column(6,
					uiOutput('vara')
				)
			),
			fluidRow(br()),
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
					actionButton("start2","Select Fracture", icon=icon("scissors"))
				),
				column(6,
					actionButton("start1","Auto Fracture", icon=icon("scissors"))
				)
			),	
			fluidRow(br()),
			fluidRow(
				column(12,
					uiOutput("landmark_switch")
				)
			),
			fluidRow(br()),
			fluidRow(
				column(6,
					actionButton("reimport", "Reimport", icon=icon("arrows-alt"))
				),
				column(6,
					actionButton("landmarks_dig","Landmarks", icon=icon("arrows-alt"))
				)
			),
			fluidRow(br()),
			fluidRow(
				column(6,
					actionButton("simplify","Simplify", icon=icon("cloud-download"))
				),
				column(6,
					actionButton("clearFile3Da", " Clear   ", icon = icon("window-close"))
				)
			),
			fluidRow(br()),
			fluidRow(
				column(6,
					uiOutput("coordinates")
				),
				column(6,
					downloadButton("savedata", " Save    ")
				)
			),
			tags$style(type = "text/css", "#reimport { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#landmarks_dig { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#pcset { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#simplify { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#start2 { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#start1 { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#previous { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#nnext { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#clearFile3Da { width:100%; font-size:85%; background-color:#126a8f }"),
			tags$style(type = "text/css", "#savedata { width:100%; font-size:85%; background-color:#126a8f }"),
			width = 3
		),
		mainPanel(
			rglwidgetOutput('webgl3Dalign', width = "1200px", height = "1200px")
		)
	)
)
