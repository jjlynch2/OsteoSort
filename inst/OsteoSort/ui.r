#' Shiny ui.r file
#' 
#' This is the ui.r file for the interface that utilizes all previous functions. 
#' runApp("osteosort")

options(warn = -1)
library(shiny)
library(rgl)
shinyUI(
navbarPage(theme = "css/flatly.min.css", windowTitle = "OsteoSort",
	tags$script(HTML(paste("var header = $('.navbar > .container-fluid');header.append('<div style=\"float:left\"><img src=\"osteosort_new.png\" alt=\"alt\" style=\"float:right; width:200px;padding-top:0px;\"></div><div style=\"float:right; padding-top:15px\">", 
					uiOutput("memUsage"), "</div>');console.log(header)", sep=""))),
	navbarMenu("Help",icon = icon("info", lib="font-awesome"),
			tabPanel("About",icon = icon("question", lib="font-awesome"),
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
			),
			tabPanel("Files",icon = icon("folder", lib="font-awesome"),
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
			),
			tabPanel("Reference",icon = icon("server", lib="font-awesome"),
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
								conditionalPanel(condition = "input.config_options == 'Non_antimere'",
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
				),
				tags$style(type = "text/css", "#clearFileRef { width:100%; font-size:85%; background-color:#126a8f }"),
				tags$style(type = "text/css", "#refdel { width:100%; font-size:85%; background-color:#126a8f }")
			),
			tabPanel("Measurements",icon = icon("archive", lib="font-awesome"),
				DT::dataTableOutput('measurement_conversion_table')
			),		
			tabPanel(
				tags$button(type = "button", id = "exitOsteoSort", class = "increment btn btn-default", onclick = "window.close();", HTML("<i class ='fa fa-window-close'></i>"), "Exit"),
				tags$style(type = "text/css", "#exitOsteoSort { width:100%; height:25%; vertical-align:middle; font-size:70%; background-color:#D3D3D3; color:#126a8f }")
			)
		), #Help tab
		navbarMenu("Osteometric",icon = icon("bar-chart", lib="font-awesome"),
			tabPanel("Single",icon = icon("gear", lib="font-awesome"),
				titlePanel(""),
				sidebarLayout(
					sidebarPanel(
						tags$style(type='text/css', ".selectize-input { font-size: 14px; line-height: 14px;} .selectize-dropdown { font-size: 14px; line-height: 14px; }"),
						tags$style(".irs-bar, .irs-bar-edge, .irs-single, irs.grid-pol {background: #126a8f; border-color: #126a8f;}"),
						uiOutput("single_reference"),
						uiOutput("single_analysis"),
						conditionalPanel(condition = "input.single_analysis == 'Antimere t-test'",
							uiOutput("single_element_pair_match"),
							fluidRow(
								column(6,
									h4("Left"),
									uiOutput("list_numeric_inputs_single_left")
								),
								column(6,
									h4("Right"),
									uiOutput("list_numeric_inputs_single_right")
								)
							)
						),
						conditionalPanel(condition = "input.single_analysis == 'Non_antimere regression'",
							fluidRow(
								column(6,
									selectInput("single_association_side_a", "Side", c(Left='Left', Right='Right')),
									uiOutput("single_elements_association_a"),
									uiOutput("list_numeric_inputs_single_A")
								),
								column(6,
									selectInput("single_association_side_b", "Side", c(Left='Left', Right='Right')),
									uiOutput("single_elements_association_b"),
									uiOutput("list_numeric_inputs_single_B")
								)
							)
						),
						conditionalPanel(condition = "input.single_analysis == 'Non_antimere t-test'",
							fluidRow(
								column(12,
									selectInput("single_non_antimere_side", "Side", c(Left='Left', Right='Right'))
								),
								column(12,
									uiOutput("single_element_non_antimere")
								),
								column(6,
									uiOutput("single_measurement_non_antimere_a")
								),
								column(6,
									uiOutput("single_measurement_non_antimere_b")
								)
							)
						),
						fluidRow(
							column(6,
								textInput(inputId = 'ID1', label = '1st ID #', value = 'X1')
							),
							column(6,
								textInput(inputId = 'ID2', label = '2nd ID #', value = 'X2')
							)
						),
						fluidRow(
							column(6,
								actionButton("settings2","Settings", icon=icon("keyboard-o"))
							),
							column(6,
								actionButton("proc","Process ", icon = icon("cog"))
							)
						),
						fluidRow(br()),
						fluidRow(
							column(6),
							column(6,
								downloadButton("downloadData2", "Save")
							)
						),
						tags$style(type = "text/css", "#downloadData2 { width:100%; font-size:85%; background-color:#126a8f }"),
						tags$style(type = "text/css", "#settings2 { width:100%; font-size:85%; background-color:#126a8f }"),
						tags$style(type = "text/css", "#proc { width:100%; font-size:85%; background-color:#126a8f }"),
						width=3
					),
					mainPanel(
						htmlOutput('single_contents'),
						imageOutput('single_plot'),
						DT::dataTableOutput('table2'),
						bsModal("settingssingle", title = "Settings", trigger = "settings2", size = "medium", 
					 		tabsetPanel(id="single_tab",
								tabPanel("Output Parameters",
									uiOutput("single_file_output1"),
									conditionalPanel(condition = "!input.single_ztransform || input.single_analysis == 'Non_antimere t-test'", 
										uiOutput("single_file_output2")
									)
						 		),
						 		tabPanel("Statistical Parameters",
									fluidRow(
										column(8, 
											conditionalPanel(condition = "input.single_analysis == 'Non_antimere regression'", 
												uiOutput("association_types")
											),
											conditionalPanel(condition = "input.single_analysis == 'Antimere t-test'", 
												uiOutput("single_ztransform")
											),
											conditionalPanel(condition = "!input.single_ztransform && input.single_analysis != 'Non_antimere regression' || input.single_analysis == 'Non_antimere t-test'",
												uiOutput("single_absolute_value"),
												uiOutput("single_boxcox"),
												uiOutput("single_mean"),
												uiOutput("single_tails")
											),
											uiOutput("common_alpha_level")
										)#column
									)#fr
								)#tp
							)#tsp
						)#bsmodal
					)#main
				)
			),#sidebarLayout
			tabPanel("Multiple",icon = icon("gears", lib="font-awesome"),
				titlePanel(""),
				sidebarLayout(
					sidebarPanel(
						uiOutput("multiple_reference"),
						uiOutput("multiple_analysis"),
						uiOutput("testtype2"),
						uiOutput('resettableInput'),
						conditionalPanel(condition = "input.multiple_analysis == 'Antimere t-test'",
							uiOutput("multiple_element_pair_match")
						),
						conditionalPanel(condition = "input.multiple_analysis == 'Non-Antimere t-test'",
							selectInput("multiple_non_antimere_side", "Side", c(Left='Left', Right='Right')),
							uiOutput("multiple_element_non_antimere")
						),
						conditionalPanel(condition = "input.multiple_analysis == 'Non-Antimere regression'",
							fluidRow(
								column(6,
									selectInput("multiple_association_side_a", "Side", c(Left='Left', Right='Right')),
									uiOutput("multiple_elements_association_a")
								),
								column(6,
									selectInput("multiple_association_side_b", "Side", c(Left='Left', Right='Right')),
									uiOutput("multiple_elements_association_b")
								)
							)
						),
						fluidRow(
							column(6,
								actionButton("settings1","settings", icon=icon("keyboard-o"))
							),
							column(6,
								actionButton("pro","process ", icon = icon("cog"))
							)
						),
						fluidRow(br()),
						fluidRow(
							column(6,
								actionButton("clearFile1", "clear   ", icon = icon("window-close"))
							),
							column(6,
								downloadButton("downloadData", "Save    ")
							)
						),
						tags$style(type = "text/css", "#settings1 { width:100%; font-size:85%; background-color:#126a8f  }"),
						tags$style(type = "text/css", "#pro { width:100%; font-size:85%; background-color:#126a8f  }"),
						tags$style(type = "text/css", "#clearFile1 { width:100%; font-size:85%; background-color:#126a8f }"),
						tags$style(type = "text/css", "#downloadData { width:100%; font-size:85%; background-color:#126a8f }"),
						width=3
					),
					mainPanel(
						htmlOutput('multiple_contents'),
						tabsetPanel(id="tabSelected",
							tabPanel("Not excluded",
								DT::dataTableOutput('table')
							),
							tabPanel("Excluded",
								DT::dataTableOutput('tablen')
							),
							bsModal("settingsmultiple", title = "Settings", trigger = "settings1", size = "medium", 
								tabsetPanel(id="multiple_tab",
									tabPanel("Output Parameters",
										uiOutput("multiple_file_output1")
									),
									tabPanel("Statistical Parameters",
										fluidRow(
											column(8,
											conditionalPanel(condition = "input.multiple_analysis == 'Non-Antimere regression'", 
												uiOutput("multiple_association_types")
											),
											conditionalPanel(condition = "input.multiple_analysis == 'Antimere t-test'", 
												uiOutput("multiple_ztransform")
											),
											conditionalPanel(condition = "!input.multiple_ztransform && input.multiple_analysis != 'Non-Antimere regression'",
												uiOutput("multiple_absolute_value"),
												uiOutput("multiple_boxcox"),
												uiOutput("multiple_mean"),
												uiOutput("multiple_tails")
											),
											uiOutput("multiple_common_alpha_level")
											)#column
										)#fr
									),#tp
									tabPanel("Computational Parameters",
										uiOutput('ncores')
									)
								)#tsp
							)#bsmodal
						)#tabpanel
					)#main
				)#main
			)#sidebar
		),#navbar osteometric
		navbarMenu("Outlier",icon = icon("sort-amount-asc", lib="font-awesome"),
			tabPanel("Metric",icon = icon("line-chart", lib="font-awesome"),	
				sidebarLayout(
					sidebarPanel(


						uiOutput('resettableInput3'),
						uiOutput("testtypem"),

						fluidRow(
							column(6,
								actionButton("settings3","Settings", icon=icon("keyboard-o"))
							),
							column(6,
								actionButton("pro3","Process ", icon = icon("cog"))
							)
						),
						fluidRow(br()),
						fluidRow(
							column(6,
								actionButton("clearFile3", "Clear   ", icon = icon("window-close"))
							),
							column(6,
								downloadButton("outlierdownload", "Save    ")
							)
						),
						tags$style(type = "text/css", "#settings3 { width:100%; font-size:85%; background-color:#126a8f }"),
						tags$style(type = "text/css", "#pro3 { width:100%; font-size:85%; background-color:#126a8f }"),
						tags$style(type = "text/css", "#clearFile3 { width:100%; font-size:85%; background-color:#126a8f }"),
						tags$style(type = "text/css", "#outlierdownload { width:100%; font-size:85%; background-color:#126a8f }"),
						width=3
					),
					mainPanel(
						htmlOutput('outliercontent'),
						imageOutput('plotoutlier'),
						tabsetPanel(id="tabSelectedoutlier",
							tabPanel("Upper outliers",
								DT::dataTableOutput('tjbingworka')
							),		
							tabPanel("Lower outliers",
						 		DT::dataTableOutput('tjbingworkb')
						 	),
						 	tabPanel("Non-outliers",
						 		DT::dataTableOutput('tjbingworkc')
						 	)
					 	),
					 	bsModal("settingsoutlier", title = "Settings", trigger = "settings3", size = "medium", 
					 		tabsetPanel(id="tabSelected2",
								tabPanel("Output Paramters",
									uiOutput("fileoutputl1"),
									uiOutput("fileoutputl2")
								),
								tabPanel("Statistical Parameters",
									fluidRow(
										column(6,
											radioButtons('method', '', c(standard_deviation='Standard_deviation', quartiles='Quartiles'),'Standard_deviation')
										),
										column(6,
											conditionalPanel(condition = "input.method == 'Standard_deviation'",
												sliderInput(inputId = "standard_dev", label = "Standard Deviation Cutoff", min=0.5, max=10, value=c(2.0,2), step = 0.1)
											),
											conditionalPanel(condition = "input.method == 'Quartiles'",
												sliderInput(inputId = "Quartiles", label = "Interquartile Cutoff", min=0.5, max=10, value=c(1.5,1.5), step = 0.1)
											)
										)
									)
								)
							)
						)
					)
				)
			),
			tabPanel("Stature",icon = icon("user", lib="font-awesome"),
				sidebarLayout(
					sidebarPanel(
							uiOutput('resettableInput4'),
							checkboxInput("custom", "Specify Slope and Intercept", TRUE),
							conditionalPanel(condition = "input.custom",
								numericInput("slope", label = "Slope", value = "", min=0,max=999,step=0.01),
								numericInput("intercept", label = "Intercept", value = "", min=0,max=999,step=0.01)
							),
							conditionalPanel(condition = "!input.custom",
								uiOutput("stature_reference")
							),
							uiOutput("testtypem1"),
							fluidRow(
								column(6,
									actionButton("settings4","Settings", icon=icon("keyboard-o"))
								),
								column(6,
									actionButton("pro4","Process ", icon = icon("cog"))
								)
							),
							fluidRow(br()),
							fluidRow(
								column(6,
									actionButton("clearFile4", "Clear   ", icon = icon("window-close"))
								),
								column(6,
									downloadButton("outlierdownload4", "Save    ")
								)
							),
							tags$style(type = "text/css", "#settings4 { width:100%; font-size:85%; background-color:#126a8f }"),
							tags$style(type = "text/css", "#pro4 { width:100%; font-size:85%; background-color:#126a8f }"),
							tags$style(type = "text/css", "#clearFile4 { width:100%; font-size:85%; background-color:#126a8f }"),
							tags$style(type = "text/css", "#outlierdownload4 { width:100%; font-size:85%; background-color:#126a8f }"),
							width=3
					),
					mainPanel(
						htmlOutput('outliercontent4'),
						imageOutput('plotoutlier4'),
						tabsetPanel(id="tabSelectedoutlier",
							tabPanel("Upper outliers",
								DT::dataTableOutput('tjbingworka4')
							),		
							tabPanel("Lower outliers",
								DT::dataTableOutput('tjbingworkb4')
						 	),
						 	tabPanel("Non-outliers",
					 			DT::dataTableOutput('tjbingworkc4')
						 	)
					 	),
					 	bsModal("settingsoutlier4", title = "Settings", trigger = "settings4", size = "medium", 
					 		tabsetPanel(id="tabSelected2",
								tabPanel("Output Paramters",
									uiOutput("fileoutputstature1"),
									uiOutput("fileoutputstature2")
								),
								tabPanel("Statistical Parameters",
									fluidRow(
										column(6,
											radioButtons('method4', '', c(standard_deviation='Standard_deviation', quartiles='Quartiles'),'Standard_deviation')
										),
										column(6,
											conditionalPanel(condition = "input.method4 == 'Standard_deviation'",
												sliderInput(inputId = "standard_dev4", label = "Standard Deviation Cutoff", min=0.5, max=10, value=c(2,2), step = 0.1)
											),
											conditionalPanel(condition = "input.method4 == 'Quartiles'",
												sliderInput(inputId = "Quartiles4", label = "Interquartile Cutoff", min=0.5, max=10, value=c(1.5,1.5), step = 0.1)
											)
										)
									)
								)
							)
						)
					)
				)
			)
		),		
		navbarMenu("Osteoshape", icon = icon("cloud", lib="font-awesome"),
			tabPanel("2D Antimere",icon = icon("picture", lib="glyphicon"),
				titlePanel(""),
					sidebarLayout(
						sidebarPanel(
							selectInput(inputId ="fragcomp", label = "Analysis:", choices = c("Complete", "Fragmented"), selected = "Complete"),
							uiOutput('resettableInput2D'),	
							uiOutput('resettableInput2DD'),
							conditionalPanel(condition = "input.fragcomp == 'Complete'",
								uiOutput('mspec')
							),
							fluidRow(
								column(6,
									actionButton("settings2D","Settings", icon=icon("keyboard-o"))
								),
								column(6,
									actionButton("pro2D","Process ", icon = icon("cog"))
								)
							),
							fluidRow(br()),
							fluidRow(
								column(6,
									actionButton("clearFile2D", "Clear   ", icon = icon("window-close"))
								),
								column(6,
									downloadButton("downloadData2D", "Save    ")
								)
							),
							tags$style(type = "text/css", "#settings2D { width:100%; font-size:85%; background-color:#126a8f }"),
							tags$style(type = "text/css", "#pro2D { width:100%; font-size:85%; background-color:#126a8f }"),
							tags$style(type = "text/css", "#clearFile2D { width:100%; font-size:85%; background-color:#126a8f }"),
							tags$style(type = "text/css", "#downloadData2D { width:100%; font-size:85%; background-color:#126a8f }"),
							width = 3
						),
						mainPanel(
							uiOutput("contents2D"),
							uiOutput("tabpanpan"),
							bsModal("settings2DD", title = "Settings", trigger = "settings2D", size = "large", 
								tabsetPanel(id="tabSelected2",
									tabPanel("Output Parameters",
										uiOutput('fileoutput2Dexcel1'),
										uiOutput('fileoutput2Dexcel2'),
										uiOutput('fileoutput2Dplot'),
										uiOutput('fileoutput2Dtps')
									),
									tabPanel("Statistical Parameters",
										fluidRow(
											column(4,
												h4("Outline"),
												uiOutput('nthreshold'),
												uiOutput('mirror2D'),
												uiOutput('efa_options3'),
								 				conditionalPanel(condition = "input.fragcomp == 'Complete'",
													uiOutput('efa_options1'),
													uiOutput('efa_options2')
												)
											),
											column(4, 
												h4("Registration"),
												conditionalPanel(condition = "input.fragcomp == 'Complete'",
														uiOutput('comp_options')
												),
												uiOutput('icp2D')
											),
											column(4,
												h4("Distance"),
												uiOutput('distance2D'),
								 				conditionalPanel(condition = "input.distance2D == 'Segmented-Hausdorff' || input.distance2D == 'Hausdorff'",
													uiOutput('max_avg_distance')
												),
								 				conditionalPanel(condition = "input.distance2D == 'Segmented-Hausdorff'",
													uiOutput('n_regions')
												),
												sliderInput(inputId = "shortlistn", label = "Number of shortest distance matches", min = 1, max = 100, value = 1, step = 1),
												checkboxInput(inputId = "hidedist", label = "Hide distance from results", value = FALSE)
											)
										)
									),
									tabPanel("Computational Parameters",
										uiOutput('ncores2D')
									)
								)
							)
						)
					)
			),
			tabPanel("3D Point Cloud Tools",icon = icon("cloud-download", lib="glyphicon"),
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
							)#modal
						)#main
					)#		
			),
			tabPanel("3D Antimere",icon = icon("sort-by-order", lib="glyphicon"),
				titlePanel(""),
					sidebarLayout(
						sidebarPanel(
							selectInput(inputId ="fragcomp3d", label = "Analysis:", choices = c("Complete", "Fragmented"), selected = "Complete"),
							uiOutput('resettableInput3D'),	
							uiOutput('resettableInput3DD'),
							uiOutput("mspec3D"),
							fluidRow(
								column(6,
									actionButton("settings3D","Settings", icon=icon("keyboard-o"))
								),
								column(6,
									actionButton("pro3D","Process ", icon = icon("cog"))
								)
							),
							fluidRow(br()),
							fluidRow(
								column(6,
									actionButton("clearFile3D", "Clear   ", icon = icon("window-close"))
								),
								column(6,
									downloadButton("downloadData3D", "Save    ")
								)
							),
							tags$style(type = "text/css", "#settings3D { width:100%; font-size:85%; background-color:#126a8f }"),
							tags$style(type = "text/css", "#pro3D { width:100%; font-size:85%; background-color:#126a8f }"),
							tags$style(type = "text/css", "#clearFile3D { width:100%; font-size:85%; background-color:#126a8f }"),
							tags$style(type = "text/css", "#downloadData3D { width:100%; font-size:85%; background-color:#126a8f }"),
							width = 3
						),
						mainPanel(
							uiOutput("contents3D"),
							tabsetPanel(id="tabSelected3D",
								tabPanel("Results ",
									DT::dataTableOutput('table3D')
								),
								tabPanel("Render ",
									rglwidgetOutput('webgl3D', width = "1200px", height = "1200px")
								)
						 	),
							bsModal("settings3DD", title = "Settings", trigger = "settings3D", size = "large", 
								tabsetPanel(id="tabSelected33",
									tabPanel("Output Parameters",
										uiOutput('fileoutput3Dexcel1'),
										uiOutput('fileoutput3Dexcel2'),
										uiOutput('fileoutput3Dtps')
									),
									tabPanel("Statistical Parameters",
										fluidRow(
											column(4,
												h4("Outline"),
												uiOutput('banding'),
												conditionalPanel(condition = "input.banding",
													uiOutput('nthreshold3D')
												)
											),
											column(4, 
												h4("Registration"),
												uiOutput('icp3D'),
												uiOutput('trans3D')
											),
											column(4,
												h4("Distance"),
												uiOutput('distance3D'),
												uiOutput('max_avg_distance3D'),
												uiOutput('shortlistn3D'),
												uiOutput('hidedist3D')
											)
										)
									),
									tabPanel("Computational Parameters",
										uiOutput('ncores3D')
									)
								)
							)
						)
					)
			)
		),
		navbarMenu("Antemortem", icon = icon("users", lib="font-awesome"),
			tabPanel("Single",icon = icon("gear", lib="font-awesome"),
				sidebarLayout(
					sidebarPanel(
						uiOutput("stature_reference_ante"),
						fluidRow(
							column(12,
								selectInput("state_reference_ante_side", "Side", c(Left='Left', Right='Right'))
							),
							column(12,
								uiOutput("single_ante_elements")
							),
							column(6,
								uiOutput("single_measurements_ante"),
								textInput(inputId = 'Postmortem_ID_ante', label = 'Postmortem ID', value = 'X1')
							),
							column(6,
								numericInput(inputId = 'antestat_input', label = 'Stature', value = ''),
								textInput(inputId = 'Antemortem_ID_ante', label = 'Antemortem ID', value = 'X2')
							)
						),
						fluidRow(
							column(6,
								actionButton("settingsante","Settings", icon=icon("keyboard-o"))
							),
							column(6,
								actionButton("proantestat","Process ", icon = icon("cog"))
							)
						),
						fluidRow(br()),
						fluidRow(
							column(6,
								downloadButton("downloadantestat", "Save    ")
							)
						),
						tags$style(type = "text/css", "#settingsante { width:100%; font-size:85%; background-color:#126a8f }"),
						tags$style(type = "text/css", "#proantestat { width:100%; font-size:85%; background-color:#126a8f }"),
						tags$style(type = "text/css", "#downloadantestat { width:100%; font-size:85%; background-color:#126a8f }"),
						width=3
					),
					mainPanel(
						htmlOutput('antestat_output'),
						imageOutput('plotplotante'),
						DT::dataTableOutput('antestat_table'),
					 	bsModal("settingsante2", title = "Settings", trigger = "settingsante", size = "medium", 
					 		tabsetPanel(id="tabSelected2s",
								tabPanel("Output Paramters",
									uiOutput("fileoutputant1"),
									uiOutput("fileoutputant2")
								),	
								tabPanel("Statistical Parameters",
									fluidRow(column(6,
											uiOutput("alphalevelsantestat")
										)
									)
								)
							)
						)
					)
				)	
			),
			tabPanel("Multiple",icon = icon("gears", lib="font-awesome"),
				sidebarLayout(
					sidebarPanel(	
						selectInput(inputId = 'metric_typem', 'Stature metric', c(Millimeters = "mm", Centimeters = "cm", Inches = "in"), selected = 'in'),
						uiOutput("antestat_testm"),
						selectInput("antestat_populationm", "Population", c(DPAA_any_male = "DPAA-any-male", DPAA_white_male = "DPAA-white-male", DPAA_black_male = "DPAA-black-male",FDB_20th_FStat_any='20th-FStat-any', FDB_20th_FStat_white_male='20th-FStat-white-male', FDB_20th_FStat_white_female='20th-FStat-white-female', FDB_20th_FStat_black_male='20th-FStat-black-male', FDB_20th_FStat_black_female='20th-FStat-black-female', Trotter_any_male='Trotter-any-male', Trotter_black_male='Trotter-black-male', Trotter_white_male='Trotter-white-male'), 'Trotter-any-male'),
						uiOutput('resettableInputante1'),
						uiOutput('resettableInputante2'),
						fluidRow(
							column(6,
								actionButton("settingsantem","Settings", icon=icon("keyboard-o"))
							),
							column(6,
								actionButton("proantestatm","Process ", icon = icon("cog"))
							)
						),
						fluidRow(br()),
						fluidRow(
							column(6,
								actionButton("clearFile1ante", "Clear   ", icon = icon("window-close"))
							),
							column(6,
								downloadButton("downloadantestatm", "Save    ")
							)
						),
						tags$style(type = "text/css", "#settingsantem { width:100%; font-size:85%; background-color:#126a8f }"),
						tags$style(type = "text/css", "#proantestatm { width:100%; font-size:85%; background-color:#126a8f }"),
						tags$style(type = "text/css", "#clearFile1ante { width:100%; font-size:85%; background-color:#126a8f }"),
						tags$style(type = "text/css", "#downloadantestatm { width:100%; font-size:85%; background-color:#126a8f }"),
						width=3
					),
					mainPanel(
						htmlOutput('antestat_outputm'),
						tabsetPanel(id="tabSelected",
							tabPanel("Not excluded",
					 			DT::dataTableOutput('antestat_table1m')),
					 		tabPanel("Excluded",
					 			DT::dataTableOutput('antestat_table2m'))),
					 	bsModal("settingsante2m", title = "Settings", trigger = "settingsantem", size = "medium", 
					 		tabsetPanel(id="tabSelected2m",
								tabPanel("Output Paramters",
									checkboxInput(inputId = "fileoutputant1m", label = "Output csv file", value = TRUE),
									checkboxInput(inputId = "fileoutputant2m", label = "Output plot (WARNING: This option will generate a plot for every comparison)", value = FALSE)
								),	
								tabPanel("Statistical Parameters",
									fluidRow(column(6,
										sliderInput(inputId = "predlevelantestatm", label = "Prediction interval level", min=0.01, max=1, value=0.95, step = 0.01),
										sliderInput(inputId = "alphalevelsantestatm", label = "Alpha level", min=0.01, max=1, value=0.05, step = 0.01)
										),
										column(6,
											radioButtons(inputId = "alphatest1m", label = "Test type", choices = c(Alpha = "Alpha", PI = "PI"), "Alpha"),
											checkboxInput(inputId = "research_mm", label = "Calculate research statistics", value = FALSE)
										)
									)
								),
								tabPanel("Computational Parameters",
									uiOutput('ncoresm')
								)
							)
						)
					)
				)
			)	
		)
	)
)
