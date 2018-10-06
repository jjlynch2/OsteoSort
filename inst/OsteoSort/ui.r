#' Shiny ui.r file
#' 
#' This is the ui.r file for the interface that utilizes all previous functions. 
#' runApp("osteosort")
options(warn = -1)
library(shiny)
library(rgl)
#Navigation bar interface
shinyUI(




	navbarPage(theme = "css/flatly.min.css", windowTitle = "OsteoSort",
	tags$script(HTML(paste("var header = $('.navbar > .container-fluid');header.append('<div style=\"float:left\"><img src=\"osteosort_new.png\" alt=\"alt\" style=\"float:right; width:200px;padding-top:0px;\"></div><div style=\"float:right; padding-top:15px\">", uiOutput("memUsage"), "</div>');console.log(header)", sep=""))),
	navbarMenu("Help",icon = icon("info", lib="font-awesome"),
			tabPanel("About",icon = icon("question", lib="font-awesome"),
				uiOutput("version_numbers"),
				HTML("
					<p>&nbsp;</p>
					<p><h3>References</h3></p>
					<p>
					<hr><span style='font-family: 'Times New Roman', serif;'> 
					Lynch JJ, Byrd JE, LeGarde CB. The Power of Exclusion using Automated Osteometric Sorting: Pair-matching. Journal of Forensic Sciences <a href='https://doi.org/10.1111/1556-4029.13560', target='_blank'>https://doi.org/10.1111/1556-4029.13560</a>
					<p>&nbsp;</p>
					Lynch JJ. An Analysis on the Choice of Alpha Level in the Osteometric Pair-matching of the Os Coxa, Scapula, and Clavicle. Journal of Forensic Sciences <a href='https://doi.org/10.1111/1556-4029.13599', target='_blank'>https://doi.org/10.1111/1556-4029.13599</a/>
					<p>&nbsp;</p>
					Lynch JJ. The Automation of Regression Modeling in Osteometric Sorting: An Ordination Approach. Journal of Forensic Sciences <a href='https://doi.org/10.1111/1556-4029.13597', target='_blank'>https://doi.org/10.1111/1556-4029.13597</a>
					<p>&nbsp;</p>
					Lynch JJ. An Automated Two-dimensional Form Registration Method for Osteological Pair-matching. Journal of Forensic Sciences <a href='https://doi.org/10.1111/1556-4029.13670', target='_blank'>https://doi.org/10.1111/1556-4029.13670</a>
					<p>&nbsp;</p>
					Lynch JJ. An Automated Two-dimensional Pairwise Form Registration Method for Pair-matching of Fragmented Skeletal Remains. Journal of Forensic Sciences <a href='http://dx.doi.org/10.1111/1556-4029.13787', target='_blank'>http://dx.doi.org/10.1111/1556-4029.13787</a>
					<p>&nbsp;</p>
					Lynch JJ, Stephan CN. Computational Tools in Forensic Anthropology: The Value of Open-Source Licensing as a Standard. Forensic Anthropology <a href='http://dx.doi.org/10.5744/fa.2018.0025', target='_blank'>http://dx.doi.org/10.5744/fa.2018.0025</a>
					<p>&nbsp;</p>
					</p>
				")
			),
			tabPanel("Files",icon = icon("folder", lib="font-awesome"),
				downloadButton('postmortem_template', 'Postmortem template'),
				downloadButton('antemortem_template', 'Antemortem template'),
				downloadButton('osteoguide', 'Help guide'),
				downloadButton('example_data', "Example data"),

				tags$style(type = "text/css", "#postmortem_template { width:10%; font-size:85%; background-color:#126a8f }"),
				tags$style(type = "text/css", "#antemortem_template { width:10%; font-size:85%; background-color:#126a8f }"),
				tags$style(type = "text/css", "#osteoguide { width:10%; font-size:85%; background-color:#126a8f }"),
				tags$style(type = "text/css", "#example_data { width:10%; font-size:85%; background-color:#126a8f }")
			),
			tabPanel("Reference",icon = icon("server", lib="font-awesome"),
				fluidRow(
					column(2, 
						uiOutput("reference_data_interface"),
						actionButton("refdel", "delete   ", icon = icon("window-close"))
					),
					column(2,
						uiOutput("importRefR"),
						actionButton("clearFileRef", "clear   ", icon = icon("window-close"))
					)
				),
				tags$style(type = "text/css", "#clearFileRef { width:100%; font-size:85%; background-color:#126a8f }"),
				tags$style(type = "text/css", "#refdel { width:100%; font-size:85%; background-color:#126a8f }")
			),
			tabPanel("Measurements",icon = icon("archive", lib="font-awesome"),
				DT::dataTableOutput('measurement_conversion_table')
			),
			tabPanel("Misc",icon = icon("terminal", lib="font-awesome"),
				actionButton('Create_Desktop_Icon', 'Desktop icon', icon = icon("gears")),
				tags$style(type = "text/css", "#Create_Desktop_Icon { width:8%; font-size:85%; background-color:#126a8f }")
			),
			tabPanel(
				title=a(img("Source Code", src='github.png',width='20px'), href='https://github.com/jjlynch2/OsteoSort', target='_blank')
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
						conditionalPanel(condition = "input.single_analysis == 'Non-Antimere regression'",
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
						conditionalPanel(condition = "input.single_analysis == 'Non-Antimere t-test'",
							fluidRow(
								column(12,
									selectInput("single_articulation_side_a", "Side", c(Left='Left', Right='Right'))
								),
								column(6,
									uiOutput("single_element_articulation_a")
								),
								column(6,
									uiOutput("single_element_articulation_b")
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
								actionButton("settings2","settings", icon=icon("keyboard-o"))
							),
							column(6,
								actionButton("proc","process ", icon = icon("cog"))
							)
						),
						fluidRow(br()),
						fluidRow(
							column(6),
							column(6,
								downloadButton("downloadData2", "save")
							)
						),
						tags$style(type = "text/css", "#downloadData2 { width:100%; font-size:85%; background-color:#126a8f }"),
						tags$style(type = "text/css", "#settings2 { width:100%; font-size:85%; background-color:#126a8f }"),
						tags$style(type = "text/css", "#proc { width:100%; font-size:85%; background-color:#126a8f }"),
						width=2
					),
					mainPanel(
						htmlOutput('single_contents'),
						imageOutput('plotplot', width=400, height=400),
						DT::dataTableOutput('table2'),
						bsModal("settingssingle", title = "Settings", trigger = "settings2", size = "large", 
					 		tabsetPanel(id="single_tab",
								tabPanel("Output Parameters",
									uiOutput("single_file_output1"),
									uiOutput("single_file_output2")
						 		),
						 		tabPanel("Statistical Parameters",
									fluidRow(
										column(4, 
											h4("Regression"),
											uiOutput("association_types"),
											uiOutput("association_alpha_prediction"),
											conditionalPanel(condition = "!input.association_alpha_prediction",
												uiOutput("association_prediction")
											),
											conditionalPanel(condition = "input.association_types == 'PCA-CCA'",
												uiOutput("association_pca"),
												conditionalPanel(condition = "input.association_pca == 'Select'",
													uiOutput("association_pca_select")
												),
												conditionalPanel(condition = "input.association_pca == 'Variance'",
													uiOutput("association_pca_variance")
												)
											)
										),
										column(4,
											h4("Common"),
											uiOutput("common_alpha_level")
										),
										column(4,
											h4("t-test"),
											uiOutput("single_absolute_value"),
											uiOutput("single_boxcox"),
											uiOutput("single_mean"),
											uiOutput("single_tails")
										)
									)#fr
								)#tp
							)#tsp
						) #bsmodal
					)#main
				)#sidebarLayout
			),#tabPanel
		tabPanel("Multiple",icon = icon("gears", lib="font-awesome"),
			titlePanel(""),
				sidebarLayout(
					sidebarPanel(
						selectInput('testtype2', 'Analysis', c(Pair='Pair_match',Articulation='Articulation_match',Association='Regression_match'), 'Pair_match'),
						uiOutput("testtype2"),
						conditionalPanel(condition = "input.testtype2 == 'Regression_match' || input.bone == 'alttp' || input.bone == 'altt' || input.bone == 'humerus' || input.bone == 'ulna' || input.bone == 'radius' || input.bone == 'femur' || input.bone == 'tibia' || input.bone == 'fibula' || input.bone == 'scapula' || input.bone == 'os_coxa' || input.bone == 'clavicle' ",
							selectInput('standard', 'Measurements', c(Standard='Standard', Supplemental='Supplemental'),'Standard')		
						),		
						uiOutput('resettableInput'),	
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
									downloadButton("downloadData", "save    ")
								)
							),
							tags$style(type = "text/css", "#settings1 { width:100%; font-size:85%; background-color:#126a8f  }"),
							tags$style(type = "text/css", "#pro { width:100%; font-size:85%; background-color:#126a8f  }"),
							tags$style(type = "text/css", "#clearFile1 { width:100%; font-size:85%; background-color:#126a8f }"),
							tags$style(type = "text/css", "#downloadData { width:100%; font-size:85%; background-color:#126a8f }"),
							width=2
					),
					mainPanel(
						htmlOutput('contents'),
						tabsetPanel(id="tabSelected",
							tabPanel("Not excluded",
					 			DT::dataTableOutput('table')),
					 		tabPanel("Excluded",
					 			DT::dataTableOutput('tablen'))),
					 	bsModal("settings", title = "Settings", trigger = "settings1", size = "large", 
					 		tabsetPanel(id="tabSelected2",
					 			tabPanel("Output Parameters",
									checkboxInput(inputId = "fileoutput1", label = "Output excel files", value = TRUE),
									checkboxInput(inputId = "fileoutput1plot", label = "Output plot files (WARNING: This option will generate a plot for every comparison)", value = FALSE)
					 			),
					 			tabPanel("Measurements",
					 				selectInput("suppp", "Measurement type: ", c(Standard='Standard', Supplemental='Supplemental'),'Standard'),
					 				conditionalPanel(condition = "input.suppp == 'Standard'",
				 						fluidRow(
				 							column(3,
												uiOutput('measurements1')
											),
											column(9,
												checkboxGroupInput(inputId = 'MeasurementsUsed1', '', c('Cla_01','Cla_04','Cla_05'), inline = TRUE, selected = c('Cla_01','Cla_04','Cla_05'))
											)
										),
				 						fluidRow(
				 							column(3,
												uiOutput('measurements2')
											),
											column(9,
												checkboxGroupInput(inputId = 'MeasurementsUsed2', ' ', c('Sca_01','Sca_02'), inline = TRUE, selected = c('Sca_01','Sca_02'))
											)
										),
				 						fluidRow(
				 							column(3,
												uiOutput('measurements3')
											),
											column(9,
												checkboxGroupInput(inputId = 'MeasurementsUsed3', '', c('Hum_01','Hum_02','Hum_03','Hum_04','Hum_05'), inline = TRUE, selected = c('Hum_01','Hum_02','Hum_03','Hum_04','Hum_05'))
											)
										),
				 						fluidRow(
				 							column(3,
												uiOutput('measurements5')
											),
											column(9,
												checkboxGroupInput(inputId = 'MeasurementsUsed5', '', c('Rad_01','Rad_05','Rad_06'), inline = TRUE, selected = c('Rad_01','Rad_05','Rad_06'))
											)
										),
				 						fluidRow(
				 							column(3,
												uiOutput('measurements4')
											),
											column(9,
												checkboxGroupInput(inputId = 'MeasurementsUsed4', '', c('Uln_01','Uln_04','Uln_05','Uln_06'), inline = TRUE, selected = c('Uln_01','Uln_04','Uln_05','Uln_06')) #52 removed
											)
										),
				 						fluidRow(
				 							column(3,
												uiOutput('measurements6')
											),
											column(9,
												checkboxGroupInput(inputId = 'MeasurementsUsed6', '', c('Osc_01','Osc_02'), inline = TRUE, selected = c('Osc_01','Osc_02'))
											)
										),
				 						fluidRow(
				 							column(3,
												uiOutput('measurements7')
											),
											column(9,
												checkboxGroupInput(inputId = 'MeasurementsUsed7', '', c('Fem_01','Fem_02','Fem_03','Fem_04','Fem_06','Fem_05','Fem_07'), inline = TRUE, selected = c('Fem_01','Fem_02','Fem_03','Fem_04','Fem_06','Fem_05','Fem_07')) #67 68 removed
											)
										),
				 						fluidRow(
				 							column(3,
												uiOutput('measurements8')
											),
											column(9,
												checkboxGroupInput(inputId = 'MeasurementsUsed8', '', c('Tib_01','Tib_02','Tib_03','Tib_04','Tib_05'), inline = TRUE, selected = c('Tib_01','Tib_02','Tib_03','Tib_04','Tib_05')) #74 removed
											)
										),
				 						fluidRow(
				 							column(3,
												uiOutput('measurements9')
											),
											column(9,
												checkboxGroupInput(inputId = 'MeasurementsUsed9', '', c('Fib_01','Fib_02'), inline = TRUE, selected = c('Fib_01','Fib_02'))
											)
										)
									),
					 				conditionalPanel(condition = "input.suppp == 'Supplemental'",
				 						fluidRow(
				 							column(3,
												uiOutput('measurementsa')
											),
											column(9,
												checkboxGroupInput(inputId = 'MeasurementsUseda', '', c('Cla_06','Cla_07','Cla_08','Cla_09'), inline = TRUE, selected = c('Cla_06','Cla_07','Cla_08','Cla_09'))
											)
										),
				 						fluidRow(
				 							column(3,
												uiOutput('measurementsb')
											),
											column(9,
												checkboxGroupInput(inputId = 'MeasurementsUsedb', ' ', c('Sca_03','Sca_04','Sca_05'), inline = TRUE, selected = c('Sca_03','Sca_04','Sca_05'))
											)
										),
				 						fluidRow(
				 							column(3,
												uiOutput('measurementsc')
											),
											column(9,
												checkboxGroupInput(inputId = 'MeasurementsUsedc', '', c('Hum_06','Hum_07','Hum_08','Hum_09'), inline = TRUE, selected = c('Hum_06','Hum_07','Hum_08','Hum_09'))
											)
										),
				 						fluidRow(
				 							column(3,
												uiOutput('measurementsd')
											),
											column(9,
												checkboxGroupInput(inputId = 'MeasurementsUsede', '', c('Rad_07','Rad_08','Rad_09','Rad_04','Rad_10'), inline = TRUE, selected = c('Rad_07','Rad_08','Rad_09','Rad_04','Rad_10'))
											)
										),
				 						fluidRow(
				 							column(3,
												uiOutput('measurementse')
											),
											column(9,
												checkboxGroupInput(inputId = 'MeasurementsUsedd', '', c('Uln_09','Uln_10','Uln_11'), inline = TRUE, selected = c('Uln_09','Uln_10','Uln_11'))
											)
										),
				 						fluidRow(
				 							column(3,
												uiOutput('measurementsf')
											),
											column(9,
												checkboxGroupInput(inputId = 'MeasurementsUsedf', '', c('Osc_14','Osc_15','Osc_16','Osc_05','Osc_17'), inline = TRUE, selected = c('Osc_14','Osc_15','Osc_16','Osc_05','Osc_17'))
											)
										),
				 						fluidRow(
				 							column(3,
												uiOutput('measurementsg')
											),
											column(9,
												checkboxGroupInput(inputId = 'MeasurementsUsedg', '', c('Fem_14','Fem_15','Fem_16','Fem_17'), inline = TRUE, selected = c('Fem_14','Fem_15','Fem_16','Fem_17'))
											)
										),
				 						fluidRow(
				 							column(3,
												uiOutput('measurementsh')
											),
											column(9,
												checkboxGroupInput(inputId = 'MeasurementsUsedh', '', c('Tib_10','Tib_11','Tib_12'), inline = TRUE, selected = c('Tib_10','Tib_11','Tib_12'))
											)
										),
				 						fluidRow(
				 							column(3,
												uiOutput('measurementsi')
											),
											column(9,
												checkboxGroupInput(inputId = 'MeasurementsUsedi', '', c('Fib_03','Fib_04','Fib_05'), inline = TRUE, selected = c('Fib_03','Fib_04','Fib_05'))
											)
										)
									)
								),
								tabPanel("Statistical Parameters",
									fluidRow(
										column(4,
											h4("Association"),
											radioButtons(inputId ="regtesttypem", label = "Regression:", choices = c("PCA-CCA", "Simple"), selected = "PCA-CCA"),
											checkboxInput(inputId = "alphapred2", label = "Use alpha levels for regression", value = TRUE),
											sliderInput(inputId = "asspredlevel", label = "Prediction interval level", min=0.01, max=1, value=0.95, step=0.01),


											conditionalPanel(condition = "input.regtesttypem == 'PCA-CCA'",
												radioButtons(inputId ="pcamultipleuse", label = "Principal Component Analysis", choices = c("All", "Select", "Variance"), selected = "Variance"),
											
												conditionalPanel(condition = "input.pcamultipleuse == 'Select'",
													sliderInput(inputId = "pcamultiple1", label = "Principal Components", min=1, max = 10, value = 1)
												),

												conditionalPanel(condition = "input.pcamultipleuse == 'Variance'",
													sliderInput(inputId = "pcamultiple2", label = "Cumulative Variance", min=0.1, max = 0.99, value = 0.99)
												)
											)
									
										),
										column(4,
											h4("Common"),
											sliderInput(inputId = "alphalevel", label = "Alpha level", min=0.01, max=1, value=0.05, step = 0.01),
											checkboxInput(inputId = "research", label = "Calculate research statistics", value = FALSE)								
										),
										column(4,
											h4("Pair & Articulation"),
											conditionalPanel(condition = "input.testtype2 == 'Articulation_match'",
												checkboxInput(inputId = "absolutevalue2", label = "Absolute D-value |a-b|", value = FALSE),

												checkboxInput(inputId = "power22", label = "Boxcox transformation", value = FALSE),
												checkboxInput(inputId = "testagainst2", label = "Zero sample mean", value = FALSE),
												sliderInput(inputId = "tails22", label = "Tails", min=1, max=2, value=2, step=1)
											),
											conditionalPanel(condition = "input.testtype2 == 'Pair_match'",
												checkboxInput(inputId = "absolutevalue", label = "Absolute D-value |a-b|", value = TRUE),

												checkboxInput(inputId = "power2", label = "Boxcox transformation", value = TRUE),
												checkboxInput(inputId = "testagainst", label = "Zero sample mean", value = FALSE),
												sliderInput(inputId = "tails2", label = "Tails", min=1, max=2, value=2, step=1)
											)
										)
									)								
								),
								tabPanel("Computational Parameters",
									uiOutput('ncores')
								)
							)
						)
					)			
				)
			)
		),
		navbarMenu("Outlier",icon = icon("sort-amount-asc", lib="font-awesome"),
			tabPanel("Metric",icon = icon("line-chart", lib="font-awesome"),	
				sidebarLayout(
					sidebarPanel(					
						uiOutput("testtype3"),
						selectInput("outlierside", "Side", c(Left='Left', Right='Right', Both='Both'), 'Both'),
						uiOutput('resettableInput3'),	
						fluidRow(
							column(6,
								actionButton("settings3","settings", icon=icon("keyboard-o"))
							),
							column(6,
								actionButton("pro3","process ", icon = icon("cog"))
							)
						),
						fluidRow(br()),
						fluidRow(
							column(6,
								actionButton("clearFile3", "clear   ", icon = icon("window-close"))
							),
							column(6,
								downloadButton("outlierdownload", "save    ")
							)
						),
						tags$style(type = "text/css", "#settings3 { width:100%; font-size:85%; background-color:#126a8f }"),
						tags$style(type = "text/css", "#pro3 { width:100%; font-size:85%; background-color:#126a8f }"),
						tags$style(type = "text/css", "#clearFile3 { width:100%; font-size:85%; background-color:#126a8f }"),
						tags$style(type = "text/css", "#outlierdownload { width:100%; font-size:85%; background-color:#126a8f }"),
						width=2
					),
					mainPanel(
						htmlOutput('outliercontent'),
						imageOutput('plotoutlier', width=400, height=400),
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
					 	bsModal("settingsoutlier", title = "Settings", trigger = "settings3", size = "large", 
					 		tabsetPanel(id="tabSelected2",
								tabPanel("Output Paramters",
									checkboxInput(inputId = "fileoutputl1", label = "Output excel file", value = TRUE),
									checkboxInput(inputId = "fileoutputl2", label = "Output plot", value = TRUE)
								),	
					 			tabPanel("Measurements",
			 						fluidRow(
			 							column(12,
											radioButtons(inputId = 'claviclemeasurements', 'Clavicle', c('Cla_01','Cla_04','Cla_05','Cla_06','Cla_07','Cla_08','Cla_09'), inline = TRUE, selected = 'Cla_01')
										)
									),
			 						fluidRow(
										column(12,
											radioButtons(inputId = 'scapulameasurements', 'Scapula', c('Sca_01','Sca_02','Sca_03','Sca_04','Sca_05'), inline = TRUE, selected = 'Sca_01')
										)
									),
			 						fluidRow(
										column(12,
											radioButtons(inputId = 'humerusmeasurements', 'Humerus', c('Hum_01','Hum_02','Hum_03','Hum_04','Hum_05','Hum_06','Hum_07','Hum_08','Hum_09'), inline = TRUE, selected = 'Hum_01')
										)
									),
			 						fluidRow(
										column(12,
											radioButtons(inputId = 'radiusmeasurements', 'Radius', c('Rad_01','Rad_05','Rad_06','Rad_07','Rad_08','Rad_09','Rad_04','Rad_10'), inline = TRUE, selected = 'Rad_01')
										)
									),
			 						fluidRow(
										column(12,
											radioButtons(inputId = 'ulnameasurements', 'Ulna', c('Uln_01','Uln_04','Uln_05','Uln_06','Uln_07','Uln_09','Uln_10','Uln_11'), inline = TRUE, selected = 'Uln_01')
										)
									),
			 						fluidRow(
										column(12,
											radioButtons(inputId = 'os_coxameasurements', 'Os_coxa', c('Osc_01','Osc_02', 'Osc_05', 'Osc_14','Osc_15','Osc_16','Osc_05','Osc_17'), inline = TRUE, selected = 'Osc_01')
										)
									),
			 						fluidRow(
										column(12,
											radioButtons(inputId = 'femurmeasurements', 'Femur', c('Fem_01','Fem_02','Fem_03','Fem_04','Fem_06','Fem_05','Fem_07','Fem_08','Fem_11','Fem_14','Fem_15','Fem_16','Fem_17'), inline = TRUE, selected = 'Fem_01')
										)
									),
			 						fluidRow(
										column(12,
											radioButtons(inputId = 'tibiameasurements', 'Tibia', c('Tib_01','Tib_02','Tib_03','Tib_04','Tib_05','Tib_06','Tib_10','Tib_11','Tib_12'), inline = TRUE, selected = 'Tib_01')
										)
									),
			 						fluidRow(
										column(12,
											radioButtons(inputId = 'fibulameasurements', 'Fibula', c('Fib_01','Fib_02','Fib_03','Fib_04','Fib_05'), inline = TRUE, selected = 'Fib_01')
										)
									)					
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
							selectInput(inputId = 'metric_type2', 'Stature metric', c(Millimeters = "mm", Centimeters = "cm", Inches = "in"), selected = 'in'),
							uiOutput("testtype4"),
							selectInput("outlierside4", "Side", c(Left='Left', Right='Right', Both='Both'), 'Both'),
							conditionalPanel(condition ="input.zz4 == 'tibia' || input.zz4 == 'femur'",
								selectInput("population5G", "Population", c(Genoves_cstat_mexican_female='genoves-cstat-mexican-female',Genoves_cstat_mexican_male='genoves-cstat-mexican-male',FDB_19th_cstat_any='19th-cstat-any',FDB_19th_cstat_white_male='19th-cstat-white-male',FDB_19th_cstat_white_female='19th-cstat-white-female',FDB_19th_cstat_black_male='19th-cstat-black-male',FDB_19th_cstat_black_female='19th-cstat-black-female',FDB_20th_FStat_any='20th-FStat-any', FDB_20th_FStat_white_male='20th-FStat-white-male', FDB_20th_FStat_white_female='20th-FStat-white-female', FDB_20th_FStat_black_male='20th-FStat-black-male', FDB_20th_FStat_black_female='20th-FStat-black-female', FDB_20th_FStat_hispanic_male='20th-FStat-hispanic-male', Trotter_any_male='Trotter-any-male', Trotter_black_male='Trotter-black-male', Trotter_white_male='Trotter-white-male'), 'genoves-cstat-mexican-female')
							),
							conditionalPanel(condition ="input.zz4 != 'tibia' && input.zz4 != 'femur'",
								selectInput("population4", "Population", c(FDB_19th_cstat_any='19th-cstat-any',FDB_19th_cstat_white_male='19th-cstat-white-male',FDB_19th_cstat_white_female='19th-cstat-white-female',FDB_19th_cstat_black_male='19th-cstat-black-male',FDB_19th_cstat_black_female='19th-cstat-black-female',FDB_20th_FStat_any='20th-FStat-any', FDB_20th_FStat_white_male='20th-FStat-white-male', FDB_20th_FStat_white_female='20th-FStat-white-female', FDB_20th_FStat_black_male='20th-FStat-black-male', FDB_20th_FStat_black_female='20th-FStat-black-female', FDB_20th_FStat_hispanic_male='20th-FStat-hispanic-male', Trotter_any_male='Trotter-any-male', Trotter_black_male='Trotter-black-male', Trotter_white_male='Trotter-white-male'), 'Trotter-any-male')
							),
							uiOutput('resettableInput4'),	
							fluidRow(
								column(6,
									actionButton("settings4","settings", icon=icon("keyboard-o"))
								),
								column(6,
									actionButton("pro4","process ", icon = icon("cog"))
								)
							),
							fluidRow(br()),
							fluidRow(
								column(6,
									actionButton("clearFile4", "clear   ", icon = icon("window-close"))
								),
								column(6,
									downloadButton("outlierdownload4", "save    ")
								)
							),
							tags$style(type = "text/css", "#settings4 { width:100%; font-size:85%; background-color:#126a8f }"),
							tags$style(type = "text/css", "#pro4 { width:100%; font-size:85%; background-color:#126a8f }"),
							tags$style(type = "text/css", "#clearFile4 { width:100%; font-size:85%; background-color:#126a8f }"),
							tags$style(type = "text/css", "#outlierdownload4 { width:100%; font-size:85%; background-color:#126a8f }"),
							width=2
					),
					mainPanel(
						htmlOutput('outliercontent4'),
						imageOutput('plotoutlier4', width=400, height=400),
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
					 	bsModal("settingsoutlier4", title = "Settings", trigger = "settings4", size = "large", 
					 		tabsetPanel(id="tabSelected2",
								tabPanel("Output Paramters",
									checkboxInput(inputId = "fileoutputstature1", label = "Output excel file", value = TRUE),
									checkboxInput(inputId = "fileoutputstature2", label = "Output plot", value = TRUE)
								),					 			
								tabPanel("Measurements",
			 						fluidRow(
										column(12,
											radioButtons(inputId = 'humerusmeasurements4', 'Humerus', c('Hum_01'), inline = TRUE, selected = 'Hum_01')
										)
									),
			 						fluidRow(
										column(12,
											radioButtons(inputId = 'radiusmeasurements4', 'Radius', c('Rad_01'), inline = TRUE, selected = 'Rad_01')
										)
									),
			 						fluidRow(
										column(12,
											radioButtons(inputId = 'ulnameasurements4', 'Ulna', c('Uln_01'), inline = TRUE, selected = 'Uln_01')
										)
									),
			 						fluidRow(
										column(12,
											radioButtons(inputId = 'femurmeasurements4', 'Femur', c('Fem_01'), inline = TRUE, selected = 'Fem_01')
										)
									),
			 						fluidRow(
										column(12,
											radioButtons(inputId = 'tibiameasurements4', 'Tibia', c('Tib_01'), inline = TRUE, selected = 'Tib_01')
										)
									),
			 						fluidRow(
										column(12,
											radioButtons(inputId = 'fibulameasurements4', 'Fibula', c('Fib_01'), inline = TRUE, selected = 'Fib_01')
										)
									)					
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
									actionButton("settings2D","settings", icon=icon("keyboard-o"))
								),
								column(6,
									actionButton("pro2D","process ", icon = icon("cog"))
								)
							),
							fluidRow(br()),
							fluidRow(
								column(6,
									actionButton("clearFile2D", "clear   ", icon = icon("window-close"))
								),
								column(6,
									downloadButton("downloadData2D", "save    ")
								)
							),
							tags$style(type = "text/css", "#settings2D { width:100%; font-size:85%; background-color:#126a8f }"),
							tags$style(type = "text/css", "#pro2D { width:100%; font-size:85%; background-color:#126a8f }"),
							tags$style(type = "text/css", "#clearFile2D { width:100%; font-size:85%; background-color:#126a8f }"),
							tags$style(type = "text/css", "#downloadData2D { width:100%; font-size:85%; background-color:#126a8f }"),
							width = 2
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
												uiOutput('icp2D'),
												uiOutput('trans2D')
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
									actionButton("previous"," previous", icon=icon("arrow-left"))
								),
								column(6,
									actionButton("nnext"," next    ", icon = icon("arrow-right"))
								)
							),
							fluidRow(br()),
							fluidRow(
								column(6,
									actionButton("start"," digitize", icon=icon("edit"))
								),
								column(6,
									actionButton("start2"," fracture", icon=icon("scissors"))
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
									actionButton("pcset","settings", icon=icon("keyboard-o"))
								),
								column(6,
									actionButton("simplify","simplify", icon=icon("cloud-download"))
								)
							),
							fluidRow(br()),
							fluidRow(
								column(6,
									actionButton("clearFile3Da", " clear   ", icon = icon("window-close"))
								),
								column(6,
									downloadButton("savedata", " save    ")
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
							width = 2
						),
						mainPanel(
							rglwidgetOutput('webgl3Dalign', width = "1200px", height = "1200px"),
							bsModal("pcset3D", title = "Settings", trigger = "pcset", size = "large", 
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
									actionButton("settings3D","settings", icon=icon("keyboard-o"))
								),
								column(6,
									actionButton("pro3D","process ", icon = icon("cog"))
								)
							),
							fluidRow(br()),
							fluidRow(
								column(6,
									actionButton("clearFile3D", "clear   ", icon = icon("window-close"))
								),
								column(6,
									downloadButton("downloadData3D", "save    ")
								)
							),
							tags$style(type = "text/css", "#settings3D { width:100%; font-size:85%; background-color:#126a8f }"),
							tags$style(type = "text/css", "#pro3D { width:100%; font-size:85%; background-color:#126a8f }"),
							tags$style(type = "text/css", "#clearFile3D { width:100%; font-size:85%; background-color:#126a8f }"),
							tags$style(type = "text/css", "#downloadData3D { width:100%; font-size:85%; background-color:#126a8f }"),
							width = 2
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
										uiOutput('fileoutput3Dtps'),
										uiOutput('fileoutput3Dplot')
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
						selectInput(inputId = 'metric_type', 'Stature metric', c(Millimeters = "mm", Centimeters = "cm", Inches = "in"), selected = 'in'),
						uiOutput("antestat_test"),
						selectInput("antestat_population", "Population", c(DPAA_any_male = "DPAA-any-male", DPAA_white_male = "DPAA-white-male", DPAA_black_male = "DPAA-black-male",FDB_20th_FStat_any='20th-FStat-any', FDB_20th_FStat_white_male='20th-FStat-white-male', FDB_20th_FStat_white_female='20th-FStat-white-female', FDB_20th_FStat_black_male='20th-FStat-black-male', FDB_20th_FStat_black_female='20th-FStat-black-female', Trotter_any_male='Trotter-any-male', Trotter_black_male='Trotter-black-male', Trotter_white_male='Trotter-white-male'), 'Trotter-any-male'),
						selectInput("ante_side", "Side", c(Left='Left', Right='Right')),									
							fluidRow(
								column(6,
									conditionalPanel(condition = "input.antestat == 'humerus'",
										numericInput(inputId = 'hu_antestat', label = 'Hum_01', value = '')																															
									),
									conditionalPanel(condition = "input.antestat == 'radius'",
										numericInput(inputId = 'ra_antestat', label = 'Rad_01', value = '')																															
									),
									conditionalPanel(condition = "input.antestat == 'ulna'",
										numericInput(inputId = 'ul_antestat', label = 'Uln_01', value = '')																															
									),
									conditionalPanel(condition = "input.antestat == 'femur'",
										numericInput(inputId = 'fe_antestat', label = 'Fem_01', value = '')																															
									),
									conditionalPanel(condition = "input.antestat == 'tibia'",
										numericInput(inputId = 'ti_antestat', label = 'Tib_01', value = '')																															
									),
									conditionalPanel(condition = "input.antestat == 'fibula'",
										numericInput(inputId = 'fi_antestat', label = 'Fib_01', value = '')																															
									),
									textInput(inputId = 'Postmortem_ID', label = 'Postmortem ID', value = 'X2')	

								),
								column(6,
									numericInput(inputId = 'antestat_input', label = 'Stature', value = ''),
									textInput(inputId = 'Antemortem_ID', label = 'Antemortem ID', value = 'X1')							

								)
							),
							fluidRow(
								column(6,
									actionButton("settingsante","settings", icon=icon("keyboard-o"))
								),
								column(6,
									actionButton("proantestat","process ", icon = icon("cog"))
								)
							),
							fluidRow(br()),
							fluidRow(
								column(6

								),
								column(6,
									downloadButton("downloadantestat", "save    ")
								)
							),
							tags$style(type = "text/css", "#settingsante { width:100%; font-size:85%; background-color:#126a8f }"),
							tags$style(type = "text/css", "#proantestat { width:100%; font-size:85%; background-color:#126a8f }"),
							tags$style(type = "text/css", "#downloadantestat { width:100%; font-size:85%; background-color:#126a8f }"),
							width=2
					),
					mainPanel(
						htmlOutput('antestat_output'),
						imageOutput('plotplotante', width=400, height=400),
						DT::dataTableOutput('antestat_table'),
					 	bsModal("settingsante2", title = "Settings", trigger = "settingsante", size = "large", 
					 		tabsetPanel(id="tabSelected2s",
								tabPanel("Output Paramters",
									checkboxInput(inputId = "fileoutputant1", label = "Output excel file", value = TRUE),
									checkboxInput(inputId = "fileoutputant2", label = "Output plot", value = TRUE)
								),	
								tabPanel("Statistical Parameters",
									fluidRow(column(6,
											sliderInput(inputId = "predlevelantestat", label = "Prediction interval level", min=0.01, max=1, value=0.95, step = 0.01),
											sliderInput(inputId = "alphalevelsantestat", label = "Alpha level", min=0.01, max=1, value=0.05, step = 0.01)
										),
										column(6,									
											radioButtons(inputId = "alphatest1s", label = "Test type", choices = c(Alpha = "Alpha", PI = "PI"),"Alpha")
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
								actionButton("settingsantem","settings", icon=icon("keyboard-o"))
							),
							column(6,
								actionButton("proantestatm","process ", icon = icon("cog"))
							)
						),
						fluidRow(br()),
						fluidRow(
							column(6,
								actionButton("clearFile1ante", "clear   ", icon = icon("window-close"))
							),
							column(6,
								downloadButton("downloadantestatm", "save    ")
							)
						),
						tags$style(type = "text/css", "#settingsantem { width:100%; font-size:85%; background-color:#126a8f }"),
						tags$style(type = "text/css", "#proantestatm { width:100%; font-size:85%; background-color:#126a8f }"),
						tags$style(type = "text/css", "#clearFile1ante { width:100%; font-size:85%; background-color:#126a8f }"),
						tags$style(type = "text/css", "#downloadantestatm { width:100%; font-size:85%; background-color:#126a8f }"),
						width=2
					),
					mainPanel(
						htmlOutput('antestat_outputm'),
						tabsetPanel(id="tabSelected",
							tabPanel("Not excluded",
					 			DT::dataTableOutput('antestat_table1m')),
					 		tabPanel("Excluded",
					 			DT::dataTableOutput('antestat_table2m'))),
					 	bsModal("settingsante2m", title = "Settings", trigger = "settingsantem", size = "large", 
					 		tabsetPanel(id="tabSelected2m",
								tabPanel("Output Paramters",
									checkboxInput(inputId = "fileoutputant1m", label = "Output excel file", value = TRUE),
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