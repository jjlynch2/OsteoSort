#' Shiny ui.r file
#' 
#' This is the ui.r file for the interface that utilizes all previous functions. 
#' runApp("osteosort")
options(warn = -1)

library(shiny)
library(shinyBS)
library(shinythemes)
#Navigation bar interface
shinyUI(
	navbarPage(theme = shinytheme("sandstone"), title=div(img(src="OsteoSort.png", width = "30px"), "OsteoSort 1.0"),


	
	
		tabPanel("Help",
					HTML("<h1><span style='font-family: 'Times New Roman', serif;'><strong>OsteoSort</strong></span></h1><hr /><p>&nbsp;</p><p>OsteoSort is an R package that automates the process of conducting outlier, pair, articulation, and association analysis of commingled human skeletal assemblages. This package provides a framework to incorporate metric, 2-dimensional, and 3-dimensional data using established methods and ongoing research in the field of anthropology. The methods are split into three primary modules:</p><p>&nbsp;</p><ul><li>Osteometric sorting</li><li>Osteoshape sorting</li><li>Outlier sorting</li></ul><p>&nbsp;</p><p>Osteometric sorting provides single and multiple pairwise commparisons for pair, articulation, and association analysis using the methods developed by Dr. John Byrd and Dr. Bradly Adams (Byrd 2008; Byrd and adams 2003, 2014). This module also provides an extension of these methods by allowing the manipulation of the statistical models. Standard measurements and supplemental measurements are supported. The Cora measurement numbering system is used in place of the original standard measurement numbers to aid in standardizing the numbering system across all measurement proposals. A copy of the Cora measurement system is provided below. The input of measurement data for the multiple pairwise comparisons uses a standardized format using comma delineated files (.CSV), which can be downloaded below.</p><p>&nbsp;</p><p>Osteoshape sorting is still under development and research, but will provide single and multiple pairwise comparisons for pair analysis. Both approaches are designed to work with fragmented remains and allow the use of partial landmark data.</p><p>&nbsp;</p><p>Outlier sorting identifies individual skeletal elements within an assemblage that are a metric measurement outlier. This is useful for identifying individuals who may have larger or smaller limb proportions than the average of the assemblage. This is further extended using the Trotter and Gleser, Genoves, and the Forensic Data Bank data to provide stature outlier identification based on maximum length measurements. This allows identification of which individuals are taller and shorter to aid in comparison with existing antemortem data.</p><p>&nbsp;</p><h1>Osteological Sorting References</h1><hr /><p>&nbsp;</p><p>Adams BJ, Byrd JE. Interobserver variation of selected postcranial skeletal measurements. Journal of Forensic Science 2002;47:1193-202.</p><p>Byrd JE, Adams BJ. Osteometric sorting of commingled human remains. Journal of Forensic Science 2003;48:717-24.</p><p>Adams BJ, Byrd JE. Resolution of small-scale commingling: a case report from the Vietnam war. Forensic Science International 2006;156-63-9</p><p>Byrd JE. Models and methods for osteometric sorting. In: Adams BJ, Byrd JE, editors. Recovery, analysis, and identification of commingled human remains. Totowa, NJ: Human Press, 2008:199-220.</p><p>LeGarde CB. Asymmetry of the Humerus: the influence of handedness on the deltoid tuberosity and possible implications for osteometric sorting. Master's Thesis, Missoula, MT: The University of Montana, 2012.</p><p>Byrd JE, LeGarde CB. Osteometric sorting. In: Adams BJ, Byrd JE, editors. Commingled Human Remains: methods in recovery, analysis, and identification. San Diego, CA: Academic Press, 2014:167-191.</p><p>Byrd JE, Adams BJ. Updated measurements for osteometric sorting. DPAA CIL document, dated 16 December 2015.</p><p>Garrido-Varas C, Rathnasinghe R, Thompson T, Savriama Y. A new method to pair-match metacarpals using bilateral symmetry and shape analysis. Journal of Forensic Science 2015;60:118-123.</p><p>Karell MA, Langstaff HK, Halazonetis DJ, Minghetti C, Frelat M, Kranioti EF. A novel method for pair-matching using three-dimensional digital models of bone: mesh-to-mesh value comparison. International Journal of Legal Medicine 2016;130:1315-22.</p><p>Finlayson JE, Bartelink EJ, Perrone A, Dalton K. Multimethod resolution of a small scale case of commingling. Journal of Forensic Science 2016</p><p>&nbsp;</p>"),
					HTML("<h1>Downloads</h1><hr /><p>&nbsp;</p>"),
					downloadButton('standardtemplate', 'Standard template'),
					downloadButton('coraguide', 'Cora measurement guide')
		),
		
		navbarMenu("Osteometric sorting",
			tabPanel("Single comparison",
				titlePanel("Single pairwise comparison"),
				sidebarLayout(
					sidebarPanel(
						radioButtons('testtype', '', c(Pair='Pair_match',Articulation='Articulation_match',Association='Regression_match'), 'Pair_match'),
						uiOutput("testtype"),
							conditionalPanel(condition = "input.zz == 'huur' || input.zz == 'hurr' || input.zz == 'hufr' || input.zz == 'hutr' || input.zz == 'hufir' || input.zz == 'ulrr' || input.zz == 'ulfr' || input.zz == 'ultr' || input.zz == 'ulfir' || input.zz == 'rafr' || input.zz == 'ratr' || input.zz == 'rafir' || input.zz == 'fetr' || input.zz == 'fefir' || input.zz == 'tifir' || input.zz == 'humerus' || input.zz == 'ulna' || input.zz == 'radius' || input.zz == 'femur' || input.zz == 'tibia' || input.zz == 'fibula' || input.zz == 'scapula' || input.zz == 'os_coxa' || input.zz == 'clavicle' ",
								selectInput("a", "Measurements", c(Standard='single_standard', Supplemental='single_supplemental'))
							),		
################std						
						conditionalPanel(condition = "input.testtype == 'Regression_match'",
							selectInput("prr", label = "Predictor", c("Bone1", "Bone2"))
						),
							conditionalPanel(condition = "input.a == 'single_standard'", 
							
							
							
								conditionalPanel(condition = "input.zz == 'huur'",
									fluidRow(
											column(4,
											h4("Humerus"),
											selectInput("huurside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'huur40', label = 'Hum_01', value = ''),								
											numericInput(inputId = 'huur41', label = 'Hum_02', value = ''),		
											numericInput(inputId = 'huur42', label = 'Hum_03', value = ''),																		
											numericInput(inputId = 'huur43', label = 'Hum_04', value = ''),	
											numericInput(inputId = 'huur44', label = 'Hum_05', value =  '')																								
										),
										column(4,
											h4("Ulna"),
											selectInput("huurside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'huur48', label = 'Uln_01', value = ''),							
											numericInput(inputId = 'huur49', label = 'Uln_04', value = ''),	
											numericInput(inputId = 'huur50', label = 'Uln_05', value = ''),	
											numericInput(inputId = 'huur51', label = 'Uln_06', value = '')
											#numericInput(inputId = 'huur52', label = '52', value = '')
										)
									)
								),
								
								
								conditionalPanel(condition = "input.zz == 'hurr'",
									fluidRow(
											column(4,
											h4("Humerus"),
											selectInput("hurrside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'hurr40', label = 'Hum_01', value = ''),								
											numericInput(inputId = 'hurr41', label = 'Hum_02', value = ''),		
											numericInput(inputId = 'hurr42', label = 'Hum_03', value = ''),																		
											numericInput(inputId = 'hurr43', label = 'Hum_04', value = ''),	
											numericInput(inputId = 'hurr44', label = 'Hum_05', value =  '')																								
										),
										column(4,
											h4("Radius"),
											selectInput("hurrside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'hurr45', label = 'Rad_01', value = ''),								
											numericInput(inputId = 'hurr46', label = 'Rad_05', value = ''),	
											numericInput(inputId = 'hurr47', label = 'Rad_06', value = '')
										)
									)
								),
								
								
								conditionalPanel(condition = "input.zz == 'hufr'",
									fluidRow(
											column(4,
											h4("Humerus"),
											selectInput("hufrside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'hufr40', label = 'Hum_01', value = ''),								
											numericInput(inputId = 'hufr41', label = 'Hum_02', value = ''),		
											numericInput(inputId = 'hufr42', label = 'Hum_03', value = ''),																		
											numericInput(inputId = 'hufr43', label = 'Hum_04', value = ''),	
											numericInput(inputId = 'hufr44', label = 'Hum_05', value =  '')																								
										),
										column(4,
											h4("Femur"),
											selectInput("hufrside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'hufr60', label = 'Fem_01', value = ''),								
											numericInput(inputId = 'hufr61', label = 'Fem_02', value = ''),
											numericInput(inputId = 'hufr62', label = 'Fem_03', value = ''),
											numericInput(inputId = 'hufr63', label = 'Fem_04', value = ''),
											numericInput(inputId = 'hufr65', label = 'Fem_05', value = ''),
											numericInput(inputId = 'hufr64', label = 'Fem_06', value = ''),
											numericInput(inputId = 'hufr66', label = 'Fem_07', value = '')
											#numericInput(inputId = 'hufr67', label = 'Fem_08', value = ''),
											#numericInput(inputId = 'hufr68', label = '68', value = '')
										)
									)
								),
								
								
								conditionalPanel(condition = "input.zz == 'hutr'",
									fluidRow(
											column(4,
											h4("Humerus"),
											selectInput("hutrside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'hutr40', label = 'Hum_01', value = ''),								
											numericInput(inputId = 'hutr41', label = 'Hum_02', value = ''),		
											numericInput(inputId = 'hutr42', label = 'Hum_03', value = ''),																		
											numericInput(inputId = 'hutr43', label = 'Hum_04', value = ''),	
											numericInput(inputId = 'hutr44', label = 'Hum_05', value =  '')																								
										),
										column(4,
											h4("Tibia"),
											selectInput("hutrside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'hutr69', label = 'Tib_01', value = ''),								
											numericInput(inputId = 'hutr70', label = 'Tib_02', value = ''),
											numericInput(inputId = 'hutr71', label = 'Tib_03', value = ''),
											numericInput(inputId = 'hutr72', label = 'Tib_04', value = ''),
											numericInput(inputId = 'hutr73', label = 'Tib_05', value = '')
											#numericInput(inputId = 'hutr74', label = 'Tib_06', value = '')
										)
									)
								),
								
								
								conditionalPanel(condition = "input.zz == 'hufir'",
									fluidRow(
											column(4,
											h4("Humerus"),
											selectInput("hufirside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'hufir40', label = 'Hum_01', value = ''),								
											numericInput(inputId = 'hufir41', label = 'Hum_02', value = ''),		
											numericInput(inputId = 'hufir42', label = 'Hum_03', value = ''),																		
											numericInput(inputId = 'hufir43', label = 'Hum_04', value = ''),	
											numericInput(inputId = 'hufir44', label = 'Hum_05', value =  '')																								
										),
										column(4,
											h4("Fibula"),
											selectInput("hufirside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'hufir75', label = 'Fib_01', value = ''),								
											numericInput(inputId = 'hufir76', label = 'Fib_02', value = '')
										)
									)
								),
								
								conditionalPanel(condition = "input.zz == 'ulrr'",
									fluidRow(
										column(4,
											h4("Ulna"),
											selectInput("ulrrside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'ulrr48', label = 'Uln_01', value = ''),							
											numericInput(inputId = 'ulrr49', label = 'Uln_04', value = ''),	
											numericInput(inputId = 'ulrr50', label = 'Uln_05', value = ''),	
											numericInput(inputId = 'ulrr51', label = 'Uln_06', value = '')
											#numericInput(inputId = 'ulrr52', label = '52', value = '')
										),
										column(4,
											h4("Radius"),
											selectInput("ulrrside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'ulrr45', label = 'Rad_01', value = ''),								
											numericInput(inputId = 'ulrr46', label = 'Rad_05', value = ''),	
											numericInput(inputId = 'ulrr47', label = 'Rad_06', value = '')
										)
									)
								),
								
								conditionalPanel(condition = "input.zz == 'ulfr'",
									fluidRow(
										column(4,
											h4("Ulna"),
											selectInput("ulfrside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'ulfr48', label = 'Uln_01', value = ''),							
											numericInput(inputId = 'ulfr49', label = 'Uln_04', value = ''),	
											numericInput(inputId = 'ulfr50', label = 'Uln_05', value = ''),	
											numericInput(inputId = 'ulfr51', label = 'Uln_06', value = '')
											#numericInput(inputId = 'ulfr52', label = '52', value = '')
										),
										column(4,
											h4("Femur"),
											selectInput("ulfrside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'ulfr60', label = 'Fem_01', value = ''),								
											numericInput(inputId = 'ulfr61', label = 'Fem_02', value = ''),
											numericInput(inputId = 'ulfr62', label = 'Fem_03', value = ''),
											numericInput(inputId = 'ulfr63', label = 'Fem_04', value = ''),
											numericInput(inputId = 'ulfr65', label = 'Fem_05', value = ''),
											numericInput(inputId = 'ulfr64', label = 'Fem_06', value = ''),
											numericInput(inputId = 'ulfr66', label = 'Fem_07', value = '')
											#numericInput(inputId = 'ulfr67', label = 'Fem_08', value = ''),
											#numericInput(inputId = 'ulfr68', label = '68', value = '')
										)
										

									)
								),
								
								conditionalPanel(condition = "input.zz == 'ultr'",
									fluidRow(
										column(4,
											h4("Ulna"),
											selectInput("ultrside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'ultr48', label = 'Uln_01', value = ''),							
											numericInput(inputId = 'ultr49', label = 'Uln_04', value = ''),	
											numericInput(inputId = 'ultr50', label = 'Uln_05', value = ''),	
											numericInput(inputId = 'ultr51', label = 'Uln_06', value = '')
											#numericInput(inputId = 'ultr52', label = '52', value = '')
										),
										
										column(4,
											h4("Tibia"),
											selectInput("ultrside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'ultr69', label = 'Tib_01', value = ''),								
											numericInput(inputId = 'ultr70', label = 'Tib_02', value = ''),
											numericInput(inputId = 'ultr71', label = 'Tib_03', value = ''),
											numericInput(inputId = 'ultr72', label = 'Tib_04', value = ''),
											numericInput(inputId = 'ultr73', label = 'Tib_05', value = '')
											#numericInput(inputId = 'ultr74', label = 'Tib_06', value = '')
										)

									)
								),
								conditionalPanel(condition = "input.zz == 'ulfir'",
									fluidRow(
										column(4,
											h4("Ulna"),
											selectInput("ulfirside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'ulfir48', label = 'Uln_01', value = ''),							
											numericInput(inputId = 'ulfir49', label = 'Uln_04', value = ''),	
											numericInput(inputId = 'ulfir50', label = 'Uln_05', value = ''),	
											numericInput(inputId = 'ulfir51', label = 'Uln_06', value = '')
											#numericInput(inputId = 'ulfir52', label = '52', value = '')
										),
										column(4,
											h4("Fibula"),
											selectInput("ulfirside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'ulfir75', label = 'Fib_01', value = ''),								
											numericInput(inputId = 'ulfir76', label = 'Fib_02', value = '')
										)

									)
								),
								conditionalPanel(condition = "input.zz == 'rafr'",
									fluidRow(
										column(4,
											h4("Radius"),
											selectInput("rafrside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'rafr45', label = 'Rad_01', value = ''),								
											numericInput(inputId = 'rafr46', label = 'Rad_05', value = ''),	
											numericInput(inputId = 'rafr47', label = 'Rad_06', value = '')
										),
										column(4,
											h4("Femur"),
											selectInput("rafrside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'rafr60', label = 'Fem_01', value = ''),								
											numericInput(inputId = 'rafr61', label = 'Fem_02', value = ''),
											numericInput(inputId = 'rafr62', label = 'Fem_03', value = ''),
											numericInput(inputId = 'rafr63', label = 'Fem_04', value = ''),
											numericInput(inputId = 'rafr65', label = 'Fem_05', value = ''),
											numericInput(inputId = 'rafr64', label = 'Fem_06', value = ''),
											numericInput(inputId = 'rafr66', label = 'Fem_07', value = '')
											#numericInput(inputId = 'rafr67', label = 'Fem_08', value = ''),
											#numericInput(inputId = 'rafr68', label = '68', value = '')
										)

									)
								),
								conditionalPanel(condition = "input.zz == 'ratr'",
									fluidRow(
										column(4,
											h4("Radius"),
											selectInput("ratrside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'ratr45', label = 'Rad_01', value = ''),								
											numericInput(inputId = 'ratr46', label = 'Rad_05', value = ''),	
											numericInput(inputId = 'ratr47', label = 'Rad_06', value = '')
										),
										column(4,
											h4("Tibia"),
											selectInput("ratrside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'ratr69', label = 'Tib_01', value = ''),								
											numericInput(inputId = 'ratr70', label = 'Tib_02', value = ''),
											numericInput(inputId = 'ratr71', label = 'Tib_03', value = ''),
											numericInput(inputId = 'ratr72', label = 'Tib_04', value = ''),
											numericInput(inputId = 'ratr73', label = 'Tib_05', value = '')
											#numericInput(inputId = 'ratr74', label = 'Tib_06', value = '')
										)


									)
								),
								conditionalPanel(condition = "input.zz == 'rafir'",
									fluidRow(
										column(4,
											h4("Radius"),
											selectInput("rafirside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'rafir45', label = 'Rad_01', value = ''),								
											numericInput(inputId = 'rafir46', label = 'Rad_05', value = ''),	
											numericInput(inputId = 'rafir47', label = 'Rad_06', value = '')
										),
										column(4,
											h4("Fibula"),
											selectInput("rafirside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'rafir75', label = 'Fib_01', value = ''),								
											numericInput(inputId = 'rafir76', label = 'Fib_02', value = '')
										)


									)
								),
								conditionalPanel(condition = "input.zz == 'fetr'",
									fluidRow(
										column(4,
											h4("Femur"),
											selectInput("fetrside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'fetr60', label = 'Fem_01', value = ''),								
											numericInput(inputId = 'fetr61', label = 'Fem_02', value = ''),
											numericInput(inputId = 'fetr62', label = 'Fem_03', value = ''),
											numericInput(inputId = 'fetr63', label = 'Fem_04', value = ''),
											numericInput(inputId = 'fetr65', label = 'Fem_05', value = ''),
											numericInput(inputId = 'fetr64', label = 'Fem_06', value = ''),
											numericInput(inputId = 'fetr66', label = 'Fem_07', value = '')
											#numericInput(inputId = 'fetr67', label = 'Fem_08', value = '')
										),
										column(4,
											h4("Tibia"),
											selectInput("fetrside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'fetr69', label = 'Tib_01', value = ''),								
											numericInput(inputId = 'fetr70', label = 'Tib_02', value = ''),
											numericInput(inputId = 'fetr71', label = 'Tib_03', value = ''),
											numericInput(inputId = 'fetr72', label = 'Tib_04', value = ''),
											numericInput(inputId = 'fetr73', label = 'Tib_05', value = '')
										)

									)
								),
								conditionalPanel(condition = "input.zz == 'fefir'",
									fluidRow(
										column(4,
											h4("Femur"),
											selectInput("fefirside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'fefir60', label = 'Fem_01', value = ''),								
											numericInput(inputId = 'fefir61', label = 'Fem_02', value = ''),
											numericInput(inputId = 'fefir62', label = 'Fem_03', value = ''),
											numericInput(inputId = 'fefir63', label = 'Fem_04', value = ''),
											numericInput(inputId = 'fefir65', label = 'Fem_05', value = ''),
											numericInput(inputId = 'fefir64', label = 'Fem_06', value = ''),
											numericInput(inputId = 'fefir66', label = 'Fem_07', value = '')
											#numericInput(inputId = 'fefir67', label = 'Fem_08', value = '')
										),
										column(4,
											h4("Fibula"),
											selectInput("fefirside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'fefir75', label = 'Fib_01', value = ''),								
											numericInput(inputId = 'fefir76', label = 'Fib_02', value = '')
										)

									)
								),
								conditionalPanel(condition = "input.zz == 'tifir'",
									fluidRow(
										column(4,
											h4("Tibia"),
											selectInput("tifirside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'tifir69', label = 'Tib_01', value = ''),								
											numericInput(inputId = 'tifir70', label = 'Tib_02', value = ''),
											numericInput(inputId = 'tifir71', label = 'Tib_03', value = ''),
											numericInput(inputId = 'tifir72', label = 'Tib_04', value = ''),
											numericInput(inputId = 'tifir73', label = 'Tib_05', value = '')
										),
										column(4,
											h4("Fibula"),
											selectInput("tifirside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'tifir75', label = 'Fib_01', value = ''),								
											numericInput(inputId = 'tifir76', label = 'Fib_02', value = '')
										)

									)
								),
							

								conditionalPanel(condition = "input.zz == 'humerus'",
									fluidRow(
										h3("Humeri"),
										column(4,
											h4("Left"),
											numericInput(inputId = 'a401', label = 'Hum_01', value = ''),								
											numericInput(inputId = 'a411', label = 'Hum_02', value = ''),		
											numericInput(inputId = 'a421', label = 'Hum_03', value = ''),																		
											numericInput(inputId = 'a431', label = 'Hum_04', value = ''),	
											numericInput(inputId = 'a441', label = 'Hum_05', value = '')																								
										),
										column(4,
											h4("Right"),
											numericInput(inputId = 'a402', label = 'Hum_01', value = ''),								
											numericInput(inputId = 'a412', label = 'Hum_02', value = ''),		
											numericInput(inputId = 'a422', label = 'Hum_03', value = ''),																		
											numericInput(inputId = 'a432', label = 'Hum_04', value = ''),	
											numericInput(inputId = 'a442', label = 'Hum_05', value = '')
										)
									)
								
								),
								
								conditionalPanel(condition = "input.zz == 'ulna'",
									fluidRow(
										h3("Ulnae"),
										column(4,
											h4("Left"),
											numericInput(inputId = 'a481', label = 'Uln_01', value = ''),							
											numericInput(inputId = 'a491', label = 'Uln_04', value = ''),	
											numericInput(inputId = 'a501', label = 'Uln_05', value = ''),	
											numericInput(inputId = 'a511', label = 'Uln_06', value = '')
											#numericInput(inputId = 'a521', label = '52', value = '')																																																						
										),
										column(4,
										   h4("Right"),
											numericInput(inputId = 'a482', label = 'Uln_01', value = ''),							
											numericInput(inputId = 'a492', label = 'Uln_04', value = ''),	
											numericInput(inputId = 'a502', label = 'Uln_05', value = ''),	
											numericInput(inputId = 'a512', label = 'Uln_06', value = '')
											#numericInput(inputId = 'a522', label = '52', value = '')				
										)
									)
								
								),
								
								conditionalPanel(condition = "input.zz == 'radius'",
									fluidRow(
										h3("Radii"),
										column(4,
											h4("Left"),
											numericInput(inputId = 'a451', label = 'Rad_01', value = ''),								
											numericInput(inputId = 'a461', label = 'Rad_05', value = ''),	
											numericInput(inputId = 'a471', label = 'Rad_06', value = '')								
										),
										column(4,
											h4("Right"),
											numericInput(inputId = 'a452', label = 'Rad_01', value = ''),								
											numericInput(inputId = 'a462', label = 'Rad_05', value = ''),	
											numericInput(inputId = 'a472', label = 'Rad_06', value = '')		
										)
									)
								),
								
								conditionalPanel(condition = "input.zz == 'femur'",
									fluidRow(
										h3("Femora"),
										column(4,
											h4("Left"),
											numericInput(inputId = 'a601', label = 'Fem_01', value = ''),								
											numericInput(inputId = 'a611', label = 'Fem_02', value = ''),
											numericInput(inputId = 'a621', label = 'Fem_03', value = ''),
											numericInput(inputId = 'a631', label = 'Fem_04', value = ''),
											numericInput(inputId = 'a651', label = 'Fem_05', value = ''),
											numericInput(inputId = 'a641', label = 'Fem_06', value = ''),
											numericInput(inputId = 'a661', label = 'Fem_07', value = '')
											#numericInput(inputId = 'a671', label = 'Fem_08', value = ''),
											#numericInput(inputId = 'a681', label = '68', value = '')														
										),
										column(4,
											h4("Right"),
											numericInput(inputId = 'a602', label = 'Fem_01', value = ''),								
											numericInput(inputId = 'a612', label = 'Fem_02', value = ''),
											numericInput(inputId = 'a622', label = 'Fem_03', value = ''),
											numericInput(inputId = 'a632', label = 'Fem_04', value = ''),
											numericInput(inputId = 'a652', label = 'Fem_05', value = ''),
											numericInput(inputId = 'a642', label = 'Fem_06', value = ''),
											numericInput(inputId = 'a662', label = 'Fem_07', value = '')
											#numericInput(inputId = 'a672', label = 'Fem_08', value = ''),
											#numericInput(inputId = 'a682', label = '68', value = '')		
										)
									)
								),
								
								
								conditionalPanel(condition = "input.zz == 'tibia'",
									fluidRow(
										h3("Tibiae"),
										column(4,
											h4("Left"),
											numericInput(inputId = 'a691', label = 'Tib_01', value = ''),								
											numericInput(inputId = 'a701', label = 'Tib_02', value = ''),
											numericInput(inputId = 'a711', label = 'Tib_03', value = ''),
											numericInput(inputId = 'a721', label = 'Tib_04', value = ''),
											numericInput(inputId = 'a731', label = 'Tib_05', value = '')	
											#numericInput(inputId = 'a741', label = 'Tib_06', value = '')													
										),
										column(4,
											h4("Right"),
											numericInput(inputId = 'a692', label = 'Tib_01', value = ''),								
											numericInput(inputId = 'a702', label = 'Tib_02', value = ''),
											numericInput(inputId = 'a712', label = 'Tib_03', value = ''),
											numericInput(inputId = 'a722', label = 'Tib_04', value = ''),
											numericInput(inputId = 'a732', label = 'Tib_05', value = '')	
											#numericInput(inputId = 'a742', label = 'Tib_06', value = '')	
										)
									)
								),	
								
								
								conditionalPanel(condition = "input.zz == 'fibula'",
									fluidRow(
										h3("Fibulae"),
										column(4,
											h4("Left"),
											numericInput(inputId = 'a751', label = 'Fib_01', value = ''),								
											numericInput(inputId = 'a761', label = 'Fib_02', value = '')								
										),
										column(4,
											h4("Right"),
											numericInput(inputId = 'a752', label = 'Fib_01', value = ''),								
											numericInput(inputId = 'a762', label = 'Fib_02', value = '')	
										)
									)
								),
								
								
								conditionalPanel(condition = "input.zz == 'os_coxa'",
									fluidRow(
										h3("Os_coxae"),
										column(4,
											h4("Left"),
											numericInput(inputId = 'a561', label = 'Osc_01', value = ''),								
											numericInput(inputId = 'a571', label = 'Osc_02', value = '')								
										),
										column(4,
											h4("Right"),
											numericInput(inputId = 'a562', label = 'Osc_01', value = ''),								
											numericInput(inputId = 'a572', label = 'Osc_02', value = '')	
										)
									)
								),
								
								
								conditionalPanel(condition = "input.zz == 'scapula'",
									fluidRow(
										h3("Scapulae"),
										column(4,
											h4("Left"),
											numericInput(inputId = 'a381', label = 'Sca_01', value = ''),								
											numericInput(inputId = 'a391', label = 'Sca_02', value = '')								
										),
										column(4,
											h4("Right"),
											numericInput(inputId = 'a382', label = 'Sca_01', value = ''),								
											numericInput(inputId = 'a392', label = 'Sca_02', value = '')	
										)
									)
								),
								
								
								conditionalPanel(condition = "input.zz == 'clavicle'",
									fluidRow(
										h3("Clavicles"),
										column(4,
											h4("Left"),
											numericInput(inputId = 'a351', label = 'Cla_01', value = ''),								
											numericInput(inputId = 'a361', label = 'Cla_04', value = ''),
											numericInput(inputId = 'a371', label = 'Cla_05', value = '')
										),
										column(4,
											h4("Right"),
											numericInput(inputId = 'a352', label = 'Cla_01', value = ''),								
											numericInput(inputId = 'a362', label = 'Cla_04', value = ''),
											numericInput(inputId = 'a372', label = 'Cla_05', value = '')	
										)
									)
								)				
								
							),
################std
################sup
							conditionalPanel(condition = "input.a == 'single_supplemental'",
								conditionalPanel(condition = "input.zz == 'humerus'",
									fluidRow(
										h3("Humeri"),
										column(4,
											h4("Left"),
											numericInput(inputId = 'a41A1', label = 'Hum_06', value = ''),								
											numericInput(inputId = 'a42A1', label = 'Hum_07', value = ''),
											numericInput(inputId = 'a44B1', label = 'Hum_08', value = ''),
											numericInput(inputId = 'a44D1', label = 'Hum_09', value = '')								
										),
										column(4,
											h4("Right"),
											numericInput(inputId = 'a41A2', label = 'Hum_06', value = ''),								
											numericInput(inputId = 'a42A2', label = 'Hum_07', value = ''),
											numericInput(inputId = 'a44B2', label = 'Hum_08', value = ''),
											numericInput(inputId = 'a44D2', label = 'Hum_09', value = '')	
										)
									)
								
								),
								
								conditionalPanel(condition = "input.zz == 'ulna'",
									fluidRow(
										h3("Ulnae"),
										column(4,
											h4("Left"),
											numericInput(inputId = 'a51A1', label = 'Uln_09', value = ''),							
											numericInput(inputId = 'a51B1', label = 'Uln_10', value = ''),
											numericInput(inputId = 'a51C1', label = 'Uln_11', value = '')							
										),
										column(4,
											h4("Right"),
											numericInput(inputId = 'a51A2', label = 'Uln_09', value = ''),							
											numericInput(inputId = 'a51B2', label = 'Uln_10', value = ''),
											numericInput(inputId = 'a51C2', label = 'Uln_11', value = '')	
										)
									)
								
								),
								
								conditionalPanel(condition = "input.zz == 'radius'",
									fluidRow(
										h3("Radii"),
										column(4,
											h4("Left"),
											numericInput(inputId = 'a47A1', label = 'Rad_07', value = ''),								
											numericInput(inputId = 'a47B1', label = 'Rad_08', value = ''),
											numericInput(inputId = 'a47C1', label = 'Rad_09', value = ''),
											numericInput(inputId = 'a47D1', label = 'Rad_04', value = ''),
											numericInput(inputId = 'a47E1', label = 'Rad_10', value = '')							
										),
										column(4,
											h4("Right"),
											numericInput(inputId = 'a47A2', label = 'Rad_07', value = ''),								
											numericInput(inputId = 'a47B2', label = 'Rad_08', value = ''),
											numericInput(inputId = 'a47C2', label = 'Rad_09', value = ''),
											numericInput(inputId = 'a47D2', label = 'Rad_04', value = ''),
											numericInput(inputId = 'a47E2', label = 'Rad_10', value = '')	
										)
									)
								),
								
								conditionalPanel(condition = "input.zz == 'femur'",
									fluidRow(
										h3("Femora"),
										column(4,
											h4("Left"),
											numericInput(inputId = 'a68A1', label = 'Fem_14', value = ''),								
											numericInput(inputId = 'a68B1', label = 'Fem_15', value = ''),
											numericInput(inputId = 'a68D1', label = 'Fem_16', value = ''),
											numericInput(inputId = 'a68E1', label = 'Fem_17', value = '')								
										),
										column(4,
											h4("Right"),
											numericInput(inputId = 'a68A2', label = 'Fem_14', value = ''),								
											numericInput(inputId = 'a68B2', label = 'Fem_15', value = ''),
											numericInput(inputId = 'a68D2', label = 'Fem_16', value = ''),
											numericInput(inputId = 'a68E2', label = 'Fem_17', value = '')	
										)
									)
								),
								
								
								conditionalPanel(condition = "input.zz == 'tibia'",
									fluidRow(
										h3("Tibiae"),
										column(4,
											h4("Left"),
											numericInput(inputId = 'a74A1', label = 'Tib_10', value = ''),								
											numericInput(inputId = 'a74B1', label = 'Tib_11', value = ''),
											numericInput(inputId = 'a74F1', label = 'Tib_12', value = '')								
										),
										column(4,
											h4("Right"),
											numericInput(inputId = 'a74A2', label = 'Tib_10', value = ''),								
											numericInput(inputId = 'a74B2', label = 'Tib_11', value = ''),
											numericInput(inputId = 'a74F2', label = 'Tib_12', value = '')	
										)
									)
								),	
								
								
								conditionalPanel(condition = "input.zz == 'fibula'",
									fluidRow(
										h3("Fibulae"),
										column(4,
											h4("Left"),
											numericInput(inputId = 'a76A1', label = 'Fib_03', value = ''),								
											numericInput(inputId = 'a76B1', label = 'Fib_04', value = ''),
											numericInput(inputId = 'a76C1', label = 'Fib_05', value = '')								
										),
										column(4,
											h4("Right"),
											numericInput(inputId = 'a76A2', label = 'Fib_03', value = ''),								
											numericInput(inputId = 'a76B2', label = 'Fib_04', value = ''),
											numericInput(inputId = 'a76C2', label = 'Fib_05', value = '')	
										)
									)
								),
								
								
								conditionalPanel(condition = "input.zz == 'os_coxa'",
									fluidRow(
										h3("Os_coxae"),
										column(4,
											h4("Left"),
											numericInput(inputId = 'a59A1', label = 'Osc_14', value = ''),								
											numericInput(inputId = 'a59B1', label = 'Osc_15', value = ''),
											numericInput(inputId = 'a59C1', label = 'Osc_16', value = ''),
											numericInput(inputId = 'a59D1', label = 'Osc_05', value = ''),
											numericInput(inputId = 'a59E1', label = 'Osc_17', value = '')								
										),
										column(4,
											h4("Right"),
											numericInput(inputId = 'a59A2', label = 'Osc_14', value = ''),								
											numericInput(inputId = 'a59B2', label = 'Osc_15', value = ''),
											numericInput(inputId = 'a59C2', label = 'Osc_16', value = ''),
											numericInput(inputId = 'a59D2', label = 'Osc_05', value = ''),
											numericInput(inputId = 'a59E2', label = 'Osc_17', value = '')
										)
									)
								),
								
								
								conditionalPanel(condition = "input.zz == 'scapula'",
									fluidRow(
										h3("Scapulae"),
										column(4,
											h4("Left"),
											numericInput(inputId = 'a39A1', label = 'Sca_03', value = ''),								
											numericInput(inputId = 'a39B1', label = 'Sca_04', value = ''),
											numericInput(inputId = 'a39D1', label = 'Sca_05', value = '')								
										),
										column(4,
											h4("Right"),
											numericInput(inputId = 'a39A2', label = 'Sca_03', value = ''),								
											numericInput(inputId = 'a39B2', label = 'Sca_04', value = ''),
											numericInput(inputId = 'a39D2', label = 'Sca_05', value = '')	
										)
									)
								),
								
								
								conditionalPanel(condition = "input.zz == 'clavicle'",
									fluidRow(
										h3("Clavicles"),
										column(4,
											h4("Left"),
											numericInput(inputId = 'a37A1', label = 'Cla_06', value = ''),								
											numericInput(inputId = 'a37B1', label = 'Cla_07', value = ''),
											numericInput(inputId = 'a37E1', label = 'Cla_08', value = ''),
											numericInput(inputId = 'a37D1', label = 'Cla_09', value = '')							
										),
										column(4,
											h4("Right"),
											numericInput(inputId = 'a37A2', label = 'Cla_06', value = ''),								
											numericInput(inputId = 'a37B2', label = 'Cla_07', value = ''),
											numericInput(inputId = 'a37E2', label = 'Cla_08', value = ''),
											numericInput(inputId = 'a37D2', label = 'Cla_09', value = '')
										)
									)
								),	
	
											
								conditionalPanel(condition = "input.zz == 'huur'",
									fluidRow(
											column(4,
											h4("Humerus"),
											selectInput("shuurside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'shuur41A', label = 'Hum_06', value = ''),								
											numericInput(inputId = 'shuur42A', label = 'Hum_07', value = ''),
											numericInput(inputId = 'shuur44B', label = 'Hum_08', value = ''),
											numericInput(inputId = 'shuur44D', label = 'Hum_09', value = '')																								
										),
										column(4,
											h4("Ulna"),
											selectInput("shuurside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'shuur51A', label = 'Uln_09', value = ''),							
											numericInput(inputId = 'shuur51B', label = 'Uln_10', value = ''),
											numericInput(inputId = 'shuur51C', label = 'Uln_11', value = '')
										)
									)
								),
								
								
								conditionalPanel(condition = "input.zz == 'hurr'",
									fluidRow(
											column(4,
											h4("Humerus"),
											selectInput("shurrside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'shurr41A', label = 'Hum_06', value = ''),								
											numericInput(inputId = 'shurr42A', label = 'Hum_07', value = ''),
											numericInput(inputId = 'shurr44B', label = 'Hum_08', value = ''),
											numericInput(inputId = 'shurr44D', label = 'Hum_09', value = '')																								
										),
										column(4,
											h4("Radius"),
											selectInput("shurrside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'shurr47A', label = 'Rad_07', value = ''),								
											numericInput(inputId = 'shurr47B', label = 'Rad_08', value = ''),
											numericInput(inputId = 'shurr47C', label = 'Rad_09', value = ''),
											numericInput(inputId = 'shurr47D', label = 'Rad_04', value = ''),
											numericInput(inputId = 'shurr47E', label = 'Rad_10', value = '')	
										)
									)
								),
								
								
								conditionalPanel(condition = "input.zz == 'hufr'",
									fluidRow(
											column(4,
											h4("Humerus"),
											selectInput("shufrside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'shufr41A', label = 'Hum_06', value = ''),								
											numericInput(inputId = 'shufr42A', label = 'Hum_07', value = ''),
											numericInput(inputId = 'shufr44B', label = 'Hum_08', value = ''),
											numericInput(inputId = 'shufr44D', label = 'Hum_09', value = '')																							
										),
										column(4,
											h4("Femur"),
											selectInput("shufrside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'shufr68A', label = 'Fem_14', value = ''),								
											numericInput(inputId = 'shufr68B', label = 'Fem_15', value = ''),
											numericInput(inputId = 'shufr68D', label = 'Fem_16', value = ''),
											numericInput(inputId = 'shufr68E', label = 'Fem_17', value = '')
										)
									)
								),
								
								
								conditionalPanel(condition = "input.zz == 'hutr'",
									fluidRow(
											column(4,
											h4("Humerus"),
											selectInput("shutrside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'shutr41A', label = 'Hum_06', value = ''),								
											numericInput(inputId = 'shutr42A', label = 'Hum_07', value = ''),
											numericInput(inputId = 'shutr44B', label = 'Hum_08', value = ''),
											numericInput(inputId = 'shutr44D', label = 'Hum_09', value = '')																							
										),
										column(4,
											h4("Tibia"),
											selectInput("shutrside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'shutr74A', label = 'Tib_10', value = ''),								
											numericInput(inputId = 'shutr74B', label = 'Tib_11', value = ''),
											numericInput(inputId = 'shutr74F', label = 'Tib_12', value = '')
										)
									)
								),
								
								
								conditionalPanel(condition = "input.zz == 'hufir'",
									fluidRow(
											column(4,
											h4("Humerus"),
											selectInput("shufirside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'shufir41A', label = 'Hum_06', value = ''),								
											numericInput(inputId = 'shufir42A', label = 'Hum_07', value = ''),
											numericInput(inputId = 'shufir44B', label = 'Hum_08', value = ''),
											numericInput(inputId = 'shufir44D', label = 'Hum_09', value = '')																							
										),
										column(4,
											h4("Fibula"),
											selectInput("shufirside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'shufir76A', label = 'Fib_03', value = ''),								
											numericInput(inputId = 'shufir76B', label = 'Fib_04', value = ''),
											numericInput(inputId = 'shufir76C', label = 'Fib_05', value = '')
										)
									)
								),
								
								conditionalPanel(condition = "input.zz == 'ulrr'",
									fluidRow(
										column(4,
											h4("Ulna"),
											selectInput("sulrrside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'sulrr51A', label = 'Uln_09', value = ''),							
											numericInput(inputId = 'sulrr51B', label = 'Uln_10', value = ''),
											numericInput(inputId = 'sulrr51C', label = 'Uln_11', value = '')
										),
										column(4,
											h4("Radius"),
											selectInput("sulrrside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'sulrr47A', label = 'Rad_07', value = ''),								
											numericInput(inputId = 'sulrr47B', label = 'Rad_08', value = ''),
											numericInput(inputId = 'sulrr47C', label = 'Rad_09', value = ''),
											numericInput(inputId = 'sulrr47D', label = 'Rad_04', value = ''),
											numericInput(inputId = 'sulrr47E', label = 'Rad_10', value = '')
										)
									)
								),
								
								conditionalPanel(condition = "input.zz == 'ulfr'",
									fluidRow(
										column(4,
											h4("Ulna"),
											selectInput("sulfrside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'sulfr51A', label = 'Uln_09', value = ''),							
											numericInput(inputId = 'sulfr51B', label = 'Uln_10', value = ''),
											numericInput(inputId = 'sulfr51C', label = 'Uln_11', value = '')
										),
										column(4,
											h4("Femur"),
											selectInput("sulfrside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'sulfr68A', label = 'Fem_14', value = ''),								
											numericInput(inputId = 'sulfr68B', label = 'Fem_15', value = ''),
											numericInput(inputId = 'sulfr68D', label = 'Fem_16', value = ''),
											numericInput(inputId = 'sulfr68E', label = 'Fem_17', value = '')
										)
										

									)
								),
								
								conditionalPanel(condition = "input.zz == 'ultr'",
									fluidRow(
										column(4,
											h4("Ulna"),
											selectInput("sultrside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'sultr51A', label = 'Uln_09', value = ''),							
											numericInput(inputId = 'sultr51B', label = 'Uln_10', value = ''),
											numericInput(inputId = 'sultr51C', label = 'Uln_11', value = '')
										),
										
										column(4,
											h4("Tibia"),
											selectInput("sultrside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'sultr74A', label = 'Tib_10', value = ''),								
											numericInput(inputId = 'sultr74B', label = 'Tib_11', value = ''),
											numericInput(inputId = 'sultr74F', label = 'Tib_12', value = '')
										)

									)
								),
								conditionalPanel(condition = "input.zz == 'ulfir'",
									fluidRow(
										column(4,
											h4("Ulna"),
											selectInput("sulfirside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'sulfir51A', label = 'Uln_09', value = ''),							
											numericInput(inputId = 'sulfir51B', label = 'Uln_10', value = ''),
											numericInput(inputId = 'sulfir51C', label = 'Uln_11', value = '')
										),
										column(4,
											h4("Fibula"),
											selectInput("sulfirside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'sulfir76A', label = 'Fib_03', value = ''),								
											numericInput(inputId = 'sulfir76B', label = 'Fib_04', value = ''),
											numericInput(inputId = 'sulfir76C', label = 'Fib_05', value = '')
										)

									)
								),
								conditionalPanel(condition = "input.zz == 'rafr'",
									fluidRow(
										column(4,
											h4("Radius"),
											selectInput("srafrside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'srafr47A', label = 'Rad_07', value = ''),								
											numericInput(inputId = 'srafr47B', label = 'Rad_08', value = ''),
											numericInput(inputId = 'srafr47C', label = 'Rad_09', value = ''),
											numericInput(inputId = 'srafr47D', label = 'Rad_04', value = ''),
											numericInput(inputId = 'srafr47E', label = 'Rad_10', value = '')
										),
										column(4,
											h4("Femur"),
											selectInput("srafrside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'srafr68A', label = 'Fem_14', value = ''),								
											numericInput(inputId = 'srafr68B', label = 'Fem_15', value = ''),
											numericInput(inputId = 'srafr68D', label = 'Fem_16', value = ''),
											numericInput(inputId = 'srafr68E', label = 'Fem_17', value = '')
										)

									)
								),
								conditionalPanel(condition = "input.zz == 'ratr'",
									fluidRow(
										column(4,
											h4("Radius"),
											selectInput("sratrside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'sratr47A', label = 'Rad_07', value = ''),								
											numericInput(inputId = 'sratr47B', label = 'Rad_08', value = ''),
											numericInput(inputId = 'sratr47C', label = 'Rad_09', value = ''),
											numericInput(inputId = 'sratr47D', label = 'Rad_04', value = ''),
											numericInput(inputId = 'sratr47E', label = 'Rad_10', value = '')
										),
										column(4,
											h4("Tibia"),
											selectInput("sratrside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'sratr74A', label = 'Tib_10', value = ''),								
											numericInput(inputId = 'sratr74B', label = 'Tib_11', value = ''),
											numericInput(inputId = 'sratr74F', label = 'Tib_12', value = '')
										)


									)
								),
								conditionalPanel(condition = "input.zz == 'rafir'",
									fluidRow(
										column(4,
											h4("Radius"),
											selectInput("srafirside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'srafir47A', label = 'Rad_07', value = ''),								
											numericInput(inputId = 'srafir47B', label = 'Rad_08', value = ''),
											numericInput(inputId = 'srafir47C', label = 'Rad_09', value = ''),
											numericInput(inputId = 'srafir47D', label = 'Rad_04', value = ''),
											numericInput(inputId = 'srafir47E', label = 'Rad_10', value = '')
										),
										column(4,
											h4("Fibula"),
											selectInput("srafirside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'srafir76A', label = 'Fib_03', value = ''),								
											numericInput(inputId = 'srafir76B', label = 'Fib_04', value = ''),
											numericInput(inputId = 'srafir76C', label = 'Fib_05', value = '')
										)


									)
								),
								conditionalPanel(condition = "input.zz == 'fetr'",
									fluidRow(
										column(4,
											h4("Femur"),
											selectInput("sfetrside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'sfetr68A', label = 'Fem_14', value = ''),								
											numericInput(inputId = 'sfetr68B', label = 'Fem_15', value = ''),
											numericInput(inputId = 'sfetr68D', label = 'Fem_16', value = ''),
											numericInput(inputId = 'sfetr68E', label = 'Fem_17', value = '')
										),
										column(4,
											h4("Tibia"),
											selectInput("sfetrside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'sfetr74A', label = 'Tib_10', value = ''),								
											numericInput(inputId = 'sfetr74B', label = 'Tib_11', value = ''),
											numericInput(inputId = 'sfetr74F', label = 'Tib_12', value = '')
										)

									)
								),
								conditionalPanel(condition = "input.zz == 'fefir'",
									fluidRow(
										column(4,
											h4("Femur"),
											selectInput("sfefirside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'sfefir68A', label = 'Fem_14', value = ''),								
											numericInput(inputId = 'sfefir68B', label = 'Fem_15', value = ''),
											numericInput(inputId = 'sfefir68D', label = 'Fem_16', value = ''),
											numericInput(inputId = 'sfefir68E', label = 'Fem_17', value = '')
										),
										column(4,
											h4("Fibula"),
											selectInput("sfefirside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'sfefir76A', label = 'Fib_03', value = ''),								
											numericInput(inputId = 'sfefir76B', label = 'Fib_04', value = ''),
											numericInput(inputId = 'sfefir76C', label = 'Fib_05', value = '')
										)

									)
								),
								conditionalPanel(condition = "input.zz == 'tifir'",
									fluidRow(
										column(4,
											h4("Tibia"),
											selectInput("stifirside1", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'stifir74A', label = 'Tib_10', value = ''),								
											numericInput(inputId = 'stifir74B', label = 'Tib_11', value = ''),
											numericInput(inputId = 'stifir74F', label = 'Tib_12', value = '')
										),
										column(4,
											h4("Fibula"),
											selectInput("stifirside2", "Side", c(Left='Left', Right='Right')),
											numericInput(inputId = 'stifir76A', label = 'Fib_03', value = ''),								
											numericInput(inputId = 'stifir76B', label = 'Fib_04', value = ''),
											numericInput(inputId = 'stifir76C', label = 'Fib_05', value = '')
										)

									)
								)
						),							
################sup
														
						conditionalPanel(condition = "input.zz == 'hu' | input.zz == 'hr' | input.zz == 'hs' | input.zz == 'hss' | input.zz == 'fi' | input.zz == 'ft' | input.zz == 'ftt'",
							fluidRow(
									column(5,
										selectInput("side1", "Side", c(Left='Left', Right='Right'))
									)							
								)
						),

						conditionalPanel(condition = "input.zz == 'hu'",								
								
								fluidRow(
								h4("Humerus_Ulna"),
									column(4,
										numericInput(inputId = 'b41A1', label = 'Hum_06', value = '')
									),
									column(4,							
										numericInput(inputId = 'b51C1', label = 'Uln_11', value = '')								
									)
								)
							
						),
						
						conditionalPanel(condition = "input.zz == 'hr'",
								fluidRow(
								h4("Humerus_Radius"),
									column(4,
										numericInput(inputId = 'b41A12', label = 'Hum_06', value = '')								
									),
									column(4,							
										numericInput(inputId = 'b47D1', label = 'Rad_04', value = '')	
									)
								)							
						),
						
						conditionalPanel(condition = "input.zz == 'hs'",
								fluidRow(
								h4("Humerus_Scapula1"),
									column(4,
										numericInput(inputId = 'b421', label = 'Hum_07', value = '')

									),
									column(4,							
										numericInput(inputId = 'b39A1', label = 'Sca_03', value = '')

									)
								)							
						),
						conditionalPanel(condition = "input.zz == 'hss'",
								fluidRow(
								h4("Humerus_Scapula2"),
									column(4,

										numericInput(inputId = 'b42A1', label = 'Hum_07', value = '')							
									),
									column(4,							

										numericInput(inputId = 'b39B1', label = 'Sca_04', value = '')	
									)
								)							
						),
						conditionalPanel(condition = "input.zz == 'fi'",
								fluidRow(
								h4("Femur_Os_coxa"),
									column(4,
										numericInput(inputId = 'b631', label = 'Fem_04', value = '')								
									),
									column(4,							
										numericInput(inputId = 'b59E1', label = 'Osc_17', value = '')	
									)
								)							
						),
						
						conditionalPanel(condition = "input.zz == 'ft'",
								fluidRow(
								h4("Femur_Tibia"),
									column(4,
										numericInput(inputId = 'b631', label = 'Fem_03', value = '')								
									),
									column(4,							
										numericInput(inputId = 'b701', label = 'Tib_02', value = '')	
									)
								)							
						),
						
						conditionalPanel(condition = "input.zz == 'ftt'",
								fluidRow(
								h4("Fibula_Tibia"),
									column(4,
										numericInput(inputId = 'b76C1', label = 'Fib_05', value = '')								
									),
									column(4,							
										numericInput(inputId = 'b74F1', label = 'Tib_12', value = '')	
									)
								)							
						),
							fluidRow(
							column(4,
							textInput(inputId = 'ID1', label = '1st ID #', value = 'X1')	
							),
							column(4,
							textInput(inputId = 'ID2', label = '2nd ID #', value = 'X2')		
							)
							),
		

							actionButton("proc","Process"),
							actionButton("settings2","Settings"),
							downloadButton("downloadData2", "Save results")
						
					),
					
		
					
					mainPanel(
						htmlOutput('contents2'),
						plotOutput('plotsingle', width = 400, height = 400),
						DT::dataTableOutput('table2'),

						bsModal("settingssingle", title = "Settings", trigger = "settings2", size = "large", 
							tabsetPanel(id="tabSelected2",
					 		
							tabPanel("Output Parameters",
								checkboxInput(inputId = "fileoutput3", label = "Output to excel files", value = TRUE),
								checkboxInput(inputId = "fileoutput4", label = "Output to individual specimen files (takes longer to run)", value = FALSE)
					 		),
					 		tabPanel("Statistical Parameters",
								checkboxInput(inputId = "regtesttypes", label = "PCA-CCA-Regression", value = TRUE),
								checkboxInput(inputId = "alphapred", label = "Use alpha levels for regression (Only apples to simple regression)", value = FALSE),
								sliderInput(inputId = "alphalevels2", label = "Prediction Interval Level", min=0.01, max=1, value=0.95, step = 0.01),
								sliderInput(inputId = "alphalevels", label = "Alpha Level", min=0.01, max=1, value=0.05, step = 0.01),
								checkboxInput(inputId = "absolutevalues", label = "Absolute D-value |a-b|", value = TRUE),
								checkboxInput(inputId = "power1", label = "Half-normalization transformation (Only applies to absolute value models)", value = TRUE),
								checkboxInput(inputId = "testagainstsingle", label = "Zero reference sample mean", value = FALSE)
							)
							)	
						)
						
					)
				)
			),
			



		
		

		tabPanel("Multiple comparison",
			titlePanel("Multiple pairwise comparison"),
				sidebarLayout(
					sidebarPanel(
					
					
						radioButtons('testtype2', '', c(Pair='Pair_match',Articulation='Articulation_match',Association='Regression_match'), 'Pair_match'),

					
							selectInput('sep', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'),','),
							uiOutput("testtype2"),
							
							conditionalPanel(condition = "input.testtype2 == 'Regression_match' || input.bone == 'alttp' || input.bone == 'altt' || input.bone == 'humerus' || input.bone == 'ulna' || input.bone == 'radius' || input.bone == 'femur' || input.bone == 'tibia' || input.bone == 'fibula' || input.bone == 'scapula' || input.bone == 'os_coxa' || input.bone == 'clavicle' ",
								selectInput('standard', 'Measurements', c(Standard='Standard', Supplemental='Supplemental'),'Standard')		
							),		
							uiOutput('resettableInput'),	
							actionButton("clearFile1", "Clear session"),
							actionButton("pro","Process"),
							actionButton("settings1","Settings"),
							downloadButton("downloadData", "Save results")
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
									checkboxInput(inputId = "fileoutput1", label = "Output to excel files ", value = TRUE),
									checkboxInput(inputId = "fileoutput2", label = "Output to individual specimen files (takes longer to run)", value = FALSE)
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
									checkboxInput(inputId = "regtesttypem", label = "PCA-CCA-Regression", value = TRUE),
									checkboxInput(inputId = "alphapred2", label = "Use alpha levels for regression (Only apples to simple regression)", value = FALSE),
									sliderInput(inputId = "asspredlevel", label = "Prediction Interval Level", min=0.01, max=1, value=0.95, step=0.01),
									sliderInput(inputId = "alphalevel", label = "Alpha Level", min=0.01, max=1, value=0.05, step = 0.01),
									checkboxInput(inputId = "absolutevalue", label = "Absolute D-value |a-b|", value = TRUE),
									checkboxInput(inputId = "power2", label = "Half-normalization transformation (Only applies to absolute value models)", value = TRUE),
									checkboxInput(inputId = "testagainst", label = "Zero reference sample mean", value = FALSE),
									checkboxInput(inputId = "research", label = "Calculate research statistics", value = FALSE)
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










		navbarMenu("Osteoshape sorting",		
			tabPanel("2D comparison"),
#				sidebarLayout(
#					sidebarPanel(
#					
#					    tabsetPanel(id="tabSelected",
#							tabPanel("Digitize",
#					 			
#								uiOutput('resettableInput2d'),
#
#								uiOutput('tpsupload'),	
#								
#
#
#									selectInput('bonelandmarkconfig', 'Elements', c(Humerus='humerus', Ulna='ulna', Radius='radius', Femur='femur', Tibia='tibia', Fibula='fibula', Scapula='scapula', Os_coxa='os_coxa', Clavicle='clavicle',Variable='variable'),'humerus'),
#									conditionalPanel(condition = "input.bonelandmarkconfig == 'variable'",
#										sliderInput(inputId = "variable", label = "Number of Landmarks", min=2, max=300, value=2, step = 1),
#										uiOutput('variablelandmark')
#									),
#									
#									
#									
#									radioButtons('bonelandmarkconfigside', 'Side', c(Left='Left',Right='Right'), 'Left'),
#									conditionalPanel(condition = "input.bonelandmarkconfig == 'humerus'",
#										radioButtons(inputId = 'landmarks', label = 'Landmarks', c('Landmark 1','Landmark 2','Landmark 3','Landmark 4','Landmark 5','Landmark 6','Landmark 7','Landmark 8','Landmark 9','Landmark 10','Scale'), selected = 'Landmark 1')
#									),
#									conditionalPanel(condition = "input.bonelandmarkconfig == 'ulna'",
#										radioButtons(inputId = 'landmarks', label = 'Landmarks', c('Landmark 1','Landmark 2','Landmark 3','Landmark 4','Landmark 5','Landmark 6','Landmark 7','Landmark 8'), selected = 'Landmark 1')
#									),	
#									conditionalPanel(condition = "input.bonelandmarkconfig == 'radius'",
#										radioButtons(inputId = 'landmarks', label = 'Landmarks', c('Landmark 1','Landmark 2','Landmark 3','Landmark 4','Landmark 5','Landmark 6','Landmark 7','Landmark 8','Landmark 9'), selected = 'Landmark 1')
#									),
																
								
#								actionButton("clearFile2", "Clear session"),
#								actionButton("previousimage", "Previous"),
#								actionButton("nextimage", "Next"),
#								downloadButton("savetps", "Save TPS"),
#								uiOutput('landmarks'),
#								tableOutput("clickinfo"),
#								uiOutput('scales'),
#								tableOutput("scaleinfo"),
#								tableOutput("sideinfo")
#							   

#							),
#							tabPanel("Pair-match")
#						)

#					),
#					mainPanel(
#						imageOutput("myimage", click="image_click", width = 1000, height = 1000)
						#uiOutput('landmarks')
#					)
#				)

#			),
			tabPanel("3D comparison")
			
			
			
			
			
		
		),
		
		
		navbarMenu("Outlier sorting",		
			tabPanel("Metric",
				sidebarLayout(
					sidebarPanel(					
							selectInput('sep3', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'),','),
							uiOutput("testtype3"),
							selectInput("outlierside", "Side", c(Left='Left', Right='Right', Both='Both'), 'Both'),
							uiOutput('resettableInput3'),	
							actionButton("clearFile3", "Clear session"),
							actionButton("pro3","Process"),
							actionButton("settings3","Settings"),
							downloadButton("outlierdownload", "Save results")
					),
					mainPanel(
					
						htmlOutput('outliercontent'),
						plotOutput('plotoutlier', width = 400, height = 400),
						tabsetPanel(id="tabSelectedoutlier",
							tabPanel("Upper outliers",
								DT::dataTableOutput('tablejustfuckingworka')
							),		
							tabPanel("Lower outliers",
						 		DT::dataTableOutput('tablejustfuckingworkb')
						 	),
						 	tabPanel("Non-outliers",
						 		DT::dataTableOutput('tablejustfuckingworkc')
						 	)
					 	),
					
					
					 	bsModal("settingsoutlier", title = "Settings", trigger = "settings3", size = "large", 
					 		tabsetPanel(id="tabSelected2",
					 			tabPanel("Measurements",
					 						fluidRow(
					 							column(12,
													radioButtons(inputId = 'claviclemeasurements', 'Clavicle', c('Cla_01','Cla_04','Cla_05','Cla_06','Cla_07','Cla_08','Cla_09'), inline = TRUE, selected = 'Cla_05')
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
													radioButtons(inputId = 'os_coxameasurements', 'Os_coxa', c('Osc_01','Osc_02','Osc_14','Osc_15','Osc_16','Osc_05','Osc_17'), inline = TRUE, selected = 'Osc_01')
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

									radioButtons('method', '', c(standard_deviation='Standard_deviation', quartiles='Quartiles'),'Standard_deviation'),
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

			),
			tabPanel("Stature",
				sidebarLayout(
					sidebarPanel(					
							selectInput('sep4', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'),','),
							uiOutput("testtype4"),
							selectInput("outlierside4", "Side", c(Left='Left', Right='Right', Both='Both'), 'Both'),
							
							conditionalPanel(condition ="input.zz4 == 'tibia' || input.zz4 == 'femur'",
								selectInput("population5G", "Population", c(Genoves_cstat_mexican_female='genoves-cstat-mexican-female',Genoves_cstat_mexican_male='genoves-cstat-mexican-male',FDB_19th_cstat_any='19th-cstat-any',FDB_19th_cstat_white_male='19th-cstat-white-male',FDB_19th_cstat_white_female='19th-cstat-white-female',FDB_19th_cstat_black_male='19th-cstat-black-male',FDB_19th_cstat_black_female='19th-cstat-black-female',FDB_20th_FStat_any='20th-FStat-any', FDB_20th_FStat_white_male='20th-FStat-white-male', FDB_20th_FStat_white_female='20th-FStat-white-female', FDB_20th_FStat_black_male='20th-FStat-black-male', FDB_20th_FStat_black_female='20th-FStat-black-female', FDB_20th_FStat_hispanic_male='20th-FStat-hispanic-male', Trotter_any_male='Trotter-any-male', Trotter_black_male='Trotter-black-male', Trotter_white_male='Trotter-white-male'), 'genoves-cstat-mexican-female')
							),
							conditionalPanel(condition ="input.zz4 != 'tibia' && input.zz4 != 'femur'",
								selectInput("population4", "Population", c(FDB_19th_cstat_any='19th-cstat-any',FDB_19th_cstat_white_male='19th-cstat-white-male',FDB_19th_cstat_white_female='19th-cstat-white-female',FDB_19th_cstat_black_male='19th-cstat-black-male',FDB_19th_cstat_black_female='19th-cstat-black-female',FDB_20th_FStat_any='20th-FStat-any', FDB_20th_FStat_white_male='20th-FStat-white-male', FDB_20th_FStat_white_female='20th-FStat-white-female', FDB_20th_FStat_black_male='20th-FStat-black-male', FDB_20th_FStat_black_female='20th-FStat-black-female', FDB_20th_FStat_hispanic_male='20th-FStat-hispanic-male', Trotter_any_male='Trotter-any-male', Trotter_black_male='Trotter-black-male', Trotter_white_male='Trotter-white-male'), 'Trotter-any-male')
							),
							
							uiOutput('resettableInput4'),	
							actionButton("clearFile4", "Clear session"),
							actionButton("pro4","Process"),
							actionButton("settings4","Settings"),
							downloadButton("outlierdownload4", "Save results")
					),
					mainPanel(
					
						htmlOutput('outliercontent4'),
						plotOutput('plotoutlier4', width = 400, height = 400),
						tabsetPanel(id="tabSelectedoutlier",
							tabPanel("Upper outliers",
								DT::dataTableOutput('tablejustfuckingworka4')
							),		
							tabPanel("Lower outliers",
								DT::dataTableOutput('tablejustfuckingworkb4')
						 	),
						 	tabPanel("Non-outliers",
					 			DT::dataTableOutput('tablejustfuckingworkc4')
						 	)
					 	),
					
					
					 	bsModal("settingsoutlier4", title = "Settings", trigger = "settings4", size = "large", 
					 		tabsetPanel(id="tabSelected2",
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

									radioButtons('method4', '', c(standard_deviation='Standard_deviation', quartiles='Quartiles'),'Standard_deviation'),
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
		
)))