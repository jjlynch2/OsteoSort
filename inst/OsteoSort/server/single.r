	################single comparison ################single comparison ################single comparison ################single comparison ################single comparison ################single comparison ################single comparison ################single comparison ################single comparison ################single comparison ################single comparison ################single comparison 
	output$contents2 <- renderUI({
	   HTML(paste("Select the parameters and enter measurements to begin"))
	})



	#######################generates options for single
	observeEvent(input$testtype, {
		if(input$testtype == 'Pair_match') {
			output$testtype <- renderUI({
				selectInput('zz', 'Elements', c(Humerus='humerus', Ulna='ulna', Radius='radius', Femur='femur', Tibia='tibia', Fibula='fibula', Scapula='scapula', Os_coxa='os_coxa', Clavicle='clavicle'),'humerus')
			})
		}
		if(input$testtype == 'Articulation_match') {
			output$testtype <- renderUI({
				selectInput('zz', 'Elements', c(Humerus_Ulna='hu',Humerus_Radius='hr', Humerus_Scapula1='hs', Humerus_Scapula2='hss', Femur_Os_coxa='fi', Femur_Tibia='ft', Fibula_Tibia='ftt'))
			})
		}
		if(input$testtype == 'Regression_match') {
			output$testtype <- renderUI({
				selectInput('zz', 'Elements', c(Humerus_Ulna = 'huur', Humerus_Radius = 'hurr', Humerus_Femur = 'hufr', Humerus_Tibia = 'hutr', Humerus_Fibula = 'hufir', Ulna_Radius = 'ulrr', Ulna_Femur = 'ulfr', Ulna_Tibia = 'ultr', Ulna_Fibula = 'ulfir', Radius_Femur = 'rafr', Radius_Tibia = 'ratr', Radius_Fibula = 'rafir', Femur_Tibia = 'fetr', Femur_Fibula = 'fefir', Tibia_Fibula = 'tifir'))
			})
		}
	})

	observeEvent(input$proc, {
	
		showModal(modalDialog(title = "Calculation has started...Window will update when finished.", easyClose = FALSE, footer = NULL))
	
		#Start display
		withProgress(message = 'Calculation has started', detail = 'This may take a while...', value = 0, {       
			for (i in 1:25) {
				incProgress(1/25)
				Sys.sleep(0.10)
			}
		})
		testt <- 'start'
		if(input$a == 'single_standard') {
			if(input$zz == 'huur') {
				reghum <- cbind(ID=input$ID1, Side=input$huurside1, Element="humerus", Hum_01=input$huur40, Hum_02=input$huur41, Hum_03=input$huur42, Hum_04=input$huur43, Hum_05=input$huur44)
				reguln <- cbind(ID=input$ID2, Side=input$huurside2, Element="ulna", Uln_01=input$huur48, Uln_04=input$huur49, Uln_05=input$huur50, Uln_06=input$huur51)
				testt <- 'reg'
			}
			if(input$zz == 'hurr') {
				reghum <- cbind(ID=input$ID1, Side=input$hurrside1, Element="humerus", Hum_01=input$hurr40, Hum_02=input$hurr41, Hum_03=input$hurr42, Hum_04=input$hurr43, Hum_05=input$hurr44)
				reguln <- cbind(ID=input$ID2, Side=input$hurrside2, Element="radius", Rad_01=input$hurr45, Rad_05=input$hurr46, Rad_06=input$hurr47)
				testt <- 'reg'
			}
			if(input$zz == 'hufr') {
				reghum <- cbind(ID=input$ID1, Side=input$hufrside1, Element="humerus", Hum_01=input$hufr40, Hum_02= input$hufr41, Hum_03=input$hufr42, Hum_04=input$hufr43, Hum_05=input$hufr44)
				reguln <- cbind(ID=input$ID2, Side=input$hufrside2, Element="femur", Fem_01=input$hufr60,Fem_02=input$hufr61,Fem_03=input$hufr62,Fem_04=input$hufr63,Fem_06=input$hufr64,Fem_05=input$hufr65,Fem_07=input$hufr66)
				testt <- 'reg'
			}
			if(input$zz == 'hutr') {
				reghum <- cbind(ID=input$ID1, Side=input$hutrside1, Element="humerus", Hum_01=input$hutr40, Hum_02=input$hutr41, Hum_03=input$hutr42, Hum_04=input$hutr43, Hum_05=input$hutr44)
				reguln <- cbind(ID=input$ID2, Side=input$hutrside2, Element="tibia", Tib_01=input$hutr69, Tib_02=input$hutr70,Tib_03=input$hutr71,Tib_04=input$hutr72,Tib_05=input$hutr73)
				testt <- 'reg'
			}	
			if(input$zz == 'hufir') {
				reghum <- cbind(ID=input$ID1, Side=input$hufirside1, Element="humerus", Hum_01=input$hufir40, Hum_02=input$hufir41, Hum_03=input$hufir42, Hum_04=input$hufir43, Hum_05=input$hufir44)
				reguln <- cbind(ID=input$ID2, Side=input$hufirside2, Element="fibula", Fib_01=input$hufir75, Fib_02=input$hufir76)
				testt <- 'reg'
			}
			if(input$zz == 'ulrr') {
				reguln <- cbind(ID=input$ID2, Side=input$ulrrside2, Element="radius", Rad_01=input$ulrr45, Rad_05=input$ulrr46, Rad_06=input$ulrr47)
				reghum <- cbind(ID=input$ID1, Side=input$ulrrside1, Element="ulna", Uln_01=input$ulrr48, Uln_04=input$ulrr49, Uln_05=input$ulrr50, Uln_06=input$ulrr51)
				testt <- 'reg'
			}
			if(input$zz == 'ulfr') {
				reguln <- cbind(ID=input$ID2, Side=input$ulfrside2, Element="femur", Fem_01=input$ulfr60,Fem_02=input$ulfr61,Fem_03=input$ulfr62,Fem_04=input$ulfr63,Fem_06=input$ulfr64,Fem_05=input$ulfr65,Fem_07=input$ulfr66)
				reghum <- cbind(ID=input$ID1, Side=input$ulfrside1, Element="ulna", Uln_01=input$ulfr48, Uln_04=input$ulfr49, Uln_05=input$ulfr50, Uln_06=input$ulfr51)
				testt <- 'reg'
			}
			if(input$zz == 'ultr') {
				reguln <- cbind(ID=input$ID2, Side=input$ultrside2, Element="tibia", Tib_01=input$ultr69, Tib_02=input$ultr70,Tib_03=input$ultr71,Tib_04=input$ultr72,Tib_05=input$ultr73)
				reghum <- cbind(ID=input$ID1, Side=input$ultrside1, Element="ulna", Uln_01=input$ultr48, Uln_04=input$ultr49, Uln_05=input$ultr50, Uln_06=input$ultr51)
				testt <- 'reg'
			}
			if(input$zz == 'ulfir') {
				reguln <- cbind(ID=input$ID2, Side=input$ulfirside2, Element="fibula", Fib_01=input$ulfir75, Fib_02=input$ulfir76)
				reghum <- cbind(ID=input$ID1, Side=input$ulfirside1, Element="ulna", Uln_01=input$ulfir48, Uln_04=input$ulfir49, Uln_05=input$ulfir50, Uln_06=input$ulfir51)
				testt <- 'reg'
			}	
			if(input$zz == 'rafr') {
				reghum <- cbind(ID=input$ID1, Side=input$rafrside1,Element= "radius", Rad_01=input$rafr45, Rad_05=input$rafr46, Rad_06=input$rafr47)
				reguln <- cbind(ID=input$ID2, Side=input$rafrside2, Element="femur", Fem_01=input$rafr60,Fem_02=input$rafr61,Fem_03=input$rafr62,Fem_04=input$rafr63,Fem_06=input$rafr64,Fem_05=input$rafr65,Fem_07=input$rafr66)
				testt <- 'reg'
			}
			if(input$zz == 'ratr') {
				reghum <- cbind(ID=input$ID1, Side=input$ratrside1, Element="radius", Rad_01=input$ratr45, Rad_05=input$ratr46, Rad_06=input$ratr47)
				reguln <- cbind(ID=input$ID2, Side=input$ratrside2, Element="tibia",  Tib_01=input$ratr69, Tib_02=input$ratr70,Tib_03=input$ratr71,Tib_04=input$ratr72,Tib_05=input$ratr73)
				testt <- 'reg'
			}
			if(input$zz == 'rafir') {
				reghum <- cbind(ID=input$ID1, Side=input$rafirside1, Element="radius", Rad_01=input$rafir45, Rad_05=input$rafir46, Rad_06=input$rafir47)
				reguln <- cbind(ID=input$ID2, Side=input$rafirside2, Element="fibula", Fib_01=input$rafir75, Fib_02=input$rafir76)
				testt <- 'reg'
			}
			if(input$zz == 'fetr') {
				reghum <- cbind(ID=input$ID1, Side=input$fetrside1, Element="femur", Fem_01=input$fetr60, Fem_02=input$fetr61,Fem_03=input$fetr62,Fem_04=input$fetr63,Fem_06=input$fetr64,Fem_05=input$fetr65,Fem_07=input$fetr66)
				reguln <- cbind(ID=input$ID2, Side=input$fetrside2, Element="tibia", Tib_01=input$fetr69, Tib_02=input$fetr70,Tib_03=input$fetr71,Tib_04=input$fetr72,Tib_05=input$fetr73)
				testt <- 'reg'		
			}	
			if(input$zz == 'fefir') {
				reghum <- cbind(ID=input$ID1, Side=input$fefirside1,Element= "femur", Fem_01=input$fefir60,Fem_02=input$fefir61,Fem_03=input$fefir62,Fem_04=input$fefir63,Fem_06=input$fefir64,Fem_05=input$fefir65,Fem_07=input$fefir66)
				reguln <- cbind(ID=input$ID2, Side=input$fefirside2, Element="fibula", Fib_01=input$fefir75, Fib_02=input$fefir76)
				testt <- 'reg'
			}	
			if(input$zz == 'tifir') {
				reghum <- cbind(ID=input$ID1, Side=input$tifirside1, Element="tibia", Tib_01=input$tifir69, Tib_02=input$tifir70,Tib_03=input$tifir71,Tib_04=input$tifir72,Tib_05=input$tifir73)
				reguln <- cbind(ID=input$ID2, Side=input$tifirside2, Element="fibula", Fib_01=input$tifir75, Fib_02=input$tifir76)
				testt <- 'reg'
			}						
			if(input$zz == 'humerus') {
				left <- cbind(input$ID1, "Left", "Humerus", input$a401, input$a411, input$a421, input$a431, input$a441)
				right <- cbind(input$ID2, "Right", "Humerus", input$a402, input$a412, input$a422, input$a432, input$a442) 
				testt <- 'pair' 
			}
			if(input$zz == 'radius') {
				left <- cbind(input$ID1, "Left", "Radius", input$a451, input$a461, input$a471)
				right <- cbind(input$ID2, "Right", "Radius", input$a452, input$a462, input$a472) 
				testt <- 'pair'                                        
			}
			if(input$zz == 'ulna') {
				left <- cbind(input$ID1,  "Left", "Ulna", input$a481, input$a491, input$a501, input$a511)
				right <- cbind(input$ID2,  "Right", "Ulna", input$a482, input$a492, input$a502, input$a512) 
				testt <- 'pair'                                 
			}
			if(input$zz == 'femur') {
				left <- cbind(input$ID1,  "Left", "Femur", input$a601, input$a611, input$a621, input$a631, input$a641, input$a651, input$a661)
				right <- cbind(input$ID2,  "Right", "Femur", input$a602, input$a612, input$a622, input$a632, input$a642, input$a652, input$a662) 
				testt <- 'pair'  
			}
			if(input$zz == 'tibia') {
				left <- cbind(input$ID1,  "Left", "Tibia", input$a691, input$a701, input$a711, input$a721, input$a731)
				right <- cbind(input$ID2,  "Right", "Tibia", input$a692, input$a702, input$a712, input$a722, input$a732) 
				testt <- 'pair'                
			}
			if(input$zz == 'fibula') {
				left <- cbind(input$ID1,  "Left", "Fibula", input$a751, input$a761)
				right <- cbind(input$ID2,  "Right", "Fibula", input$a752, input$a762) 
				testt <- 'pair'                               
			}
			if(input$zz == 'scapula') {
				left <- cbind(input$ID1,  "Left", "Scapula", input$a381, input$a391)
				right <- cbind(input$ID2,  "Right", "Scapula", input$a382, input$a392) 
				testt <- 'pair'                      
			}
			if(input$zz == 'clavicle') {
				left <- cbind(input$ID1,  "Left", "Clavicle", input$a351, input$a361, input$a371)
				right <- cbind(input$ID2,  "Right", "Clavicle", input$a352, input$a362, input$a372) 
				testt <- 'pair'                          
			}
			if(input$zz == 'os_coxa') {
				left <- cbind(input$ID1,  "Left", "Os_coxa", input$a561, input$a571)
				right <- cbind(input$ID2,  "Right", "Os_coxa", input$a562, input$a572) 
				testt <- 'pair'             
			}                
		} 
		if(input$a == 'single_supplemental') {
			if(input$zz == 'humerus') {
				left <- cbind(input$ID1,  "Left", "Humerus", input$a41A1,input$a42A1,input$a44B1,input$a44D1)
				right <- cbind(input$ID2,  "Right", "Humerus",  input$a41A2,input$a42A2,input$a44B2,input$a44D2) 
				testt <- 'pair'                   
			}
			if(input$zz == 'radius') {
				left <- cbind(input$ID1,  "Left", "Radius", input$a47A1,input$a47B1,input$a47C1,input$a47D1,input$a47E1)
				right <- cbind(input$ID2,  "Right", "Radius", input$a47A2,input$a47B2,input$a47C2,input$a47D2,input$a47E2) 
				testt <- 'pair'                                        
			}
			if(input$zz == 'ulna') {
				left <- cbind(input$ID1,  "Left", "Ulna", input$a51A1,input$a51B1,input$a51C1)
				right <- cbind(input$ID2,  "Right", "Ulna", input$a52A2,input$a52B2,input$a52C2) 
				testt <- 'pair'                                
			}
			if(input$zz == 'femur') {
				left <- cbind(input$ID1,  "Left", "Femur", input$a68A1,input$a68B1,input$a68D1,input$a68E1)
				right <- cbind(input$ID2,  "Right", "Femur", input$a68A2,input$a68B2,input$a68D2,input$a68E2)
				testt <- 'pair'                     
			}
			if(input$zz == 'tibia') {
				left <- cbind(input$ID1,  "Left", "Tibia", input$a74A1, input$a74B1, input$a74F1)
				right <- cbind(input$ID2,  "Right", "Tibia", input$a74A2, input$a74B2, input$a74F2) 
				testt <- 'pair'                 
			}
			if(input$zz == 'fibula') {
				left <- cbind(input$ID1,  "Left", "Fibula", input$a76A1, input$a76B1, input$a76C1)
				right <- cbind(input$ID2,  "Right", "Fibula", input$a76A2, input$a76B2, input$a76C2) 
				testt <- 'pair'                              
			}
			if(input$zz == 'scapula') {
				left <- cbind(input$ID1,  "Left", "Scapula", input$a39A1, input$a39B1, input$a39D1)
				right <- cbind(input$ID2,  "Right", "Scapula",  input$a39A2, input$a39B2, input$a39D2) 
				testt <- 'pair'                      
			}
			if(input$zz == 'clavicle') {
				left <- cbind(input$ID1,  "Left", "Clavicle", input$a37A1, input$a37B1, input$a37E1, input$a37D1)
				right <- cbind(input$ID2,  "Right", "Clavicle", input$a37A2, input$a37B2, input$a37E2, input$a37D2) 
				testt <- 'pair'                      
			}
			if(input$zz == 'os_coxa') {
				left <- cbind(input$ID1,  "Left", "Os_coxa", input$a59A1, input$a59B1, input$a59C1, input$a59D1, input$a59E1)
				right <- cbind(input$ID2,  "Right", "Os_coxa", input$a59A2, input$a59B2, input$a59C2, input$a59D2, input$a59E2) 
				testt <- 'pair'               
			} 
			if(input$zz == 'huur') {
				reghum <- cbind(ID=input$ID1, Side=input$shuurside1, Element="humerus", Hum_06=input$shuur41A, Hum_07=input$shuur42A, Hum_08=input$shuur44B, Hum_09=input$shuur44D)
				reguln <- cbind(ID=input$ID2, Side=input$shuurside2, Element="ulna", Uln_09=input$shuur51A, Uln_10=input$shuur51B, Uln_11=input$shuur51C)
				testt <- 'reg'
			}
			if(input$zz == 'hurr') {
				reghum <- cbind(ID=input$ID1,Side= input$shurrside1, Element="humerus", Hum_06=input$shurr41A, Hum_07=input$shurr42A, Hum_08=input$shurr44B, Hum_09=input$shurr44D)
				reguln <- cbind(ID=input$ID2, Side=input$shurrside2, Element="radius", Rad_07=input$shurr47A, Rad_08=input$shurr47B, Rad_09=input$shurr47C, Rad_04=input$shurr47D, Rad_10=input$shurr47E)
				testt <- 'reg'
			}
			if(input$zz == 'hufr') {
				reghum <- cbind(ID=input$ID1,Side= input$shufrside1, Element="humerus", Hum_06=input$shufr41A, Hum_07=input$shufr42A, Hum_08=input$shufr44B, Hum_09=input$shufr44D)
				reguln <- cbind(ID=input$ID2,Side= input$shufrside2, Element="femur", Fem_14=input$shufr68A,Fem_15=input$shufr68B,Fem_16=input$shufr68D,Fem_18=input$shufr68E)
				testt <- 'reg'
			}
			if(input$zz == 'hutr') {
				reghum <- cbind(ID=input$ID1,Side= input$shutrside1, Element="humerus", Hum_06=input$shutr41A, Hum_07=input$shutr42A, Hum_08=input$shutr44B, Hum_09=input$shutr44D)
				reguln <- cbind(ID=input$ID2,Side= input$shutrside2, Element="tibia", Tib_10=input$shutr74A, Tib_11=input$shutr74B,Tib_12=input$shutr74F)
				testt <- 'reg'
			}	
			if(input$zz == 'hufir') {
				reghum <- cbind(ID=input$ID1,Side= input$shufirside1, Element="humerus", Hum_06=input$shufir41A, Hum_07=input$shufir42A, Hum_08=input$shufir44B, Hum_09=input$shufir44D)
				reguln <- cbind(ID=input$ID2,Side= input$shufirside2,Element= "fibula", Fib_03=input$shufir76A, Fib_04=input$shufir76B, Fib_05=input$shufir76C)
				testt <- 'reg'
			}
			if(input$zz == 'ulrr') {
				reghum <- cbind(ID=input$ID1,Side= input$sulrrside1, Element="ulna", Uln_09=input$sulrr51A, Uln_10=input$sulrr51B, Uln_11=input$sulrr51C)
				reguln <- cbind(ID=input$ID2, Side=input$sulrrside2, Element="radius", Rad_07=input$sulrr47A, Rad_08=input$sulrr47B, Rad_09=input$sulrr47C, Rad_04=input$sulrr47D, Rad_10=input$sulrr47E)
				testt <- 'reg'
			}
			if(input$zz == 'ulfr') {
				reguln <- cbind(ID=input$ID2, Side=input$sulfrside2,Element= "femur", Fem_14=input$sulfr68A,Fem_15=input$sulfr68B,Fem_16=input$sulfr68D,Fem_18=input$sulfr68E)
				reghum <- cbind(ID=input$ID1, Side=input$sulfrside1, Element="ulna", Uln_09=input$sulfr51A, Uln_10=input$sulfr51B, Uln_11=input$sulfr51C)
				testt <- 'reg'
			}
			if(input$zz == 'ultr') {
				reguln <- cbind(ID=input$ID2, Side=input$sultrside2,Element= "tibia", Tib_10=input$sultr74A, Tib_11=input$sultr74B,Tib_12=input$sultr74F)
				reghum <- cbind(ID=input$ID1, Side=input$sultrside1, Element="ulna", Uln_09=input$sultr51A, Uln_10=input$sultr51B, Uln_11=input$sultr51C)
				testt <- 'reg'
			}
			if(input$zz == 'ulfir') {
				reguln <- cbind(ID=input$ID2, Side=input$sulfirside2, Element="fibula", Fib_03=input$sulfir76A, Fib_04=input$sulfir76B, Fib_05=input$sulfir76C)
				reghum <- cbind(ID=input$ID1, Side=input$sulfirside1, Element="ulna", Uln_09=input$sulfir51A, Uln_10=input$sulfir51B, Uln_11=input$sulfir51C)
				testt <- 'reg'
			}	
			if(input$zz == 'rafr') {
				reghum <- cbind(ID=input$ID1, Side=input$srafrside1, Element="radius", Rad_07=input$srafr47A, Rad_08=input$srafr47B, Rad_09=input$srafr47C, Rad_04=input$srafr47D, Rad_10=input$srafr47E)
				reguln <- cbind(ID=input$ID2, Side=input$srafrside2, Element="femur", Fem_14=input$srafr68A,Fem_15=input$srafr68B,Fem_16=input$srafr68D,Fem_18=input$srafr68E)
				testt <- 'reg'
			}
			if(input$zz == 'ratr') {
				reghum <- cbind(ID=input$ID1, Side=input$sratrside1,Element= "radius", Rad_07=input$sratr47A, Rad_08=input$sratr47B, Rad_09=input$sratr47C, Rad_04=input$sratr47D, Rad_10=input$sratr47E)
				reguln <- cbind(ID=input$ID2, Side=input$sratrside2, Element="tibia",  Tib_10=input$sratr74A, Tib_11=input$sratr74B, Tib_12=input$sratr74F)
				testt <- 'reg'
			}
			if(input$zz == 'rafir') {
				reghum <- cbind(ID=input$ID1, Side=input$srafirside1,Element="radius", Rad_07=input$srafir47A, Rad_08=input$srafir47B, Rad_09=input$srafir47C, Rad_04=input$srafir47D, Rad_010=input$srafir47E)
				reguln <- cbind(ID=input$ID2, Side=input$srafirside2,Element= "fibula", Fib_03=input$srafir76A, Fib_04=input$srafir76B, Fib_05=input$srafir76C)
				testt <- 'reg'
			}
			if(input$zz == 'fetr') {
				reghum <- cbind(ID=input$ID1, Side=input$sfetrside1, Element="femur", Fem_14=input$sfetr68A, Fem_15=input$sfetr68B, Fem_16=input$sfetr68D,Fem_18=input$sfetr68E)
				reguln <- cbind(ID=input$ID2, Side=input$sfetrside2, Element="tibia", Tib_10=input$sfetr74A, Tib_11=input$sfetr74B, Tib_12=input$sfetr74F)
				testt <- 'reg'		
			}	
			if(input$zz == 'fefir') {
				reghum <- cbind(ID=input$ID1, Side=input$sfefirside1, Element="femur", Fem_14=input$sfefir68A,Fem_15=input$sfefir68B,Fem_16=input$sfefir68D,Fem_18=input$sfefir68E)
				reguln <- cbind(ID=input$ID2, Side=input$sfefirside2, Element="fibula", Fib_03=input$sfefir76A, Fib_04=input$sfefir76B, Fib_05=input$sfefir76C)
				testt <- 'reg'
			}	
			if(input$zz == 'tifir') {
				reghum <- cbind(ID=input$ID1, Side=input$stifirside1, Element="tibia", Tib_10=input$stifir74A, Tib_11=input$stifir74B, Tib_12=input$stifir74F)
				reguln <- cbind(ID=input$ID2, Side=input$stifirside2, Element="fibula", Fib_03=input$stifir76A, Fib_04=input$stifir76B, Fib_05=input$stifir76C)
				testt <- 'reg'
			}			
		}
		if(input$zz == 'ftt') {
			dft <- cbind(input$ID1, input$ID2, input$side1, input$side1, "Fibula", "Tibia", input$b76C1, input$b74F1)
			colnames(dft) <- c("X-#", "X-#", "Side", "Side","Element", "Element", "Fib_05", "Tib_12")
			testt <- 'art'
		}
		if(input$zz == 'ft') {
			dft <- cbind(input$ID1, input$ID2, input$side1, input$side1, "Femur", "Tibia", input$b631, input$b701)
			colnames(dft) <- c("X-#", "X-#", "Side", "Side","Element", "Element", "Fem_04", "Tib_02")
			testt <- 'art'     
		}
		if(input$zz == 'fi') {
			dft <- cbind(input$ID1, input$ID2, input$side1, input$side1, "Os_coxa", "Femur", input$b59E1, input$b631)
			colnames(dft) <- c("X-#", "X-#", "Side", "Side","Element", "Element", "Osc_17", "Fem_04")
			testt <- 'art'   
		}
		if(input$zz == 'hs') {
			dft <- cbind(input$ID1, input$ID2, input$side1, input$side1, "Scapula", "Humerus", input$b39A1, input$b421)
			colnames(dft) <- c("X-#", "X-#", "Side", "Side","Element", "Element", "Sca_03", "Hum_03")
			testt <- 'art'  
		}
		if(input$zz == 'hss') {
			dft <- cbind(input$ID1, input$ID2, input$side1, input$side1, "Scapula", "Humerus", input$b39B1, input$b42A1)
			colnames(dft) <- c("X-#", "X-#", "Side", "Side","Element", "Element", "Sac_04", "Hum_07")
			testt <- 'art'  
		}
		if(input$zz == 'hr') {
			dft <- cbind(input$ID1, input$ID2, input$side1, input$side1, "Humerus", "Radius", input$b41A12, input$b47D1)
			colnames(dft) <- c("X-#", "X-#", "Side", "Side","Element", "Element", "Hum_06", "Rad_04")
			testt <- 'art'   
		}
		if(input$zz == 'hu') {
			dft <- cbind(input$ID1, input$ID2, input$side1, input$side1, "Humerus", "Ulna", input$b41A1, input$b51C1)
			colnames(dft) <- c("X-#", "X-#", "Side", "Side","Element", "Element", "Hum_06", "Uln_11")
			testt <- 'art'    
		}
		if(testt == 'reg') {
			if(all(is.na(reghum[,4:length(reghum)])) && all(is.na(reguln[,4:length(reguln)]))) {removeModal(); return(NULL)}###stops crashing if empty
				if(input$prr == "Bone1") { sort1 <- reghum; sort2 <- reguln}
				if(input$prr == "Bone2") { sort2 <- reghum; sort1 <- reguln}
				
				#sort1 <- reghum
				#sort2 <- reguln
				sortreg <- rbind.fill(as.data.frame(sort1),as.data.frame(sort2))
				outputtemp1 <- reg.input(sort = sortreg, bone1 = sort1[3], side1 = sort1[2], bone2 = sort2[3], side2 = sort2[2], template = input$a)
				direc2 <- reg.multitest(sort = outputtemp1[[1]], ref = outputtemp1[[2]], splitn = outputtemp1[[3]], predlevel = input$alphalevels2, alphatest = input$alphapred, alphalevel = input$alphalevels, sessiontempdir = sessiontemp, oo = c(input$fileoutput3, input$fileoutput4), plotme = TRUE, testtype = input$regtesttypes)		
				
				direc2tab <- rbind(direc2[[2]], direc2[[3]]) #combine exlcuded and not excluded for table display
				
				output$table2 <- DT::renderDataTable({
					DT::datatable(direc2tab, options = list(lengthMenu = c(1), pageLength = 1), rownames = FALSE)
				})   
				output$contents2 <- renderUI({  HTML(paste(""))})    
				output$plotsingle <- renderPlot({direc2[[4]]})
		}
		if(testt == 'pair') {
			if(all(is.na(left[,4:length(left)])) && all(is.na(right[,4:length(right)]))) {removeModal(); return(NULL)}###stops crashing if empty
				dft <- rbind(left,right) 
				dft <- data.frame(dft)
				colnames(dft)[1:3] <- c("ID","Side","Element")

				wtf <- pm.input(bone=toString(input$zz), sort=dft, template=strsplit(input$a,"_")[[1]][2],tresh=1)                                      
				direc2 <- pm.ttest(refdata = wtf[[2]], sortdata = wtf[[1]], stdout = FALSE, sessiontemp = sessiontemp, alphalevel = input$alphalevels, absolutevalue = input$absolutevalues, testagainst = input$testagainstsingle, oo = c(input$fileoutput3, input$fileoutput4), power = input$power1, plotme = TRUE)  
				
				tempDF <- rbind(direc2[[2]], direc2[[3]]) #combines both dataframes into a single one. Both are needed for multiple but only 1 for single.
				output$table2 <- DT::renderDataTable({
					DT::datatable(tempDF, options = list(lengthMenu = c(1), pageLength = 1), rownames = FALSE)
				})  
				
				output$contents2 <- renderUI({  HTML(paste(""))})  
				#output$plotsingle <- renderUI({HTML(paste(""))})
				output$plotsingle <- renderPlot({direc2[[4]]})
		}      
		if(testt == 'art') {
			if(all(is.na(dft[,7:length(dft)]))) {removeModal(); return(NULL)}###stops crashing if empty
				dft <- data.frame(dft)
				wtf <- art.input(bone=toString(input$zz), sort=dft)
				direc2 <- art.ttest(refdata = wtf[[2]], sortdata = wtf[[1]], stdout = FALSE, sessiontemp = sessiontemp, alphalevel = input$alphalevels, absolutevalue = input$absolutevalues, testagainst = input$testagainstsingle, oo = c(input$fileoutput3, input$fileoutput4), power = input$power1, plotme = TRUE)           
				
				tempDF <- rbind(direc2[[2]], direc2[[3]]) #combines both dataframes into a single one. Both are needed for multiple but only 1 for single.
				
				
				
				output$table2 <- DT::renderDataTable({
					DT::datatable(tempDF, options = list(lengthMenu = c(1), pageLength = 1), rownames = FALSE)
				})   
				                      
				output$contents2 <- renderUI({  HTML(paste(""))})   
				output$plotsingle <- renderPlot({direc2[[4]]})
		 }

		for(i in 10) { gc() } #clean up 
		removeModal()
		if(testt != "start" && any(oo=c(input$fileoutput3, input$fileoutput4))) {                       
			#Zip and download handler
			direc <- direc2[[1]]
			files <- list.files(direc, recursive = TRUE)
			setwd(direc)
			zip(zipfile = direc, files = files)
			setwd(sessiontemp)  
			output$downloadData2 <- downloadHandler(
				filename <- function() {
					paste("results.zip")
				},      
				content <- function(file) {
					setwd(direc)
					file.copy(paste(direc,'.zip',sep=''), file)  
					setwd(sessiontemp)  
				},
				contentType = "application/zip"
			)
		}
		setwd(sessiontemp)
	})  