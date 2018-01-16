#' A function estimate and sort by stature
#' 
#' Equation reference from the Forensic Data Bank
#'
#' @param sort Sorted data for comparison
#' @param bone Specifies the bone type ("left", "right", "both")
#' @param side The bone type side
#' @param population The reference sample for stature estimation
#' @param method The outlier method ("quartiles" or "standard deviations")
#' @param measurements The measurement types to be used
#' @param sessiontempdir Specifies temporary directory for analytical session
#' @param cutoff The outlier cutoff value for either quartiles or standard deviations
#' @param output_options First index TRUE outputs to excel, second index TRUE outputs plot
#' @param metric Specifies millimeters (mm), centimeters (cm), or inches (in) for stature
#'
#' @keywords random
#' @export
#' @examples
#' statsort()

statsort <- function (sort, bone = "femur", side = "both", population = "trotter-any-male", method = "Quartiles", measurements = NULL, sessiontempdir = NULL, cutoff = 1.5, output_options = c(TRUE,TRUE), metric = "mm") {	
     print("Outlier analysis started")	
	options(stringsAsFactors = FALSE)  	
	upperfile = "upper.csv"
	lowerfile = "lower.csv"
	nonoutliersfile = "non-outliers.csv"
	cutoffmax <- cutoff[2]
	cutoff <- cutoff[1]

	nocut <- FALSE
	if(cutoffmax == cutoff) {nocut <- TRUE}
	
	side <- tolower(side)
	population <- tolower(population)
	
	workingdir = getwd()

	direc <- OsteoSort:::analytical_temp_space(output_options, sessiontempdir) #creates temporary space 

	sortdata <- array(NA,c(length(sort[,1]),4)) 
	sortdata[,1] <- as.matrix(sort[["id"]]) 
	sortdata[,2] <- tolower(sort[["Side"]]) 
	sortdata[,3] <- tolower(sort[["Element"]])
	sortdata[,4] <- sort[[paste(measurements)]] ### will this work?? it does! 

	sort <- sortdata
	sort <- sort[sort[,3] == bone,]

	if(side == "left") {
		sort <- sort[sort[,2] == "left",]
	}
	if(side == "right") {
		sort <- sort[sort[,2] == "right",]
	}
	
	sort <- na.omit(sort)

	#is it worth adding all bones and every equation from fordisc? maybe.
	if(bone == "femur") {
		if(population == "genoves-cstat-mexican-female") {
			intercept <- 19.58346
			slope <- 1.019685
		}		
		if(population == "genoves-cstat-mexican-male") {
			intercept <- 26.13346
			slope <- 0.8897638
		}		
		if(population == "19th-cstat-black-female") {
			intercept <- 22.29
			slope <- 0.09351
		}
		if(population == "19th-cstat-black-male") {
			intercept <- 22.27
			slope <- 0.09637
		}
		if(population == "19th-cstat-white-female") {
			intercept <- 21.16
			slope <- 0.09767
		}
		if(population == "19th-cstat-white-male") {
			intercept <- 20.02
			slope <- 0.10313
		}
		if(population == "19th-cstat-any") {
			intercept <- 18.91
			slope <- 0.10361
		}
		if(population == "20th-fstat-hispanic-male") {
			intercept <- 24.89
			slope <- 0.09329
		}	
		if(population == "20th-fstat-black-female") {
			intercept <- 22.69
			slope <- 0.09449
		}
		if(population == "20th-fstat-black-male") {
			intercept <- 16.05
			slope <- 0.10896
		}
		if(population == "20th-fstat-white-female") {
			intercept <- 20.88
			slope <- 0.09905
		}
		if(population == "20th-fstat-white-male") {
			intercept <- 26.77
			slope <- 0.08979
		}
		if(population == "20th-fstat-any") {
			intercept <- 19.56
			slope <- 0.10380
		}
		if(population == "trotter-any-male") {
			intercept <- 27.51
			slope <- 0.08646
		}
		if(population == "trotter-black-male") {
			intercept <- 28.41
			slope <- 0.08165
		}
		if(population == "trotter-white-male") {
			intercept <- 26.64
			slope <- 0.08854
		}
	}
	if(bone == "tibia") {
		if(population == "genoves-cstat-mexican-female") {
			intercept <- 63.781
			slope <- 2.72
		}		
		if(population == "genoves-cstat-mexican-male") {
			intercept <- 93.752
			slope <- 1.96
		}	
		if(population == "19th-cstat-black-female") {
			intercept <- 28.34
			slope <- 0.09668
		}
		if(population == "19th-cstat-black-male") {
			intercept <- 26.80
			slope <- 0.10339
		}
		if(population == "19th-cstat-white-female") {
			intercept <- 25.77
			slope <- 0.10864
		}
		if(population == "19th-cstat-white-male") {
			intercept <- 26.71
			slope <- 0.10870
		}
		if(population == "19th-cstat-any") {
			intercept <- 28.48
			slope <- 0.10006
		}
		if(population == "20th-fstat-hispanic-male") {
			intercept <- 42.15
			slope <- 0.06663
		}
		if(population == "20th-fstat-black-female") {
			intercept <- 22.32
			slope <- 0.11471
		}
		if(population == "20th-fstat-black-male") {
			intercept <- 23.50
			slope <- 0.11136
		}
		if(population == "20th-fstat-white-female") {
			intercept <- 30.57
			slope <- 0.09378
		}
		if(population == "20th-fstat-white-male") {
			intercept <- 30.24
			slope <- 0.09954
		}
		if(population == "20th-fstat-any") {
			intercept <- 26.82
			slope <- 0.10653
		}
		if(population == "trotter-any-male") {
			intercept <- 34.77
			slope <- 0.08645
		}
		if(population == "trotter-black-male") {
			intercept <- 32.93
			slope <- 0.08568
		}
		if(population == "trotter-white-male") {
			intercept <- 32.44
			slope <- 0.09286
		}
	}
	if(bone == "fibula") {
		if(population == "19th-cstat-black-female") {
			intercept <- 26.99
			slope <- 0.10206
		}
		if(population == "19th-cstat-black-male") {
			intercept <- 25.51
			slope <- 0.10920
		}
		if(population == "19th-cstat-white-female") {
			intercept <- 23.14
			slope <- 0.11662
		}
		if(population == "19th-cstat-white-male") {
			intercept <- 25.01
			slope <- 0.11454
		}
		if(population == "19th-cstat-any") {
			intercept <- 25.21
			slope <- 0.11071
		}
		if(population == "20th-fstat-hispanic-male") {
			intercept <- 29.07
			slope <- 0.10267
		}
		if(population == "20th-fstat-black-female") {
			intercept <- 22.15
			slope <- 0.11794
		}
		if(population == "20th-fstat-black-male") {
			intercept <- 22.22
			slope <- 0.11696
		}
		if(population == "20th-fstat-white-female") {
			intercept <- 28.56
			slope <- 0.10118
		}
		if(population == "20th-fstat-white-male") {
			intercept <- 30.44
			slope <- 0.10048
		}
		if(population == "20th-fstat-any") {
			intercept <- 25.21
			slope <- 0.11250
		}
		if(population == "trotter-any-male") {
			intercept <- 32.99
			slope <- 0.09268
		}
		if(population == "trotter-black-male") {
			intercept <- 34.17
			slope <- 0.08490
		}
		if(population == "trotter-white-male") {
			intercept <- 30.71
			slope <- 0.09907
		}
	}
	if(bone == "humerus") {
		if(population == "19th-cstat-black-female") {
			intercept <- 24.92
			slope <- 0.12442
		}
		if(population == "19th-cstat-black-male") {
			intercept <- 21.88
			slope <- 0.13660
		}
		if(population == "19th-cstat-white-female") {
			intercept <- 20.02
			slope <- 0.14250
		}
		if(population == "19th-cstat-white-male") {
			intercept <- 26.57
			slope <- 0.12293
		}
		if(population == "19th-cstat-any") {
			intercept <- 21.04
			slope <- 0.13887
		}
		if(population == "20th-fstat-hispanic-male") {
			intercept <- 28.25
			slope <- 0.12101
		}
		if(population == "20th-fstat-black-female") {
			intercept <- 19.17
			slope <- 0.14776
		}
		if(population == "20th-fstat-black-male") {
			intercept <- 25.00
			slope <- 0.13049
		}
		if(population == "20th-fstat-white-female") {
			intercept <- 29.15
			slope <- 0.11437
		}
		if(population == "20th-fstat-white-male") {
			intercept <- 27.18
			slope <- 0.12567
		}
		if(population == "20th-fstat-any") {
			intercept <- 21.36
			slope <- 0.14196
		}
		if(population == "trotter-any-male") {
			intercept <- 30.01
			slope <- 0.11453
		}
		if(population == "trotter-black-male") {
			intercept <- 25.03
			slope <- 0.12660
		}
		if(population == "trotter-white-male") {
			intercept <- 30.15
			slope <- 0.11433
		}
	}
	if(bone == "ulna") {
		if(population == "19th-cstat-black-female") {
			intercept <- 29.25
			slope <- 0.13352
		}
		if(population == "19th-cstat-black-male") {
			intercept <- 26.13
			slope <- 0.14903
		}
		if(population == "19th-cstat-white-female") {
			intercept <- 27.39
			slope <- 0.14962
		}
		if(population == "19th-cstat-white-male") {
			intercept <- 27.33
			slope <- 0.15169
		}
		if(population == "19th-cstat-any") {
			intercept <- 29.25
			slope <- 0.13352
		}
		if(population == "20th-fstat-hispanic-male") {
			intercept <- 33.80
			slope <- 0.12612
		}
		if(population == "20th-fstat-black-female") {
			intercept <- 29.25
			slope <- 0.13964
		}
		if(population == "20th-fstat-black-male") {
			intercept <- 20.53
			slope <- 0.17051
		}
		if(population == "20th-fstat-white-female") {
			intercept <- 31.80
			slope <- 0.13196
		}
		if(population == "20th-fstat-white-male") {
			intercept <- 25.81
			slope <- 0.16004
		}
		if(population == "20th-fstat-any") {
			intercept <- 25.64
			slope <- 0.15852
		}	
		if(population == "trotter-any-male") {
			intercept <- 34.91
			slope <- 0.12431
		}
		if(population == "trotter-black-male") {
			intercept <- 31.86
			slope <- 0.12700
		}
		if(population == "trotter-white-male") {
			intercept <- 31.30
			slope <- 0.13836
		}
	}
	if(bone == "radius") {
		if(population == "19th-cstat-black-female") {
			intercept <- 28.00
			slope <- 0.14910
		}
		if(population == "19th-cstat-black-male") {
			intercept <- 26.04
			slope <- 0.15988
		}
		if(population == "19th-cstat-white-female") {
			intercept <- 22.45
			slope <- 0.18330
		}
		if(population == "19th-cstat-white-male") {
			intercept <- 28.25
			slope <- 0.15920
		}
		if(population == "19th-cstat-any") {
			intercept <- 29.99
			slope <- 0.14659
		}
		if(population == "20th-fstat-hispanic-male") {
			intercept <- 34.74
			slope <- 0.13126
		}
		if(population == "20th-fstat-black-female") {
			intercept <- 26.11
			slope <- 0.16254
		}
		if(population == "20th-fstat-black-male") {
			intercept <- 20.65
			slope <- 0.18185
		}
		if(population == "20th-fstat-white-female") {
			intercept <- 30.67
			slope <- 0.14640
		}
		if(population == "20th-fstat-white-male") {
			intercept <- 28.16
			slope <- 0.16208
		}
		if(population == "20th-fstat-any") {
			intercept <- 26.83
			slope <- 0.16483
		
		}	
		if(population == "trotter-any-male") {
			intercept <- 36.27
			slope <- 0.12818
		}
		if(population == "trotter-black-male") {
			intercept <- 32.98
			slope <- 0.13211
		}
		if(population == "trotter-white-male") {
			intercept <- 33.04
			slope <- 0.14175
		}
	}

	if(metric == "cm") {
		intercept <- intercept * 2.54
		slope <- slope * 2.54
	}
	if(metric == "mm") {
		intercept <- (intercept * 2.54) * 10
		slope <- (slope * 2.54) * 10	
	}

	#return if incorrect population reference
	if(is.null(intercept)) { return(NULL) }
	
	#plot point estimates with 1 and 2 standard deviation lines. all pointestimates above 2 STD are grouped. 
	pointestimate <- array(NA,c(length(sort[,1]),4))
	for(i in 1:length(sort[,1])) {
		pointestimate[i,1] <- sort[i,1] #id name
		pointestimate[i,2] <- sort[i,2] #Side name
		pointestimate[i,3] <- sort[i,3] #Bone name
		pointestimate[i,4] <- round( as.numeric(sort[i,4]) * slope + intercept, digits=2) #PE name #add *0.1 for trotter shit when testing
	}
	
	
	s <- sd(as.numeric(pointestimate[,4]))
	m <- mean(as.numeric(pointestimate[,4]))
	me <- mean(as.numeric(pointestimate[,4]))
	IQQ <- quantile(as.numeric(pointestimate[,4]))[4] -  quantile(as.numeric(pointestimate[,4]))[2]
	
	if(method == "Standard_deviation") {
		#two standard deviations and mean
		standarddeviation <- sd(as.numeric(pointestimate[,4])) 
		meann <- mean(as.numeric(pointestimate[,4]))
	
		#upper and lower limits for defining outliers roughly 95% two stds
		upper <- meann + standarddeviation * cutoff
		lower <- meann - standarddeviation * cutoff
		uppermax <- meann + standarddeviation * cutoffmax
		lowermax <- meann - standarddeviation * cutoffmax
		
		plotme <- mean(as.numeric(pointestimate[,4]))
	}
	if(method == "Quartiles") {
		Q1 <- quantile(as.numeric(pointestimate[,4]))[2]
		Q3 <- quantile(as.numeric(pointestimate[,4]))[4]
		IQ <- Q3 - Q1
		
		upper <- Q3 + IQ * cutoff
		lower <- Q1 - IQ * cutoff
		
		uppermax <- Q3 + IQ * cutoffmax
		lowermax <- Q1 - IQ * cutoffmax

		plotme <- median(as.numeric(pointestimate[,4]))
	}
	
	outlierdfupper <- array(NA,c(length(sort[,1]),4))
	outlierdflower <- array(NA,c(length(sort[,1]),4))
	nonoutliersdf <- array(NA,c(length(sort[,1]),4))
	#shitty forloop
	for(i in 1:length(sort[,1])) {
	
		if(nocut) {

			if(as.numeric(pointestimate[i,4]) > upper) {
				outlierdfupper[i,1:4] <- as.matrix(pointestimate[i,1:4])

			}
			if(as.numeric(pointestimate[i,4]) < lower) {
				outlierdflower[i,1:4] <- as.matrix(pointestimate[i,1:4])
			}
			if(as.numeric(pointestimate[i,4]) >= lower && as.numeric(pointestimate[i,4]) <= upper) {
				nonoutliersdf[i,1:4] <- as.matrix(pointestimate[i,1:4])
			}
		}
		if(!nocut) {
			if(as.numeric(pointestimate[i,4]) > upper && as.numeric(pointestimate[i,4]) < uppermax) {
				outlierdfupper[i,1:4] <- pointestimate[i,1:4]
			}
			if(as.numeric(pointestimate[i,4]) < lower && as.numeric(pointestimate[i,4]) > lowermax) {
				outlierdflower[i,1:4] <- pointestimate[i,1:4]
			}
			if(as.numeric(pointestimate[i,4]) >= lower && as.numeric(pointestimate[i,4]) <= upper || as.numeric(pointestimate[i,4]) <= lowermax && as.numeric(pointestimate[i,4]) >= uppermax) {
				nonoutliersdf[i,1:4] <- pointestimate[i,1:4]
			}
		}
	}
	colnames(nonoutliersdf) <- c("id", "Side", "Element", "Point Estimate")
	colnames(outlierdflower) <- c("id", "Side", "Element", "Point Estimate")
	colnames(outlierdfupper) <- c("id", "Side", "Element", "Point Estimate")
	
	if(all(is.na(nonoutliersdf))) {nonoutliersdf <- NULL}
	if(all(is.na(outlierdflower))) {outlierdflower <- NULL}
	if(all(is.na(outlierdfupper))) {outlierdfupper <- NULL}

	if(!is.null(upperfile)) {
		if(!all(is.null(outlierdfupper))) { #skips if all NA (no outliers)
			outlierdfupper <- na.omit(outlierdfupper)
			if(output_options[1] && nrow(outlierdfupper) > 0) {
				no <- OsteoSort:::output_function(hera1 = list(outlierdfupper, upperfile), method = "OS", type = "csv")
			}
		}
	}
	if(!is.null(lowerfile)) {
		if(!all(is.null(outlierdflower))) {
			outlierdflower <- na.omit(outlierdflower)
			if(output_options[1] && nrow(outlierdflower) > 0) {
				no <- OsteoSort:::output_function(hera1 = list(outlierdflower, lowerfile), method = "OS", type = "csv")
			}
		}
	}
	if(!is.null(nonoutliersfile)) {
		if(!all(is.null(nonoutliersdf))) {
			nonoutliersdf <- na.omit(nonoutliersdf)
			if(output_options[1] && nrow(nonoutliersdf) > 0) {
				no <- OsteoSort:::output_function(hera1 = list(nonoutliersdf, nonoutliersfile), method = "OS", type = "csv")
			}
		}
	}	
	if(output_options[2]) {
		no <- OsteoSort:::output_function(hera1 = list(as.numeric(pointestimate[,4]), bone, plotme, upper, lower, lowermax, uppermax, nocut), method = "OS", type = "plot")
	}
	
	setwd(workingdir)
	options(stringsAsFactors = TRUE) #restore default R  
     print("Outlier analysis completed")	
	return(list(direc,outlierdflower,outlierdfupper,nonoutliersdf,round(m, digits = 2),round(s, digits=2),round(me, digits=2),round(IQQ, digits=2)))
}