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

statsort <- function (sort, ref, method = "Quartiles", measurements = NULL, cutoff = 1.5, sessiontempdir = NULL, output_options = c(TRUE,TRUE)) {

sortg <<- sort
refg <<- ref
methodg <<- method
measurementsg <<- measurements
cutoffg <<- cutoff

	print("Outlier analysis started")
	options(stringsAsFactors = FALSE)
	upperfile = "upper.csv"
	lowerfile = "lower.csv"
	nonoutliersfile = "non-outliers.csv"
	cutoffmax <- cutoff[2]
	cutoff <- cutoff[1]
	nocut <- FALSE
	if(cutoffmax == cutoff) {nocut <- TRUE}

	workingdir = getwd()

	direc <- OsteoSort:::analytical_temp_space(output_options, sessiontempdir) #creates temporary space 

	sortdata <- array(NA,c(length(sort[,1]),4)) 
	sortdata[,1] <- as.matrix(sort[["id"]]) 
	sortdata[,2] <- tolower(sort[["Side"]]) 
	sortdata[,3] <- tolower(sort[["Element"]])
	sortdata[,4] <- sort[[paste(measurements)]]
	sort <- sortdata
	sort <- na.omit(sort)

	if(ref[1] == "Custom") {
		slope <- as.numeric(ref[2])
		intercept <- as.numeric(ref[3])
		pointestimate <- array(NA,c(length(sort[,1]),4))
		for(i in 1:length(sort[,1])) {
			pointestimate[i,1] <- sort[i,1] #id name
			pointestimate[i,2] <- sort[i,2] #Side name
			pointestimate[i,3] <- sort[i,3] #Bone name
			pointestimate[i,4] <- round( as.numeric(sort[i,4]) * slope + intercept, digits=2) #PE name #add *0.1 for trotter shit when testing
		}
	} else {
		refdata <- array(NA,c(length(ref[,1]),5)) 
		refdata[,1] <- as.matrix(ref[["id"]]) 
		refdata[,2] <- tolower(ref[["Side"]]) 
		refdata[,3] <- tolower(ref[["Stature"]])
		refdata[,4] <- tolower(ref[["Element"]])
		refdata[,5] <- ref[[paste(measurements)]]
		ref <- na.omit(refdata)

		sort_left <- sort[sort[,2] == "left",]
		sort_right <- sort[sort[,2] == "right",]
		ref_left <- ref[ref[,2] == "left",]
		ref_right <- ref[ref[,2] == "right",]

		if(nrow(sort_left) >= 1) {
			OLSL <- lm(X3 ~ X5, data = data.frame(ref_left))
		}
		if(nrow(sort_right) >= 1) {
			OLSR <- lm(X3 ~ X5, data = data.frame(ref_right))
		}

		pointestimate <- array(NA,c(length(sort[,1]),4))
		for(i in 1:nrow(sort)) {
			if(sort[i,2] == "left") {
				model <- OLSL
			}
			if(sort[i,2] == "right") {
				model <- OLSR
			}

			pointestimate[i,1] <- sort[i,1] #id name
			pointestimate[i,2] <- sort[i,2] #Side name
			pointestimate[i,3] <- sort[i,3] #Bone name
			pointestimate[i,4] <- round(predict(model, data.frame(X5 = sort[i,4])), digits=2)
		}
	}

	s <- sd(as.numeric(pointestimate[,4]))
	m <- mean(as.numeric(pointestimate[,4]))
	me <- median(as.numeric(pointestimate[,4]))
	IQQ <- quantile(as.numeric(pointestimate[,4]))[4] -  quantile(as.numeric(pointestimate[,4]))[2]
	
	if(method == "Standard_deviation") {
		standarddeviation <- sd(as.numeric(pointestimate[,4])) 
		meann <- mean(as.numeric(pointestimate[,4]))
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
		no <- OsteoSort:::output_function(hera1 = list(as.numeric(pointestimate[,4]), measurements, plotme, upper, lower, lowermax, uppermax, nocut), method = "OS", type = "plot")
	}
	
	setwd(workingdir)
	options(stringsAsFactors = TRUE) #restore default R  
	print("Outlier analysis completed")
	return(list(direc,outlierdflower,outlierdfupper,nonoutliersdf,round(m, digits = 2),round(s, digits=2),round(me, digits=2),round(IQQ, digits=2)))
}
