metricsort <- function (sort, method = "Quartiles", measurements = NULL, sessiontempdir = NULL, cutoff = c(1.5,2), output_options = c(TRUE, TRUE)) {
	print("Outlier analysis started")
	upperfile = "upper.csv"
	lowerfile = "lower.csv"
	nonoutliersfile = "non-outliers.csv"
	cutoffmax <- cutoff[2]
	cutoff <- cutoff[1]
	nocut <- FALSE
	if(cutoffmax == cutoff) {nocut <- TRUE}

	direc <- OsteoSort:::analytical_temp_space(output_options, sessiontempdir) #creates temporary space 
	sd <- paste(sessiontempdir, direc, sep="/")

	sortdata <- array(NA,c(length(sort[,1]),4))
	sortdata[,1] <- as.matrix(sort[["id"]])
	sortdata[,2] <- tolower(sort[["Side"]])
	sortdata[,3] <- tolower(sort[["Element"]])
	sortdata[,4] <- sort[[paste(measurements)]] 
	sort <- sortdata
	sort <- na.omit(sort)
	s <- sd(as.numeric(sort[,4]))
	m <- mean(as.numeric(sort[,4]))
	me <- median(as.numeric(sort[,4]))
	IQQ <- quantile(as.numeric(sort[,4]))[4] -  quantile(as.numeric(sort[,4]))[2]
		
	if(method == "Standard_deviation") {
		#two standard deviations and mean
		standarddeviation <- sd(as.numeric(sort[,4]))
		meann <- mean(as.numeric(sort[,4]))
		upper <- meann + standarddeviation * cutoff
		lower <- meann - standarddeviation * cutoff
		uppermax <- meann + standarddeviation * cutoffmax
		lowermax <- meann - standarddeviation * cutoffmax
		plotme <- mean(as.numeric(sort[,4]))
	}
	if(method == "Quartiles") {
		Q1 <- quantile(as.numeric(sort[,4]))[2]
		Q3 <- quantile(as.numeric(sort[,4]))[4]
		IQ <- Q3 - Q1
		upper <- Q3 + IQ * cutoff
		lower <- Q1 - IQ * cutoff
		uppermax <- Q3 + IQ * cutoffmax
		lowermax <- Q1 - IQ * cutoffmax	
		plotme <- median(as.numeric(sort[,4]))
	}
	
	outlierdfupper <- array(NA,c(length(sort[,1]),4))
	outlierdflower <- array(NA,c(length(sort[,1]),4))
	nonoutliersdf <- array(NA,c(length(sort[,1]),4))

	for(i in 1:length(sort[,1])) {
		if(nocut) {
			if(as.numeric(sort[i,4]) > upper) {
				outlierdfupper[i,1:4] <- as.matrix(sort[i,1:4])
			}
			if(as.numeric(sort[i,4]) < lower) {
				outlierdflower[i,1:4] <- as.matrix(sort[i,1:4])
			}
			if(as.numeric(sort[i,4]) >= lower && as.numeric(sort[i,4]) <= upper) {
				nonoutliersdf[i,1:4] <- as.matrix(sort[i,1:4])
			}
		}
		if(!nocut) {
			if(as.numeric(sort[i,4]) > upper && as.numeric(sort[i,4]) < uppermax) {
				outlierdfupper[i,1:4] <- as.matrix(sort[i,1:4])
			}
			if(as.numeric(sort[i,4]) < lower && as.numeric(sort[i,4]) > lowermax) {
				outlierdflower[i,1:4] <- as.matrix(sort[i,1:4])
			}
			if(as.numeric(sort[i,4]) >= lower && as.numeric(sort[i,4]) <= upper || as.numeric(sort[i,4]) <= lowermax && as.numeric(sort[i,4]) >= uppermax) {
				nonoutliersdf[i,1:4] <- as.matrix(sort[i,1:4])
			}
		}
	}

	colnames(nonoutliersdf) <- c("id", "Side", "Element", "measurements")
	colnames(outlierdflower) <- c("id", "Side", "Element", "measurements")
	colnames(outlierdfupper) <- c("id", "Side", "Element", "measurements")
	
	if(all(is.na(nonoutliersdf))) {nonoutliersdf <- NULL}
	if(all(is.na(outlierdflower))) {outlierdflower <- NULL}
	if(all(is.na(outlierdfupper))) {outlierdfupper <- NULL}

	if(!is.null(upperfile)) {
		if(!all(is.null(outlierdfupper))) { #skips if all NA (no outliers)
			outlierdfupper <- na.omit(outlierdfupper)
			if(output_options[1] && nrow(outlierdfupper) > 0) {
				no <- OsteoSort:::output_function(hera1 = list(outlierdfupper, upperfile), method = "OS", type = "csv", fpath=sd)
			}
		}
	}
	if(!is.null(lowerfile)) {
		if(!all(is.null(outlierdflower))) {
			outlierdflower <- na.omit(outlierdflower)
			if(output_options[1] && nrow(outlierdflower) > 0) {
				no <- OsteoSort:::output_function(hera1 = list(outlierdflower, lowerfile), method = "OS", type = "csv", fpath=sd)
			}
		}
	}
	if(!is.null(nonoutliersfile)) {
		if(!all(is.null(nonoutliersdf))) {
			nonoutliersdf <- na.omit(nonoutliersdf)
			if(output_options[1] && nrow(nonoutliersdf) > 0) {
				no <- OsteoSort:::output_function(hera1 = list(nonoutliersdf, nonoutliersfile), method = "OS", type = "csv", fpath=sd)
			}
		}
	}
	if(output_options[2]) {
		no <- OsteoSort:::output_function(hera1 = list(sort[,4], measurements, plotme, upper, lower, lowermax, uppermax, nocut), method = "OS", type = "plot", fpath=sd)
	}
	
	print("Outlier analysis completed")	
	return(list(direc,outlierdflower,outlierdfupper,nonoutliersdf,round(m, digits=2),round(s, digits=2) ,round(me, digits=2),round(IQQ, digits=2)))
}
