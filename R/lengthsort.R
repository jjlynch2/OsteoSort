#' A function to sort by bone length
#' 
#' 
#' @param file test
#' @param population test
#' @keywords random
#' @export
#' @examples
#' lengthsort()

lengthsort <- function (file, bone = "femur", side = "both", upperfile = "upper.csv", lowerfile = "lower.csv", nonoutliersfile = "non-outliers.csv", method = "Quartiles", measurement = NULL, sessiontempdir = NULL, a = FALSE, stdout = FALSE, cutoff = 1.5) {	
	
	cutoffmax <- cutoff[2]
	cutoff <- cutoff[1]
	
	nocut <- FALSE
	if(cutoffmax == cutoff) {nocut <- TRUE}
	
	side <- tolower(side)
	
	workingdir = getwd()
	if(!stdout) {
		if(!a) {
			if(!is.null(sessiontempdir)) {
				setwd(sessiontempdir)
			}
			direc <- randomstring(n = 1, length = 12)
			dir.create(direc)
			setwd(direc)
		}
		if(a) {
			setwd(sessiontempdir)
			direc <- NULL
		}
	}
	
	sortdata <- array(NA,c(length(file[,1]),4))

	sortdata[,1] <- as.matrix(file[["ID"]])
	sortdata[,2] <- tolower(file[["Side"]])
	sortdata[,3] <- tolower(file[["Element"]])
	sortdata[,4] <- file[[paste(measurement)]] 
	file <- sortdata

	file <- file[file[,3] == bone,]

	if(side == "left") {
		file <- file[file[,2] == "left",]
	}
	if(side == "right") {
		file <- file[file[,2] == "right",]
	}

	file <- na.omit(file)



		s <- sd(as.numeric(file[,4]))
		m <- mean(as.numeric(file[,4]))
		me <- mean(as.numeric(file[,4]))
		IQQ <- quantile(as.numeric(file[,4]))[4] -  quantile(as.numeric(file[,4]))[2]
		
	if(method == "Standard_deviation") {
		#two standard deviations and mean
		standarddeviation <- sd(as.numeric(file[,4]))
		meann <- mean(as.numeric(file[,4]))
		upper <- meann + standarddeviation * cutoff
		lower <- meann - standarddeviation * cutoff
		uppermax <- meann + standarddeviation * cutoffmax
		lowermax <- meann - standarddeviation * cutoffmax
		plotme <- mean(as.numeric(file[,4]))
	}
	if(method == "Quartiles") {
		Q1 <- quantile(as.numeric(file[,4]))[2]
		Q3 <- quantile(as.numeric(file[,4]))[4]
		IQ <- Q3 - Q1
		
		upper <- Q3 + IQ * cutoff
		lower <- Q1 - IQ * cutoff
		
		uppermax <- Q3 + IQ * cutoffmax
		lowermax <- Q1 - IQ * cutoffmax	
			
		plotme <- median(as.numeric(file[,4]))
	}
	
	
	outlierdfupper <- array(NA,c(length(file[,1]),4))
	outlierdflower <- array(NA,c(length(file[,1]),4))
	nonoutliersdf <- array(NA,c(length(file[,1]),4))
	#shitty forloop
	for(i in 1:length(file[,1])) {
	
		if(nocut) {
			if(as.numeric(file[i,4]) > upper) {
				outlierdfupper[i,1:4] <- as.matrix(file[i,1:4])
			}
			if(as.numeric(file[i,4]) < lower) {
				outlierdflower[i,1:4] <- as.matrix(file[i,1:4])
			}
			if(as.numeric(file[i,4]) >= lower && as.numeric(file[i,4]) <= upper) {
				nonoutliersdf[i,1:4] <- as.matrix(file[i,1:4])
			}
		}
		if(!nocut) {
			if(as.numeric(file[i,4]) > upper && as.numeric(file[i,4]) < uppermax) {
				outlierdfupper[i,1:4] <- as.matrix(file[i,1:4])
			}
			if(as.numeric(file[i,4]) < lower && as.numeric(file[i,4]) > lowermax) {
				outlierdflower[i,1:4] <- as.matrix(file[i,1:4])
			}
			if(as.numeric(file[i,4]) >= lower && as.numeric(file[i,4]) <= upper || as.numeric(file[i,4]) <= lowermax && as.numeric(file[i,4]) >= uppermax) {
				nonoutliersdf[i,1:4] <- as.matrix(file[i,1:4])
			}
		}
	}


	colnames(nonoutliersdf) <- c("ID", "Side", "Element", "Measurement")
	colnames(outlierdflower) <- c("ID", "Side", "Element", "Measurement")
	colnames(outlierdfupper) <- c("ID", "Side", "Element", "Measurement")


	
	if(all(is.na(nonoutliersdf))) {nonoutliersdf <- NULL}
	if(all(is.na(outlierdflower))) {outlierdflower <- NULL}
	if(all(is.na(outlierdfupper))) {outlierdfupper <- NULL}

	

	if(!is.null(upperfile)) {
		if(!all(is.null(outlierdfupper))) { #skips if all NA (no outliers)
			outlierdfupper <- na.omit(outlierdfupper)
			write.csv(outlierdfupper, file = upperfile)
		}
	}
	if(!is.null(lowerfile)) {
		if(!all(is.null(outlierdflower))) {
			outlierdflower <- na.omit(outlierdflower)
			write.csv(outlierdflower, file = lowerfile, row.names = FALSE)
		}
	}
	if(!is.null(nonoutliersfile)) {
		if(!all(is.null(nonoutliersdf))) {
			nonoutliersdf <- na.omit(nonoutliersdf)
			write.csv(nonoutliersdf, file = nonoutliersfile, row.names = FALSE)
		}
	}
	
	

	
	################plotting################
	jpeg(paste("graph",".jpeg",sep=''),height = 800, width = 800)
	dev.control('enable')	
	hist(x = as.numeric(file[,4]), xlab = bone, main = NULL)
	abline(v = plotme, lty = 2, col="darkred")
	abline(v = upper, lty = 2, col="darkblue")
	abline(v = lower, lty = 2, col="darkblue")
	if(!nocut) {
		abline(v = lowermax, lty = 2, col="black")
		abline(v = uppermax, lty = 2, col="black")
	}
	
	p1 <- recordPlot()
	dev.off()
	
	if(stdout) {p1}
	
	setwd(workingdir)
	return(list(direc,outlierdflower,outlierdfupper,nonoutliersdf,p1,m,s,me,IQQ))
}