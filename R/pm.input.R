#' Pair-match Input Function
#' 
#' @param bone Specifies the bone type
#' @param sort Data to be sorted
#' @param measurement_standard Specifies the measurement standards to use ("standard" or "supplemental")
#' @param threshold Threshold value for number of measurements per comparison
#' @param measurements The measurement types to be used
#' 
#' @keywords pm.input
#' @export
#' @examples
#' pm.input()

pm.input <- function (bone = NULL, sort = NULL, measurement_standard = 'standard', threshold = 1, measurements = NULL) {	
	print("Import and reference generation has started.")
	options(warn = -1) #disables warnings
	options(as.is = TRUE)
	if(is.na(bone) || is.null(sort)) {return(NULL)} #input san
	if(is.na(bone) || is.null(sort)) {return(NULL)} #input san
	if(measurement_standard != 'standard' && measurement_standard != 'supplemental') {return(NULL)} #input san
	if(!is.numeric(threshold)) {return(NULL)} #input san

	if(measurement_standard == "standard") {
		filename_bone <- bone
		scapula <- c("Sca_01","Sca_02")
		clavicle <- c("Cla_01","Cla_04","Cla_05")
		humerus <- c("Hum_01","Hum_02","Hum_03","Hum_04","Hum_05")
		radius <- c("Rad_01","Rad_05","Rad_06")
		ulna <- c("Uln_01","Uln_04","Uln_05","Uln_06") #52 removed
		os_coxa <- c("Osc_01","Osc_02")
		femur <- c("Fem_01","Fem_02","Fem_03","Fem_04","Fem_06","Fem_05","Fem_07") #67 and 68 removed
		tibia <- c("Tib_01","Tib_02","Tib_03","Tib_04","Tib_05") #74 removed
		fibula <- c("Fib_01","Fib_02")
	}
	if(measurement_standard == "supplemental") {
		filename_bone <- paste(bone,"sup",sep="")
		scapula <- c("Sca_03","Sca_04","Sca_05")
		clavicle <- c("Cla_06","Cla_07","Cla_08","Cla_09")
		humerus <- c("Hum_06","Hum_07","Hum_08","Hum_09")
		radius <- c("Rad_07","Rad_08","Rad_09","Rad_04","Rad_10")
		ulna <- c("Uln_09","Uln_10","Uln_11")
		os_coxa <- c("Osc_14","Osc_15","Osc_16","Osc_05","Osc_17")
		femur <- c("Fem_14","Fem_15","Fem_16","Fem_18")
		tibia <- c("Tib_10","Tib_10","Tib_12")
		fibula <- c("Fib_03","Fib_04","Fib_05")
	}

	#lower case filter
	sort$Side <- tolower(sort$Side)
	sort$Element <- tolower(sort$Element)
	bone <- tolower(bone)#
	#sort by bone
	sort <- sort[sort$Element == bone,]

	if(is.null(measurements)) {
		measurements <- eval(as.symbol(bone))
		colnames(sort) <- c("id", "Side", "Element",measurements)
	}

	sortdata <- array(NA,c(nrow(sort),length(measurements)+3))
	sortdata[,1] <- as.character(sort$id)
	sortdata[,2] <- as.character(sort$Side)
	sortdata[,3] <- as.character(sort$Element)

	for(i in 1:length(measurements)) {
		sortdata[,i+3] <- as.character(sort[[measurements[i]]])
	}

	reftemp <- read.table(system.file("extdata", filename_bone, package = "OsteoSort"), header = TRUE, sep=",")
	refdata <- array(NA,c(nrow(reftemp),length(measurements)*2)) #times two for left and right but doesnt create one for id since it's not needed for reference
	x <- 1

	namedf <- array(NA,c(ncol(refdata), 1))
	for(i in seq(1,ncol(refdata),2)) {
		refdata[,i] <- reftemp[[measurements[x]]]
		refdata[,i+1] <- reftemp[[paste(measurements[x],"R",sep="")]]
	
		#used to generate column names	
		namedf[i] <- measurements[x]
		namedf[i+1] <- paste(measurements[x],"R",sep="")
		x <- x + 1
	}

	colnames(sortdata) <- c(c("A", "B", "C",measurements))
	colnames(refdata) <- namedf

	sortdata <- as.data.frame(sortdata, stringsAsFactors = FALSE)
	reff <- as.data.frame(refdata, stringsAsFactors = FALSE)

	#creates unique combination between left and right elements and reorders them 
	#so left and right measurements are next to each other similar to the reference data
	###########################################################
	n_varsleft <- length(unique(sortdata[sortdata$B == 'left',][,1]))
	n_varsright <- length(unique(sortdata[sortdata$B == 'right',][,1]))
	indices <- expand.grid(1:n_varsleft, 1:n_varsright)
	left <- sortdata[sortdata$B == 'left',]
	right <- sortdata[sortdata$B == 'right',]
	res <- cbind(left[indices[,1],], right[indices[,2],])
	res <- res[,order(names(res))]

	#Removes rows from sort if there is no corresponding match from left to right
	###########################################################
	c <- col(res)[is.na(res)]
	r <- row(res)[is.na(res)]

	if(length(c) != 0) {
		d <- ifelse(c %% 2 == 1, c + 1, c - 1)

		ids <- split(cbind(d, c), r)
		na.rows <- unique(sort(r))


		modified <- lapply(seq_along(na.rows), function(i) {
			res[na.rows[i], -(ids[[i]]), drop = F]
		})



		unmodified <- split(res[-na.rows, ], (1:nrow(res))[-na.rows]) 


		#combined 
		recombined <- list()
		recombined[na.rows] <- modified
		recombined[(1:nrow(res))[-na.rows]] <- unmodified
	}
	if(length(c) == 0) {
		recombined <- res
	}
	###########################################################

	###################treshold
	if(threshold == 1){threshold2 <- 1}
	if(threshold == 2){threshold2 <- 2}
	if(threshold == 3){threshold2 <- 4}
	if(threshold == 4){threshold2 <- 6}
	if(threshold == 5){threshold2 <- 8}
	if(threshold == 6){threshold2 <- 10}
	if(threshold == 7){threshold2 <- 12}
	if(threshold == 8){threshold2 <- 14}
	if(threshold == 9){threshold2 <- 16}
	if(threshold == 10){threshold2 <- 18}
	if(threshold == 11){threshold2 <- 20}
	###################treshold

	#fix for if full measurements are available convert to list of dataframes
	if (is.list(recombined[[1]]) == FALSE)
	{
		recombined <- split(recombined, 1:nrow(recombined))
	}
	recombined <- recombined[sapply(recombined, ncol) > (threshold2+6)] 

	#nottested <- recombined[sapply(recombined, ncol) < (threshold2+6)] 
	
	#returns reff being the reference and recombined being the test combinations
	###########################################################
	if(length(recombined) == 0) {recombined <- NA}
     print("Import and reference generation completed.")
	gc()

	return(list(recombined,reff))
}