#' Pair-match Input Function
#' 
#' This function takes input from the user to be pair-matched and generates
#' a reference database for each possible combination based on the
#' number of measurements available. It's designed to work as a single user input or
#' with the multiple comparison standardized csv format. It accounts for NA values automatically.
#' 
#' @param bone The name of the bone you wish to pairmatch. bone = "humerus".
#' @param sort The data you wish to pair-match. Example: sort <- rbind(c(ID <- "X1", Side <- "Left", Element <- "Humerus", v42 <- 40), c(ID <- "X1", Side <- "Left", Element <- "Humerus", v42 <- 40))
#' @param template Defines which measurement standard. Either standard or supplemental.
#' @param tresh Sets the threshold value for the minimum number of measurements required for a combination.
#' 
#' @keywords pm.input
#' @export
#' @examples
#' pm.input()

pm.input <- function (bone = NULL, sort = NULL, template = 'standard', tresh = 1, measurements = NULL) {
	suppressMessages(library(data.table))
 	print("Import and reference generation has started.")
	options(warn = -1) #disables warnings
	options(as.is = TRUE)
	if(is.na(bone) || is.null(sort)) {return(NULL)} #input san
	if(is.na(bone) || is.null(sort)) {return(NULL)} #input san
	if(template != 'standard' && template != 'supplemental') {return(NULL)} #input san
	if(!is.numeric(tresh)) {return(NULL)} #input san

	if(template == "standard") {
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
	if(template == "supplemental") {
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
		colnames(sort) <- c("ID", "Side", "Element",measurements)
	}

	sortdata <- array(NA,c(nrow(sort),length(measurements)+3))
	sortdata[,1] <- as.character(sort$ID)
	sortdata[,2] <- as.character(sort$Side)
	sortdata[,3] <- as.character(sort$Element)

	for(i in 1:length(measurements)) {
		sortdata[,i+3] <- as.character(sort[[measurements[i]]])
	}


	reftemp <- read.table(system.file("extdata", filename_bone, package = "osteosort"), header = TRUE, sep=",")
	refdata <- array(NA,c(nrow(reftemp),length(measurements)*2)) #times two for left and right but doesnt create one for ID since it's not needed for reference
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

	sortdata <- data.table(sortdata) 
	reff <- data.table(refdata)
	

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
	if(tresh == 1){tresh2 <- 1}
	if(tresh == 2){tresh2 <- 2}
	if(tresh == 3){tresh2 <- 4}
	if(tresh == 4){tresh2 <- 6}
	if(tresh == 5){tresh2 <- 8}
	if(tresh == 6){tresh2 <- 10}
	if(tresh == 7){tresh2 <- 12}
	if(tresh == 8){tresh2 <- 14}
	if(tresh == 9){tresh2 <- 16}
	if(tresh == 10){tresh2 <- 18}
	if(tresh == 11){tresh2 <- 20}
	###################treshold

	#fix for if full measurements are available convert to list of dataframes
	if (is.list(recombined[[1]]) == FALSE)
	{
		recombined <- split(recombined, 1:nrow(recombined))
	}
	recombined <- recombined[sapply(recombined, ncol) > (tresh2+6)] 

	#nottested <- recombined[sapply(recombined, ncol) < (tresh2+6)] 
	
	#returns reff being the reference and recombined being the test combinations
	###########################################################
	if(length(recombined) == 0) {recombined <- NA}
     print("Import and reference generation completed.")
	gc()

	return(list(recombined,reff))
}