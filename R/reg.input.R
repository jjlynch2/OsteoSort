#' reg.match Input Function
#' Function to produce combinations for associating elements with regression
#' @param sort data to be sorted 
#' @keywords reg.input
#' @export
#' @examples 
#' reg.input()

reg.input <- function(sort = NULL, bone1 = "radius", side1 = "left", bone2 = "ulna", side2 = "right", template = 'standard', tresh = NULL, measurements1 = NULL, measurements2 = NULL) {
     print("Import and reference generation has started.")

	r1 <- FALSE
	r2 <- FALSE

	#disable warnings
	options(warn = -1)
	
	#check input
	if(length(bone2) > 1 || is.null(bone2) || is.null(bone1) || is.null(sort)) {return(NULL)}
	
	#sanitasion
	side1 <- tolower(side1)
	side2 <- tolower(side2)
	sort$Side <- tolower(sort$Side)
	sort$Element <- tolower(sort$Element)
	bone1 <- tolower(bone1)
	bone2 <- tolower(bone2)
	template <- tolower(template)
	
	#define file name for standard and supplemental
	if(template == "supplemental" || template == "single_supplemental") {
		boneref1 <- paste(bone1,"sup",sep="")
		boneref2 <- paste(bone2,"sup",sep="")
		
		scapula <- c("Sca_03","Sca_04","Sca_05")
		clavicle <- c("Cla_06","Cla_07","Cla_08","Cla_09")
		humerus <- c("Hum_06","Hum_07","Hum_08","Hum_09")
		radius <- c("Rad_07","Rad_08","Rad_09","Rad_04","Rad_10")
		ulna <- c("Uln_09","Uln_10","Uln_11")
		os_coxa <- c("Osc_14","Osc_15","Osc_16","Osc_05","Osc_17")
		femur <- c("Fem_14","Fem_15","Fem_16","Fem_18")
		tibia <- c("Tib_10","Tib_11","Tib_12")
		fibula <- c("Fib_03","Fib_04","Fib_05")
	}
	if(template == "standard" || template == "single_standard") {
		boneref1 <- bone1
		boneref2 <- bone2
		
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
	
	#reduces amount of memory usage during sorting
	sort1T <- sort[sort$Element == bone1,]	
	sort2T <- sort[sort$Element == bone2,]
	sort1T <- sort1T[sort1T$Side == side1,]
	sort2T <- sort2T[sort2T$Side == side2,]
	
	#cuts down sort to only measurements from bone
	sort1 <- data.frame(NA)
	sort2 <- data.frame(NA)
	
	if(is.null(measurements1) || is.null(measurements2)) {
		measurements <- c("ID","Side","Element",eval(as.symbol(bone1)))
		measurements2 <- c("ID","Side","Element",eval(as.symbol(bone2)))
	}
	if(!is.null(measurements1)) {
		measurements <- c("ID", "Side", "Element", measurements1)
		measurements2 <- c("ID", "Side", "Element", measurements2)
	}
	
	for(i in measurements) {
		sort1 <- cbind(sort1, sort1T[[i]])
	}
	for(i in measurements2) {
		sort2 <- cbind(sort2, sort2T[[i]])
	}
	
	
	
	
	sort1 <- sort1[,-1]
	sort2 <- sort2[,-1]
	
	
	sort1 <- as.data.frame(sort1[rowSums(!is.na(sort1[-1][-1][-1])) > 0,])
	sort2 <- as.data.frame(sort2[rowSums(!is.na(sort2[-1][-1][-1])) > 0,])
	
	#measurement names
	colnames(sort1) <- measurements
	colnames(sort2) <- measurements2


	bone1temp <- ncol(sort1[,-1][,-1][,-1]) #number of measurements   
	bone2temp <- ncol(sort2[,-1][,-1][,-1]) #number of measurements     


	#creates correct column numbers for reference based on side
	if(side1 == 'left') {
		hcol <- seq(2, bone1temp*2, by=2)
	}
	if(side1 == 'right') {
		hcol <- seq(3, bone1temp*2+1, by=2)
		r1 <- TRUE
	}
	if(side2 == 'left') {
		ucol <- seq(2, bone2temp*2, by=2)
	}
	if(side2 == 'right') {
		ucol <- seq(3, bone2temp*2+1, by=2)
		r2 <- TRUE
	}

	ref1 <- read.table(system.file("extdata", boneref1, package = "osteosort"), header = TRUE, sep=",")[,c(1,hcol)]
	ref2 <- read.table(system.file("extdata", boneref2, package = "osteosort"), header = TRUE, sep=",")[,c(1,ucol)]

	#removes R after measurement name from the reference dataset
	if(r1) {
		colnames(ref1) <- c("ID",substr(names(ref1[-1]), 1, nchar(names(ref1[-1]))-1))
	}
	if(r2) {
		colnames(ref2) <- c("ID",substr(names(ref2[-1]), 1, nchar(names(ref2[-1]))-1))
	}
	
	#creates reference data for the same ID across two bone types
	#this works since we are limited to two bones and I have full measurements in data. Not 
	#limited by missing measurements...
	ref1[,1] <- as.character(ref1[,1])
	ref2[,1] <- as.character(ref2[,1])
	test <- array()
	for(i in 1:nrow(ref1)) {
		for(a in 1:nrow(ref2)) {
			if(ref1[i,1] == ref2[a,1]) {
				temp <- cbind(ref1[i,], ref2[a,])
				test <- rbind(test,temp)
			}
		}
	}
	ref <- test[-1,]
	
	
	###################treshold
	if(tresh == 1){tresh2 <- 4}
	if(tresh == 2){tresh2 <- 4}
	if(tresh == 3){tresh2 <- 5}
	if(tresh == 4){tresh2 <- 6}
	if(tresh == 5){tresh2 <- 7}
	if(tresh == 6){tresh2 <- 8}
	if(tresh == 7){tresh2 <- 9}
	if(tresh == 8){tresh2 <- 10}
	if(tresh == 9){tresh2 <- 11}
	if(tresh == 10){tresh2 <- 12}
	if(tresh == 11){tresh2 <- 13}
	###################treshold
	
	
	sort1 <- sort1[rowSums(!is.na(sort1)) > tresh2,] #threshold	
	sort2 <- sort2[rowSums(!is.na(sort2)) > tresh2,] #threshold	
	
	#creates combinations of sort data
	nvars1 <- length(unique(sort1[,1]))
	nvars2 <- length(unique(sort2[,1]))
	indices <- expand.grid(1:nvars1, 1:nvars2)

	res <- cbind(sort1[indices[,1],], sort2[indices[,2],]) #Alternative approach combining both into a single data.frame


	
     print("Import and reference generation completed.")
	return(list(res, ref, splitn = c(bone1temp+3, bone2temp+3+bone1temp+3)))
}