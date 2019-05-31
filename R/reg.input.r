#' reg.match Input Function
#'
#' Function to produce combinations for associating elements with regression
#'
#' @param sort Data to be sorted
#' @param bone1 The first bone type
#' @param bone2 The second bone type
#' @param side1 The first bone type side
#' @param side2 The second bone type side
#' @param measurement_standard Specifies the measurement standards to use ("standard" or "supplemental")
#' @param threshold Threshold value for number of measurements per comparison
#' @param measurements1 The first bone types measurement types to be used
#' @param measurements2 The second bone types measurement types to be used
#'
#' @keywords reg.input
#' @export
#' @examples 
#' reg.input()

reg.input <- function(ref = NULL, sorta = NULL, sortb = NULL, bonea = NULL, boneb = NULL, sidea = NULL, sideb = NULL, threshold = 1, measurementsa = NULL, measurementsb = NULL) {
	print("Filtering data by element types, specified measurements, and threshold value...")
	options(stringsAsFactors = FALSE)
	options(as.is = TRUE)
	options(warn = -1)
	#if(is.null(bones) || is.null(sort)){return(NULL)}

	sidea <- tolower(sidea)
	sideb <- tolower(sideb)
	bonea <- tolower(bonea)
	boneb <- tolower(boneb)

	ref$Side <- tolower(ref$Side)
	ref$Element <- tolower(ref$Element)

	refa <- ref[ref$Element == bonea,]
	refa <- refa[refa$Side == sidea,]
	refa <- cbind(refa[,c(1:3)], refa[measurementsa])
	refa <- refa[order(refa$id),]

	refb <- ref[ref$Element == boneb,]
	refb <- refb[refb$Side == sideb,]
	refb <- cbind(refb[,c(1:3)], refb[measurementsb])
	refb <- refb[order(refb$id),]

	n_refa <- refa[refa$id %in% refb$id,]
	n_refb <- refb[refb$id %in% refa$id,]

	sorta$Side <- tolower(sorta$Side)
	sorta$Element <- tolower(sorta$Element)
	sortb$Side <- tolower(sortb$Side)
	sortb$Element <- tolower(sortb$Element)

	sorta <- sorta[sorta$Element == bonea,]
	sorta <- sorta[sorta$Side == sidea,]
	sorta <- cbind(sorta[,c(1:3)], sorta[measurementsa])

	sortb <- sortb[sortb$Element == boneb,]
	sortb <- sortb[sortb$Side == sideb,]
	sortb <- cbind(sortb[,c(1:3)], sortb[measurementsb])

	sort_A <- data.frame()
	sort_B <- data.frame()
	rejected <- data.frame()

	for(i in 1:nrow(sorta)) {
		if((length(measurementsa) - sum(is.na(sorta[i,c(4:length(measurementsa))]))) >= threshold) {
			sort_A <- rbind(sort_A, sorta[i,])
		}
		else {
			rejected <- rbind(rejected, sorta[i,])
		}
	}
	for(i in 1:nrow(sortb)) {
		if((length(measurementsb) - sum(is.na(sortb[i,c(4:length(measurementsb))]))) >= threshold) {
			sort_B <- rbind(sort_B, sortb[i,])
		}
		else {
			rejected <- rbind(rejected, sortb[i,])
		}
	}

	sort_A[is.na(sort_A)] <- 0
	sort_B[is.na(sort_B)] <- 0
	n_refa[is.na(n_refa)] <- 0
	n_refb[is.na(n_refb)] <- 0

	options(stringsAsFactors = TRUE) #restore default R  
	print("Finished...")
	return(list(n_refa, n_refb, sort_A, sort_B, rejected))
}