#' Articulation comparison Input Function
#'
#' @param bone Specifies the bone type
#' @param sort Data to be sorted
#'
#' @keywords art.input
#' @export
#' @examples
#' art.input()

art.input <- function (bones = NULL, side = NULL, ref = NULL, sort = NULL, measurementsa = NULL, measurementsb = NULL, threshold = 1) {
	print("Filtering data by element types, specified measurements, and threshold value...")
	options(stringsAsFactors = FALSE)
	options(as.is = TRUE)
	options(warn = -1)
	if(is.null(bones) || is.null(sort)){return(NULL)}

	side <- tolower(side)
	ref$Side <- tolower(ref$Side)
	ref$Element <- tolower(ref$Element)
	ref <- ref[ref$Element == bones,]
	ref <- ref[ref$Side == side,]
	ref <- cbind(ref[,c(1:3)], ref[c(measurementsa, measurementsb)])
	ref <- ref[order(ref$id),]
	n_ref <- data.frame()

	for(i in seq(from = 1, to = nrow(ref)-1, by = 2)) {
		if(ref[i,1] == ref[i+1,1]) {
			n_ref <- rbind(n_ref, ref[i,], ref[i+1,])
		}
	}
	refa <- n_ref[n_ref$Element == bones[1],]
	refb <- n_ref[n_ref$Element == bones[2],]
	refa <- cbind(refa[,c(1:3)], refa[measurementsa])
	refb <- cbind(refb[,c(1:3)], refb[measurementsb])

	sort$Side <- tolower(sort$Side)
	sort$Element <- tolower(sort$Element)
	sort <- sort[sort$Element == bones,]
	sort <- sort[sort$Side == side,]
	sort <- cbind(sort[,c(1:3)], sort[c(measurementsa, measurementsb)])
	sorta <- sort[sort$Element == bones[1],]
	sortb <- sort[sort$Element == bones[2],]
	sorta <- cbind(sorta[,c(1:3)], sorta[measurementsa])
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
	refa[is.na(refa)] <- 0
	refb[is.na(refb)] <- 0

	options(stringsAsFactors = TRUE) #restore default R  
	print("Finished...")
	return(list(refa, refb, sort_A, sort_B, rejected))
}