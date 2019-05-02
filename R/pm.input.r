#' Pair-match Input Function
#' 
#' @param bone Specifies the bone type
#' @param sort Data to be sorted
#' @param ref Reference data to be sorted
#' @param threshold Threshold value for number of measurements per comparison
#' @param measurements The measurement types to be used
#' 
#' @keywords pm.input
#' @export
#' @examples
#' pm.input()

pm.input <- function (bone = NULL, ref = NULL, sort = NULL, measurements = NULL, threshold = 1) {	
	print("Filtering data by element type, specified measurements, and threshold value...")
	options(stringsAsFactors = FALSE)
	options(warn = -1) #disables warnings

	if(is.null(bone) || is.null(sort) || is.null(ref) || is.null(threshold) || is.null(measurements)) {
		return(NULL)
	}

	bone <- tolower(bone)

	#reference data sorting
	ref$Side <- tolower(ref$Side)
	ref$Element <- tolower(ref$Element)
	ref <- ref[ref$Element == bone,]
	ref <- cbind(ref[,c(1:3)], ref[measurements])
	#orders and sorts by duplicate entries (i.e. pair-matches)
	ref <- ref[order(ref$id),]
	n_ref <- data.frame()
	for(i in seq(from = 1, to = nrow(ref)-1, by = 2)) {
		print(i)
		if(ref[i,1] == ref[i+1,1] && ref[i,2] == "left" && ref[i+1,2] == "right" || ref[i,1] == ref[i+1,1] && ref[i,2] == "right" && ref[i+1,2] == "left") {
			n_ref <- rbind(n_ref, ref[i,], ref[i+1,])
		}
	}
	refleft <- n_ref[n_ref$Side == "left",]
	refright <- n_ref[n_ref$Side == "right",]

	#case data sorting
	sort$Side <- tolower(sort$Side)
	sort$Element <- tolower(sort$Element)
	sort <- sort[sort$Element == bone,]
	sort <- cbind(sort[,c(1:3)], sort[measurements])
	sortleft_t <- sort[sort$Side == "left",]
	sortright_t <- sort[sort$Side == "right",]

	sortleft <- data.frame()
	sortright <- data.frame()
	rejected <- data.frame()

	#sorting by threshold and saving rejected elements
	for(i in 1:nrow(sortleft_t)) {
		if((length(measurements) - sum(is.na(sortleft_t[i,c(4:length(measurements))]))) >= threshold) {
			sortleft <- rbind(sortleft, sortleft_t[i,])
		}
		else {
			rejected <- rbind(rejected, sortleft_t[i,])
		}
	}
	for(i in 1:nrow(sortright_t)) {
		if((length(measurements) - sum(is.na(sortright_t[i,c(4:length(measurements))]))) >= threshold) {
			sortright <- rbind(sortright, sortright_t[i,])
		}
		else {
			rejected <- rbind(rejected, sortright_t[i,])
		}
	}

	#replace NA with zero
	sortleft[is.na(sortleft)] <- 0
	sortright[is.na(sortright)] <- 0
	refleft[is.na(refleft)] <- 0
	refright[is.na(refright)] <- 0

	options(stringsAsFactors = TRUE) #restore default R  
     print("Finished...")
	return(list(refleft, refright, sortleft, sortright, rejected))
}

