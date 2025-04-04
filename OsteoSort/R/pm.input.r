pm.input <- function (bone = NULL, ref = NULL, sort = NULL, measurements = NULL, threshold = 1) {
	options(warn = -1) #disables warnings

	if(is.null(bone) || is.null(sort) || is.null(ref) || is.null(threshold) || is.null(measurements)) {
		return(NULL)
	}
	cnsb <- colnames(sort)
	cb <- duplicated(c(measurements, cnsb), fromLast = TRUE)
	if(!any(cb)) {return(NULL)}
	measurements <- measurements[cb[1:length(measurements)]]

	bone <- tolower(bone)
	#reference data sorting
	ref$Side <- tolower(ref$Side)
	ref$Element <- tolower(ref$Element)
	ref <- ref[ref$Element == bone,]
	ref <- cbind(ref[,c(1:3)], ref[measurements])
	#orders and sorts by duplicate entries (i.e. pair-matches)
	ref <- ref[order(ref$id),]
	refleft <- ref[ref$Side == "left",]
	refright <- ref[ref$Side == "right",]
	refleft <- refleft[refleft$id %in% refright$id,]
	refright <- refright[refright$id %in% refleft$id,]

	if(nrow(refleft) == 0 || nrow(refright) == 0) {return(NULL)}

	#case data sorting
	sort$Side <- tolower(sort$Side)
	sort$Element <- tolower(sort$Element)
	sort <- sort[sort$Element == bone,]
	sort <- cbind(sort[,c(1:3)], sort[measurements])
	sortleft_t <- sort[sort$Side == "left",]
	sortright_t <- sort[sort$Side == "right",]

	if(nrow(sortleft_t) == 0 || nrow(sortright_t) == 0) {return(NULL)}

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

	if(nrow(sortleft_t) == 0 || nrow(sortright_t) == 0) {return(NULL)}

	#replace NA with zero
	sortleft[is.na(sortleft)] <- 0
	sortright[is.na(sortright)] <- 0
	refleft[is.na(refleft)] <- 0
	refright[is.na(refright)] <- 0
	return(list(refleft, refright, sortleft, sortright, rejected))
}

