art.input <- function (bonea = NULL, boneb = NULL, side = NULL, ref = NULL, sorta = NULL, sortb = NULL, measurementsa = NULL, measurementsb = NULL, threshold = 1) {
	if(is.null(bonea) || is.null(boneb) || is.null(sort)){return(NULL)}

	side <- tolower(side)
	ref$Side <- tolower(ref$Side)
	ref$Element <- tolower(ref$Element)

	cnsb <- colnames(sortb)
	cb <- duplicated(c(measurementsb, cnsb), fromLast = TRUE)
	if(!any(cb)) {return(NULL)}
	measurementsb <- measurementsb[cb[1:length(measurementsb)]]
	cnsb <- colnames(sorta)
	cb <- duplicated(c(measurementsa, cnsb), fromLast = TRUE)
	if(!any(cb)) {return(NULL)}
	measurementsa <- measurementsa[cb[1:length(measurementsa)]]

	refa <- ref[ref$Element == bonea,]
	refb <- ref[ref$Element == boneb,]

	refa <- refa[refa$Side == side,]
	refb <- refb[refb$Side == side,]
	refa <- cbind(refa[,c(1:3)], refa[measurementsa])
	refb <- cbind(refb[,c(1:3)], refb[measurementsb])
	refa <- refa[order(refa$id),]
	refb <- refb[order(refb$id),]

	n_refa <- refa[refa$id %in% refb$id,]
	n_refb <- refb[refb$id %in% refa$id,]

	if(nrow(n_refa) == 0 || nrow(n_refb) == 0) {return(NULL)}

	sorta$Side <- tolower(sorta$Side)
	sorta$Element <- tolower(sorta$Element)
	sorta <- sorta[sorta$Element == bonea,]
	sorta <- sorta[sorta$Side == side,]
	sorta <- cbind(sorta[,c(1:3)], sorta[measurementsa])

	sortb$Side <- tolower(sortb$Side)
	sortb$Element <- tolower(sortb$Element)
	sortb <- sortb[sortb$Element == boneb,]
	sortb <- sortb[sortb$Side == side,]
	sortb <- cbind(sortb[,c(1:3)], sortb[measurementsb])

	if(nrow(sorta) == 0 || nrow(sortb) == 0) {return(NULL)}

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

	if(nrow(sorta) == 0 || nrow(sortb) == 0) {return(NULL)}

	sort_A[is.na(sort_A)] <- 0
	sort_B[is.na(sort_B)] <- 0
	n_refa[is.na(n_refa)] <- 0
	n_refb[is.na(n_refb)] <- 0

	return(list(n_refa, n_refb, sort_A, sort_B, rejected))
}
