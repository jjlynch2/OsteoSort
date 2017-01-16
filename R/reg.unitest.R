#' reg.test Input Function
#' Function to produce combinations for associating elements with regression
#' @param sort data to be sorted 
#' @keywords reg.test
#' @export
#' @examples 
#' reg.unitest()

reg.unitest <- function(sort1 = NULL, sort2 = NULL, ref = NULL, predlevel = 0.90, stdout = FALSE, sessiontempdir = NULL) {
	options(warn = -1)
	workingdir = getwd()
	if(!stdout) { 
		if (!is.null(sessiontempdir)) {
			setwd(sessiontempdir)
		}
		direc <- randomstring(n = 1, length = 12)
		dir.create(direc)
		setwd(direc)
	}

	sort1info <- sort1[1:3]
	sort2info <- sort2[1:3]
	sort1c <- length(sort1[colSums(!is.na(sort1)) > 0]) - 3
	sort2c <- length(sort2[colSums(!is.na(sort2)) > 0]) - 3
	sort1file <- paste(sort1info[1], "-", sort1info[1])
	sort1 <- log(sum(as.numeric(sort1[colSums(!is.na(sort1)) > 0][-1][-1][-1])))
	sort2 <- log(sum(as.numeric(sort2[colSums(!is.na(sort2)) > 0][-1][-1][-1])))

	#splits reference by ID
	for(i in 1:ncol(ref)) {

		if(!is.numeric(ref[1,i+1])) {

			ref1 <- ref[-1][1:i-1]
			ref2 <- ref[-1][i:ncol(ref[-1])][-1]
			break
		}
	}

	reff <- array(0, c(nrow(ref1),2))
	ref1 <- as.data.frame(ref1)
	ref2 <- as.data.frame(ref2)
	
	for(i in 1:nrow(ref1)) {
		reff[i,1] <- log(sum(reff[i,1], as.numeric(ref1[i,1:length(ref1)])))
		reff[i,2] <- log(sum(reff[i,2], as.numeric(ref2[i,1:length(ref2)])))
	}
	
	################regression model################
	lm1 <- lm(V1~V2, data = as.data.frame(reff))
	rsqr <- summary(lm1)$r.squared
	################regression model################
	
	################pvalue################
	f <- summary(lm1)$fstatistic
	################pvalue################

	
	################prediction interval testing################
	pm1 <- predict(lm1, data.frame(V2 = sort2), interval="predict", level = predlevel)



	if(sort1 <= pm1[3] && sort1 >= pm1[2]) {
		within <- "Cannot Exclude"
	}
	else	within <- "Excluded"
	################prediction interval testing################
	
	################plotting################
	lmp1 <- predict(lm1, interval="predict", level = predlevel)
	jpeg(paste(sort1file,".jpeg",sep=''),height = 800, width = 800)
	dev.control('enable')	
	plot(reff[,1],reff[,2], xlab = sort1info[3], ylab = sort2info[3])
	points(sort1,sort2,col="blue",pch=16)
	matlines(lmp1[,1], reff[,2], col=c("red"))
	matlines(lmp1[,2], reff[,2], col=c("blue"), lty = 4)
	matlines(lmp1[,3], reff[,2], col=c("blue"), lty = 4)
	p1 <- recordPlot()
	dev.off()
	
	if(stdout) { p1 }
	################plotting################
	if(stdout) {
		print(paste("R^2: ", rsqr))
		print(paste("Prediction Interval Level: ", predlevel))
		print(paste("p-value: ", pf(f[1],f[2],f[3],lower.tail = F)))
	}
	pvalue <- pf(f[1],f[2],f[3],lower.tail = F)



	if(!stdout) {
		sink(sort1file, append = TRUE, split = FALSE)
		cat('Number of measurements: ')
		cat(sort1info[1])
		cat(': ',sort1c)
		cat('\n',sort2info[1])
		cat(': ',sort2c)
		cat('\nSample size: ')
		cat(nrow(reff))
		cat('\nElement: ')
		cat(sort1info[3])
		cat('  Side: ')
		cat(sort1info[2])
		cat('\nElement: ')
		cat(sort2info[3])
		cat('  Side: ')
		cat(sort2info[2])
		cat('\np value: ')
		cat(pvalue)
		cat('\nMatch ', within)
		cat('\n____________________________________________________________\n')
		cat('\nDate: ', strftime(Sys.time(), "%Y-%m-%d %H:%M:%S"), 'Analyst___________', ' Initials___________') 
		cat('\nFor Official Use Only') 
		sink()
	}

	if(within == 'Excluded') {
  		df5 <- c(sort1info[1], sort2info[1], rsqr, pvalue, paste(sort1info[1],':',sort1c,sep=''),paste(sort2info[1],':',sort2c,sep=''), nrow(reff))
  		df5 <- as.data.frame(as.matrix(t(unname(df5))))
  		colnames(df5) <- c("ID","ID","R-squared","p-value","# measurements","# measurements","sample size")
  		if(!stdout) {
		write.csv(df5, file = "non-match-list.csv")
		}
	}
	
	if(within == 'Cannot Exclude') {
  		df5 <- c(sort1info[1], sort2info[1], rsqr, pvalue, paste(sort1info[1],':',sort1c,sep=''),paste(sort2info[1],':',sort2c,sep=''), nrow(reff))
  		df5 <- as.data.frame(as.matrix(t(unname(df5))))
  		colnames(df5) <- c("ID","ID","R-squared","p-value","# measurements","# measurements","sample size")
  		if(!stdout) {
		write.csv(df5, file = "match-list.csv")
		}
	}

	setwd(workingdir)
	return(list(direc,df5,p1))
	
}