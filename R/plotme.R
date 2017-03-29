#' A function to generate plots for single shiny interface
#' 
#'
#' @param file test
#' @param population teast
#' @keywords random
#' @export
#' @examples
#' plotme()

plotme <- function (refdata = NULL, sortdata = NULL, power = TRUE, absolutevalue = TRUE, ttype = "pm", splitn = NULL, predlevel = 0.90, testtype = TRUE) {   
	library(CCA)
	if(power) {p1 <- 0.00005; p2 <- 0.33} #half normal transformation
	if(!power) {p1 <- 0; p2 <- 1} #used to prevent writing new code inside loop. This transformation doesn't change the data

     if(ttype == "pm") {
		X <- sortdata[[1]]
		temp1 <- names(X[-c(1:6)][c(T,F)])
		temp1 <- sort(c(temp1, paste(temp1,"R",sep="")))
		y <- refdata[temp1]
		if(absolutevalue) { 
			difa <- (( rowSums(abs((y[c(T,F)] - y[c(F,T)]))) + p1 ) ** p2)
			difa1 <- ((sum(abs(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)])) + p1) ** p2)
		}
		if(!absolutevalue) {
			difa <- rowSums(y[c(T,F)] - y[c(F,T)])
			difa1 <-sum(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)])
		}
		
		jpeg(paste("graph",".jpeg",sep=''),height = 800, width = 800)
		dev.control('enable')	
		hist(x = difa, xlab = "", main = NULL)
		abline(v = difa1, lty = 2, col="darkred")
		plotted <- recordPlot()
		dev.off()
	}
	
	if(ttype == "art") {
		y <- sortdata[[1]]
		if(absolutevalue) { 
			difa <- (( abs(refdata[,1] - refdata[,2]) + p1 ) ** p2)
			difa1 <- ((abs(as.numeric(y[7]) - as.numeric(y[8])) + p1) ** p2)
		}
		if(!absolutevalue) {
			difa <- refdata[,1] - refdata[,2]
			difa1 <- as.numeric(y[7]) - as.numeric(y[8])
		}
		
		jpeg(paste("graph",".jpeg",sep=''),height = 800, width = 800)
		dev.control('enable')	
		hist(x = difa, xlab = "", main = NULL)
		abline(v = difa1, lty = 2, lwd = 2, col="darkred")
		plotted <- recordPlot()
		dev.off()
	}
	if(ttype == "reg" && testtype == FALSE) {
		x <- sortdata
		ref <- refdata

		temp1 <- x[seq(from = splitn[1]+1, to = splitn[2])]
		temp1 <- temp1[ , colSums(is.na(temp1)) == 0]
		temp1n <- names(temp1[-1][-1][-1]) #captures measurement names
		temp2 <- x[seq(from = 1, to = splitn[1])]
		temp2 <- temp2[ , colSums(is.na(temp2)) == 0]
		temp2n <- names(temp2[-1][-1][-1]) #captures measurement names

		t1 <- rowSums(ref[temp1n])# reference
		t2 <- rowSums(ref[temp2n])
		
		model1 <- lm(t2~t1) #linear model
		
		rsqr1 <- summary(model1)$r.squared

		temp2p <- rowSums(data.frame(t(as.numeric(as.matrix(temp2[-1][-1][-1])))))
		temp1p <- rowSums(data.frame(t(as.numeric(as.matrix(temp1[-1][-1][-1])))))
		
	
	lmp1 <- predict(model1, interval="prediction", level = predlevel)
	jpeg(paste("graph",".jpeg",sep=''),height = 800, width = 800)
	dev.control('enable')
		
	plot(t2,t1, xlab = "", ylab = "")
	
	points(temp2p,temp1p,col="blue",pch=16)
	
	matlines(lmp1[,1], t1, col=c("red"))
	matlines(lmp1[,2], t1, col=c("blue"), lty = 4)
	matlines(lmp1[,3], t1, col=c("blue"), lty = 4)
	
	plotted <- recordPlot()
	dev.off()

	}
	if(ttype == "reg" && testtype == TRUE) {
		x <- sortdata
		ref <- refdata
	
		temp1 <- x[seq(from = splitn[1]+1, to = splitn[2])]
		temp1 <- temp1[ , colSums(is.na(temp1)) == 0]
		temp1n <- names(temp1[-1][-1][-1]) #captures measurement names
		temp2 <- x[seq(from = 1, to = splitn[1])]
		temp2 <- temp2[ , colSums(is.na(temp2)) == 0]
		temp2n <- names(temp2[-1][-1][-1]) #captures measurement names

		t1 <- as.data.frame(ref[temp1n])# reference
		t2 <- as.data.frame(ref[temp2n])
		
		B1PCAt <- prcomp(t1) #PCA one
		B2PCAt <- prcomp(t2) #PCA two

		B1PCA <- B1PCAt$x #PC scores
		B2PCA <- B2PCAt$x #PC scores
			
		cmodel1 <- cc(B1PCA, B2PCA) #CCA model
		score1 <- cmodel1$scores$xscores[,1] #takes first variate
		score2 <- cmodel1$scores$yscores[,1] #takes first variate
		model1 <- lm(score1~score2) #linear model
		
		rsqr1 <- summary(model1)$r.squared

		temp2p <- data.frame(t(as.numeric(as.matrix(temp2[-1][-1][-1]))))
		temp1p <- data.frame(t(as.numeric(as.matrix(temp1[-1][-1][-1]))))
		
		names(temp2p) <- temp2n
		names(temp1p) <- temp1n		
		
		temp1p <- as.data.frame(predict(B1PCAt, temp1p))
		temp2p <- as.data.frame(predict(B2PCAt, temp2p))
		
		#create CV from coef of cva
		df1 <- 0
		for(i in 1:length(temp1p)) {
			p1 <- temp1p[i] * cmodel1$xcoef[i,1]
			df1 <- (df1 + p1)
		}
		
		df2 <- 0
		for(i in 1:length(temp2p)) {
			p2 <- temp2p[i] * cmodel1$ycoef[i,1]
			df2 <- (df2 + p2)
		}
	
	lmp1 <- predict(model1, interval="prediction", level = predlevel)
	jpeg(paste("graph",".jpeg",sep=''),height = 800, width = 800)
	dev.control('enable')
		
	plot(score1,score2, xlab = "", ylab = "")
	
	points(df2,df1,col="blue",pch=16)
	
	matlines(lmp1[,1], score2, col=c("red"))
	matlines(lmp1[,2], score2, col=c("blue"), lty = 4)
	matlines(lmp1[,3], score2, col=c("blue"), lty = 4)
	
	plotted <- recordPlot()
	dev.off()
	}
	
	return(list(plotted))
}