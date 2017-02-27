#' A function to generate plots for single shiny interface
#' 
#'
#' @param file test
#' @param population teast
#' @keywords random
#' @export
#' @examples
#' plotme()

plotme <- function (refdata = NULL, sortdata = NULL, power = TRUE, absolutevalue = TRUE, ttype = "pm", splitn = NULL, predlevel = 0.90, corlevel = 0.5) {   
	if(power) {p1 <- 0.00005; p2 <- 0.33} #half normal transformation
	if(!power) {p1 <- 0; p2 <- 1} #used to prevent writing new code inside loop. This transformation doesn't change the data
	
     if(ttype == "pm") {
     	X <- data.frame(sortdata)
		temp1 <- names(as.data.frame(X)[-c(1:6)])
		temp1 <- temp1[seq(1,length(temp1),2)]
		temp1 <- sort(c(temp1, paste(temp1,"R",sep="")))
		y <- as.data.frame(refdata)[temp1]
		if(absolutevalue) { 
			difa <- ( rowSums(abs((y[c(T,F)] - y[c(F,T)]))) + p1 ) ^ p2
			difa1 <- (sum(abs(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)])) + p1) ^p2
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
		y <- data.frame(sortdata)
		if(absolutevalue) { 
			difa <- ( abs(refdata[,1] - refdata[,2]) + p1 ) ^ p2
			difa1 <- (abs(as.numeric(y[7]) - as.numeric(y[8])) + p1) ^ p2
		}
		if(!absolutevalue) {
			difa <- refdata[,1] - refdata[,2]
			difa1 <- as.numeric(y[7]) - as.numeric(y[8])
		}
		
		jpeg(paste("graph",".jpeg",sep=''),height = 800, width = 800)
		dev.control('enable')	
		hist(x = difa, xlab = "", main = NULL)
		abline(v = difa1, lty = 2, col="darkred")
		plotted <- recordPlot()
		dev.off()
	}
	
	if(ttype == "reg") {
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
		
		B1PCAt <- prcomp(t1)
		B2PCAt <- prcomp(t2)

		B1PCA <- B1PCAt$x 
		B2PCA <- B2PCAt$x
			
		t <- as.data.frame(cor(B1PCA, B2PCA, use="complete.obs"))
			
		predictednames <- c()
		for(i in colnames(t)) {
		    if(any(t[,i] > corlevel)) {
			   predictednames <- c(predictednames, i)
		    }
		}

		predictornames <- c()
		for(i in rownames(t)) {
		    if(any(t[i,]  > corlevel)) {
			   predictornames <- c(predictornames, i)
			   }
		}
		
		if(length(predictednames) < 1 || length(predictornames) < 1) {
			return(NULL)
		}
		
		if(length(predictornames) > 1) { B1PCA <- rowSums(B1PCA[,predictornames])}
		if(length(predictednames) > 1) { B2PCA <- rowSums(B2PCA[,predictednames])}
		if(length(predictornames) == 1) { B1PCA <- B1PCA[,predictornames]}
		if(length(predictednames) == 1) { B2PCA <- B2PCA[,predictednames]}
			
		names(B1PCA) <- predictornames
		names(B2PCA) <- predictednames
		
		df1 <- as.data.frame(cbind(B1PCA, B2PCA))
		model1 <- lm(B2PCA ~ B1PCA, data = df1)
		

	   
		rsqr1 <- summary(model1)$r.squared


		temp2p <- data.frame(as.numeric(as.matrix(temp2[-1][-1][-1])))
		temp1p <- data.frame(as.numeric(as.matrix(temp1[-1][-1][-1])))
		
		names(temp2p) <- temp2n
		names(temp1p) <- temp1n	
		
		temp1p <- as.data.frame(predict(B1PCAt, temp1p))
		temp2p <- as.data.frame(predict(B2PCAt, temp2p))

		if(length(predictednames) > 1) {predicted <- sum(temp2p[,predictednames])}
		if(length(predictornames) > 1) {predictors <- sum(temp1p[,predictornames])}
		if(length(predictednames) <= 1) {predicted <- temp2p[,predictednames]}
		if(length(predictornames) <= 1) {predictors <- temp1p[,predictornames]}

		names(predictors) <- predictornames
		names(predicted) <- predictednames

		pm1 <- predict(model1, newdata = data.frame(B1PCA = predictors), interval="prediction", level = predlevel) #prediction interval based on the lm from model1
		
	
	lmp1 <- predict(model1, interval="prediction", level = predlevel)
	jpeg(paste("graph",".jpeg",sep=''),height = 800, width = 800)
	dev.control('enable')
		
	plot(B2PCA,B1PCA, xlab = "", ylab = "")
	
	points(predicted,predictors,col="blue",pch=16)
	
	matlines(lmp1[,1], B1PCA, col=c("red"))
	matlines(lmp1[,2], B1PCA, col=c("blue"), lty = 4)
	matlines(lmp1[,3], B1PCA, col=c("blue"), lty = 4)
	
	plotted <- recordPlot()
	dev.off()
	}
	
	return(list(plotted))
}