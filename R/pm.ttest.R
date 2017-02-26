#' Pair-match T.test Function
#' 
#' This function takes the combinations and reference data generated from the pm.input() function 
#' and runs a t.test for each combination. If the results are not statistically significant
#' the combination is considered a possible match and saved in two formats, a single .tyt file with all matches
#' and a .csv file with matches. If it's not a match the combination is saved to a .csv file with non-matches
#'
#' @param refdata The reference data from pm.input().
#' @param sortdata The combination data from pm.input().
#' @param sessiontempdir This creates a temporary session directory. Intended for use with the GUI.
#' @param stdout TRUE returns the dataframes only. FALSE returns the dataframes and creates the files specified above.
#' @param alphalevel Sets the alphalevel used in the t.test. 
#' @param absolutevalue Sets if the absolute value of differences should be used.
#' @param testagainst Sets if the t.test should be against the reference mean (FALSE) or zero (TRUE).
#' @param a Intended for use with all.tests() function that runs all bone tests. 
#' @param oo Defines which files to generate. oo = c(TRUE,FALSE) where oo[1] is excel files and oo[2] is individual specimen files
#'
#' @keywords pm.ttest
#' @eyport
#' @examples
#' pm.ttest()

pm.ttest <- function (refdata = NULL, sortdata = NULL, sessiontempdir = NULL, stdout = TRUE, alphalevel = 0.1, power = TRUE, absolutevalue = TRUE, a = FALSE, testagainst = FALSE, oo = c(TRUE,FALSE), no_cores = 1) {
   
   print("alpha")
   print(alphalevel)
     print("Statistical pair match comparisons have started.")
	library(parallel)
	library(foreach)
	library(doSNOW)
	require(compiler)
	enableJIT(3)
	
	options(warn = -1) #disables warnings
	if(is.na(sortdata) || is.null(sortdata)) {return(NULL)} #input san
	if(is.na(refdata) || is.null(refdata)) {return(NULL)} #input san
	
	if(power) {p1 <- 0.00005; p2 <- 0.33} #half normal transformation
	if(!power) {p1 <- 0; p2 <- 1} #used to prevent writing new code inside loop. This transformation doesn't change the data
	
	workingdir = getwd()

	if(!stdout) { 
		if(!a) {
			if (!is.null(sessiontempdir)) {
				setwd(sessiontempdir)
			}
			direc <- randomstring(n = 1, length = 12)
			dir.create(direc)
			setwd(direc)
		}
		if(a) {
			setwd(sessiontempdir)
			direc <- NULL
		}
	}


	myfun<-function(X){
		temp1 <- names(as.data.frame(X)[-c(1:6)])
		temp1 <- temp1[seq(1,length(temp1),2)]
		temp1 <- sort(c(temp1, paste(temp1,"R",sep="")))
		y <- as.data.frame(refdata)[temp1]
		if(absolutevalue) { 
			difa <- ( rowSums(abs((y[c(T,F)] - y[c(F,T)]))) + p1 ) ^ p2
			difsd <- sd(difa)
			if(testagainst) {difm <- 0} 
			if(!testagainst) {difm <- mean(difa)}
		
			tt <- (sum(abs(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)])) + p1) ^p2
			p.value <- pt((tt - difm) / difsd, df = length(difa) - 1, lower.tail = FALSE) #one-tail for absolute value model
		}
		
		if(!absolutevalue) {
			difa <- rowSums(y[c(T,F)] - y[c(F,T)])
			difsd <- sd(difa)
			if(testagainst) {difm <- 0}
			if(!testagainst) {difm <- mean(difa)}
			p.value <- 2 * pt(-abs((sum(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)]) - difm) / difsd), df = length(difa) - 1)
		} 
		
		
		
		return(data.frame(a=X[,1],b=X[,3],c=X[,5],d=X[,2],e=X[,4],f=X[,6],g=gsub(",","",toString(colnames(X)[7:length(X)][c(T,F)])),h=round(p.value, digits = 4),i=ncol(y)/2,j=nrow(y), k=round(difm, digits = 4), l=round(difsd, digits = 4),stringsAsFactors=FALSE)) 
	}
	
	
	if(.Platform$OS.type == "unix") {hera1 <- mclapply(FUN = myfun, X = sortdata, mc.cores = no_cores, mc.preschedule = TRUE); hera1 <- melt(hera1, id.vars = c("a","b","c","d","e","f","g","h","i","j","k","l")); hera1 <- hera1[-13]}  
	if(.Platform$OS.type != "unix") {hera1 <- lapply(FUN = myfun, X = sortdata); hera1 <- data.frame(hera1)}


	colnames(hera1) <- c("ID","Side","Element","ID","Side","Element","Measurements","p.value","# of measurements","Sample size", "mean", "sd")
     print("Statistical pair match comparisons completed.")
     print("File generation has started.")

     
	if(!stdout) {		
		if(oo[2]) {
			not_excluded <- hera1[hera1$p.value > alphalevel,]
			temp1 <- unique(not_excluded[,1])
			temp2 <- unique(not_excluded[,4])
			unique_IDs <- unique(c(temp1,temp2))

			cl <- makeCluster(no_cores)
			registerDoSNOW(cl)
			clusterExport(cl, "not_excluded", envir=environment())
			foreach(i = unique_IDs) %dopar% {
				library(stargazer) #ugh
				if(any(not_excluded[,1] == i)) {
					stargazer(not_excluded[not_excluded[,1] == i,], type = 'text', out = i, summary = FALSE, rownames = FALSE, title = paste("Potential pair matches not excluded with specimen: ", i, sep=""))
				}
				if(any(not_excluded[,4] == i)) {
					stargazer(not_excluded[not_excluded[,4] == i,], type = 'text', out = i, summary = FALSE, rownames = FALSE, title = paste("Potential pair matches not excluded with specimen: ", i, sep=""))
				}
				sink(as.character(i), append = TRUE, split = FALSE)
				cat('\nDate: ', strftime(Sys.time(), "%Y-%m-%d %H:%M:%S"), 'Analyst___________', ' Initials___________') 
				cat('\nFor Official Use Only') 
				sink()	
			}
			stopCluster(cl)
		}
		if(oo[1]) {
			write.csv(as.matrix(hera1[hera1$p.value > alphalevel,]), file = "not-excluded-list.csv", row.names=FALSE, col.names = TRUE)
			write.csv(as.matrix(hera1[hera1$p.value <= alphalevel,]), file = "excluded-list.csv",row.names=FALSE, col.names = TRUE)
		}
	}
	
	
	
	setwd(workingdir)
     print("File generation has completed.")
	enableJIT(0)
	return(list(direc,hera1[hera1$p.value > alphalevel,],hera1[hera1$p.value <= alphalevel,]))	
}