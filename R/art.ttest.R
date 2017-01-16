#' Articulation-match T.test Function
#' 
#' This function takes the combinations and reference data generated from the art.input() function 
#' and runs a t.test for each combination. If the results are not statistically significant
#' the combination is considered a possible match and saved in two formats, a single .txt file with all matches
#' and a .csv file with matches. If it's not a match the combination is saved to a .csv file with non-matches
#'
#' @param refdata The reference data from art.input().
#' @param sortdata The combination data from art.input().
#' @param sessiontempdir This creates a temporary session directory. Intended for use with the GUI.
#' @param stdout TRUE returns the dataframes only. FALSE returns the dataframes and creates the files specified above.
#' @param alphalevel Sets the alphalevel used in the t.test. 
#' @param absolutevalue Sets if the absolute value of differences should be used.
#' @param testagainst Sets if the t.test should be against the reference mean (FALSE) or zero (TRUE).
#' @param a Intended for use with all.tests() function that runs all bone tests. 
#'
#' @keywords art.ttest
#' @export
#' @examples
#' art.ttest()


art.ttest <- function (refdata = NULL, sortdata = NULL, sessiontempdir = NULL, stdout = TRUE, alphalevel = 0.1, absolutevalue = TRUE, a = FALSE, testagainst = FALSE, oo = c(TRUE,FALSE)) {
     print("Statistical articulation comparisons have started.")
	library(parallel)
	library(foreach)
	library(doSNOW)
	require(compiler)
	library(reshape2)
	enableJIT(3)

	if(detectCores() > 1) {no_cores <- round(detectCores() / 2)}
	if(detectCores() == 1) {no_cores <- 1}
	
	options(warn = -1) #disables warnings
	if(is.na(sortdata) || is.null(sortdata)) {return(NULL)} #input san
	if(is.na(refdata) || is.null(refdata)) {return(NULL)} #input san
	
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
	
	hera1 <- apply(sortdata, 1, function(y){
		if(absolutevalue) {
			difa <- abs(refdata[,1] - refdata[,2])
			difsd <- sd(difa)
			if(testagainst) {difm <- 0} 
			if(!testagainst) {difm <- mean(difa)}
			p.value <- 2 * pt(-abs((abs(as.numeric(y[7]) - as.numeric(y[8])) - difm) / difsd), df = length(difa) - 1)
		}
		if(!absolutevalue) {
			difa <- refdata[,1] - refdata[,2]
			difsd <- sd(difa)
			if(testagainst) {difm <- 0} 
			if(!testagainst) {difm <- mean(difa)}
			p.value <- 2 * pt(-abs((difb <- as.numeric(y[7]) - as.numeric(y[8]) - difm) / difsd), df = length(difa) - 1)
		}
		
		return(data.frame(a=y[1],b=y[3],c=y[5],d=y[2],e=y[4],f=y[6],g=gsub(",","",toString(names(y)[7:length(y)])),h=round(p.value, digits = 3),i=ncol(refdata)/2,j=nrow(refdata), stringsAsFactors=FALSE)) 
		
	})
	

	
	if(length(hera1) == 1) {
		hera1 <- data.frame(hera1)
	} #transform datatype if only a single row
	if(length(hera1) != 1) {
		hera1 <- melt(hera1, id.vars = c("a","b","c","d","e","f","g","h","i","j"))
		hera1 <- hera1[-11]
	}
	
	colnames(hera1) <- c("ID","Side","Element","ID","Side","Element","Measurements","p.value","# of measurements","Sample size")
     print("Statistical articulation comparisons completed.")
     print("File generation has started.")
	if(!stdout) {
		if(oo[2]) {
			not_excluded <<- hera1[hera1$p.value > alphalevel,]
		
			temp1 <<- unique(not_excluded[,1])
			temp2 <<- unique(not_excluded[,4])
			unique_IDs <<- c(temp1,temp2)
		
			cl <- makeCluster(no_cores)
			registerDoSNOW(cl)
			clusterExport(cl, "not_excluded", envir=environment())
			foreach(i = unique_IDs) %dopar% {
				library(stargazer) #ugh
				if(any(not_excluded[,1] == i)) {
					stargazer(not_excluded[not_excluded[,1] == i,], type = 'text', out = i, summary = FALSE, rownames = FALSE, title = paste("Potential articulation matches not excluded with specimen: ", i, sep=""))
				}
				if(any(not_excluded[,4] == i)) {
					stargazer(not_excluded[not_excluded[,4] == i,], type = 'text', out = i, summary = FALSE, rownames = FALSE, title = paste("Potential articulation matches not excluded with specimen: ", i, sep=""))
				}
				sink(i, append = TRUE, split = FALSE)
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