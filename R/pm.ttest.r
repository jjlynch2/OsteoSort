#' Pair-match T.test Function
#' 
#' @param ref Reference data
#' @param sort Sorted data for comparison
#' @param sessiontempdir Specifies temporary directory for analytical session
#' @param threads The number of threads to use
#' @param alphalevel The alpha level for exclusion
#' @param absolutevalue if TRUE uses absolute value for D-values
#' @param testagainstzero if TRUE uses 0 for sample mean
#' @param output_options C(TRUE,FALSE) First logic specifies excel output, second specifies plot output
#' @param boxcox If TRUE uses boxcox power transformation
#' @param zero_v variable to avoid having zero values in boxcox transformation
#' @param tails The number of tails for the t-distribution
#' 
#' @keywords pm.ttest
#' @export
#' @examples
#' pm.ttest()

pm.ttest <- function (refleft = NULL, refright = NULL, sortleft = NULL, sortright = NULL, sessiontempdir = NULL, alphalevel = 0.1, absolute = TRUE, realmean = FALSE, output_options = c(TRUE, FALSE), threads = 1, tails = 2, boxcox = TRUE) {
	JuliaSetup(cores = threads, recall = TRUE)
	force(alphalevel)
	force(absolute)
	force(realmean)
	force(threads)
	force(tails)
	force(boxcox)
	force(output_options)
	force(sessiontempdir)

	options(stringsAsFactors = FALSE)	
     print("Pair-matching comparisons are running...")
	options(warn = -1) #disables warnings
	if(is.na(sortleft) || is.null(sortleft)) {return(NULL)} #input san
	if(is.na(sortright) || is.null(sortright)) {return(NULL)} #input san
	if(is.na(refleft) || is.null(refleft)) {return(NULL)} #input san
	if(is.na(refright) || is.null(refright)) {return(NULL)} #input san
	workingdir = getwd()

	direc <- OsteoSort:::analytical_temp_space(output_options, sessiontempdir) #creates temporary space 

	#remove first 3 rows convert to matrix for fast operations
	if(absolute && realmean && boxcox) {
		results <- julia_call("PMABM", as.matrix(sortleft[,-c(1:3)]), as.matrix(sortright[,-c(1:3)]), as.matrix(refleft[,-c(1:3)]), as.matrix(refright[,-c(1:3)]), tails)
	}
	else if(absolute && realmean) {
		results <- julia_call("PMAM", as.matrix(sortleft[,-c(1:3)]), as.matrix(sortright[,-c(1:3)]), as.matrix(refleft[,-c(1:3)]), as.matrix(refright[,-c(1:3)]), tails)
	}
	else if(absolute && boxcox) {
		results <- julia_call("PMAB", as.matrix(sortleft[,-c(1:3)]), as.matrix(sortright[,-c(1:3)]), as.matrix(refleft[,-c(1:3)]), as.matrix(refright[,-c(1:3)]), tails)
	}
	else if(realmean && boxcox) {
		results <- julia_call("PMBM", as.matrix(sortleft[,-c(1:3)]), as.matrix(sortright[,-c(1:3)]), as.matrix(refleft[,-c(1:3)]), as.matrix(refright[,-c(1:3)]), tails)
	}
	else if(absolute) {
		results <- julia_call("PMA", as.matrix(sortleft[,-c(1:3)]), as.matrix(sortright[,-c(1:3)]), as.matrix(refleft[,-c(1:3)]), as.matrix(refright[,-c(1:3)]), tails)
	}
	else if(boxcox) {
		results <- julia_call("PMB", as.matrix(sortleft[,-c(1:3)]), as.matrix(sortright[,-c(1:3)]), as.matrix(refleft[,-c(1:3)]), as.matrix(refright[,-c(1:3)]), tails)
	}
	else if(realmean) {
		results <- julia_call("PMM", as.matrix(sortleft[,-c(1:3)]), as.matrix(sortright[,-c(1:3)]), as.matrix(refleft[,-c(1:3)]), as.matrix(refright[,-c(1:3)]), tails)
	}
	else {
		results <- julia_call("PM", as.matrix(sortleft[,-c(1:3)]), as.matrix(sortright[,-c(1:3)]), as.matrix(refleft[,-c(1:3)]), as.matrix(refright[,-c(1:3)]), tails)
	}

	#only do plot if 1 comparison
	#if(output) {
	#	no_return_value <- OsteoSort:::output_function(hera1 = list(SL$id, SR$id, ref_dif, sort_dif), method="exclusion", type="plot")
	#}

	#transform numerical T/F to measurement names
	#measurements <- results[,c(8:ncol(results))]

	#for(x in 1:nrow(measurements)) {
	#	for(i in 1:ncol(measurements)) {
	#		measurements[measurements[x,i] == 1,] <- colnames(sortleft[x,i+3]) #does this introduce a bug where comparisons are removed in julia? It shouldn't... unless there is a ref problem! 
	#	}
	#	measurements <- paste(measurements[,c(1:ncol(measurements))], sep=" ")
	#}

	results_formatted <- data.frame(cbind(id = sortleft[results[,1],1], element = sortleft[results[,1],2], side = sortleft[results[,1],3], id = sortright[results[,2],1], element = sortright[results[,2],2], side = sortright[results[,2],3], measurements = "NOTADDEDYET", p_value = round(results[,4], digits = 4), mean = round(results[,5], digits = 4), sd = round(results[,6], digits =4), sample = results[,7]), Result = NA)

g1 <<- results_formatted
	#Append exclusion results
	for(i in nrow(results_formatted)) {
		if(results_formatted[i,8] > alphalevel) {
			results_formatted[i,12] <- c("Cannot Exclude")
		}
		if(results_formatted[i,8] <= alphalevel) {
			results_formatted[i,12] <- c("Excluded")
		}
	}
g2 <<- results_formatted
	if(output_options[1]) {
		no_return_value <- OsteoSort:::output_function(results, method="exclusion", type="csv")
	}
	
	gc()
	setwd(workingdir)
	options(stringsAsFactors = TRUE) #restore default R  
     print("Finished.")
	return(list(direc,results_formatted[results_formatted$p_value > alphalevel,],results_formatted[results_formatted$p_value <= alphalevel,]))
}