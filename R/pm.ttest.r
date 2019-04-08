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
	julia_call("Set_Procs", threads,detectCores())
	julia_source(system.file("jl", "library.jl", package = "OsteoSort"))
###############################
grl <<- refleft
grr <<- refright
gsl <<- sortleft
gsr <<- sortright





###############################
	force(alphalevel)
	force(absolutevalue)
	force(testagainstzero)
	force(threads)
	force(tails)
	force(boxcox)
	force(output_options)
	force(sessiontempdir)

	options(stringsAsFactors = FALSE)	
     print("Pair-matching comparisons are running...")
	options(warn = -1) #disables warnings
	if(is.na(sort) || is.null(sort)) {return(NULL)} #input san
	if(is.na(ref) || is.null(ref)) {return(NULL)} #input san
	
	workingdir = getwd()

	direc <- OsteoSort:::analytical_temp_space(output_options, sessiontempdir) #creates temporary space 

	if(absolute && realmean && boxcox) {
		results <- julia_call("PMABM", sortleft, sortright, refleft, refright, tails)
	}
	else if(absolute && realmean) {
		results <- julia_call("PMAM", sortleft, sortright, refleft, refright, tails)
	}
	else if(absolute && boxcox) {
		results <- julia_call("PMAB", sortleft, sortright, refleft, refright, tails)
	}
	else if(realmean && boxcox) {
		results <- julia_call("PMBM", sortleft, sortright, refleft, refright, tails)
	}
	else if(absolute) {
		results <- julia_call("PMA", sortleft, sortright, refleft, refright, tails)
	}
	else if(boxcox) {
		results <- julia_call("PMB", sortleft, sortright, refleft, refright, tails)
	}
	else if(realmean) {
		results <- julia_call("PMM", sortleft, sortright, refleft, refright, tails)
	}
	else {
		results <- julia_call("PM", sortleft, sortright, refleft, refright, tails)
	}
	#julia functions port above... do neo piece at a time

	#if(output) {
	#	no_return_value <- OsteoSort:::output_function(hera1 = list(SL$id, SR$id, ref_dif, sort_dif), method="exclusion", type="plot")
	#}
#need to parse measurement names before this output call
	if(output_options[1]) {
		no_return_value <- OsteoSort:::output_function(results, method="exclusion", type="csv")
	}

	#transform numerical T/F to measurement names
	measurements <- results[,c(8:ncol(results))]
	for(i in 1:ncol(measurements)) {
		measurements[measurements[,i] == 1,] <- names(sortleft[,i+3])
	}
	measurements <- paste(measurements[,c(1:ncol(measurements))], sep=" ")

	results_formatted <- cbind(id = sortleft[results[,1],1], element = sortleft[results[,1],2], side = sortleft[results[,1],3], id = sortright[results[,2],1], element = sortright[results[,2],2], side = sortright[results[,2],3], measurements = measurements, p_value = round(results[,4], digits = 4), mean = results[,5], sd = results[,6], sample = results[,7],stringsAsFactors=FALSE)
	gc()
	setwd(workingdir)
	options(stringsAsFactors = TRUE) #restore default R  
     print("Finished.")
	return(list(direc,hera1[hera1$p.value > alphalevel,],hera1[hera1$p.value <= alphalevel,]))	
}