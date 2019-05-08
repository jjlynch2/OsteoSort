#' T.test Function
#' 
#' @param ref Reference data
#' @param sort Sorted data for comparison
#' @param sessiontempdir Specifies temporary directory for analytical session
#' @param threads The number of threads to use
#' @param alphalevel The alpha level for exclusion
#' @param absolute if TRUE uses absolute value for D-values
#' @param output_options C(TRUE,FALSE) First logic specifies excel output, second specifies plot output
#' @param boxcox If TRUE uses boxcox power transformation
#' @param tails The number of tails for the t-distribution
#' 
#' @keywords TTEST.ttest
#' @export
#' @examples
#' ttest()

ttest <- function (refa = NULL, refb = NULL, sorta = NULL, sortb = NULL, sessiontempdir = NULL, alphalevel = 0.1, absolute = TRUE, zmean = FALSE, output_options = c(TRUE, FALSE), threads = 1, tails = 2, boxcox = TRUE) {
	JuliaSetup(cores = threads, recall = TRUE)
	force(alphalevel)
	force(absolute)
	force(zmean)
	force(threads)
	force(tails)
	force(boxcox)
	force(output_options)
	force(sessiontempdir)

	#appends a variable with 0 to make sure the data structure stays the same in Julia
	refa <- cbind(refa,0)
	refb <- cbind(refb,0)
	sorta <- cbind(sorta,0)
	sortb <- cbind(sortb,0)


	options(stringsAsFactors = FALSE)
	print("Comparisons are running...")
	options(warn = -1) #disables warnings
	if(is.na(sorta) || is.null(sorta)) {return(NULL)}
	if(is.na(sortb) || is.null(sortb)) {return(NULL)}
	if(is.na(refa) || is.null(refa)) {return(NULL)}
	if(is.na(refb) || is.null(refb)) {return(NULL)}
	workingdir = getwd()
	direc <- OsteoSort:::analytical_temp_space(output_options, sessiontempdir) #creates temporary space 

	#remove first 3 rows convert to matrix for fast operations
	if(absolute && zmean && boxcox) {
		results <- julia_call("TTESTABM", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]), tails)
		if(output_options[2] && nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sorta)) == 1) { 
			plot_data <- julia_call("TTESTAB_plot", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]))
		}
	}
	else if(absolute && zmean) {
		results <- julia_call("TTESTAM", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]), tails)
		if(output_options[2] && nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sorta)) == 1) { 
			plot_data <- julia_call("TTESTA_plot", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]))
		}
	}
	else if(absolute && boxcox) {
		results <- julia_call("TTESTAB", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]), tails)
		if(output_options[2] && nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sorta)) == 1) { 
			plot_data <- julia_call("TTESTAB_plot", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]))
		}
	}
	else if(zmean && boxcox) {
		results <- julia_call("TTESTBM", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]), tails)
		if(output_options[2] && nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sorta)) == 1) { 
			plot_data <- julia_call("TTESTB_plot", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]))
		}
	}
	else if(absolute) {
		results <- julia_call("TTESTA", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]), tails)
		if(output_options[2] && nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sorta)) == 1) { 
			plot_data <- julia_call("TTESTA_plot", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]))
		}
	}
	else if(boxcox) {
		results <- julia_call("TTESTB", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]), tails)
		if(output_options[2] && nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sorta)) == 1) { 
			plot_data <- julia_call("TTESTB_plot", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]))
		}
	}
	else if(zmean) {
		results <- julia_call("TTESTM", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]), tails)
		if(output_options[2] && nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sorta)) == 1) { 
			plot_data <- julia_call("TTEST_plot", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]))
		}
	}
	else {
		results <- julia_call("TTEST", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]), tails)
		if(output_options[2] && nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sorta)) == 1) { 
			plot_data <- julia_call("TTEST_plot", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]))
		}
	}

	#transform numerical T/F to measurement names
	if(nrow(results) > 1) {
		measurements <- data.frame(results[,c(8:ncol(results))])
	}else {
		measurements <- data.frame(t(results[c(8:length(results))]))
	}
	measurement_names <- colnames(sorta[,-c(1:3)])
	for(i in 1:ncol(measurements)) {
		measurements[measurements[,i] == 1,i] <- paste(measurement_names[i], " ", sep="")
		measurements[measurements[,i] == 0,i] <- ""
	}
	measurements <- do.call(paste0, measurements[c(1:ncol(measurements))])
	#format data.frame to return
	results_formatted <- data.frame(cbind(id_1 = sorta[results[,1],1], element_1 = sorta[results[,1],2], side_1 = sorta[results[,1],3], id_2 = sortb[results[,2],1], element_2 = sortb[results[,2],2], side_2 = sortb[results[,2],3], measurements = measurements, p_value = round(results[,4], digits = 4), mean = round(results[,5], digits = 4), sd = round(results[,6], digits =4), sample = results[,7]), Result = NA, stringsAsFactors = FALSE)

	#Append exclusion results
	for(i in 1:nrow(results_formatted)) {
		if(results_formatted[i,8] > alphalevel) {
			results_formatted[i,12] <- c("Cannot Exclude")
		}
		if(results_formatted[i,8] <= alphalevel) {
			results_formatted[i,12] <- c("Excluded")
		}
	}

	if(output_options[1]) {
		no_return_value <- OsteoSort:::output_function(results_formatted, method="exclusion", type="csv")
	}
	if(output_options[2] && nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sorta)) == 1) { 
		no_return_value <- OsteoSort:::output_function(hera1 <- list(results_formatted[1,1], results_formatted[1,2], plot_data[1:nrow(plot_data)-1,], plot_data[nrow(plot_data),]), method="exclusion", type="plot")
	}

	#cleanup
	gc()
	setwd(workingdir)
	options(stringsAsFactors = TRUE) #restore default R
	print("Finished.")
	return(list(direc,results_formatted[results_formatted$Result == "Cannot Exclude",],results_formatted[results_formatted$Result == "Excluded",]))
}