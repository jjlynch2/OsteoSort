#' reg.test Input Function
#' Function to produce combinations for associating elements with regression
#'
#' @param ref Reference data
#' @param sort Sorted data for comparison
#' @param splitn The index location of each bone types measurements. Used internally from reg.input()
#' @param prediction_interval Specifies the prediction interval 
#' @param sessiontempdir Specifies temporary directory for analytical session 
#' @param output_options C(TRUE,FALSE) First logic specifies excel output, second specifies plot output
#' @param threads The number of threads to use
#' @param test If true, PCA-CCA-Regression, if false, Simple Linear Regression 
#' @param alphalevel The alpha level for exclusion
#' @param pca Specifies the number of principal components to use
#'
#' @keywords reg.test
#' @export
#' @examples 
#' reg.multitest()

reg.test <- function(refa = NULL, refb = NULL, sorta = NULL, sortb = NULL, sessiontempdir = NULL, ztest = NULL, output_options = c(TRUE,FALSE), threads = 1, type = "Logarithm Composite", pca = 0.99, alphalevel = 0.05) {	
	if(threads != julia_call("nprocs")) {
		print("Setting up Julia workers...")
		JuliaSetup(add_cores = threads, source = TRUE, recall_libraries = TRUE)
		print("Finished.")
	}
	force(alphalevel)
	force(threads)
	force(type)
	force(pca)
	force(ztest)
	force(output_options)
	force(sessiontempdir)

	#appends a variable with 0 to make sure the data structure stays the same in Julia
	refa <- cbind(refa,fa = 0)
	refb <- cbind(refb,fa = 0)
	sorta <- cbind(sorta,fa = 0)
	sortb <- cbind(sortb,fa = 0)

	print("Comparisons are running...")
	options(stringsAsFactors = FALSE)    

	options(warn = -1) #disables warnings
	options(as.is = TRUE)
	if(is.na(sorta) || is.null(sorta)) {return(NULL)}
	if(is.na(sortb) || is.null(sortb)) {return(NULL)}
	if(is.na(refa) || is.null(refa)) {return(NULL)}
	if(is.na(refb) || is.null(refb)) {return(NULL)}
	workingdir = getwd()
	direc <- OsteoSort:::analytical_temp_space(output_options, sessiontempdir) #creates temporary space 

	if(type == "Logarithm Composite") { #true PCA CCA regression
		results <- julia_call("REGS", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]))
	} else if (type == "CCA Ordination") { #false simple regression
		if(is.numeric(pca) && ztest) {

		} else if (ztest) {
			output_options[1] = FALSE
		} else if (is.numeric(pca)) {

		}
	}
	#transform numerical T/F to measurement names
	if(nrow(results) > 1) {
		measurements <- data.frame(results[,c(8:ncol(results))])
	}else {
		measurements <- data.frame(t(results[c(8:length(results))]))
	}
	measurement_names <- unique(c(colnames(sorta[,-c(1:3)]), colnames(sortb[,-c(1:3)])))
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
	if(output_options[2] && nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sortb)) == 1) { 
		no_return_value <- OsteoSort:::output_function(hera1 <- list(results_formatted[1,1], results_formatted[1,2], plot_data[1:nrow(plot_data)-1,], plot_data[nrow(plot_data),]), method="exclusion", type="plot")
	}

	gc()
	setwd(workingdir)
	options(stringsAsFactors = TRUE) #restore default R
	print("Finished.")
	return(list(direc,results_formatted[results_formatted$Result == "Cannot Exclude",],results_formatted[results_formatted$Result == "Excluded",]))
}