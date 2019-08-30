#' antestat.regtest Input Function
#'
#' Function to produce a p-value evaluating the strength of evidence for comparing an antemortem stature to postmortem measurement
#'
#' @param sort The antemortem stature and postmorterm measurement for comparison
#' @param ref The reference sample used in the regression model
#' @param prediction_interval The prediction interval level for exclusion
#' @param alphalevel The alpha level for exclusion
#' @param alphatest If TRUE specifies the use of alphalevels for exclusion. If FALSE specifies the use of prediction intervals
#' @param tails The number of tails for the t-distribution
#' @param output_options C(TRUE,FALSE) First logic specifies excel output, second specifies plot output
#' @param sessiontempdir Specifies temporary directory for analytical session 
#' @param threads The number of threads to use
#'
#' @keywords antestat
#' @export
#' @examples 
#' antestat.regtest()

antestat.regtest <- function(antemortem = NULL, postmortem = NULL, ref = NULL, sessiontempdir = NULL, output_options = c(TRUE,FALSE), alphalevel = 0.05, threads = 1) {
	force(alphalevel)
	force(threads)
	force(output_options)
	force(sessiontempdir)
	if(threads != julia_call("nprocs")) {
		print("Setting up Julia workers...")
		JuliaSetup(add_cores = threads, source = TRUE, recall_libraries = TRUE)
		print("Finished.")
	}

	print("Comparisons are running...")
	start_time <- start_time()
	options(stringsAsFactors = FALSE)
	options(warn = -1) #disables warnings
	options(as.is = TRUE)

	if(is.na(antemortem) || is.null(antemortem)) {return(NULL)}
	if(is.na(postmortem) || is.null(postmortem)) {return(NULL)}
	if(is.na(ref) || is.null(ref)) {return(NULL)}

	workingdir = getwd()
	direc <- OsteoSort:::analytical_temp_space(output_options, sessiontempdir) #creates temporary space 

	results <<- julia_call("REGS_Ante",)
	#if(output_options[2] && nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sortb)) == 1) { 
		plot_data <- julia_call("REGS_Ante_plot", )
	#}



	#format data.frame to return
	results_formatted <- data.frame(cbind(id_1 = sorta[results[,1],1], element_1 = sorta[results[,1],3], side_1 = sorta[results[,1],2], id_2 = sortb[results[,2],1], element_2 = sortb[results[,2],3], side_2 = sortb[results[,2],2], measurements = measurements, p_value = round(results[,4], digits = 4), r2 = round(results[,6], digits = 4), sample = results[,5]), Result = NA, stringsAsFactors = FALSE)

	#Append exclusion results
	for(i in 1:nrow(results_formatted)) {
		if(results_formatted[i,7] > alphalevel) {
			results_formatted[i,11] <- c("Cannot Exclude")
		}
		if(results_formatted[i,7] <= alphalevel) {
			results_formatted[i,11] <- c("Excluded")
		}
	}

	if(output_options[1]) {
		no_return_value <- OsteoSort:::output_function(results_formatted, method="exclusion", type="csv")
	}
	if(output_options[2] && nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sortb)) == 1) { 
		no_return_value <- OsteoSort:::output_function(hera1 <- list(results_formatted[1,1], results_formatted[1,4], plot_data[[1]],plot_data[[2]], plot_data[[3]], plot_data[[4]]), method="exclusion", type="plot2")
	}

	gc()
	setwd(workingdir)
	options(stringsAsFactors = TRUE) #restore default R
	print("Finished.")
	t_time <- end_time(start_time)
	return(list(direc,results_formatted[results_formatted$Result == "Cannot Exclude",],results_formatted[results_formatted$Result == "Excluded",], t_time))
}
