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

antestat.regtest <- function(antemortem = NULL, postmortem = NULL, ref = NULL, sessiontempdir = NULL, labtfa = TRUE, output_options = c(TRUE,FALSE,FALSE), alphalevel = 0.05, threads = 1) {
	force(alphalevel)
	force(threads)
	force(output_options)
	force(sessiontempdir)
	if(threads != julia_call("nprocs")) {
		print("Setting up Julia workers...")
		JuliaSetup(add_cores = threads, source = TRUE, recall_libraries = TRUE)
		print("Finished.")
	}
	measurements <- colnames(postmortem)[4]
	antemortem <- rbind(antemortem, 0)
	postmortem <- rbind(postmortem, 0)

	print("Comparisons are running...")
	start_time <- start_time()

	if(is.na(antemortem) || is.null(antemortem)) {return(NULL)}
	if(is.na(postmortem) || is.null(postmortem)) {return(NULL)}
	if(is.na(ref) || is.null(ref)) {return(NULL)}

	workingdir = getwd()
	direc <- OsteoSort:::analytical_temp_space(output_options, sessiontempdir) #creates temporary space 
	results <- julia_call("REGS_Ante", as.matrix(antemortem[,2]), as.matrix(postmortem[,4]), as.matrix(ref[c(4,5)]))
	#format data.frame to return

	antemortem <- antemortem[-c(nrow(antemortem)),]
	postmortem <- postmortem[-c(nrow(postmortem)),]

	if(nrow(postmortem) == 1) { postmortem[results[,1],1] <- as.character(postmortem[results[,1],1])}
	if(nrow(antemortem) == 1) { antemortem[results[,2],1] <- as.character(antemortem[results[,2],1])}

	results_formatted <- data.frame(cbind(
								am_id = as.character(antemortem[results[,1],1]),          #1
								Stature = antemortem[results[,1],2],       #2
								pm_id = as.character(postmortem[results[,2],1]),       #3
								side = postmortem[results[,2],2],        #4
								element = postmortem[results[,2],3],     #5
								
								measurements = colnames(ref)[5],           #6
								p_value = round(results[,3], digits = 4),  #7
								r2 = round(results[,5], digits = 4),       #8
								sample = results[,4]
								),                     #9
								result = NA,                               #10
								stringsAsFactors = FALSE
	)

	#Append exclusion results
	for(i in 1:nrow(results_formatted)) {
		if(results_formatted[i,7] > alphalevel) {
			results_formatted[i,10] <- c("Cannot Exclude")
		}
		if(results_formatted[i,7] <= alphalevel) {
			results_formatted[i,10] <- c("Excluded")
		}
	}

	if(output_options[1]) {
		no_return_value <- OsteoSort:::output_function(results_formatted, method="exclusion", type="csv")
	}
	if(output_options[2]) { 
		no_return_value <- OsteoSort:::output_function(
							hera1 <- list(results_formatted[1,1], 
										results_formatted[1,3], 
										ref[,4], 
										ref[,5], 
										antemortem[1,2],
										postmortem[1,4],
										alphalevel),
										method="exclusion", 
										type="plot2"
						)
	}
	if(length(output_options) > 2) { 
		if(output_options[3]) {
			no_return_value <- OsteoSort:::output_function(hera1 <- results_formatted[results_formatted$result == "Cannot Exclude",], method="networkanalysis", type="ante", labtf = labtfa)
		}
	}

	gc()
	setwd(workingdir)
	print("Finished.")
	t_time <- end_time(start_time)
	return(list(direc,results_formatted[results_formatted$result == "Cannot Exclude",],results_formatted[results_formatted$result == "Excluded",], t_time))
}
