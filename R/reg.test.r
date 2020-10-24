reg.test <- function(refa = NULL, refb = NULL, sorta = NULL, sortb = NULL, sessiontempdir = NULL, ztest = NULL, reference = NULL, labtf = TRUE, output_options = c(TRUE,FALSE, FALSE), threads = 1, type = "Logarithm Composite", alphalevel = 0.05) {	
	force(alphalevel)
	force(threads)
	force(type)
	force(ztest)
	force(output_options)
	force(sessiontempdir)
	if(threads != julia_call("nprocs")) {
		print("Setting up Julia workers...")
		JuliaSetup(add_cores = threads, source = TRUE, recall_libraries = TRUE)
		print("Finished.")
	}

	#appends a variable with 0 to make sure the data structure stays the same in Julia
	refa <- cbind(refa,fa = 0)
	refb <- cbind(refb,fa = 0)
	sorta <- cbind(sorta,fa = 0)
	sortb <- cbind(sortb,fa = 0)

	print("Comparisons are running...")
	start_time <- start_time()

	options(warn = -1) #disables warnings
	options(as.is = TRUE)
	if(is.na(sorta) || is.null(sorta)) {return(NULL)}
	if(is.na(sortb) || is.null(sortb)) {return(NULL)}
	if(is.na(refa) || is.null(refa)) {return(NULL)}
	if(is.na(refb) || is.null(refb)) {return(NULL)}
	direc <- OsteoSort:::analytical_temp_space(output_options, sessiontempdir) #creates temporary space 
	sd <- paste(sessiontempdir, direc, sep="/")
	
	results <- julia_call("REGSL", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]))
	if(output_options[2] && nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sortb)) == 1) { 
		plot_data <- julia_call("REGSL_plot", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]))
	}

	#transform numerical T/F to measurement names
	if(nrow(results) > 1) {
		measurements <- data.frame(results[,c(6:ncol(results))])
	}else {
		measurements <- data.frame(t(results[c(6:length(results))]))
	}
	measurement_names <- unique(c(colnames(sorta[,-c(1:3)]), colnames(sortb[,-c(1:3)])))
	for(i in 1:ncol(measurements)) {
		measurements[measurements[,i] == 1,i] <- paste(measurement_names[i], " ", sep="")
		measurements[measurements[,i] == 0,i] <- ""
	}
	measurements <- do.call(paste0, measurements[c(1:ncol(measurements))])
	#format data.frame to return
	results_formatted <- data.frame(cbind(x_id = sorta[results[,1],1], 
								x_element = sorta[results[,1],3], 
								x_side = sorta[results[,1],2], 
								y_id = sortb[results[,2],1], 
								y_element = sortb[results[,2],3], 
								y_side = sortb[results[,2],2], 
								measurements = measurements, 
								p_value = round(results[,3], digits = 4), 
								r2 = round(results[,5], digits = 4), 
								sample = results[,4]
								), 
								result = NA, 
								stringsAsFactors = FALSE
	)

	rejected <- results_formatted[results_formatted$measurements == "",]
	results_formatted <- results_formatted[results_formatted$measurements != "",]

	#Append exclusion results
	for(i in 1:nrow(results_formatted)) {
		if(results_formatted[i,8] > alphalevel) {
			results_formatted[i,11] <- c("Cannot Exclude")
		}
		if(results_formatted[i,8] <= alphalevel) {
			results_formatted[i,11] <- c("Excluded")
		}
	}
	no_return_value <- OsteoSort:::output_function(method = "options", options = data.frame(alphalevel = alphalevel, tails = 2, ztransform = ztest, type = type, reference = reference), fpath=sd)
	if(output_options[1]) {
		no_return_value <- OsteoSort:::output_function(results_formatted, method="exclusion", type="csv", fpath=sd)
	}
	if(output_options[2]) { 
		no_return_value <- OsteoSort:::output_function(hera1 <- list(results_formatted[1,1], 
															results_formatted[1,4], 
															plot_data[[1]],
															plot_data[[2]], 
															plot_data[[3]], 
															plot_data[[4]], 
															alphalevel), 
															method="exclusion", 
															type="plot2",
															fpath=sd
						)
	}
	if(length(output_options) > 2) { 
		if(output_options[3]) {
			no_return_value <- OsteoSort:::output_function(hera1 <- results_formatted[results_formatted$result == "Cannot Exclude",], method="networkanalysis", type="association", labtf = labtf, fpath=sd)
		}
	}

	gc()
	print("Finished.")
	t_time <- end_time(start_time)
	return(list(direc,results_formatted[results_formatted$result == "Cannot Exclude",],results_formatted[results_formatted$result == "Excluded",], t_time, rejected))
}
