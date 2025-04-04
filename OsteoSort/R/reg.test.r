reg.test <- function(refa = NULL, refb = NULL, sorta = NULL, sortb = NULL, sessiontempdir = NULL, reference = NULL, type = "Logarithm Composite", alphalevel = 0.05) {	
	start_time <- start_time()

	force(alphalevel)
	force(type)
	force(sessiontempdir)

	#appends a variable with 0 to make sure the data structure stays the same in Julia
	refa <- cbind(refa,fa = 0)
	refb <- cbind(refb,fa = 0)
	sorta <- cbind(sorta,fa = 0)
	sortb <- cbind(sortb,fa = 0)

	if(all(is.na(sorta)) || is.null(sorta)) {return(NULL)}
	if(all(is.na(sortb)) || is.null(sortb)) {return(NULL)}
	if(all(is.na(refa)) || is.null(refa)) {return(NULL)}
	if(all(is.na(refb)) || is.null(refb)) {return(NULL)}

	direc <- analytical_temp_space(sessiontempdir) #creates temporary space 
	sd <- paste(sessiontempdir, direc, sep="/")
	
	results <- julia_call("OSJ.REGSL", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]))
	if(nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sortb)) == 1) { 
		plot_data <- julia_call("OSJ.REGSL_plot", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]))
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
	results_formatted <- data.frame(
		cbind(x_id = sorta[results[,1],1],
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

    results_formatted[results_formatted[,8] > alphalevel,11] <- "Cannot Exclude"
    results_formatted[results_formatted[,8] <= alphalevel,11] <- "Excluded"
	output_function(method = "options", options = data.frame(alphalevel = alphalevel, tails = 2, type = type, reference = reference), fpath=sd)
	output_function(results_formatted, method="exclusion", type="csv", fpath=sd)
	if(nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sortb)) == 1) { 
		output_function(hera1 <- list(
			results_formatted[1,1], 
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
	t_time <- end_time(start_time)
	return(list(direc,results_formatted[results_formatted$result == "Cannot Exclude",],results_formatted[results_formatted$result == "Excluded",], t_time, rejected))
}
