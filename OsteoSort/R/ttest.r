ttest <- function (refa = NULL, refb = NULL, sorta = NULL, sortb = NULL, sessiontempdir = NULL, alphalevel = 0.1, absolute = TRUE, zmean = FALSE, tails = 2, yeojohnson = TRUE, reference = NULL) {
	start_time <- start_time()

	force(alphalevel)
	force(absolute)
	force(zmean)
	force(tails)
	force(yeojohnson)
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

	if(absolute && zmean && yeojohnson) {
		results <- julia_call("OSJ.TTESTABM", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]), tails)
		if(nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sortb)) == 1) { 
			plot_data <- julia_call("OSJ.TTESTAB_plot", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]))
		}
	}
	else if(absolute && zmean) {
		results <- julia_call("OSJ.TTESTAM", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]), tails)
		if(nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sortb)) == 1) { 
			plot_data <- julia_call("OSJ.TTESTA_plot", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]))
		}
	}
	else if(absolute && yeojohnson) {
		results <- julia_call("OSJ.TTESTAB", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]), tails)
		if(nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sortb)) == 1) { 
			plot_data <- julia_call("OSJ.TTESTAB_plot", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]))
		}
	}
	else if(zmean && yeojohnson) {
		results <- julia_call("OSJ.TTESTBM", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]), tails)
		if(nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sortb)) == 1) { 
			plot_data <- julia_call("OSJ.TTESTB_plot", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]))
		}
	}
	else if(absolute) {
		results <- julia_call("OSJ.TTESTA", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]), tails)
		if(nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sortb)) == 1) { 
			plot_data <- julia_call("OSJ.TTESTA_plot", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]))
		}
	}
	else if(yeojohnson) {
		results <- julia_call("OSJ.TTESTB", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]), tails)
		if(nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sortb)) == 1) { 
			plot_data <- julia_call("OSJ.TTESTB_plot", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]))
		}
	}
	else if(zmean) {
		results <- julia_call("OSJ.TTESTM", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]), tails)
		if(nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sortb)) == 1) { 
			plot_data <- julia_call("OSJ.TTEST_plot", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]))
		}
	}
	else {
		results <- julia_call("OSJ.TTEST", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]), tails)
		if(nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sortb)) == 1) { 
			plot_data <- julia_call("OSJ.TTEST_plot", as.matrix(sorta[,-c(1:3)]), as.matrix(sortb[,-c(1:3)]), as.matrix(refa[,-c(1:3)]), as.matrix(refb[,-c(1:3)]))
		}
	}

	#transform numerical T/F to measurement names
	if(nrow(results) > 1) {
		measurements <- data.frame(results[,c(8:ncol(results))])
	}else {
		measurements <- data.frame(t(results[c(8:length(results))]))
	}

	measurement_names <- unique(c(colnames(sorta[,-c(1:3)]), colnames(sortb[,-c(1:3)])))
	if(sorta[results[,1],3][1] != sortb[results[,2],3][1]) {
		measurements[2] = 1
		measurement_names = c(measurement_names[1], measurement_names[3])
	} #if articulation test hack

	for(i in 1:ncol(measurements)) {
		measurements[measurements[,i] != 0,i] <- paste(measurement_names[i], " ", sep="")
		measurements[measurements[,i] == 0,i] <- ""
	}

	measurements <- do.call(paste0, measurements[c(1:ncol(measurements))])

	#format data.frame to return
	results_formatted <- data.frame(
		cbind(id_1 = sorta[results[,1],1], 
			element_1 = sorta[results[,1],3], 
			side_1 = sorta[results[,1],2], 
			id_2 = sortb[results[,2],1], 
			element_2 = sortb[results[,2],3], 
			side_2 = sortb[results[,2],2], 
			measurements = measurements, 
			p_value = round(results[,4], digits = 5), 
			mean = round(results[,5], digits = 4), 
			sd = round(results[,6], digits =4), 
			sample = results[,7]
		), 
		result = NA, 
		stringsAsFactors = FALSE
	)
	rejected <- results_formatted[results_formatted$measurements == "",1:7]
	results_formatted <- results_formatted[results_formatted$measurements != "",]

	#Append exclusion results
    results_formatted[results_formatted[,8] > alphalevel,12] <- "Cannot Exclude"
    results_formatted[results_formatted[,8] <= alphalevel,12] <- "Excluded"
	output_function(method = "options", options = data.frame(alphalevel = alphalevel, absolute_value = absolute, zero_mean = zmean, tails = tails, yeojohnson = yeojohnson, reference = reference), fpath=sd)
	output_function(results_formatted, rejected = rejected, method="exclusion", type="csv",fpath=sd)
	if(nrow(as.matrix(sorta)) == 1 && nrow(as.matrix(sortb)) == 1) { 
		output_function(hera1 <- list(results_formatted[1,1], results_formatted[1,4], plot_data[1:nrow(plot_data)-1,], plot_data[nrow(plot_data),]), method="exclusion", type="plot", fpath=sd)
	}
	t_time <- end_time(start_time)
	return(list(direc,results_formatted[results_formatted$result == "Cannot Exclude",],results_formatted[results_formatted$result == "Excluded",], t_time, rejected))
}
