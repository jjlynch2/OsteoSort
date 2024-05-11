match.3d <- function(path1 = NULL, path2 = NULL, names1 = NULL, names2 = NULL, sessiontempdir = NULL, output_options = c(TRUE,TRUE,TRUE,FALSE), cores = 1, n_lowest_distances = 1) {
	print("Comparisons started")
	start_time <- start_time()
	workingdir = getwd()
	direc <- OsteoSort:::analytical_temp_space(output_options, sessiontempdir) #creates temporary space 
	sd <- paste(sessiontempdir, direc, sep="/")

	RJS(add_cores = cores) #reloads libraries and source code after cores are added

	results <- julia_call("OMS", path1, path2)

	results[,1] <- names1[as.numeric(results[,1])]
	results[,2] <- names2[as.numeric(results[,2])]
	matches <- rbind(results, cbind(results[,2], results[,1], results[,3]))

	resmatches <- array()
	for(a in unique(matches[,1])) {
		m <- matches[matches[,1] == a,]
		if(is.null(nrow(m))) {
			ind <- m
		}
		if(!is.null(nrow(m))) {
			ind <- m[order(as.numeric(m[,3]), decreasing=FALSE),][1:n_lowest_distances,]
		}
		resmatches <- rbind(resmatches, ind) 
		if(n_lowest_distances > 1) { #removes duplicate match from other direction
			for(bb in 1:nrow(ind)) {
				if(length(unique(OsteoSort:::m.row(ind[bb,], resmatches))) > 1) {
					resmatches <- resmatches[-unique(OsteoSort:::m.row(ind[bb,], resmatches))[-1],]
				}
			}
		}
		if(n_lowest_distances == 1) { #removes duplicate match from other direction
			if(length(unique(OsteoSort:::m.row(ind, resmatches))) > 1) {
				resmatches <- resmatches[-unique(OsteoSort:::m.row(ind, resmatches))[-1],]
			}
		}
	}
	resmatches <- resmatches[-1,] #remove NA row
	if(is.null(nrow(resmatches))) {names(resmatches) <- c("ID", "Match-ID", "Distance")}
	if(!is.null(nrow(resmatches))) {colnames(resmatches) <- c("ID", "Match-ID", "Distance")}

	no_return_value <- OsteoSort:::output_function(method = "options", options = data.frame(lowest_distance = n_lowest_distances), fpath=sd)
	if(output_options[1]) {no <- OsteoSort:::output_function(resmatches, method="3D", type="csv-res", fpath=sd)}
	if(output_options[2]) {no <- OsteoSort:::output_function(matches, method="3D", type="csv-all", fpath=sd)}
	
	comparisons <- length(path1) * length(path2) #number of comparisons
	
	gc()
	print("Comparisons completed")
	t_time <- end_time(start_time)
	return(list(resmatches, direc, comparisons, matches, t_time))
}
