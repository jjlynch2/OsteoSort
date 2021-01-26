match.2d <- function(outlinedata = NULL, sessiontempdir = NULL, fragment = FALSE, output_options = c(TRUE,TRUE,TRUE,TRUE), iteration = 10, threads=1, n_lowest_distances = 1, hide_distances = FALSE, dist = "average") {
	print("Form comparisons started")
	start_time <- start_time()

	direc <- OsteoSort:::analytical_temp_space(output_options, sessiontempdir) #creates temporary space 
	sd <- paste(sessiontempdir, direc, sep="/")
	specmatrix <- outlinedata[[1]]
	matches1 <- array(NA,c((length(outlinedata[[2]])*length(outlinedata[[3]])), 3)) #side 1
	matches2 <- array(NA,c((length(outlinedata[[2]])*length(outlinedata[[3]])), 3)) #side 2
	matches <- array(NA,c((length(outlinedata[[2]])*length(outlinedata[[3]]))*2, 3)) #combines sides
	nz <- 1 #comparison counter

	pairwise_coords <- list() #saved pairwise registration
	pwc <- 1
	withProgress(message = '', detail = '', value = 1, min=0, max=length(outlinedata[[2]]) * length(outlinedata[[3]]), {
		for(z in 1:length(outlinedata[[2]])) {
			for(x in length(outlinedata[[2]])+1:length(outlinedata[[3]])) {
				zzz <- 0
				if(nrow(specmatrix[[z]]) >= nrow(specmatrix[[x]])) {moving <- specmatrix[[x]]; target <- specmatrix[[z]];zzz <- 1}
				if(nrow(specmatrix[[z]]) < nrow(specmatrix[[x]])) {moving <- specmatrix[[z]]; target <- specmatrix[[x]];zzz <- 2}
				moving <- Morpho::icpmat(moving, target, iterations = iteration, type = "rigid", threads=threads) 
				#identifies indices of fragmented ends
				r1 <- fragment_margins(moving)
				moving <- r1[[1]]
				moving_indices <- r1[[2]]
				r1 <- fragment_margins(target)
				target <- r1[[1]]
				target_indices <- r1[[2]]

				distance <- hausdorff_dist(moving, target, dist = dist, indices = list(moving_indices, target_indices), threads = threads)

				matches1[nz,] <- c(names(specmatrix)[[z]], names(specmatrix)[[x]], distance)
				matches2[nz,] <- c(names(specmatrix)[[x]], names(specmatrix)[[z]], distance)
				incProgress(amount = 1, message = paste("Specimens: ", names(specmatrix)[[z]], " - ", names(specmatrix)[[x]], " ", " distance: ", distance, sep=""), detail = '')
				print(paste("Specimens: ", names(specmatrix)[[z]], " - ", names(specmatrix)[[x]], " ", " distance: ", distance, sep=""))
				nz <- nz + 1

				#saves coords for output
				pairwise_coords[[pwc]] <- moving
				pairwise_coords[[pwc+1]] <- target
				if(zzz == 1) {names(pairwise_coords)[[pwc+1]] <- names(specmatrix)[[z]]; names(pairwise_coords)[[pwc]] <- names(specmatrix)[[x]]}
				if(zzz == 2) {names(pairwise_coords)[[pwc+1]] <- names(specmatrix)[[x]]; names(pairwise_coords)[[pwc]] <- names(specmatrix)[[z]]}
				pwc <- pwc + 2 #skips by 2 since we use two indices
			}
		}
	})
	coords <- pairwise_coords 
	matches <- rbind(matches1, matches2) #combine both directions

	resmatches <- array()
	for(a in unique(matches[,1])) {
		m <- matches[matches[,1] == a,]
		
		if(is.null(nrow(m))) {ind <- m}
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

	if(hide_distances) {resmatches[,3] <- "Hidden"}
	withProgress(message = '', detail = '', value = 1, min=0, max=6, {
		setProgress(value = 1, message = "Saving: settings.csv", detail='');no_return_value <- OsteoSort:::output_function(method = "options", options = data.frame(lowest_distance = n_lowest_distances, distance_type = "average"), fpath=sd)
		if(output_options[1]) {setProgress(value = 1, message = "Saving: potential-matches.csv", detail='');no <- OsteoSort:::output_function(resmatches, method="2D", type="csv-res", fpath=sd)}
		if(output_options[2]) {setProgress(value = 1, message = "Saving: all-distances.csv", detail='');no <- OsteoSort:::output_function(matches, method="2D", type="csv-all", fpath=sd)}
		if(output_options[3]) {setProgress(value = 1, message = "Saving: registration plots", detail='');no <- OsteoSort:::output_function(coords, method="2D", type="plot", fpath=sd)}
		if(output_options[4]) {setProgress(value = 1, message = "Saving: coordinates.tps", detail='');no <- OsteoSort:::output_function(coords, method="2D", type="coord", fpath=sd)}
		setProgress(value = 5, message = "Completed", detail='')
	})
	gc()
	comparisons <- length(outlinedata[[2]]) * length(outlinedata[[3]]) #number of comparisons
	print("Form comparisons completed")
	t_time <- end_time(start_time)
	return(list(coords,resmatches,direc,comparisons,matches,t_time))

}
