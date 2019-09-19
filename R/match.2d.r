#' two-dimensional pair-match function
#' 
#' @param outlinedata The outline data taken from outline.images()
#' @param min minimum distance for ICP
#' @param iteration The number of iterations for Iterative Closest Point
#' @param transformation The type of Iterative Closest Point transformation ("Rigid", "Similarity", "Affine")
#' @param threads The number of threads to use
#' @param mean_iterations The number of mean iterations
#' @param test Specifies the distance calculation ("Segmented-Hausdorff", "Hausdorff", "Uni-Hausdorff")
#' @param sessiontempdir Specifies temporary directory for analytical session
#' @param hide_distances Hides the distance values in short lists to avoid analytical bias 
#' @param n_lowest_distances The number of lowest distance matches to return as a potential match
#' @param temporary_mean_specimen The specimen to be used as the temporary mean
#' @param output_options C(TRUE,FALSE) First logic specifies excel output for matches, second specifies excel output for all distances, third specifies registered plots, and fourth specifies TPS coordinate files
#' @param dist Specifies distance type either maximum, average, or dilated
#' @param n_regions Specifies the number of regions for Segmented-Hausdorff
#' @param fragment TRUE FALSE specifies if complete or fragmented registration matching procedure should be conducted
#'
#' @keywords match.2d
#' @export
#' @examples
#' match.2d()

match.2d <- function(outlinedata = NULL, min = 1e+15, sessiontempdir = NULL, fragment = FALSE, output_options = c(TRUE,TRUE,TRUE,TRUE), iteration = 10, transformation = "rigid", threads=1, test = "Hausdorff", temporary_mean_specimen = 1, mean_iterations = 5, n_lowest_distances = 1, hide_distances = FALSE, n_regions = 6, dist = "average") {
	if(threads != julia_call("nprocs")) {
		print("Setting up Julia workers...")
		JuliaSetup(add_cores = threads, source = TRUE, recall_libraries = TRUE)
		print("Finished.")
	}

	print("Form comparisons started")
	start_time <- start_time()
	dist <- tolower(dist)
	transformation <- tolower(transformation)
	workingdir = getwd()
	direc <- OsteoSort:::analytical_temp_space(output_options, sessiontempdir) #creates temporary space 
	specmatrix <- outlinedata[[1]]
	matches1 <- array(NA,c((length(outlinedata[[2]])*length(outlinedata[[3]])), 3)) #side 1
	matches2 <- array(NA,c((length(outlinedata[[2]])*length(outlinedata[[3]])), 3)) #side 2
	matches <- array(NA,c((length(outlinedata[[2]])*length(outlinedata[[3]]))*2, 3)) #combines sides
	nz <- 1 #comparison counter
	if(!fragment){
		meann <- specmatrix[,,temporary_mean_specimen]
		homolog <- specmatrix

		#shifts to long axis of specimens#
		D <- e_dist(meann, meann)
		index <- apply(as.matrix(apply(D, 2, max)), 2, which.max)
		shiftm <- function(d, k) rbind(tail(d,k), head(d,-k), deparse.level = 0)
		index <- shiftm(as.matrix(1:nrow(meann)), -index)
		meann <- meann[index,]
		#shifts to long axis of specimens#

		for(b in 1:mean_iterations) {
			target <- meann
			for(i in 1:dim(homolog)[3]) {
				print(paste("Registering specimen: ", dimnames(homolog)[[3]][i], " mean iteration: ", b, sep=""))
				moving <- homolog[,,i]
				temp <- Morpho::icpmat(moving, target, iterations = iteration, mindist = min, type = transformation, threads=threads)
				homolog[,,i] <- shiftmatrices(first_configuration = temp, second_configuration = target, threads) #shifts matrices to match
			}
			meann <- apply(homolog, c(1,2), mean)
			#shifts to long axis of specimens#
			D <- e_dist(meann, meann)
			index <- apply(as.matrix(apply(D, 2, max)), 2, which.max)
			shiftm <- function(d, k) rbind(tail(d,k), head(d,-k), deparse.level = 0)
			index <- shiftm(as.matrix(1:nrow(meann)), -index)
			meann <- meann[index,]
			#shifts to long axis of specimens#
		}

		for(z in 1:length(outlinedata[[2]])) {
			for(x in length(outlinedata[[2]])+1:length(outlinedata[[3]])) {
				distance <- hausdorff_dist(homolog[,,z], homolog[,,x], test = test, n_regions = n_regions, dist = dist)
				matches1[nz,] <- c(dimnames(homolog)[[3]][z], dimnames(homolog)[[3]][x], distance)
				matches2[nz,] <- c(dimnames(homolog)[[3]][x], dimnames(homolog)[[3]][z], distance)
				print(paste("Specimens: ", dimnames(homolog)[[3]][x], "-", dimnames(homolog)[[3]][z], " ", test, " distance: ", distance, sep=""))
				nz <- nz + 1
			}
		}
		coords <- homolog
		matches <- rbind(matches1, matches2) #combine both directions
	}#complete

	if(fragment) {
		pairwise_coords <- list() #saved pairwise registration
		pwc <- 1
		for(z in 1:length(outlinedata[[2]])) {
			for(x in length(outlinedata[[2]])+1:length(outlinedata[[3]])) {
				zzz <- 0
				if(nrow(specmatrix[[z]]) >= nrow(specmatrix[[x]])) {moving <- specmatrix[[x]]; target <- specmatrix[[z]];zzz <- 1}
				if(nrow(specmatrix[[z]]) < nrow(specmatrix[[x]])) {moving <- specmatrix[[z]]; target <- specmatrix[[x]];zzz <- 2}
				moving <- Morpho::icpmat(moving, target, iterations = iteration, mindist = min, type = transformation, threads=threads) 
				#identifies indices of fragmented ends
				r1 <- fragment_margins(moving)
				moving <- r1[[1]]
				moving_indices <- r1[[2]]
				r1 <- fragment_margins(target)
				target <- r1[[1]]
				target_indices <- r1[[2]]

				distance <- hausdorff_dist(moving, target, test = test, dist = dist, indices = list(moving_indices, target_indices))

				matches1[nz,] <- c(names(specmatrix)[[z]], names(specmatrix)[[x]], distance)
				matches2[nz,] <- c(names(specmatrix)[[x]], names(specmatrix)[[z]], distance)
				print(paste("Specimens: ", names(specmatrix)[[z]], " - ", names(specmatrix)[[x]], " ", test, " distance: ", distance, sep=""))
				nz <- nz + 1

				#saves coords for output
				pairwise_coords[[pwc]] <- moving
				pairwise_coords[[pwc+1]] <- target
				if(zzz == 1) {names(pairwise_coords)[[pwc+1]] <- names(specmatrix)[[z]]; names(pairwise_coords)[[pwc]] <- names(specmatrix)[[x]]}
				if(zzz == 2) {names(pairwise_coords)[[pwc+1]] <- names(specmatrix)[[x]]; names(pairwise_coords)[[pwc]] <- names(specmatrix)[[z]]}
				pwc <- pwc + 2 #skips by 2 since we use two indices
			}
		}

		coords <- pairwise_coords 
		matches <- rbind(matches1, matches2) #combine both directions
	}#fragment

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
	if(output_options[1]) {no <- OsteoSort:::output_function(resmatches, method="2D", type="csv-res")}
	if(output_options[2]) {no <- OsteoSort:::output_function(matches, method="2D", type="csv-all")}
	if(output_options[3]) {no <- OsteoSort:::output_function(coords, method="2D", type="plot")}
	if(output_options[4]) {no <- OsteoSort:::output_function(coords, method="2D", type="coord")}

	gc()
	setwd(workingdir)

	comparisons <- length(outlinedata[[2]]) * length(outlinedata[[3]]) #number of comparisons

	print("Form comparisons completed")
	t_time <- end_time(start_time)
	return(list(coords,resmatches,direc,comparisons,matches,t_time))

}
