#' two-dimensional pair-match function
#' 
#' @param outlinedata The outline data taken from outline.images()
#' @param min minimum distance for ICP
#' @param iteration The number of iterations for Iterative Closest Point
#' @param transformation The type of Iterative Closest Point transformation ("Rigid", "Similarity", "Affine")
#' @param cores Number of cores for parallel processing
#' @param mean_iterations The number of mean iterations
#' @param test Specifies the distance calculation ("Segmented-Hausdorff", "Hausdorff")
#' @param sessiontempdir Specifies temporary directory for analytical session
#' @param hide_distances Hides the distance values in short lists to avoid analytical bias 
#' @param n_lowest_distances The number of lowest distance matches to return as a potential match
#' @param temporary_mean_specimen The specimen to be used as the temporary mean
#' @param output_options If true, writes to .csv file
#' @param dist Specifies distance per region, either maximum or average distance
#' @param n_regions Specifies number of regions per Segmented-Hausdorff
#' @param fragment TRUE FALSE specifies if complete or fragmented registration matching procedure should be conducted
#'
#' @keywords match.2d
#' @export
#' @examples
#' match.2d()

match.2d <- function(outlinedata = NULL, min = 1e+15, sessiontempdir = NULL, fragment = FALSE, output_options = c(TRUE,TRUE,TRUE,TRUE), iteration = 10, transformation = "rigid", cores=1, test = "Segmented-Hausdorff", temporary_mean_specimen = 1, mean_iterations = 20, n_lowest_distances = 1, hide_distances = FALSE, n_regions = 6, dist = "average") {
	print("Two-dimensional pair match comparisons have started.")	

	suppressMessages(library(compiler))
	enableJIT(3)

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
				print(paste("specimen: ", dimnames(homolog)[[3]][i], " iteration: ", b, sep=""))
				moving <- homolog[,,i]
				temp <- icpmat(moving, target, iterations = iteration, mindist = min, type = transformation, threads=cores)
				homolog[,,i] <- shiftmatrices(first_configuration = temp, second_configuration = target, cores) #shifts matrices to match
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
				print(paste(dimnames(homolog)[[3]][x], "-", dimnames(homolog)[[3]][z], " ", test, " distance: ", distance, sep=""))
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
		
				moving <- icpmat(moving, target, iterations = iteration, mindist = min, type = transformation, threads=cores) 
				#trims from one spec to the other
				t1 <- target[target[,1] >= min(moving[,1]), ]
				t1 <- t1[t1[,2] >= min(moving[,2]), ]
				t1 <- target[target[,1] <= max(moving[,1]), ]
				t1 <- t1[t1[,2] <= max(moving[,2]), ]
				#trims opposite direction
				t2 <- moving[moving[,1] >= min(target[,1]), ]
				t2 <- t2[t2[,2] >= min(target[,2]), ]
				t2 <- moving[moving[,1] <= max(target[,1]), ]
				t2 <- t2[t2[,2] <= max(target[,2]), ]

				#finds smallest fragment among the comparison
				if(nrow(t1) >= nrow(t2)) {moving <- t1; target <- t2}		
				if(nrow(t1) <= nrow(t2)) {moving <- t2; target <- t1}	

				distance <- hausdorff_dist(moving, target, test = test, dist = dist)
				matches1[nz,] <- c(names(specmatrix)[[z]], names(specmatrix)[[x]], distance)
				matches2[nz,] <- c(names(specmatrix)[[x]], names(specmatrix)[[z]], distance)
				print(paste(names(specmatrix)[[z]], " - ", names(specmatrix)[[x]], " ", test, " distance: ", distance, sep=""))
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
		ind <- m[order(as.numeric(m[,3]), decreasing=FALSE),][1:n_lowest_distances,] #does as.numeric work for both registrations?
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
	colnames(resmatches) <- c("ID", "Match-ID", "Distance")

	if(hide_distances) {resmatches[,3] <- "Hidden"}
	if(output_options[1]) {no <- OsteoSort:::output_function(resmatches, method="2D", type="csv-res")}
	if(output_options[2]) {no <- OsteoSort:::output_function(matches, method="2D", type="csv-all")}
	if(output_options[3]) {no <- OsteoSort:::output_function(coords, method="2D", type="plot")}
	if(output_options[4]) {no <- OsteoSort:::output_function(coords, method="2D", type="coord")}

	gc()
	setwd(workingdir)
	enableJIT(0)

	comparisons <- length(outlinedata[[2]]) * length(outlinedata[[3]]) #number of comparisons

	print("Two-dimensional pair match comparisons have completed.")	

	return(list(coords,resmatches,direc,comparisons,matches))

}