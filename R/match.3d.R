#' three-dimensional pair-match function
#' 
#' @param outlinedata The outline data taken from outline.images()
#' @param min minimum distance for ICP
#' @param iteration The number of iterations for Iterative Closest Point
#' @param transformation The type of Iterative Closest Point transformation ("Rigid", "Similarity", "Affine")
#' @param threads Number of threads for parallel processing
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

match.3d <- function(outlinedata = NULL, min = 1e+15, sessiontempdir = NULL, fragment = FALSE, output_options = c(TRUE,TRUE,TRUE,TRUE), iteration = 10, transformation = "rigid", threads=1, test = "Hausdorff", temporary_mean_specimen = 1, mean_iterations = 5, n_lowest_distances = 1, hide_distances = FALSE, n_regions = 6, dist = "average") {
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


	if(!pairwise) {

		mean <- specmatrix[[temporary_mean_specimen]]  #start mean with element that has the most landmarks

		#align along principal axes and mirror one side
		if(pcaalign) {
			for(z in 1:length(outlinedata[[2]])) {
				specmatrix[[z]] <- pca_align(specmatrix[[z]], mirror=TRUE)
			}
			for(x in length(outlinedata[[2]])+1:length(outlinedata[[3]])) {
				specmatrix[[x]] <- pca_align(specmatrix[[x]], mirror=FALSE)
			}
		}

		#register to estimated mean
		array3d <- array(NA,c(nrow(mean), 3, length(specmatrix))) #temporary 3D array storage
		for(i in 1:mean_iterations) {
			for(x in 1:length(specmatrix)) {
				specmatrix[[x]] <- icpmat(specmatrix[[x]], mean, iterations=iteration, type=transformation, threads = threads) 
				index1 <- mcNNindex(mean, specmatrix[[x]], k = 1, threads = threads) 
				index2 <- mcNNindex(specmatrix[[x]], mean[index1,], k = 1, threads = threads) 
				temp <- mean 

				temp[index1,] <- specmatrix[[x]][index2,]
				array3d[,,x] <- temp
			}
			mean <- apply(array3d, c(1,2), mean)
		}

		for(z in 1:length(outlinedata[[2]])) {
			for(x in length(outlinedata[[2]])+1:length(outlinedata[[3]])) {

				#mutual nearest neighbor!
				index1 <- mcNNindex(specmatrix[[x]], specmatrix[[z]], k=1, threads = threads)
				index2 <- mcNNindex(specmatrix[[z]], specmatrix[[x]][index1,], k=1, threads = threads)
				specmatrix[[z]] <- specmatrix[[z]][index2,]
				specmatrix[[x]] <- specmatrix[[x]][index1,]



				distance <- hausdorff_dist(t1, t2, test = test, dist = dist)
				matches1[nz,] <- c(names(specmatrix)[[z]], names(specmatrix)[[x]], distance)
				matches2[nz,] <- c(names(specmatrix)[[x]], names(specmatrix)[[z]], distance)
				print(paste(names(specmatrix)[[z]], " - ", names(specmatrix)[[x]], " ", test, " distance: ", distance, sep=""))
				nz <- nz + 1
			}
		}
		matches <- rbind(matches1, matches2) #combine both directions
		coords <- specmatrix
	}

	if(pairwise) {
		for(z in 1:length(outlinedata[[2]])) {
			for(x in length(outlinedata[[2]])+1:length(outlinedata[[3]])) {


				if(pcaalign) {
					moving <- pca_align(specmatrix[[z]])
					target <- pca_align(specmatrix[[x]])
				}
# 8 possible reflections...



if(any(a > 1)) {
	for(aa in a) {
		moving[,aa] <- -pca1[,aa]
	}
}

moving[,1] <- as.numeric(moving[,1])
moving[,2] <- as.numeric(moving[,2])
moving[,3] <- as.numeric(moving[,3])

				index1 <- mcNNindex(target, moving, k=1, threads = threads)
				index2 <- mcNNindex(moving, target[index1,], k=1, threads = threads)

				moving <- moving[index2,]
				target <- target[index1,]

				distance <- mean(sqrt(sum(moving - target)^2))

				matches1[nz,] <- c(names(specmatrix)[[z]], names(specmatrix)[[x]], distance)
				matches2[nz,] <- c(names(specmatrix)[[x]], names(specmatrix)[[z]], distance)
				print(paste(names(specmatrix)[[z]], " - ", names(specmatrix)[[x]], " distance: ", distance, sep=""))
				nz <- nz + 1

	
			}
		}
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

	if(output_options[4]) {no <- OsteoSort:::output_function(coords, method="2D", type="coord")}

	gc()
	setwd(workingdir)
	enableJIT(0)

	comparisons <- length(outlinedata[[2]]) * length(outlinedata[[3]]) #number of comparisons

	print("Two-dimensional pair match comparisons have completed.")	

	return(list(coords,resmatches,direc,comparisons,matches))

}