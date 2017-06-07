#' two-dimensional pair-match function
#' 
#' @param outlinedata The outline data taken from outline.images()
#' @param min minimum distance for ICP
#' @param iteration The number of iterations for Iterative Closest Point
#' @param transformation The type of Iterative Closest Point transformation ("Rigid", "Similarity", "Affine")
#' @param cores Number of cores for parallel processing
#' @param mean_iterations The number of mean iterations
#' @param test Specifies the distance calculation ("Segmented-Hausdorff", "Hausdorff")
#' @param sessiontempdir Specifies temporary directory for analytical session if stdout is false
#' @param stdout If true, output will be data.frames only
#' @param hide_distances Hides the distance values in short lists to avoid analytical bias 
#' @param n_lowest_distances The number of lowest distance matches to return as a potential match
#' @param plot Plots results
#' @param temporary_mean_specimen The specimen to be used as the temporary mean
#' @param output_options If true, writes to .csv file
#'
#' @keywords match.2d.invariant
#' @export
#' @examples
#' match.2d.invariant()

match.2d.invariant <- function(outlinedata = NULL, min = 1e+15, stdout = TRUE, sessiontempdir = NULL, output_options = FALSE, iteration = 10, transformation = "rigid", cores=1, test = "Segmented-Hausdorff", temporary_mean_specimen = 1, mean_iterations = 5, plot = FALSE, n_lowest_distances = 1, hide_distances = FALSE) {
	print("Two-dimensional pair match comparisons have started.")	
	library(Morpho)
	library(pracma)
	library(shapes)
	suppressMessages(library(compiler))
	enableJIT(3)

	workingdir = getwd()

	if(!stdout) { 
		if (!is.null(sessiontempdir)) {
			setwd(sessiontempdir)
		}
		direc <- randomstring(n = 1, length = 12)
		dir.create(direc)
		setwd(direc)
	}

	
	specmatrix <- outlinedata[[1]]

	homolog <<- array(NA,c(dim(specmatrix)[1], dim(specmatrix)[2], dim(specmatrix)[3]))
	namess <- dimnames(specmatrix)[[3]] #capture specimen names
	
	meann <<- specmatrix[,,temporary_mean_specimen]
	homolog <<- specmatrix
	
	#shifts to long axis of specimens#
	D <- distmat(meann, meann)
	index <- apply(as.matrix(apply(D, 2, max)), 2, which.max)
	shiftm <- function(d, k) rbind(tail(d,k), head(d,-k), deparse.level = 0)
	index <- shiftm(as.matrix(1:nrow(meann)), -index)
	meann <<- meann[index,]
	#shifts to long axis of specimens#

	for(b in 1:mean_iterations) {
		target <<- meann
		for(i in 1:dim(homolog)[3]) {
			moving <<- homolog[,,i]
			temp <<- icpmat(moving, target, iterations = iteration, mindist = min, type = transformation, threads=cores)
			homolog[,,i] <<- shiftmatrices(first_configuration = temp, second_configuration = target, cores) #shifts matrices to match
			print(dimnames(homolog)[[3]][i])
		}
		
		meann <<- apply(homolog, c(1,2), mean)
	}



	
	dimnames(homolog)[[3]] <- namess #set specimen names again

#do these need to be global?
	matches <<- array(NA,c(dim(homolog)[3]*dim(homolog)[3], 3))
	nz <<- 1

	for(z in 1:length(outlinedata[[2]])) {
		for(x in length(outlinedata[[2]])+1:length(outlinedata[[3]])) {
			distance <- segmented_hausdorff_dist(homolog[,,z], homolog[,,x], test = test)
			matches[nz,] <- c(dimnames(homolog)[[3]][z], dimnames(homolog)[[3]][x], distance)
			print(distance)
			nz <<- nz + 1
		}
	}

	for(z in length(outlinedata[[2]])+1:length(outlinedata[[3]])) {
		for(x in 1:length(outlinedata[[2]])) {
			distance <- segmented_hausdorff_dist(homolog[,,z], homolog[,,x], test = test)
			matches[nz,] <- c(dimnames(homolog)[[3]][z], dimnames(homolog)[[3]][x], distance)
			print(distance)
			nz <<- nz + 1
		}
	}

	resmatches <- array()
	for(a in dimnames(homolog)[[3]]) {
		m <- matches[matches[,1] == a,]
		resmatches <- rbind(resmatches, m[order(m[,3], decreasing=FALSE),][1:n_lowest_distances,])
		
	}
	resmatches <- resmatches[-1,]

	if(plot) {
		plot(meann, col="white", xlim=c(min(homolog),max(homolog)), ylim=c(max(homolog),min(homolog)), xlab="", ylab="")
		for(a in 1:dim(homolog)[3]) {
			points(homolog[,,a], col=a)	
		}
		points(meann, col="black", bg="blue", pch=23)
	}

	colnames(resmatches) <- c("ID", "Match-ID", "Distance")
	print("Two-dimensional pair match comparisons have completed.")	

	if(output_options) {
		write.csv(matches, file = "potential-matches.csv", row.names=FALSE, col.names=TRUE)
		png(filename="registration.png")
		plot(meann, col="white", xlim=c(min(homolog),max(homolog)), ylim=c(max(homolog),min(homolog)), xlab="", ylab="")
		for(a in 1:dim(homolog)[3]) {
			points(homolog[,,a], col=a)	
		}
		points(meann, col="black", bg="blue", pch=23)
		dev.off()
	}
	gc()
	setwd(workingdir)
	enableJIT(0)
	if(hide_distances) {resmatches[,3] <- "Hidden"}
	return(list(homolog,resmatches,direc,nz))

}