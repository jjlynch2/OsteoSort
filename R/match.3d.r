#' three-dimensional pair-match function
#' 
#'
#' @keywords match.3d
#' @export
#' @examples
#' match.3d()

match.3d <- function(data = NULL, min = 1e+15, sessiontempdir = NULL, output_options = c(TRUE,TRUE,TRUE,TRUE), iteration = 1, transformation = "rigid", threads = 1, test = "Hausdorff", n_lowest_distances = 1, hide_distances = FALSE, dist = "average", band_threshold = 4, band = TRUE, fragment = FALSE) {
	if(threads != julia_call("nprocs")) {
		print("Setting up Julia workers...")
		JuliaSetup(add_cores = threads, source = TRUE, recall_libraries = TRUE)
		print("Finished.")
	}

	print("Form comparisons started")
	options(stringsAsFactors = FALSE)

	if(fragment == "Complete") {fragment <- FALSE}
	if(fragment == "Fragmented") {fragment <- TRUE} 	

	dist <- tolower(dist)
	transformation <- tolower(transformation)
	workingdir = getwd()
	direc <- OsteoSort:::analytical_temp_space(output_options, sessiontempdir) #creates temporary space 

	list1 <- data[[1]]
	list2 <- data[[2]]

	matches1 <- array(NA,c(length(list1)*length(list2), 3)) #side 1
	matches2 <- array(NA,c(length(list1)*length(list2), 3)) #side 2
	matches <- array(NA,c(length(list1)*length(list2)*2, 3)) #combines sides
	nz <- 1 #comparison counter


	pairwise_coords <- list() #saved pairwise registration
	renderlist <- data.frame(0,0,0)
	pwc <- 1

	if(fragment) {
		for(i in 1:length(list1)) {
			for(x in 1:length(list2)) {
				list1[[i]][,2] <- list1[[i]][,2] * -1 #mirror

				L1 <- as.matrix(translation(list1[[i]][,c(1:3)]))
				L1p <- as.numeric(na.omit(list1[[i]][,c(4:6)]))
				L1p <- rbind(L1p, as.numeric(na.omit(list1[[i]][,c(7:9)])))
				L1p <- rbind(L1p, as.numeric(na.omit(list1[[i]][,c(10:12)])))
				L1mp <- as.numeric(na.omit(list1[[i]][,c(13)]))

				L1p[,2] <- L1p[,2] * -1 #mirror 

				L2 <- as.matrix(translation(list2[[i]][,c(1:3)]))
				L2p <- as.numeric(na.omit(list2[[i]][,c(4:6)]))
				L2p <- rbind(L2p, as.numeric(na.omit(list2[[i]][,c(7:9)])))
				L2p <- rbind(L2p, as.numeric(na.omit(list2[[i]][,c(10:12)])))
				L2mp <- as.numeric(na.omit(list2[[i]][,c(13)]))

				rot <- rotation(L1p, L2p)

				L1 <- L1 - rep(1,nrow(L1)) %*% t(mean_shape(L1p))
				L2 <- L2 - rep(1,nrow(L2)) %*% t(mean_shape(L2p))
				L2 <- L2 %*% rot$rotation

				L1 <- icpmat(L1, L2, type = transformation, threads = threads, iterations = iteration)
				lh_combined <- rbind(L1, L2)
				lh_combined <- pca_align(lh_combined)

				lhr <- nrow(L1)
				lhc <- nrow(lh_combined)

				L1 <- lh_combined[1:lhr,]
				L2 <- lh_combined[(lhr+1):lhc,]

				temp1 <- rep(FALSE, nrow(L1))
				temp1[L1mp] <- TRUE

				temp2 <- rep(FALSE, nrow(L2))
				temp2[L2mp] <- TRUE


				L1 <- cbind(L1,temp1)
				L2 <- cbind(L2,temp2)

				centroid <- apply(lh_combined, 2, mean) #centroid of combined 
				tt <- L1[L1[,1] <= centroid[1]+band_threshold, ]
				a1 <- tt[tt[,1] >= centroid[1]-band_threshold, ]

				tt <- L1[L1[,2] <= centroid[2]+band_threshold, ]
				a2 <- tt[tt[,2] >= centroid[2]-band_threshold, ]

				tt <- L1[L1[,3] <= centroid[3]+band_threshold, ]
				a3 <- tt[tt[,3] >= centroid[3]-band_threshold, ]
				a <- rbind(a1, a2, a3)


				tt <- L2[L2[,1] <= centroid[1]+band_threshold, ]
				b1 <- tt[tt[,1] >= centroid[1]-band_threshold, ]

				tt <- L2[L2[,2] <= centroid[2]+band_threshold, ]
				b2 <- tt[tt[,2] >= centroid[2]-band_threshold, ]

				tt <- L2[L2[,3] <= centroid[3]+band_threshold, ]
				b3 <- tt[tt[,3] >= centroid[3]-band_threshold, ]
				b <- rbind(b1, b2, b3)

				moving_indices <- matrix(which(a[,4] == 1))
				target_indices <- matrix(which(b[,4] == 1))
				moving <- a[,-4]
				target <- b[,-4]

				tte <- remove_fragmented_margins(moving, target, list(moving_indices, target_indices))
				dd <- mean(mean(tte[[1]]), mean(tte[[2]]))


				pairwise_coords[[pwc+1]] <- target
				pairwise_coords[[pwc]] <- moving
				names(pairwise_coords)[[pwc]] <- names(list1)[i]
				names(pairwise_coords)[[pwc+1]] <- names(list2)[x]

				renderlist[nz,] <- rbind(pwc, pwc+1, paste(names(list1)[i], names(list2)[x], sep="_"))

				pwc <- pwc + 2 #skips by 2 since we use two indices

				matches1[nz,] <- c(names(list1)[i], names(list2)[x], dd)
				matches2[nz,] <- c(names(list2)[x], names(list1)[i], dd)
				print(paste("Specimens: ", names(list1)[i], " - ", names(list2)[x], " ", test, " distance: ", dd, sep=""))
				nz <- nz + 1
			}
		}
	} #end of fragment if


	if(!fragment) {
		lista <- list()
		listb <- list()
		for(i in 1:length(list1)) {
			A <- list1[[i]][,c(1:3)]
			A <- OsteoSort::pca_align(A)
			
			if(band == TRUE) {
				centroid <- apply(A, 2, mean)

				t1 <- A[A[,1] <= centroid[1]+band_threshold, ]
				test1 <- t1[t1[,1] >= centroid[1]-band_threshold, ]
				t2 <- A[A[,2] <= centroid[2]+band_threshold, ]
				test2 <- t2[t2[,2] >= centroid[2]-band_threshold, ]
				t3 <- A[A[,3] <= centroid[3]+band_threshold, ]
				test3 <- t3[t3[,3] >= centroid[3]-band_threshold, ]
				lista[[i]] <- rbind(test1, test2, test3)
			}
			if(band == FALSE) {
				lista[[i]] <- as.matrix(A)
			}
		}

		for(i in 1:length(list2)) {
			B <- list2[[i]][,c(1:3)]
			B <- OsteoSort::pca_align(B)

			if(band == TRUE) {
				centroid <- apply(B, 2, mean)

				t1 <- B[B[,1] <= centroid[1]+band_threshold, ]
				test1 <- t1[t1[,1] >= centroid[1]-band_threshold, ]
				t2 <- B[B[,2] <= centroid[2]+band_threshold, ]
				test2 <- t2[t2[,2] >= centroid[2]-band_threshold, ]
				t3 <- B[B[,3] <= centroid[3]+band_threshold, ]
				test3 <- t3[t3[,3] >= centroid[3]-band_threshold, ]
				listb[[i]] <- rbind(test1, test2, test3)
			}
			if(band == FALSE) {
				listb[[i]] <- as.matrix(B)
			}
		}


		for(i in 1:length(lista)) {
			for(x in 1:length(listb)) {
				lt <- icpmat(lista[[i]], listb[[x]], iterations = iteration, type = transformation, threads = threads)
				d1 <- hausdorff_dist(lt, listb[[x]], test = test, dist = dist)

				lt1 <- cbind( lista[[i]][,1]*-1, lista[[i]][,2],lista[[i]][,3])
				lt1 <- icpmat(lt1, listb[[x]], iterations = iteration, type = transformation, threads = threads)
				d2 <- hausdorff_dist(lt1, listb[[x]], test = test, dist = dist)

				lt2 <- cbind( lista[[i]][,1], lista[[i]][,2]*-1,lista[[i]][,3])
				lt2 <- icpmat(lt2, listb[[x]], iterations = iteration, type = transformation, threads = threads)
				d3 <- hausdorff_dist(lt2, listb[[x]], test = test, dist = dist)

				lt3 <- cbind( lista[[i]][,1], lista[[i]][,2],lista[[i]][,3]*-1)
				lt3 <- icpmat(lt3, listb[[x]], iterations = iteration, type = transformation, threads = threads)
				d4 <- hausdorff_dist(lt3, listb[[x]], test = test, dist = dist)

				lt4 <- cbind( lista[[i]][,1]*-1, lista[[i]][,2]*-1,lista[[i]][,3])
				lt4 <- icpmat(lt4, listb[[x]], iterations = iteration, type = transformation, threads = threads)
				d5 <- hausdorff_dist(lt4, listb[[x]], test = test, dist = dist)

				lt5 <- cbind( lista[[i]][,1]*-1, lista[[i]][,2],lista[[i]][,3]*-1)
				lt5 <- icpmat(lt5, listb[[x]], iterations = iteration, type = transformation, threads = threads)
				d6 <- hausdorff_dist(lt5, listb[[x]], test = test, dist = dist)

				lt6 <- cbind( lista[[i]][,1], lista[[i]][,2]*-1,lista[[i]][,3]*-1)
				lt6 <- icpmat(lt6, listb[[x]], iterations = iteration, type = transformation, threads = threads)
				d7 <- hausdorff_dist(lt6, listb[[x]], test = test, dist = dist)

				lt7 <- cbind( lista[[i]][,1]*-1, lista[[i]][,2]*-1,lista[[i]][,3]*-1)
				lt7 <- icpmat(lt7, listb[[x]], iterations = iteration, type = transformation, threads = threads)
				d8 <- hausdorff_dist(lt7, listb[[x]], test = test, dist = dist)

				if(d1 < d2 && d1 < d3 && d1 < d4 && d1 < d5 && d1 < d6 && d1 < d7 && d1 < d8) {dd <- d1; pairwise_coords[[pwc]] <- lt}
				if(d2 < d1 && d2 < d3 && d2 < d4 && d2 < d5 && d2 < d6 && d2 < d7 && d2 < d8) {dd <- d2; pairwise_coords[[pwc]] <- lt1}
				if(d3 < d2 && d3 < d1 && d3 < d4 && d3 < d5 && d3 < d6 && d3 < d7 && d3 < d8) {dd <- d3; pairwise_coords[[pwc]] <- lt2}
				if(d4 < d2 && d4 < d1 && d4 < d3 && d4 < d5 && d4 < d6 && d4 < d7 && d4 < d8) {dd <- d4; pairwise_coords[[pwc]] <- lt3}
				if(d5 < d2 && d5 < d1 && d5 < d3 && d5 < d4 && d5 < d6 && d5 < d7 && d5 < d8) {dd <- d5; pairwise_coords[[pwc]] <- lt4}
				if(d6 < d2 && d6 < d1 && d6 < d3 && d6 < d5 && d6 < d4 && d6 < d7 && d6 < d8) {dd <- d6; pairwise_coords[[pwc]] <- lt5}
				if(d7 < d2 && d7 < d1 && d7 < d3 && d7 < d5 && d7 < d6 && d7 < d4 && d7 < d8) {dd <- d7; pairwise_coords[[pwc]] <- lt6}
				if(d8 < d2 && d8 < d1 && d8 < d3 && d8 < d5 && d8 < d6 && d8 < d4 && d8 < d7) {dd <- d8; pairwise_coords[[pwc]] <- lt7}


				pairwise_coords[[pwc+1]] <- listb[[x]]
				names(pairwise_coords)[[pwc]] <- names(list1)[i]
				names(pairwise_coords)[[pwc+1]] <- names(list2)[x]

				renderlist[nz,] <- rbind(pwc, pwc+1, paste(names(list1)[i], names(list2)[x], sep="_"))

				pwc <- pwc + 2 #skips by 2 since we use two indices

				matches1[nz,] <- c(names(list1)[i], names(list2)[x], dd)
				matches2[nz,] <- c(names(list2)[x], names(list1)[i], dd)
				print(paste("Specimens: ", names(list1)[i], " - ", names(list2)[x], " ", test, " distance: ", dd, sep=""))
				nz <- nz + 1
			}
		}
	} #end of non-fragment if


	matches <- rbind(matches1, matches2)

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

		if(output_options[4]) {
			plotmatches <- array()
			if(is.null(nrow(m))) { 
				indplot <- m
			}
			if(!is.null(nrow(m))) {
				indplot <- m[order(as.numeric(m[,3]), decreasing=FALSE),]
			}
			plotmatches <- rbind(plotmatches, indplot)
			plotmatches <- plotmatches[-1,]
			for(ff in 1:nrow(indplot)) {
				if(length(unique(OsteoSort:::m.row(indplot[ff,], plotmatches))) > 1) {
					temp_plot <- plotmatches[-unique(OsteoSort:::m.row(indplot[ff,], plotmatches))[-1],]
				}
				else temp_plot <- plotmatches 					
			}
			no <- OsteoSort:::output_function(temp_plot, method = "3D", type = "dot")
		}
	}
	resmatches <- resmatches[-1,] #remove NA row
	if(is.null(nrow(resmatches))) {names(resmatches) <- c("ID", "Match-ID", "Distance")}
	if(!is.null(nrow(resmatches))) {colnames(resmatches) <- c("ID", "Match-ID", "Distance")}


	if(hide_distances) {resmatches[,3] <- "Hidden"}
	if(output_options[1]) {no <- OsteoSort:::output_function(resmatches, method="3D", type="csv-res")}
	if(output_options[2]) {no <- OsteoSort:::output_function(matches, method="3D", type="csv-all")}
	if(output_options[3]) {no <- OsteoSort:::output_function(pairwise_coords, method="3D", type="coord")}

	comparisons <- length(list1) * length(list2) #number of comparisons

	gc()
	setwd(workingdir)

	print("Form comparisons completed")	
	options(stringsAsFactors = TRUE) #restore default R  
	return(list(pairwise_coords, resmatches, direc, comparisons, matches, renderlist))

}