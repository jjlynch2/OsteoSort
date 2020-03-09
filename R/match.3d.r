match.3d <- function(data = NULL, min = 1e+15, sessiontempdir = NULL, labtf3d = TRUE, output_options = c(TRUE,TRUE,TRUE,FALSE,TRUE), iteration = 50, threads = 1, n_lowest_distances = 1, hide_distances = FALSE, dist = "average", band_threshold = 4, band = TRUE, fragment = FALSE) {
	print("Form comparisons started")
	start_time <- start_time()
	if(fragment == "Complete") {fragment <- FALSE}
	if(fragment == "Fragmented") {fragment <- TRUE}
	dist <- tolower(dist)
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

	if(fragment) {
		withProgress(message = '', detail = '', value = 1, min=0, max=length(list1) * length(list2), {
			for(z in 1:length(list1)) {
				for(i in 1:length(list2)) {
					section_save1 <- 99999999
					section_save2 <- 99999999
					section_d1 <- 9999999
					section_d1t <- 9999999
					section_savet <- c(0,0,0)
					section_savett <- c(0,0,0)
					if(ncol(list2[[i]]) == 3) {list2[[i]] <- cbind(list2[[i]],0)}
					if(ncol(list1[[z]]) == 3) {list1[[z]] <- cbind(list1[[z]],0)}
					n_splits <<- section_split(list2[[i]], list1[[z]])
					for(x in 1:length(n_splits[[1]])) {
						d1 <- 999999
						for(k in 1:8) {
							if (k == 1) {lt1 <- cbind( n_splits[[2]][,1], n_splits[[2]][,2],n_splits[[2]][,3], n_splits[[2]][,4])}
							else if (k == 2) {lt1 <- cbind( n_splits[[2]][,1]*-1, n_splits[[2]][,2]*-1,n_splits[[2]][,3]*-1, n_splits[[2]][,4])}
							else if (k == 3) {lt1 <- cbind( n_splits[[2]][,1], n_splits[[2]][,2]*-1,n_splits[[2]][,3]*-1, n_splits[[2]][,4])}
							else if (k == 4) {lt1 <- cbind( n_splits[[2]][,1]*-1, n_splits[[2]][,2],n_splits[[2]][,3]*-1, n_splits[[2]][,4])}
							else if (k == 5) {lt1 <- cbind( n_splits[[2]][,1]*-1, n_splits[[2]][,2]*-1,n_splits[[2]][,3], n_splits[[2]][,4])}
							else if (k == 6) {lt1 <- cbind( n_splits[[2]][,1], n_splits[[2]][,2],n_splits[[2]][,3]*-1, n_splits[[2]][,4])}
							else if (k == 7) {lt1 <- cbind( n_splits[[2]][,1], n_splits[[2]][,2]*-1,n_splits[[2]][,3], n_splits[[2]][,4])}
							else if(k == 8) {lt1 <- cbind( n_splits[[2]][,1]*-1, n_splits[[2]][,2],n_splits[[2]][,3], n_splits[[2]][,4])}
							lt <- icpmat(lt1[,1:3], n_splits[[1]][[x]][,1:3], iterations = iteration, type = "rigid", threads = threads)
							lh_combined <- rbind(lt,n_splits[[1]][[x]][,1:3])
							lh_combined <- pca_align(lh_combined)
							lhr <- nrow(lt)
							lhc <- nrow(lh_combined)
							centroid <- apply(lh_combined[,1:3], 2, mean)
							L1 <- lh_combined[1:lhr,]
							L2 <- lh_combined[(lhr+1):lhc,]
							A <- CentroidBand(cbind(L1,lt1[,4]), threshold = band_threshold, centroid = centroid)
							B <- CentroidBand(cbind(L2, n_splits[[1]][[x]][,4]), threshold = band_threshold, centroid = centroid)
							moving_indices <- matrix(which(A[,4] == 1))
							target_indices <- matrix(which(B[,4] == 1))
							tte <- remove_fragmented_margins(A[,1:3], B[,1:3], list(moving_indices, target_indices), threads = threads)
							d1t <- max(mean(tte[[1]]), mean(tte[[2]]))
							if(d1t < d1) {
								d1 <- d1t
								section_d1t <- d1
								section_savet <- cbind(lt, n_splits[[2]][,4])
								section_savett <- n_splits[[1]][[1]]
								for(ii in 2:length(n_splits[[1]])) {
									section_savett <- rbind(section_savett, n_splits[[1]][[ii]])
								}
							}
						}
						if(section_d1t < section_d1) {
							section_d1 <- section_d1t
							section_save1 <- section_savet
							section_save2 <- section_savett
						}
					}
					ss2 <- section_save2[,4]
					lt <- icpmat(section_save1[,1:3], section_save2[,1:3], iterations = iteration, type = "rigid", threads = threads)
					lh_combined <- rbind(section_save1[,1:3],section_save2[,1:3])
					lh_combined <- pca_align(lh_combined)
					centroid <- apply(rbind(section_save1[,1:3],section_save2[,1:3]), 2, mean)
					lhr <- nrow(section_save1)
					lhc <- nrow(lh_combined)
					section_save1 <- lh_combined[1:lhr,]
					section_save2 <- lh_combined[(lhr+1):lhc,]
					A <- CentroidBand(cbind(section_save1, n_splits[[2]][,4]), threshold = band_threshold, centroid = centroid)
					B <- CentroidBand(cbind(section_save2,ss2), threshold = band_threshold, centroid = centroid)
					moving_indices <- matrix(which(A[,4] == 1))
					target_indices <- matrix(which(B[,4] == 1))
					tte <<- remove_fragmented_margins(A[,1:3], B[,1:3], list(moving_indices, target_indices), threads = threads)
					d1 <- max(mean(tte[[1]]), mean(tte[[2]]))
					write.tmp.data(A, B, paste(names(list2)[i], names(list1)[z], sep="-"), direc, sessiontempdir)
					renderlist[nz,] <- paste(names(list2)[i], names(list1)[z], sep="-")
					matches1[nz,] <- c(names(list2)[i], names(list1)[z], d1)
					matches2[nz,] <- c(names(list1)[z], names(list2)[i], d1)
					print(paste("Specimens: ", names(list2)[i], " - ", names(list1)[z], " ", "Hausdorff", " distance: ", d1, sep=""))
					incProgress(amount = 1, message = paste("Specimens: ", names(list2)[i], " - ", names(list1)[z], " ", "Hausdorff", " distance: ", d1, sep=""), detail = '')
					nz <- nz + 1
				}
			}
		})
	}
	if(!fragment) {
		lista <- list()
		listb <- list()
		withProgress(message = '', detail = '', value = 1, min=0, max=length(list1) * length(list2), {
			for(i in 1:length(list1)) {
				incProgress(amount = i, message = paste("Extracting centroid band: ", names(list1)[i], sep=""), detail = '')
				A <- list1[[i]][,c(1:3)]
				A <- OsteoSort::pca_align(A)
				if(band == TRUE) {
					lista[[i]] <- CentroidBand(A, band_threshold)
				}
				if(band == FALSE) {
					lista[[i]] <- as.matrix(A)
				}
			}
			for(i in 1:length(list2)) {
				incProgress(amount = i, message = paste("Extracting centroid band: ", names(list2)[i], sep=""), detail = '')
				B <- list2[[i]][,c(1:3)]
				B <- OsteoSort::pca_align(B)
				if(band == TRUE) {
					listb[[i]] <- CentroidBand(B, band_threshold)
				}
				if(band == FALSE) {
					listb[[i]] <- as.matrix(B)
				}
			}
		})
		withProgress(message = '', detail = '', value = 1, min=0, max=length(lista) * length(listb), {
			for(i in 1:length(lista)) {
				for(x in 1:length(listb)) {
					d1 <- 999999
					ptemp <- NULL
					for(k in 1:8) {
						if (k == 1) {lt1 <- cbind( lista[[i]][,1], lista[[i]][,2],lista[[i]][,3])}
						else if (k == 2) {lt1 <- cbind( lista[[i]][,1]*-1, lista[[i]][,2]*-1,lista[[i]][,3]*-1)}
						else if (k == 3) {lt1 <- cbind( lista[[i]][,1], lista[[i]][,2]*-1,lista[[i]][,3]*-1)}
						else if (k == 4) {lt1 <- cbind( lista[[i]][,1]*-1, lista[[i]][,2],lista[[i]][,3]*-1)}
						else if (k == 5) {lt1 <- cbind( lista[[i]][,1]*-1, lista[[i]][,2]*-1,lista[[i]][,3])}
						else if (k == 6) {lt1 <- cbind( lista[[i]][,1], lista[[i]][,2],lista[[i]][,3]*-1)}
						else if (k == 7) {lt1 <- cbind( lista[[i]][,1], lista[[i]][,2]*-1,lista[[i]][,3])}
						else if(k == 8) {lt1 <- cbind( lista[[i]][,1]*-1, lista[[i]][,2],lista[[i]][,3])}
						lt <- icpmat(lt1, listb[[x]], iterations = iteration, type = "rigid", threads = threads)
						d1t <- hausdorff_dist(lt, listb[[x]], dist = dist, threads = threads)
						if(d1t < d1) {
							ptemp <- lt
							d1 <- d1t
						}
					}
					if(output_options[[5]]) {
						write.tmp.data(ptemp, listb[[x]], paste(names(list1)[i], names(list2)[x], sep="-"), direc, sessiontempdir)
					}
					renderlist[nz,] <- paste(names(list1)[i], names(list2)[x], sep="-")
					matches1[nz,] <- c(names(list1)[i], names(list2)[x], d1)
					matches2[nz,] <- c(names(list2)[x], names(list1)[i], d1)
					print(paste("Specimens: ", names(list1)[i], " - ", names(list2)[x], " ", "Hausdorff", " distance: ", d1, sep=""))
					incProgress(amount = 1, message = paste("Specimens: ", names(list1)[i], " - ", names(list2)[x], " ", "Hausdorff", " distance: ", d1, sep=""), detail = '')
					nz <- nz + 1
				}
			}
		})
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
	}
	resmatches <- resmatches[-1,] #remove NA row
	if(is.null(nrow(resmatches))) {names(resmatches) <- c("ID", "Match-ID", "Distance")}
	if(!is.null(nrow(resmatches))) {colnames(resmatches) <- c("ID", "Match-ID", "Distance")}
	if(hide_distances) {resmatches[,3] <- "Hidden"}
	if(output_options[1]) {no <- OsteoSort:::output_function(resmatches, method="3D", type="csv-res")}
	if(output_options[2]) {no <- OsteoSort:::output_function(matches, method="3D", type="csv-all")}
	if(output_options[3]) {no <- OsteoSort:::output_function(pairwise_coords, method="3D", type="coord")}
	if(output_options[4]) {no <- OsteoSort:::output_function(hera1 = resmatches, method="networkanalysis", type="2D-3D", labtf = labtf3d)}
	comparisons <- length(list1) * length(list2) #number of comparisons
	gc()
	setwd(workingdir)
	print("Form comparisons completed")
	t_time <- end_time(start_time)
	return(list(resmatches, direc, comparisons, matches, renderlist, t_time))
}
