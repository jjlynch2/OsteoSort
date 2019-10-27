outline.images <- function (imagelist1, imagelist2, threshold = 0.8, scale = FALSE, mirror = TRUE) {
	print("Outline generation started")	
	nimages <- length(imagelist1) + length(imagelist2)
	imagelist <- c(imagelist1, imagelist2)
	speclist <- list()
	for(iii in 1:nimages) {
		print(paste("Tracing specimen: ", paste(gsub(".*/\\s*|.JPG.*","",imagelist[iii]), ".JPG", sep=""), sep=""))
		M <- jpeg::readJPEG(imagelist[iii])
		M <- suppressWarnings(pixmap::pixmapGrey(M))
		M@grey[which(M@grey > threshold)] <- 1#white
		M@grey[which(M@grey <= threshold)] <- 0#black
		for(i in 1:10) {
			M@grey <- cbind(matrix(rep(1, nrow(M@grey))), M@grey, matrix(rep(1, nrow(M@grey)))) #adds column to left and right
			M@grey <- rbind(rep(1, ncol(M@grey)), M@grey, rep(1, ncol(M@grey))) #adds row to top and bottom
		}
		orig_size <- M@size
		
		temp_matrix <- M@grey
		x <- t(which(temp_matrix == 0, arr.ind = TRUE, useNames=FALSE)[round(nrow(which(temp_matrix == 0, arr.ind = TRUE)) / 2),]) #locate starting point
		I <- M@grey #b/w matrix
		while (abs(I[x[1], x[2]] - I[x[1], (x[2] - 1)]) < 0.1) {
			x[2] <- x[2] - 1
		}
		a <- 1
		M <- matrix(c(0, -1, -1, -1, 0, 1, 1, 1, 1, 1, 0, -1, -1, -1, 0, 1), 2, 8, byrow = TRUE)
		M <- cbind(M[, 8], M, M[, 1])
		X <- 0
		Y <- 0
		x1 <- x[1]
		x2 <- x[2]
		SS <- NA
		S <- 6
		while ((any(c(X[a], Y[a]) != c(x1, x2)) | length(X) < 3)) {
			if (abs(I[x[1] + M[1, S + 1], x[2] + M[2, S + 1]] - I[x[1], x[2]]) < 0.1) {
				a <- a + 1
				X[a] <- x[1]
				Y[a] <- x[2]
				x <- x + M[, S + 1]
				SS[a] <- S + 1
				S <- (S + 7)%%8
			}
			else if (abs(I[x[1] + M[1, S + 2], x[2] + M[2, S + 2]] - I[x[1], x[2]]) < 0.1) {
				a <- a + 1
				X[a] <- x[1]
				Y[a] <- x[2]
				x <- x + M[, S + 2]
				SS[a] <- S + 2
				S <- (S + 7)%%8
			}
			else if (abs(I[x[1] + M[1, (S + 3)], x[2] + M[2, (S + 3)]] - I[x[1], x[2]]) < 0.1) {
				a <- a + 1
				X[a] <- x[1]
				Y[a] <- x[2]
				x <- x + M[, (S + 3)]
				SS[a] <- S + 3
				S <- (S + 7)%%8
			}
			else S <- (S + 1)%%8
			if (a > (dim(I)[1] + dim(I)[2]) * 100) {
				X[a] = x1
				Y[a] = x2
			}
		}
		spec1 <- list(X = (Y[-1]), Y = ((dim(I)[1] - X))[-1])
		spec1 <- as.matrix(data.frame(spec1))
		spec1 <- round(spec1) #round to whole numbers
		spec1 <- spec1[spec1[,2] < orig_size[1],]
		spec1 <- spec1[spec1[,1] < orig_size[2],] #original is minus 10 already
		spec1 <- spec1[spec1[,2] > 10,] #10 for added border
		spec1 <- spec1[spec1[,1] > 10,]
		if(mirror) {
			if(imagelist[iii] %in% imagelist2) {
				spec1[,1] <- -spec1[,1] + min(spec1[,1]) * 2 #swap X axis to mirror ### should this by multiply by -1????
			}
		}
		if(scale) {
			centroid <- apply(spec1,2,mean)
			centroidsize <- sqrt(sum((t(t(spec1)-centroid))^2))
			spec1 <- spec1 / centroidsize
		}
		spec1 <- scale(spec1, scale=FALSE)
		speclist[[iii]] <- spec1 #save to list since points are unequal
	}
	names(speclist) <- paste(gsub(".*/\\s*|.JPG.*","",imagelist), ".JPG", sep="")
	results <- speclist
	print("Outline generation completed")	
	return(list(results, imagelist1, imagelist2))
}
