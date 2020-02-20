CentroidBand <- function(pointcloud = NULL, threshold = 4, centroid = NULL) {
	if(is.null(centroid)) {
		centroid <- apply(pointcloud[,1:3], 2, mean)
	}
	t1 <- pointcloud[pointcloud[,1] <= centroid[1]+threshold, ]
	band_1 <- t1[t1[,1] >= centroid[1]-threshold, ]
	t2 <- pointcloud[pointcloud[,2] <= centroid[2]+threshold, ]
	band_2 <- t2[t2[,2] >= centroid[2]-threshold, ]
	t3 <- pointcloud[pointcloud[,3] <= centroid[3]+threshold, ]
	band_3 <- t3[t3[,3] >= centroid[3]-threshold, ]
	return(unique(rbind(band_1, band_2, band_3)))
}
