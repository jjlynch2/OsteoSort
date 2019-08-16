#' Centroid Band Extraction
#' 
#' @param data A list of matrices of XYZ point clouds
#' @param pc Align along principal components before extracting
#' @param threshold Metric for band thickness relative to the point cloud scale
#'
#' @keywords CentroidBand
#' @export
#' @examples
#' CentroidBand(data = pointclouds, pc = TRUE, threshold = 4)

CentroidBand <- function(pointcloud = NULL, threshold = 4, centroid = NULL) {
	if(is.null(centroid)) {
		centroid <- apply(pointcloud, 2, mean)
	}
	t1 <- pointcloud[pointcloud[,1] <= centroid[1]+threshold, ]
	band_1 <- t1[t1[,1] >= centroid[1]-threshold, ]
	t2 <- pointcloud[pointcloud[,2] <= centroid[2]+threshold, ]
	band_2 <- t2[t2[,2] >= centroid[2]-threshold, ]
	t3 <- pointcloud[pointcloud[,3] <= centroid[3]+threshold, ]
	band_3 <- t3[t3[,3] >= centroid[3]-threshold, ]
	return(unique(rbind(band_1, band_2, band_3)))
}