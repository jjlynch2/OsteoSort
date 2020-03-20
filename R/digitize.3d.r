digitize.3d <- function(align_data, landmarks = FALSE) {
	if(ncol(align_data) >5) {
		if(any(is.na(align_data[,c(4:6)]))) { 
			cc <- "dimgrey"
		}
		else {
			cc <- rgb(align_data[,c(4:6)], max=255)
		}
	}
	else {cc <- "dimgrey"}
	align_data <- align_data[,1:3]
	options(rgl.useNULL=FALSE)
	open3d()
	rgl.bringtotop(stay = TRUE)
	if(!isFALSE(landmarks)) { 
		print("Select landmarks")
		dt <- rep(0,10)
		points3d(align_data, aspect="iso", size = 10, col = cc, box = FALSE)
		for(i in as.numeric(landmarks)) {
			dt[i] <- identify3d(align_data, n = 1)
			spheres3d(align_data[dt[i],1:3], color = "dodgerblue")
		}
	} else {
		print("Select fragmented boundary")
		ids <- plot3d(align_data, aspect = "iso", size = 10, col=cc, box=FALSE)
		mp <- selectpoints3d(ids["data"], value = FALSE, button = c("right"), multiple = function(ids) {
				spheres3d(align_data[ids[, "index"], , drop=FALSE], color = "dodgerblue")
				TRUE
			}
		)
		dt <- mp[,2]
	}
	options(rgl.useNULL=TRUE)
	return(dt)
}
