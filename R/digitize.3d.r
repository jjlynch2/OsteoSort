digitize.3d <- function(align_data) {
	if(ncol(align_data) >5) {
		if(any(is.na(align_data[,c(4:6)]))) { 
			cc <- "dimgrey"
		}
		else {
			cc <- rgb(align_data[,c(4:6)], max=255)
		}
	}
	else {cc <- "dimgrey"}

	options(rgl.useNULL=FALSE)
	open3d()
	points3d(align_data, aspect = "iso", size = 15, col=cc, box=FALSE)

	rgl.bringtotop(stay = TRUE)

	print("Select fragmented boundary")
	mp <- selectpoints3d(value = FALSE, button = c("right"), multiple = TRUE)
	dt <- mp[,2]

	options(rgl.useNULL=TRUE)
	return(dt)
}
