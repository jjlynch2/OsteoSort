RGB.calibrate.3d <- function(align_data) {
	if(ncol(align_data) < 6) {return(NULL)}
	cc <- rgb(align_data[,c(4:6)], max=255)	

	options(rgl.useNULL=FALSE)
	open3d()
	points3d(align_data, aspect = "iso", size = 15, col=cc, box=FALSE)

	rgl.bringtotop(stay = TRUE)

	print("Select four colored points for calibration")
	dt <- align_data[identify3d(align_data, n = 4),c(4:6)]

	try(rgl.close())
	options(rgl.useNULL=TRUE) #required to avoid rgl device opening in shiny
	return(dt)
}
