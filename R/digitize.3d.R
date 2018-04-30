#' three-dimensional digitizer
#' 
#'
#' @keywords digitize.3d
#' @export
#' @examples
#' digitize.3d()

digitize.3d <- function(align_data) {


	myColorRamp <- function(colors, values) {
	    v <- (values - min(values))/diff(range(values))
	    x <- colorRamp(colors)(v)
	    rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
	}

	cols <- myColorRamp(c("dodgerblue",  "dimgrey"), align_data[,3]) 


	options(rgl.useNULL=FALSE)
	open3d()
	points3d(align_data, aspect = "iso", size = 15, col=cols, box=FALSE)


	rgl.bringtotop(stay = TRUE)
	print("Select three points for alignment")

	points <- identify3d(align_data, n = 3)

	print("Select fragmented boundary")
	mp <- selectpoints3d(value = FALSE, button = c("right"), multiple = TRUE)
	options(rgl.useNULL=TRUE) #required to avoid rgl device opening in shiny
	return(list(points, mp[,2]))
}