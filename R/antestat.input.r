#' antestat.input Input Function
#'
#' Function to generate combinations of antemortem stature to postmortem measurements for use with antestat.regtest.R
#'
#' @param antemortem_stature The antemortem stature for comparison
#' @param postmortem_measurement The postmortem measurement for comparison
#' @param bone The bone type to compare
#' @param population The population data to use for modeling
#' @param metric Specifies millimeters (mm), centimeters (cm), or inches (in) for stature
#'
#' @keywords antestat
#' @export
#' @examples 
#' antestat.input()

antestat.input <- function(antemortem_stature = NULL, postmortem_measurement = NULL, ref = NULL, measurement = NULL, side = NULL, bone = NULL) {
	print("Filtering data by element type and specified measurement...")
	options(stringsAsFactors = FALSE)
	options(as.is = TRUE)
	options(warn = -1)
	if(is.na(antemortem_stature) || is.null(antemortem_stature)) {return(NULL)} #input san
	if(is.na(postmortem_measurement) || is.null(postmortem_measurement)) {return(NULL)} #input san	
	antemortem_stature <- data.frame(antemortem_stature, stringsAsFactors = FALSE)
	postmortem_measurement <- data.frame(postmortem_measurement, stringsAsFactors = FALSE)
	bone <- tolower(bone)
	side <- tolower(side)
	ref$Side <- tolower(ref$Side)
	ref$Element <- tolower(ref$Element)
	refa <- ref[ref$Element == bone,]
	refa <- refa[refa$Side == side,]

	cnsb <- colnames(postmortem_measurement)
	cb <- duplicated(c(measurement, cnsb), fromLast = TRUE)
	if(!any(cb)) {return(NULL)}
	measurement <- measurement[cb[1:length(measurement)]]

	refa <- cbind(refa[,c(1:3)], refa$Stature, refa[measurement])
	refa <- refa[rowSums(is.na(refa)) < 1,] #remove NA rows

	if(nrow(refa) == 0) {return(NULL)}

	postmortem_measurement$Element <- tolower(postmortem_measurement$Element) #lower case bone name
	postmortem_measurement$Side <- tolower(postmortem_measurement$Side) #lower case side
	postmortem_measurement <- postmortem_measurement[postmortem_measurement$Element == bone,] #sort by bone
	postmortem_measurement <- postmortem_measurement[postmortem_measurement$Side == side,] #sort by side

	postmortem_measurement <- cbind.data.frame(postmortem_measurement$id, postmortem_measurement$Side, postmortem_measurement$Element, postmortem_measurement[measurement], stringsAsFactors = FALSE) #id, Side, element, and measurement

	postmortem_measurement <- postmortem_measurement[rowSums(is.na(postmortem_measurement)) < 1,] #remove NA rows
	postmortem_measurement[,4] <- as.numeric(postmortem_measurement[,4])
	antemortem_stature <- antemortem_stature[rowSums(is.na(antemortem_stature)) < 1,] #remove NA rows
	if(nrow(postmortem_measurement) == 0) {return(NULL)}
	if(nrow(antemortem_stature) == 0) {return(NULL)}
	options(stringsAsFactors = TRUE) #restore default R  
	print("Finished...")
	return(list(antemortem_stature, postmortem_measurement, refa))
}
