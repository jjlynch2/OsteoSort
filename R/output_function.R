#' output_excel function
#'
#' @param results
#' Only used to call internally
#'

output_function <- function(hera1) {
	if(nrow(hera1[hera1$Result == "Cannot Exclude",]) > 0) {
		write.csv(hera1[hera1$Result == "Cannot Exclude",], file = "not-excluded-list.csv", row.names=FALSE, col.names = TRUE)
	}
	if(nrow(hera1[hera1$Result == "Excluded",]) > 0) {
		write.csv(hera1[hera1$Result == "Excluded",], file = "excluded-list.csv",row.names=FALSE, col.names = TRUE)
	}

}