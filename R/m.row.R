#' Match rows from different cols
#' 
#' @param xx to be matched
#' @param table table to search
#'
#' @examples
#' m.row()

m.row <- function (xx, table) {
	tt <-c()
	table <- as.data.frame(table)
	table <- table[,-3]
	ct <- do.call("paste", c(table[, , drop = FALSE], sep = "\r"))
	xx <- as.data.frame(matrix(xx, nrow = 1))
	xx <- xx[,-3]
	cx <- do.call("paste", c(xx[, , drop = FALSE], sep = "\r"))
	t <- which(ct %in% cx)
	#switch
	xx <- c(as.character(xx[1,2]), as.character(xx[1,1]))
	xx <- as.data.frame(matrix(xx, nrow = 1))
	cx <- do.call("paste", c(xx[, , drop = FALSE], sep = "\r"))
	ttt <- which(ct %in% cx)
	if(length(ttt) > 0) {tt <- c(tt, ttt)}
	if(length(t) > 0) {tt <- c(tt, t)}
	return(tt)
}