#' A Random String Function
#' 
#' This function allows you to generate a random string of length 12 for 
#' generating temporary directories and files. It's used to create the temporary local 
#' environments for saving and zipping results
#' @param n how many
#' @param length length of string
#' @keywords random
#' @export
#' @examples
#' randomstring()

randomstring <- function(n=1, length=12) {

randomString <- c(1:n)
for (i in 1:n) {
	randomString[i] <- paste(sample(c(0:9, letters, LETTERS), length, replace=TRUE),collapse="")
}

return(randomString)
}