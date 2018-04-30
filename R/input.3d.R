#' three-dimensional pair-match function
#' 
#'
#' @keywords input.3d
#' @export
#' @examples
#' input.3d()

input.3d <- function(list1 = NULL, list2 = NULL) {
	print("Importing 3D data")

	if(!is.null(list1)) {
		Rlist <- list()
		for(i in 1:length(list1)) {
			Rtemp <- read.table(list1[i], quote="\"", comment.char="", stringsAsFactors=FALSE, header = FALSE)
			if(is.character(Rtemp[1,1])) {
				Rlist[[i]] <- read.table(list1[i], quote="\"", comment.char="", stringsAsFactors=FALSE, header = TRUE)
			}
			if(!is.character(Rtemp[1,1])) {
				Rlist[[i]] <- read.table(list1[i], quote="\"", comment.char="", stringsAsFactors=FALSE, header = FALSE)
			}
		}
		names(Rlist) <- gsub(".*/\\s*", "", list1)
	}

	if(!is.null(list2)) {
		Llist <- list()
		for(i in 1:length(list2)) {
			Ltemp <- read.table(list2[i], quote="\"", comment.char="", stringsAsFactors=FALSE, header = FALSE)
			if(is.character(Ltemp[1,1])) {
				Llist[[i]] <- read.table(list2[i], quote="\"", comment.char="", stringsAsFactors=FALSE, header = TRUE)
			}
			if(!is.character(Ltemp[1,1])) {
				Llist[[i]] <- read.table(list2[i], quote="\"", comment.char="", stringsAsFactors=FALSE, header = FALSE)
			}
		}
		names(Llist) <- gsub(".*/\\s*", "", list2)
	}

	print("3D data imported")

	if(!is.null(list1) && !is.null(list2)) {
		return(list(Rlist, Llist))
	}
	if(!is.null(list1)) {
		return(Rlist)

	}
	if(!is.null(list2)) {
		return(Llist)
	}
}