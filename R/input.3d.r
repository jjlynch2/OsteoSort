input.3d <- function(list1 = NULL, list2 = NULL) {
	print("Importing 3D data")

	if(!is.null(list1)) {
		Rlist <- list()
		withProgress(message = '', detail = '', value = 0, min=0, max=length(list1), {
			for(i in 1:length(list1)) {
				incProgress(amount = i, message = paste("Importing Set 1: ", gsub(".*/\\s*", "", list1)[i], sep=""), detail = '')
				Rtemp <- read.table(list1[i], quote="\"", comment.char="", stringsAsFactors=FALSE, header = FALSE)
				if(is.character(Rtemp[1,1])) {
					Rlist[[i]] <- read.table(list1[i], quote="\"", comment.char="", stringsAsFactors=FALSE, header = TRUE)
				}
				if(!is.character(Rtemp[1,1])) {
					Rlist[[i]] <- read.table(list1[i], quote="\"", comment.char="", stringsAsFactors=FALSE, header = FALSE)
				}
			}
			names(Rlist) <- gsub(".*/\\s*", "", list1)
		})
	}

	if(!is.null(list2)) {
		Llist <- list()
		withProgress(message = '', detail = '', value = 0, min=0, max=length(list2), {
			for(i in 1:length(list2)) {
				incProgress(amount = i, message = paste("Importing Set 2: ", gsub(".*/\\s*", "", list2)[i], sep=""), detail = '')
				Ltemp <- read.table(list2[i], quote="\"", comment.char="", stringsAsFactors=FALSE, header = FALSE)
				if(is.character(Ltemp[1,1])) {
					Llist[[i]] <- read.table(list2[i], quote="\"", comment.char="", stringsAsFactors=FALSE, header = TRUE)
				}
				if(!is.character(Ltemp[1,1])) {
					Llist[[i]] <- read.table(list2[i], quote="\"", comment.char="", stringsAsFactors=FALSE, header = FALSE)
				}
			}
			names(Llist) <- gsub(".*/\\s*", "", list2)
		})
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
