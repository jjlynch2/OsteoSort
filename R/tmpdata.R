write.tmp.data <- function(A, B, comp_name, direc, sessiontemp) {
	mainDir1 <- paste(sessiontemp, "/", direc , "/1/", sep="")
	mainDir2 <- paste(sessiontemp, "/", direc , "/2/", sep="")
	if(!file.exists(mainDir1) && !file.exists(mainDir2)) {
		dir.create(file.path(mainDir1))
		dir.create(file.path(mainDir2))
	}
	write.csv(A, file = paste(mainDir1, comp_name, sep=""), row.names=FALSE, col.names=FALSE)
	write.csv(B, file = paste(mainDir2, comp_name, sep=""), row.names=FALSE, col.names=FALSE)
}

import.tmp.data <- function(comp_name, direc, sessiontemp) {
	file1 <- paste(sessiontemp, "/", direc , "/1/", comp_name, sep="")
	file2 <- paste(sessiontemp, "/", direc , "/2/", comp_name, sep="")
	data1 <- read.csv(file1, stringsAsFactors=FALSE)
	data2 <- read.csv(file2, stringsAsFactors=FALSE)
	return(list(data1, data2))
}

delete.tmp.data <- function(direc, sessiontemp) {
	file1 <- paste(sessiontemp, "/", direc , "/1/", sep="")
	file2 <- paste(sessiontemp, "/", direc , "/2/", sep="")
	filelist <- c(list.files(file1, full.names=TRUE), list.files(file2, full.names=TRUE))
	for(i in filelist) {
		file.remove(i)
	}
}
