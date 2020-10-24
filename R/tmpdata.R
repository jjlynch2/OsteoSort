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
	unlink(paste(sessiontemp, "/", direc, sep=""), recursive=TRUE)
}

import.tmp.data.pct <- function(comp_name, sessiontemp) {
	file1 <- paste(sessiontemp, "/" , comp_name, sep="")
	data1 <- input.3d(file1)
	return(data1[[1]])
}

delete.tmp.data.pct <- function(comp_name, sessiontemp, direc) {
	unlink(paste(sessiontemp,"/",direc,sep=""), recursive=TRUE)
	for(i in comp_name) {
		file.remove(paste(sessiontemp,"/",i,sep=""))
	}
}

write.tmp.data.pct <- function(A, comp_name, sessiontemp) {
	mainDir1 <- paste(sessiontemp, "/", comp_name, sep="")
	write.table(A, sep = ' ', file = mainDir1, row.names=FALSE)

}
