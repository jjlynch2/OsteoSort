writetps <- function (shape_matrices, file) {
	file.create(file, showWarnings = FALSE) 

	if(!is.list(shape_matrices)) {
		n <- dim(shape_matrices)[3]
		k <- dim(shape_matrices)[2]
		p <- dim(shape_matrices)[1]
		lmline <- ifelse(k == 2, paste("LM=", p, sep = ""), paste("LM3=", p, sep = ""))
		for (i in 1:n) {
			write(lmline, file, append = TRUE)
			write.table(shape_matrices[, , i], file, col.names = FALSE, row.names = FALSE, append = TRUE)
			idline <- paste("ID=", dimnames(shape_matrices)[[3]][i], sep = "")
			write(idline, file, append = TRUE)
			write("", file, append = TRUE)
		}
	}
	if(is.list(shape_matrices)) {
		for(i in 1:length(shape_matrices)){
			k <- ncol(shape_matrices[[i]])
			p <- nrow(shape_matrices[[i]])
			lmline <- ifelse(k == 2, paste("LM=", p, sep = ""), paste("LM3=", p, sep = ""))
			write(lmline, file, append = TRUE)
			write.table(shape_matrices[[i]], file, col.names = FALSE, row.names = FALSE, append = TRUE)
			idline <- paste("ID=", names(shape_matrices)[[i]], sep = "")
			write(idline, file, append = TRUE)
			write("", file, append = TRUE)

		}#for

	}#list
}
