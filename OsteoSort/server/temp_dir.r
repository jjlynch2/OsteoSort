#generates temporary directories
sessiontempd <- randomstring(n = 1, length = 24)
dir.create(paste(tempdir(), sessiontempd,sep="/"))
sessiontemp <- paste(tempdir(), sessiontempd,sep="/")
