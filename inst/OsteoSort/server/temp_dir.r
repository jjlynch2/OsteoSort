#generates temporary directories
workingdd <- tempdir()
setwd(workingdd)
sessiontempd <- OsteoSort:::randomstring(n = 1, length = 24)
dir.create(sessiontempd)
setwd(sessiontempd)
sessiontemp <- getwd()