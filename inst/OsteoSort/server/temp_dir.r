#generates temporary directories
workingdd <- getwd()
sessiontempd <- OsteoSort:::randomstring(n = 1, length = 12)
dir.create('tmp')
setwd('tmp')
dir.create(sessiontempd)
setwd(sessiontempd)
sessiontemp <- getwd()