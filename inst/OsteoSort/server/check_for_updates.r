url <- paste0("https://raw.github.com/jjlynch2/OsteoSort/master/DESCRIPTION")
x <- readLines(url)
remote_version <- gsub("Version:\\s*", "", x[grep('Version:', x)])
if(remote_version > packageVersion(gsub(".*/","","OsteoSort"))) {


}