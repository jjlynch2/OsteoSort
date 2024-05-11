#load Julia environment
showModal(modalDialog(title = "Loading analytical environment...", easyClose = FALSE, footer = NULL))
JV <- RJS(libraries = TRUE)
removeModal()