#load Julia environment
showModal(modalDialog(title = "Loading analytical environment...", easyClose = FALSE, footer = NULL))
JV <- JuliaSetup(libraries = TRUE)
removeModal()