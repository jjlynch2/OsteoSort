#load Julia environment
showModal(modalDialog(title = "Loading Analytical environment...", easyClose = FALSE, footer = NULL))
JV <- JuliaSetup(libraries = TRUE)
removeModal()  