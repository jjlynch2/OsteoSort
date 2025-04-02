#load Julia environment
showModal(modalDialog(title = "Loading Julia Environment...", easyClose = FALSE, footer = NULL))
julia_setup(sysimage_path = "./OsteoSort.so")
removeModal()