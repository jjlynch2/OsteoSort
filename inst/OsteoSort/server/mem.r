autoMem <- reactiveTimer(10000) #10 seconds
if(Sys.info()[['sysname']] == "Linux") {
	output$memUsage <- renderUI({
		autoMem()
		HTML(paste("<strong><font color=\"#FFFFFF\">Memory Usage: ", gsub('VmSize:	', '', system(paste0("cat /proc/",Sys.getpid(),"/status | grep VmSize"), intern = TRUE)), "</strong></font>"))
	})
}
if(Sys.info()[['sysname']] == "Windows") {
	output$memUsage <- renderUI({
		autoMem()
		HTML(paste("<strong><font color=\"#FFFFFF\">Memory Usage: ", memory.size(), "</strong></font>"))
	})
}