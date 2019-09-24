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
		temp <- gc()
		HTML(paste("<strong><font color=\"#FFFFFF\">Memory Usage: ", temp[1,2] + temp[2,2], " Mb</strong></font>"))
	})
}

system_name <- Sys.info()[['sysname']]
system_mem <- NULL
if(system_name == "Linux") {
	system_name <- paste(icon = icon("linux", lib="font-awesome"), system_name, sep = " ")
	system_mem <- gsub('MemTotal:       ', '', system(paste0("cat /proc/meminfo | grep MemTotal"), intern = TRUE))
}
if(system_name == "Windows") {
	system_name <- paste(icon = icon("windows", lib="font-awesome"), system_name, sep = " ")
	system_mem <- paste(gsub("FreePhysicalMemory=", '', system('wmic OS get TotalVisibleMemorySize /Value')), " kB", sep="")
}
if(system_name == "Darwin") {
	system_name <- paste(icon = icon("apple", lib="font-awesome"), system_name, sep = " ")
	system_mem <- "not available"
}

output$system_info <- renderUI({
	HTML(paste("<p><h4><b>System Details</b></h4></p>", 
	"<strong>Platform:  </strong>", system_name, "<p></p>",
	"<strong>Cores: </strong>", OsteoSort:::detectCores(), "<p></p>",
	"<strong>Memory: </strong>", system_mem,
	"<br><br><br><br><p></p>"
	,sep=""))
})
