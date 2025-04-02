#Set options for environment
options(scipen = 999) #no scientific notation
options(stringsAsFactors = FALSE) #no strings as factors
options(warn = -1) #disables warnings
options(shiny.maxRequestSize=40*1024^2) #40MB file size limit
options(as.is = TRUE)

#load libraries
library(shiny)
library(htmltools)
library(zip)
library(ggplot2)
library(dplyr)
library(shinyalert)
library(DT)
library(JuliaCall)

#load analytical R code
source("./R/analytical_temp_space.r", local=TRUE) 
source("./R/art.input.r", local=TRUE) 
source("./R/bsModal.r", local=TRUE) 
source("./R/misc.r", local=TRUE) 
source("./R/output_function.r", local=TRUE) 
source("./R/pm.input.r", local=TRUE) 
source("./R/randomstring.r", local=TRUE) 
source("./R/reg.input.r", local=TRUE) 
source("./R/reg.test.r", local=TRUE) 
source("./R/timer.r", local=TRUE) 
source("./R/tmpdata.r", local=TRUE) 
source("./R/ttest.r", local=TRUE) 

shinyServer(function(input, output, session) {
    source("./server/reference.r", local=TRUE) 
    source("./server/temp_dir.r", local=TRUE) 
    source("./server/julia.r", local=TRUE) 
    source("./server/single.r", local=TRUE) 
    source("./server/multiple.r", local=TRUE) 
    source("./server/files.r", local=TRUE) 
    source("./server/exit_parameters.r", local=TRUE) 
})
