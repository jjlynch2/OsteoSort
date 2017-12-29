#' Articulation comparison Input Function
#'
#' @param bone Specifies the bone type
#' @param sort Data to be sorted
#'
#' @keywords art.input
#' @export
#' @examples
#' art.input()

art.input <- function (bone = NULL, sort = NULL) {
	print("Import and reference generation started")
	options(stringsAsFactors = FALSE)
	options(as.is = TRUE)
	options(warn = -1)
	if(is.null(bone) || is.null(sort)){return(NULL)}
	if (bone == "fi") {	
		measurements <- c("Fem_04","Osc_17")	
		bone1 <- 'femur'
		bone2 <- 'os_coxa'	
	}
	if (bone == "hu") {
		measurements <- c("Hum_06","Uln_11")	
		bone1 <- 'humerus'
		bone2 <- 'ulna'		
	}
	if (bone == "hr") {
		measurements <- c("Hum_06","Rad_04")	
		bone1 <- 'humerus'
		bone2 <- 'radius'
	}
	if (bone == "hs") {
		measurements <- c("Hum_07","Sca_03")	
		bone1 <- 'humerus'
		bone2 <- 'scapula'
	}
	if (bone == "hss") {
		measurements <- c("Hum_07","Sca_04")	
		bone1 <- 'humerus'
		bone2 <- 'scapula'
	}
	if (bone == "ft") {
		measurements <- c("Fem_03","Tib_02")	
		bone1 <- 'femur'
		bone2 <- 'tibia'
	}
	if (bone == "ftt") {
		measurements <- c("Fib_05","Tib_12")	
		bone1 <- 'fibula'
		bone2 <- 'tibia'		
	}
	
	#reference input
	reftemp <- read.table(system.file("extdata", 'artmleft.csv', package = "OsteoSort"), header = TRUE, sep=",")
	refdata <- cbind(reftemp[[measurements[1]]], reftemp[[measurements[2]]])
	refdata <- na.omit(refdata)

	if(nrow(sort) == 1) {sort[7] <- as.numeric(as.matrix(sort[7])); sort[8] <- as.numeric(as.matrix(sort[8])); return(list(list(as.data.frame(sort, stringsAsFactors = FALSE)), as.data.frame(refdata, stringsAsFactors=FALSE)))} #returns if single user input since no sorting required

	cols1 <- c(grep(paste("^","id","$",sep=""), colnames(sort)), grep("Side", colnames(sort)), grep("Element", colnames(sort)), grep(paste("^",measurements[1],"$",sep=""), colnames(sort))) #greps column name to grab column index 
	cols2 <- c(grep(paste("^","id","$",sep=""), colnames(sort)), grep("Side", colnames(sort)), grep("Element", colnames(sort)), grep(paste("^",measurements[2],"$",sep=""), colnames(sort))) #greps column name to grab column index

	#uses regex to capture first match in grep but this can be removed eventually. Just some input cleaning due to shitty excel files
	sortdata <- sort
	#lower case
	sortdata$Side <- tolower(sortdata$Side)
	sortdata$Element <- tolower(sortdata$Element)

	sortdata1 <- sortdata[sortdata$Side == 'left',]
	sortdata2 <- sortdata[sortdata$Side == 'right',]
	
	sortdataleftf <- na.omit(sortdata1[sortdata1$Element == bone1,][,cols1])
	sortdatalefti <- na.omit(sortdata1[sortdata1$Element == bone2,][,cols2])
	sortdatarightf <- na.omit(sortdata2[sortdata2$Element == bone1,][,cols1])
	sortdatarighti <- na.omit(sortdata2[sortdata2$Element == bone2,][,cols2])
	
	##########################################################
	colnames(sortdataleftf) <- c('b', 'c', 'd', 'e')
	colnames(sortdatalefti) <- c('b', 'c', 'd', 'e')
	colnames(sortdatarightf) <- c('b', 'c', 'd', 'e')
	colnames(sortdatarighti) <- c('b', 'c', 'd', 'e')
	sortdataleft <- rbind(sortdataleftf, sortdatalefti)
	sortdataright <- rbind(sortdatarightf, sortdatarighti)
	##########################################################

	###########################################################
	n_vars1 <- length(unique(sortdataleft[sortdataleft$d == bone1,][,1]))
	n_vars2 <- length(unique(sortdataleft[sortdataleft$d == bone2,][,1]))
	indices <- expand.grid(1:n_vars1, 1:n_vars2)
	bone11 <- sortdataleft[sortdataleft$d == bone1,]
	bone22 <- sortdataleft[sortdataleft$d == bone2,]
	res <- cbind(bone11[indices[,1],], bone22[indices[,2],])
	resleft <- res[,order(names(res))]
	###########################################################

	###########################################################
	n_vars1 <- length(unique(sortdataright[sortdataright$d == bone1,][,1]))
	n_vars2 <- length(unique(sortdataright[sortdataright$d == bone2,][,1]))
	indices <- expand.grid(1:n_vars1, 1:n_vars2)
	bone11 <- sortdataright[sortdataright$d == bone1,]
	bone22 <- sortdataright[sortdataright$d == bone2,]
	res <- cbind(bone11[indices[,1],], bone22[indices[,2],])
	resright <- res[,order(names(res))]
	###########################################################	

	sortdata <- rbind(resleft, resright) #Brings left and rights back together in final combo dataframe
	sortdata <- na.omit(sortdata)
	
	colnames(sortdata) <- c("id","id","Side","Side","Element","Element",measurements[1],measurements[2])

     sortdata <- split(sortdata, 1:nrow(sortdata)) #needs to be replaced

	gc()
	refdata <- as.data.frame(refdata, stringsAsFactors=FALSE)
	options(stringsAsFactors = TRUE) #restore default R  
     print("Import and reference generation completed")
	return(list(sortdata, refdata))
}