#' All T.test Function
#' 
#' 
#' @param sort sort
#' @keywords all.ttest
#' @export
#' @examples
#' all.ttest()

all.ttest <- function(sort = NULL, test = NULL, tresh = NULL, measurements = NULL, sessiontemp = NULL, stdout = FALSE, alphalevel = 0.1, absolutevalue = TRUE, template = "Standard", testagainst = FALSE, oo = c(TRUE, FALSE), no_cores = 1, power = TRUE) {
	options(warn = -1)
	if(is.null(sort) || is.null(test) || is.null(sessiontemp)) { return(NULL)}
	if(test != "all_pm" && test != "all_art" && test != "all") { return(NULL)}
	workingdir = getwd()
	
	template <- tolower(template)
	
	if(!stdout) {
		if(!is.null(sessiontemp)) {
			setwd(sessiontemp)
		}
		direca <- randomstring(n = 1, length = 12)
		dir.create(direca)
		setwd(direca)
		direcb <- getwd()
     }
     
     if(is.null(tresh)) {
     	tresh <- c(1,1,1,1,1,1,1,1,1)
     }
     
	direc2 <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)	
	ll <- 0
	nbb <- 0			
	directemp <- NULL
	if(test == "all_pm") {
		bones <- as.matrix(c("clavicle", "scapula", "humerus", "ulna", "radius", "os_coxa", "femur", "tibia", "fibula"))
	}
	if(test == "all_art") {
		bones <- as.matrix(c("hu", "hr", "hs", "hss", "fi", "ft", "ftt"))
	}
	if(test == "all") {
		bones <- as.matrix(c("clavicle", "scapula", "humerus", "ulna", "radius", "os_coxa", "femur", "tibia", "fibula","hu", "hr", "hs", "hss", "fi", "ft", "ftt"))
	}
	
	for(i in bones) {

		if(i == 'clavicle') {threshold <- tresh[1]; m <- measurements[[1]]} 
		if(i == 'scapula') {threshold <- tresh[2]; m <- measurements[[2]]}   
		if(i == 'humerus') {threshold <- tresh[3]; m <- measurements[[3]]} 
		if(i == 'ulna') {threshold <- tresh[4]; m <- measurements[[4]]} 
		if(i == 'radius') {threshold <- tresh[5]; m <- measurements[[5]]} 
		if(i == 'os_coxa') {threshold <- tresh[6]; m <- measurements[[6]]} 
		if(i == 'femur') {threshold <- tresh[7]; m <- measurements[[7]]} 
		if(i == 'tibia') {threshold <- tresh[8]; m <- measurements[[8]]} 
		if(i == 'fibula') {threshold <- tresh[9]; m <- measurements[[9]]} 
	
		dir.create(paste(i))
		
		print(paste("Current Element:", i))
		
		if(i == "clavicle" || i == "scapula" || i ==  "humerus" || i ==  "ulna" || i ==  "radius" || i ==  "os_coxa" || i ==  "femur" || i ==  "tibia" || i ==  "fibula") {
			wtf <- pm.input(bone=i, sort=sort, template=template, tresh=threshold, measurements=m) 
			if(class(wtf[[1]]) == "list") {
				directemp <- pm.ttest(power = power, refdata = wtf[[2]], sortdata = wtf[[1]], stdout = stdout, sessiontempdir=paste(direcb,i,sep="/"), alphalevel = alphalevel, absolutevalue = absolutevalue, a = TRUE, testagainst = testagainst, oo = oo, no_cores = no_cores)
			}
		}
		if(i == "hu" || i == "hr" || i == "hs" || i == "hss" || i == "fi" || i == "ft" || i == "ftt") {
			wtf <- art.input(bone=i, sort=sort)
			if(class(wtf[[1]]) == "data.frame") {
				directemp <- art.ttest(power = power, refdata = wtf[[2]], sortdata = wtf[[1]], stdout = stdout, sessiontempdir=paste(direcb,i,sep="/"), alphalevel = alphalevel, absolutevalue = absolutevalue, a = TRUE, testagainst = testagainst, oo = oo, no_cores = no_cores)   	
			}
		}	
		if(class(directemp) == "list") {
			direc2[[2]] <- na.omit(rbind(direc2[[2]], directemp[[2]])) #na.omit removes the first row created by row binding an empty dataframe in the list
		}
		if(class(directemp) == "list") {
			direc2[[3]] <- na.omit(rbind(direc2[[3]], directemp[[3]])) #na.omit removes the first row created by row binding an empty dataframe in the list
		}
			lla <- nrow(direc2[[2]]) + nrow(direc2[[3]])
			ll <- lla + ll
			nbb <- nrow(direc2[[2]]) + nbb	
	}
	
	#direc2[[2]] <- direc2[[2]][-1,]
	#direc2[[3]] <- direc2[[3]][-1,]
	
	setwd(workingdir)
	return(list(direca,direc2, nbb, ll))



}