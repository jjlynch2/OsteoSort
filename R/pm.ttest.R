#' Pair-match T.test Function
#' 
#' @param ref Reference data
#' @param sort Sorted data for comparison
#' @param sessiontempdir Specifies temporary directory for analytical session
#' @param cores Number of cores for parallel processing
#' @param alphalevel Specifies alpha level
#' @param absolutevalue Uses absolute value for D-values if true
#' @param testagainstzero Uses 0 for mean if true 
#' @param output_options C(TRUE,FALSE) First logic specifies excel output, second specifies plot output
#' @param power If true, uses half-normal distribution power transformation
#' @param tails number of tails
#' 
#' @keywords pm.ttest
#' @export
#' @examples
#' pm.ttest()

pm.ttest <- function (ref = NULL, sort = NULL, sessiontempdir = NULL, alphalevel = 0.1, power = TRUE, absolutevalue = TRUE, testagainstzero = FALSE, output_options = c(TRUE, FALSE), cores = 1, tails = 2) {
	print("Statistical pair match comparisons have started.")
	enableJIT(3)

	options(warn = -1) #disables warnings
	if(is.na(sort) || is.null(sort)) {return(NULL)} #input san
	if(is.na(ref) || is.null(ref)) {return(NULL)} #input san
	
	if(power) {p1 <- 0.00005; p2 <- 0.33} #half normal transformation
	else {p1 <- 0; p2 <- 1} #used to prevent writing new code inside loop. This transformation doesn't change the data
	
	workingdir = getwd()

	direc <- OsteoSort:::analytical_temp_space(output_options, sessiontempdir) #creates temporary space 
	

	is.uniquepm <<- list()
	unique.difsd <<- list()
	unique.difm <<- list()
	unique.df <<- list()
	unique.ycol <<- list()
	unique.yrow <<- list()
	unique.difa <<- list()

	myfunpm<-function(X){
		temp1n <- names(X[-c(1:6)][c(T,F)])
		temp1 <- sort(c(temp1n, paste(temp1n,"R",sep="")))

		output1 <- lapply(is.uniquepm, function(zz) { 
			ident <- identical(zz, temp1)
			return(ident) 
		})
		index <- match(TRUE,output1) #index of model if exists

		if(is.na(index)) {
			y <- ref[temp1]
			ycol <- ncol(y)
			yrow <- nrow(y)
			if(absolutevalue) { 
				difa <- ((rowSums(abs(y[c(T,F)] - y[c(F,T)]))+p1) ** p2)
				difsd <- sd(difa)
				if(testagainstzero) {difm <- 0} 
				else difm <- mean(difa)
				
				difa1 <- ((sum(abs(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)])) + p1) ** p2)
				p.value <- tails * pt(-abs(difa1 - difm) / difsd, df = length(difa) - 1) #one-tail for absolute value model
			}
			else {
				difa <- rowSums(y[c(T,F)] - y[c(F,T)])
				difsd <- sd(difa)
				if(testagainstzero) {difm <- 0} 
				else difm <- mean(difa)

				difa1 <- sum(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)])
				p.value <- tails * pt(-abs((difa1 - difm) / difsd), df = length(difa) - 1)
			} 
			is.uniquepm[[length(is.uniquepm)+1]] <<- temp1 #cache me outside 
			unique.difsd[[length(unique.difsd)+1]] <<- difsd
			unique.difm[[length(unique.difm)+1]] <<- difm
			unique.df[[length(unique.df)+1]] <<- length(difa) - 1 #1 for degrees of freedom
			unique.ycol[[length(unique.ycol)+1]] <<- ycol
			unique.yrow[[length(unique.yrow)+1]] <<- yrow
			unique.difa[[length(unique.difa)+1]] <<- difa
		}
		else {
			ycol <- as.numeric(unique.ycol[[index]])
			difm <- as.numeric(unique.difm[[index]])
			yrow <- as.numeric(unique.yrow[[index]])
			difsd <- as.numeric(unique.difsd[[index]])
			difdf <- as.numeric(unique.df[[index]])
			difa <- as.numeric(unique.difa[[index]])

			if(absolutevalue) { 
				difa1 <- ((sum(abs(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)])) + p1) ** p2)
				p.value <- tails * pt(-abs(difa1 - difm) / difsd, df = difdf) #one-tail for absolute value model
			}
			else {
				difa1 <- sum(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)])
				p.value <- tails * pt(-abs((difa1 - difm) / difsd), df = difdf)
			} 
		}
		
		if(round(p.value, digits = 4) > alphalevel) {result1 <- "Cannot Exclude"}
		if(round(p.value, digits = 4) <= alphalevel) {result1 <- "Excluded"}

		if(output_options[2]) {
			no_return_value <- OsteoSort:::output_function(hera1 = list(X[,1], X[,2], difa, difa1), method="exclusion", type="plot")
		}		

		return(data.frame(X[,1], X[,3],X[,5],X[,2],X[,4],X[,6],toString(temp1n),round(p.value, digits = 4),ycol/2,yrow,round(difm, digits = 4),round(difsd, digits = 4),result1, stringsAsFactors=FALSE)) 
	} 

	if(Sys.info()[['sysname']] == "Windows") {
		op <- system.time ( hera1 <- lapply(FUN = myfunpm, X = sort) )
		print(op)
	}
	else {
		op <- system.time ( hera1 <- mclapply(FUN = myfunpm, X = sort, mc.cores = cores, mc.preschedule = TRUE) )
		print(op)
	}

	hera1 <- as.data.frame(data.table::rbindlist(hera1))
	
     colnames(hera1) <- c("id","Side","Element","id","Side","Element","Measurements","p.value","# of measurements","Sample size", "mean", "sd","Result")
     print("Statistical pair match comparisons completed.")
     
	rm(is.uniquepm) #making the environment clean again
	rm(unique.difsd)
	rm(unique.difm)
	rm(unique.df)
	rm(unique.ycol)
	rm(unique.yrow) 
	rm(unique.difa)


	if(output_options[1]) {
		no_return_value <- OsteoSort:::output_function(hera1, method="exclusion", type="csv")
	}
		


	gc()
	setwd(workingdir)
	enableJIT(0)
	return(list(direc,hera1[hera1$p.value > alphalevel,],hera1[hera1$p.value <= alphalevel,]))	

}