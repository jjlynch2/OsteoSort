#' Pair-match T.test Function
#' 
#' @param ref Reference data
#' @param sort Sorted data for comparison
#' @param sessiontempdir Specifies temporary directory for analytical session
#' @param threads The number of threads to use
#' @param alphalevel The alpha level for exclusion
#' @param absolutevalue if TRUE uses absolute value for D-values
#' @param testagainstzero if TRUE uses 0 for sample mean
#' @param output_options C(TRUE,FALSE) First logic specifies excel output, second specifies plot output
#' @param boxcox If TRUE uses boxcox power transformation
#' @param zero_v variable to avoid having zero values in boxcox transformation
#' @param tails The number of tails for the t-distribution
#' 
#' @keywords pm.ttest
#' @export
#' @examples
#' pm.ttest()

pm.ttest <- function (ref = NULL, sort = NULL, sessiontempdir = NULL, alphalevel = 0.1, absolutevalue = TRUE, testagainstzero = FALSE, output_options = c(TRUE, FALSE), threads = 1, tails = 2, zero_v = 5e-05, boxcox = TRUE) {
	options(stringsAsFactors = FALSE)	
     print("Statistical comparisons started")
	options(warn = -1) #disables warnings
	if(is.na(sort) || is.null(sort)) {return(NULL)} #input san
	if(is.na(ref) || is.null(ref)) {return(NULL)} #input san
	
	workingdir = getwd()

	direc <- OsteoSort:::analytical_temp_space(output_options, sessiontempdir) #creates temporary space 
	
	is.uniquepm <<- list()
	unique.difsd <<- list()
	unique.difm <<- list()
	unique.df <<- list()
	unique.ycol <<- list()
	unique.yrow <<- list()
	unique.difa <<- list()
	if(boxcox) {
		unique.boxcox <<- list()
	}

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
				difa <- rowSums(abs(y[c(T,F)] - y[c(F,T)]))
				difa1 <- sum(abs(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)]))
			}
			if(!absolutevalue) {
				difa <- rowSums(y[c(T,F)] - y[c(F,T)])
				difa1 <- sum(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)])
			}

			if(boxcox) {
				bx <- car::powerTransform((difa + zero_v))$lambda
				difa <- (difa + zero_v) ^ bx
				difa1 <- (difa1 + zero_v) ^ bx
			}

			difsd <- sd(difa)
			if(testagainstzero) {
				difm <- 0
			} 
			else difm <- mean(difa)

			p.value <- tails * pt(-abs((difa1 - difm) / difsd), df = length(difa) - 1)
			
			is.uniquepm[[length(is.uniquepm)+1]] <<- temp1 #cache me outside 
			unique.difsd[[length(unique.difsd)+1]] <<- difsd
			unique.difm[[length(unique.difm)+1]] <<- difm
			unique.df[[length(unique.df)+1]] <<- length(difa) - 1 #1 for degrees of freedom
			unique.ycol[[length(unique.ycol)+1]] <<- ycol
			unique.yrow[[length(unique.yrow)+1]] <<- yrow
			unique.difa[[length(unique.difa)+1]] <<- difa
			if(boxcox) {
				unique.boxcox[[length(unique.boxcox)+1]] <<- bx
			}
		}
		else {
			ycol <- as.numeric(unique.ycol[[index]])
			difm <- as.numeric(unique.difm[[index]])
			yrow <- as.numeric(unique.yrow[[index]])
			difsd <- as.numeric(unique.difsd[[index]])
			difdf <- as.numeric(unique.df[[index]])
			difa <- as.numeric(unique.difa[[index]])
			if(boxcox) {
				bx <- as.numeric(unique.boxcox[[index]])
			}

			if(absolutevalue) { 
				difa1 <- sum(abs(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)]))
			}
			if(!absolutevalue) {
				difa1 <- sum(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)])
			}

			if(boxcox) {
				difa1 <- (difa1 + zero_v) ^ bx
			}

			p.value <- tails * pt(-abs((difa1 - difm) / difsd), df = difdf) 
		}
		
		if(round(p.value, digits = 4) > alphalevel) {result1 <- "Cannot Exclude"}
		if(round(p.value, digits = 4) <= alphalevel) {result1 <- "Excluded"}

		if(output_options[2]) {
			no_return_value <- OsteoSort:::output_function(hera1 = list(X[,1], X[,2], difa, difa1), method="exclusion", type="plot")
		}		

		return(data.frame(X[,1], X[,3],X[,5],X[,2],X[,4],X[,6],toString(temp1n),round(p.value, digits = 4),ycol/2,yrow,round(difm, digits = 4),round(difsd, digits = 4),result1, stringsAsFactors=FALSE)) 
	} 

	if(Sys.info()[['sysname']] == "Windows") {
		cl <- makeCluster(threads)
		clusterExport(cl, list("ref", "alphalevel", "p1", "absolutevalue", "testagainstzero", "output_options", "tails", "is.uniquepm", "unique.difsd", "unique.difm", "unique.df", "unique.ycol", "unique.yrow", "unique.difa", "unique.boxcox"), envir = environment())
		op <- system.time ( hera1 <- parLapply(cl=cl, fun = myfunpm, X = sort) )
		print(op)
		stopCluster(cl)
	}
	else {
		op <- system.time ( hera1 <- mclapply(FUN = myfunpm, X = sort, mc.cores = threads, mc.preschedule = TRUE) )
		print(op)
	}

	hera1 <- as.data.frame(data.table::rbindlist(hera1))
	
     colnames(hera1) <- c("id","Side","Element","id","Side","Element","Measurements","p.value","# of measurements","Sample size", "mean", "sd","Result")
     
	rm(is.uniquepm) #making the environment clean again
	rm(unique.difsd)
	rm(unique.difm)
	rm(unique.df)
	rm(unique.ycol)
	rm(unique.yrow) 
	rm(unique.difa)
	if(boxcox) {
		rm(unique.boxcox)
	}

	if(output_options[1]) {
		no_return_value <- OsteoSort:::output_function(hera1, method="exclusion", type="csv")
	}

	gc()
	setwd(workingdir)
	options(stringsAsFactors = TRUE) #restore default R  
     print("Statistical comparisons completed")
	return(list(direc,hera1[hera1$p.value > alphalevel,],hera1[hera1$p.value <= alphalevel,]))	
}