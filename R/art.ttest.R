#' Articulation comparison t-test Function
#' 
#' @param sort Sorted data for comparison
#' @param ref Reference data
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
#' @keywords art.ttest
#' @export
#' @examples
#' art.ttest()


art.ttest <- function (ref = NULL, sort = NULL, sessiontempdir = NULL, threads = 1, alphalevel = 0.1, absolutevalue = FALSE, testagainstzero = FALSE, output_options = c(TRUE,FALSE), boxcox = TRUE, tails = 2, zero_v = 5e-05) {

	alphalevel
	absolutevalue
	testagainstzero
	threads
	tails
	zero_v
	boxcox
	output_options
	sessiontempdir

	options(stringsAsFactors = FALSE)  
     print("Statistical comparisons started")
	
	options(warn = -1) #disables warnings
	options(as.is = TRUE)
	if(is.na(sort) || is.null(sort)) {return(NULL)} #input san
	if(is.na(ref) || is.null(ref)) {return(NULL)} #input san

	workingdir = getwd()

	direc <- OsteoSort:::analytical_temp_space(output_options, sessiontempdir) #creates temporary space 

	is.uniqueart <<- list()
	unique.difsd2 <<- list()
	unique.difm2 <<- list()
	unique.df2 <<- list()
	unique.ycol2 <<- list()
	unique.yrow2 <<- list()
	unique.difa <<- list()
	if(boxcox) {
		unique.boxcox <<- list()
	}

	myfun<-function(X){

		Xname <- names(X[-c(1:6)])
		output1 <- lapply(is.uniqueart, function(zz) { 
			ident <- identical(zz, Xname)
			return(ident) 
		})
		index <- match(TRUE,output1) #index of model if exists
		if(is.na(index)) {
			ycol <- ncol(ref)
			yrow <- nrow(ref)
			if(absolutevalue) {
				difa <- rowSums(abs(ref[c(T,F)] - ref[c(F,T)]))
				difa1 <- sum(abs(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)]))
			}
			if(!absolutevalue){
				difa <- rowSums(ref[c(T,F)] - ref[c(F,T)])
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

			p.value <- tails * pt(-abs(difa1 - difm) / difsd, df = length(difa) - 1)

			is.uniqueart[[length(is.uniqueart)+1]] <<- Xname #cache me outside 
			unique.difsd2[[length(unique.difsd2)+1]] <<- difsd
			unique.difm2[[length(unique.difm2)+1]] <<- difm
			unique.df2[[length(unique.df2)+1]] <<- length(difa) - 1 #1 for degrees of freedom
			unique.ycol2[[length(unique.ycol2)+1]] <<- ycol
			unique.yrow2[[length(unique.yrow2)+1]] <<- yrow
			unique.difa[[length(unique.difa)+1]] <<- difa
			if(boxcox) {
				unique.boxcox[[length(unique.boxcox)+1]] <<- bx
			}
		}
		else {
			ycol <- as.numeric(unique.ycol2[[index]])
			difm <- as.numeric(unique.difm2[[index]])
			yrow <- as.numeric(unique.yrow2[[index]])
			difsd <- as.numeric(unique.difsd2[[index]])
			difdf <- as.numeric(unique.df2[[index]])
			difa <- as.numeric(unique.difa[[index]])
			if(boxcox) {
				bx <- as.numeric(unique.boxcox[[index]])
			}

			if(absolutevalue) {
				difa1 <- sum(abs(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)]))
			}
			if(!absolutevalue){
				difa1 <- sum(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)])
			}
			p.value <- tails * pt(-abs(difa1 - difm) / difsd, df = difdf)

		}

		if(round(p.value, digits = 4) > alphalevel) {result1 <- "Cannot Exclude"}
		if(round(p.value, digits = 4) <= alphalevel) {result1 <- "Excluded"}
		
		if(output_options[2]) {
			no_return_value <- OsteoSort:::output_function(hera1 = list(X[,1], X[,2], difa, difa1), method="exclusion", type="plot")
		}


		return(data.frame(X[1],X[3],X[5],X[2],X[4],X[6],toString(Xname),ycol,round(p.value, digits = 4),yrow,round(difm, digits = 4),round(difsd, digits = 4),result1, stringsAsFactors=FALSE)) 
	}


	if(Sys.info()[['sysname']] == "Windows") {
		cl <- makeCluster(threads)
		clusterExport(cl, list("ref", "alphalevel", "p1", "absolutevalue", "testagainstzero", "output_options", "tails", "is.uniqueart", "unique.difsd2", "unique.difm2", "unique.df2", "unique.ycol2", "unique.yrow2", "unique.difa","unique.boxcox"), envir = environment())
		op <- system.time ( hera1 <- parLapply(cl=cl, fun = myfun, X = sort) )
		print(op)
		stopCluster(cl)
	}
	else {
		op <- system.time ( hera1 <- mclapply(FUN = myfun, X = sort, mc.cores = threads, mc.preschedule = TRUE) )
		print(op)
	}
	hera1 = as.data.frame(data.table::rbindlist(hera1))

	colnames(hera1) <- c("id","Side","Element","id","Side","Element","Measurements","# of measurements","p.value","Sample size","mean","sd","Result")
     print("Statistical articulation comparisons completed.")

	rm(is.uniqueart) #making the environment clean again
	rm(unique.difsd2)
	rm(unique.difm2)
	rm(unique.df2)
	rm(unique.ycol2)
	rm(unique.yrow2)
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
	return(list(direc,hera1[as.numeric(as.character(hera1$p.value)) > alphalevel,],hera1[as.numeric(as.character(hera1$p.value)) <= alphalevel,]))	
}