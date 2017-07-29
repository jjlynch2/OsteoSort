#' Articulation comparison t-test Function
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
#'
#' @keywords art.ttest
#' @export
#' @examples
#' art.ttest()


art.ttest <- function (ref = NULL, sort = NULL, sessiontempdir = NULL, cores = 1, alphalevel = 0.1, absolutevalue = TRUE, testagainstzero = FALSE, output_options = c(TRUE,FALSE), power = TRUE) {
     print("Statistical articulation comparisons have started.")
	enableJIT(3)
	
	options(warn = -1) #disables warnings
	options(as.is = TRUE)
	if(is.na(sort) || is.null(sort)) {return(NULL)} #input san
	if(is.na(ref) || is.null(ref)) {return(NULL)} #input san
	
	if(power) {p1 <- 0.00005; p2 <- 0.33} #half normal transformation
	else {p1 <- 0; p2 <- 1} #used to prevent writing new code inside loop. This transformation doesn't change the data
	
	workingdir = getwd()

	direc <- OsteoSort:::analytical_temp_space(output_options, sessiontempdir) #creates temporary space 

	is.uniqueart <<- list()
	unique.difsd2 <<- list()
	unique.difm2 <<- list()
	unique.df2 <<- list()
	unique.ycol2 <<- list()
	unique.yrow2 <<- list()
	unique.difa <<- list()

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
				difa <- (( rowSums(abs(ref[c(T,F)] - ref[c(F,T)])) + p1 ) ** p2)
				difsd <- sd(difa)
				if(testagainstzero) {difm <- 0} 
				else difm <- mean(difa)
				difa1 <- ((sum(abs(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)])) + p1) ** p2)
				p.value <- pt((difa1 - difm) / difsd, df = length(difa) - 1, lower.tail = FALSE) #one-tail for absolute value model
			}
			else {
				difa <- rowSums(ref[c(T,F)] - ref[c(F,T)])
				difsd <- sd(difa)
				if(testagainstzero) {difm <- 0} 
				else difm <- mean(difa)
				difa1 <- sum(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)])
				p.value <- 2 * pt(-abs((difa1 - difm) / difsd), df = length(difa) - 1)
			}

			is.uniqueart[[length(is.uniqueart)+1]] <<- Xname #cache me outside 
			unique.difsd2[[length(unique.difsd2)+1]] <<- difsd
			unique.difm2[[length(unique.difm2)+1]] <<- difm
			unique.df2[[length(unique.df2)+1]] <<- length(difa) - 1 #1 for degrees of freedom
			unique.ycol2[[length(unique.ycol2)+1]] <<- ycol
			unique.yrow2[[length(unique.yrow2)+1]] <<- yrow
			unique.difa[[length(unique.difa)+1]] <<- difa
		}
		else {
			ycol <- as.numeric(unique.ycol2[[index]])
			difm <- as.numeric(unique.difm2[[index]])
			yrow <- as.numeric(unique.yrow2[[index]])
			difsd <- as.numeric(unique.difsd2[[index]])
			difdf <- as.numeric(unique.df2[[index]])
			difa <- as.numeric(unique.difa[[index]])

			if(absolutevalue) {
				difa1 <- ((sum(abs(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)])) + p1) ** p2)
				p.value <- pt((difa1 - difm) / difsd, df = difdf, lower.tail = FALSE) #one-tail for absolute value model
			}
			else {
				difa1 <- sum(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)])
				p.value <- 2 * pt(-abs((difa1 - difm) / difsd), df = difdf)
			}

		}

		if(round(p.value, digits = 4) > alphalevel) {result1 <- "Cannot Exclude"}
		if(round(p.value, digits = 4) <= alphalevel) {result1 <- "Excluded"}
		
		if(output_options[2]) {
			jpeg(paste("graph",X[,1],"-",X[,2],".jpg",sep=''),height = 800, width = 800)
			dev.control('enable')	
			hist(x = difa, xlab = "", main = NULL)
			abline(v = difa1, lty = 2, lwd = 2, col="darkred")
			dev.off()
		}


		return(data.frame(X[1],X[3],X[5],X[2],X[4],X[6],toString(Xname),ycol,round(p.value, digits = 4),yrow,round(difm, digits = 4),round(difsd, digits = 4),result1, stringsAsFactors=FALSE)) 
	}


	if(Sys.info()[['sysname']] == "Windows") {
		op <- system.time ( hera1 <- lapply(FUN = myfun, X = sort) )
		print(op)
	}
	else {
		op <- system.time ( hera1 <- mclapply(FUN = myfun, X = sort, mc.cores = cores, mc.preschedule = TRUE) )
		print(op)
	}

	hera1 = as.data.frame(data.table::rbindlist(hera1))

	colnames(hera1) <- c("ID","Side","Element","ID","Side","Element","Measurements","# of measurements","p.value","Sample size","mean","sd","Result")
     print("Statistical articulation comparisons completed.")

	rm(is.uniqueart) #making the environment clean again
	rm(unique.difsd2)
	rm(unique.difm2)
	rm(unique.df2)
	rm(unique.ycol2)
	rm(unique.yrow2)
	rm(unique.difa)

	print("File generation has started.")
	if(output_options[1]) {
		no_return_value <- OsteoSort:::output_function(hera1)
	}
	print("File generation has completed.")
	
	gc()
	setwd(workingdir)
	enableJIT(0)
	return(list(direc,hera1[as.numeric(as.character(hera1$p.value)) > alphalevel,],hera1[as.numeric(as.character(hera1$p.value)) <= alphalevel,]))	

}