#' Pair-match T.test Function
#' 
#' This function takes the combinations and reference data generated from the pm.input() function 
#' and runs a t.test for each combination. If the results are not statistically significant
#' the combination is considered a possible match and saved in two formats, a single .tyt file with all matches
#' and a .csv file with matches. If it's not a match the combination is saved to a .csv file with non-matches
#'
#' @param refdata The reference data from pm.input().
#' @param sortdata The combination data from pm.input().
#' @param sessiontempdir This creates a temporary session directory. Intended for use with the GUI.
#' @param stdout TRUE returns the dataframes only. FALSE returns the dataframes and creates the files specified above.
#' @param alphalevel Sets the alphalevel used in the t.test. 
#' @param absolutevalue Sets if the absolute value of differences should be used.
#' @param testagainst Sets if the t.test should be against the reference mean (FALSE) or zero (TRUE).
#' @param a Intended for use with all.tests() function that runs all bone tests. 
#' @param oo Defines which files to generate. oo = c(TRUE,FALSE) where oo[1] is excel files and oo[2] is individual specimen files
#'
#' @keywords pm.ttest
#' @eyport
#' @examples
#' pm.ttest()

pm.ttest <- function (refdata = NULL, sortdata = NULL, sessiontempdir = NULL, stdout = TRUE, alphalevel = 0.1, power = TRUE, absolutevalue = TRUE, a = FALSE, testagainst = FALSE, oo = c(TRUE,FALSE), no_cores = 1, plotme = FALSE) {
	print("Statistical pair match comparisons have started.")
	suppressMessages(library(parallel))
	suppressMessages(library(doSNOW))
	suppressMessages(library(compiler))
	suppressMessages(library(data.table))
	enableJIT(3)

	options(warn = -1) #disables warnings
	if(is.na(sortdata) || is.null(sortdata)) {return(NULL)} #input san
	if(is.na(refdata) || is.null(refdata)) {return(NULL)} #input san
	
	if(power) {p1 <- 0.00005; p2 <- 0.33} #half normal transformation
	else {p1 <- 0; p2 <- 1} #used to prevent writing new code inside loop. This transformation doesn't change the data
	
	workingdir = getwd()

	if(!stdout) { 
		if(!a) {
			if (!is.null(sessiontempdir)) {
				setwd(sessiontempdir)
			}
			direc <- randomstring(n = 1, length = 12)
			dir.create(direc)
			setwd(direc)
		}
		else {
			setwd(sessiontempdir)
			direc <- NULL
		}
	}
	
	is.uniquepm <<- list()
	unique.difsd <<- list()
	unique.difm <<- list()
	unique.df <<- list()
	unique.ycol <<- list()
	unique.yrow <<- list()

	myfunpm<-function(X){
		temp1n <- names(X[-c(1:6)][c(T,F)])
		temp1 <- sort(c(temp1n, paste(temp1n,"R",sep="")))

		output1 <- lapply(is.uniquepm, function(zz) { 
			ident <- identical(zz, temp1)
			return(ident) 
		})
		index <- match(TRUE,output1) #index of model if exists

		if(is.na(index)) {
			y <- refdata[temp1]
			ycol <- ncol(y)
			yrow <- nrow(y)
			if(absolutevalue) { 
				difa <- ((rowSums(abs(y[c(T,F)] - y[c(F,T)]))+p1) ** p2)
				difsd <- sd(difa)
				if(testagainst) {difm <- 0} 
				else difm <- mean(difa)
				p.value <- pt((((sum(abs(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)])) + p1) ** p2) - difm) / difsd, df = length(difa) - 1, lower.tail = FALSE) #one-tail for absolute value model
			}
			else {
				difa <- rowSums(y[c(T,F)] - y[c(F,T)])
				difsd <- sd(difa)
				if(testagainst) {difm <- 0} 
				else difm <- mean(difa)
				p.value <- 2 * pt(-abs((sum(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)]) - difm) / difsd), df = length(difa) - 1)
			} 
			is.uniquepm[[length(is.uniquepm)+1]] <<- temp1 #cache me outside 
			unique.difsd[[length(unique.difsd)+1]] <<- difsd
			unique.difm[[length(unique.difm)+1]] <<- difm
			unique.df[[length(unique.df)+1]] <<- length(difa) - 1 #1 for degrees of freedom
			unique.ycol[[length(unique.ycol)+1]] <<- ycol
			unique.yrow[[length(unique.yrow)+1]] <<- yrow
		}
		else {
			ycol <- as.numeric(unique.ycol[[index]])
			difm <- as.numeric(unique.difm[[index]])
			yrow <- as.numeric(unique.yrow[[index]])
			difsd <- as.numeric(unique.difsd[[index]])
			difdf <- as.numeric(unique.df[[index]])
			if(absolutevalue) { 
				p.value <- pt((((sum(abs(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)])) + p1) ** p2) - difm) / difsd, df = difdf, lower.tail = FALSE) #one-tail for absolute value model
			}
			else {

				p.value <- 2 * pt(-abs((sum(as.numeric(X[-c(1:6)])[c(T,F)] - as.numeric(X[-c(1:6)])[c(F,T)]) - difm) / difsd), df = difdf)
			} 
		}
		
		return(data.frame(X[,1], X[,3],X[,5],X[,2],X[,4],X[,6],toString(temp1n),round(p.value, digits = 4),ycol/2,yrow,round(difm, digits = 4),round(difsd, digits = 4), stringsAsFactors=FALSE)) 
	} 

	if(Sys.info()[['sysname']] == "Windows") {
		op <- system.time ( hera1 <- lapply(FUN = myfunpm, X = sortdata) )
		print(op)
	}
	else {
		op <- system.time ( hera1 <- mclapply(FUN = myfunpm, X = sortdata, mc.cores = no_cores, mc.preschedule = TRUE) )
		print(op)
	}

	hera1 <- as.data.frame(data.table::rbindlist(hera1))
	
     colnames(hera1) <- c("ID","Side","Element","ID","Side","Element","Measurements","p.value","# of measurements","Sample size", "mean", "sd")
     print("Statistical pair match comparisons completed.")
     
	rm(is.uniquepm) #making the environment clean again
	rm(unique.difsd)
	rm(unique.difm)
	rm(unique.df)
	rm(unique.ycol)
	rm(unique.yrow)

     #calls plot function for generating single user interface plots
     if(plotme) {
		plotres <- plotme(refdata = refdata, sortdata = sortdata, power = power, absolutevalue = absolutevalue, ttype = "pm")
     }
	else plotres <- NULL
     
	if(!stdout) {	
     	print("File generation has started.")	
		if(oo[2]) {
			suppressMessages(library(foreach))
			not_excluded <- hera1[hera1$p.value > alphalevel,]
			temp1 <- unique(not_excluded[,1])
			temp2 <- unique(not_excluded[,4])
			unique_IDs <- unique(c(temp1,temp2))

			cl <- makeCluster(no_cores)
			registerDoSNOW(cl)
			clusterExport(cl, "not_excluded", envir=environment())
			foreach(i = unique_IDs) %dopar% {
				library(stargazer) #ugh
				if(any(not_excluded[,1] == i)) {
					stargazer(not_excluded[not_excluded[,1] == i,], type = 'text', out = i, summary = FALSE, rownames = FALSE, title = paste("Potential pair matches not excluded with specimen: ", i, sep=""))
				}
				if(any(not_excluded[,4] == i)) {
					stargazer(not_excluded[not_excluded[,4] == i,], type = 'text', out = i, summary = FALSE, rownames = FALSE, title = paste("Potential pair matches not excluded with specimen: ", i, sep=""))
				}
				sink(as.character(i), append = TRUE, split = FALSE)
				cat('\nDate: ', strftime(Sys.time(), "%Y-%m-%d %H:%M:%S"), 'Analyst___________', ' Initials___________') 
				cat('\nFor Official Use Only') 
				sink()	
			}
			stopCluster(cl)
		}
		if(oo[1]) {
			write.csv(as.matrix(hera1[hera1$p.value > alphalevel,]), file = "not-excluded-list.csv", row.names=FALSE, col.names = TRUE)
			write.csv(as.matrix(hera1[hera1$p.value <= alphalevel,]), file = "excluded-list.csv",row.names=FALSE, col.names = TRUE)
		}
		
		print("File generation has completed.")
	}
	gc()
	setwd(workingdir)
	enableJIT(0)
	return(list(direc,hera1[hera1$p.value > alphalevel,],hera1[hera1$p.value <= alphalevel,],plotres))	
}