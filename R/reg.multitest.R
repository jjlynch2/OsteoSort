#' reg.test Input Function
#' Function to produce combinations for associating elements with regression
#' @param sort data to be sorted 
#' @keywords reg.test
#' @export
#' @examples 
#' reg.multitest()

reg.multitest <- function(sort = NULL, ref = NULL, splitn = NULL, predlevel = 0.90, stdout = FALSE, sessiontempdir = NULL, a = FALSE, oo = c(TRUE,FALSE), plotme = FALSE, no_cores = 1, testtype = TRUE, alphatest = TRUE, alphalevel = 0.05) {	    
     print("Statistical association comparisons have started.")
	suppressMessages(library(parallel))
	suppressMessages(library(doSNOW))
	suppressMessages(library(compiler))
	suppressMessages(library(CCA))
    	suppressMessages(library(data.table))
	enableJIT(3)
	
	options(warn = -1) #disables warnings
	options(as.is = TRUE)
	if(is.na(sort) || is.null(sort)) {return(NULL)} #input san
	if(is.na(ref) || is.null(ref)) {return(NULL)} #input san
	
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

	
	if(testtype) { #true PCA CCA regression
		#global stores models. Works for multi-user environment
		#since reference doesn't change between them.
		is.unique <<- list() 
		unique.model <<- list()
		unique.pca1 <<- list()
		unique.pca2 <<- list()
		unique.cca <<- list()
		unique.t1r <<- list()

		myfunreg<-function(X){
			temp1 <- X[seq(from = splitn[1]+1, to = splitn[2])]
			temp1 <- temp1[ , colSums(is.na(temp1)) == 0]
			temp1n <- names(temp1[-1][-1][-1]) #captures measurement names
			temp2 <- X[seq(from = 1, to = splitn[1])]
			temp2 <- temp2[ , colSums(is.na(temp2)) == 0]
			temp2n <- names(temp2[-1][-1][-1]) #captures measurement names

			#check for cache
			output1 <- lapply(is.unique, function(x) { 
				ident <- identical(x, c(temp1n, temp2n))
				return(ident) 
			})

			index <- match(TRUE,output1) #index of model if exists

			if(is.na(index)) {
				t1 <- ref[temp1n]# reference
				t2 <- ref[temp2n]
				t1r <- nrow(t1)
				B1PCAt <- prcomp(t1) #PCA one
				B2PCAt <- prcomp(t2) #PCA two

				B1PCA <- B1PCAt$x #PC scores
				B2PCA <- B2PCAt$x #PC scores
			
				cmodel1 <- cc(B1PCA, B2PCA) #CCA model
				score1 <- cmodel1$scores$xscores[,1] #takes first variate y 
				score2 <- cmodel1$scores$yscores[,1] #takes first variate x

				model1 <- lm(score2~score1) #linear model
			
				is.unique[[length(is.unique)+1]] <<-   c(temp1n,temp2n) #cache me outside 
				unique.model[[length(unique.model)+1]] <<- model1
				unique.pca1[[length(unique.pca1)+1]] <<- B1PCAt
				unique.pca2[[length(unique.pca2)+1]] <<- B2PCAt
				unique.cca[[length(unique.cca)+1]] <<- cmodel1
				unique.t1r[[length(unique.t1r)+1]] <<- nrow(t1)
			}
			else {
				t1r <- unique.t1r[[index]]
				model1 <- unique.model[[index]]
				B1PCAt <- unique.pca1[[index]]
				B2PCAt <- unique.pca2[[index]]
				cmodel1 <- unique.cca[[index]]
				B1PCA <- B1PCAt$x #PC scores
				B2PCA <- B2PCAt$x #PC scores
			}
			
			rsqr1 <- summary(model1)$r.squared

			temp2p <- data.frame(t(as.numeric(as.matrix(temp2[-1][-1][-1]))))
			temp1p <- data.frame(t(as.numeric(as.matrix(temp1[-1][-1][-1]))))
		
			names(temp2p) <- temp2n
			names(temp1p) <- temp1n		
		
			temp1p <- as.data.frame(predict(B1PCAt, temp1p))
			temp2p <- as.data.frame(predict(B2PCAt, temp2p))
		
			#create CV from coef of cva
			df1 <- 0
			for(i in 1:length(temp1p)) {
				p1 <- temp1p[i] * cmodel1$xcoef[i,1]
				df1 <- (df1 + p1)
			}
		
			df2 <- 0
			for(i in 1:length(temp2p)) {
				p2 <- temp2p[i] * cmodel1$ycoef[i,1]
				df2 <- (df2 + p2)
			}
		
			pm1 <- predict(model1, newdata = data.frame(score1 = as.numeric(df1)), interval = "prediction", level = predlevel)


			#tt <- abs(pm1[1] - as.numeric(df2)) / ( sqrt(diag(vcov(model1)))[2] * sqrt( 1+(1/t1r) + ((as.numeric(df1) - mean(as.numeric(cmodel1$scores$xscores[,1])))^2) / (t1r * sd(as.numeric(cmodel1$scores$xscores[,1]))^2) ) )
			#pp <- 2 * pt(-abs(tt), df = t1r - 1)

			#if(alphatest) {
			#	if(pp > alphalevel) { #checks if predicted falls within prediction interval for the predictors
			#		within <- "Cannot Exclude"
			#	}
			#	else within <- "Excluded"
			#}
			#if(!alphatest) {
				if(df2 <= pm1[3] && df2 >= pm1[2]) { #checks if predicted falls within prediction interval for the predictors
					within <- "Cannot Exclude"
				}
				else within <- "Excluded"
			#}



			return(data.frame(temp1[1],temp1[2],temp1[3],temp2[1],temp2[2],temp2[3],round(rsqr1, digits = 3),t1r,NA,within, stringsAsFactors=FALSE))

		}
	
		sortlist <- split(sort, 1:nrow(sort))

		op <- system.time ( hera1 <- mclapply(FUN = myfunreg, X = sortlist, mc.cores = no_cores, mc.preschedule = TRUE) )
		print(op)
		hera1 = as.data.frame(data.table::rbindlist(hera1))
	
		rm(is.unique) #making the environment clean again
		rm(unique.model)
		rm(unique.pca1)
		rm(unique.pca2)
		rm(unique.cca)
		rm(unique.t1l)
	}

	if(!testtype) { #false simple regression
		#global stores models. Works for multi-user environment
		#since reference doesn't change between them.
		is.uniques <<- list() 
		unique.models <<- list()
		unique.t1rs <<- list()

		myfunregs<-function(X){
			temp1 <- X[seq(from = splitn[1]+1, to = splitn[2])]
			temp1 <- temp1[ , colSums(is.na(temp1)) == 0]
			temp1n <- names(temp1[-1][-1][-1]) #captures measurement names
			temp2 <- X[seq(from = 1, to = splitn[1])]
			temp2 <- temp2[ , colSums(is.na(temp2)) == 0]
			temp2n <- names(temp2[-1][-1][-1]) #captures measurement names

			#check for cache
			output1 <- lapply(is.uniques, function(x) { 
				ident <- identical(x, c(temp1n, temp2n))
				return(ident) 
			})

			index <- match(TRUE,output1) #index of model if exists
				t1 <- log(rowSums(ref[temp1n]))# reference
				t2 <- log(rowSums(ref[temp2n]))
			if(is.na(index)) {
				#t1 <- log(rowSums(ref[temp1n]))# reference
				#t2 <- log(rowSums(ref[temp2n]))
				t1r <- length(t1)

				model1 <- lm(t2~t1) #linear model
			
				is.uniques[[length(is.uniques)+1]] <<-   c(temp1n,temp2n) #cache me outside 
				unique.models[[length(unique.models)+1]] <<- model1
				unique.t1rs[[length(unique.t1rs)+1]] <<- length(t1)
			}
			else {
				t1r <- unique.t1rs[[index]]
				model1 <- unique.models[[index]]
			}
			
			rsqr1 <- summary(model1)$r.squared

			temp2p <- log(rowSums(data.frame(t(as.numeric(as.matrix(temp2[-1][-1][-1]))))))
			temp1p <- log(rowSums(data.frame(t(as.numeric(as.matrix(temp1[-1][-1][-1]))))))

			pm1 <- predict(model1, newdata = data.frame(t1 = as.numeric(temp1p)), interval = "prediction", level = predlevel)
			tt <- abs(pm1[1] - temp2p) / ( sqrt(diag(vcov(model1)))[2] * sqrt( 1+(1/t1r) + ((temp1p - mean(t1))^2) / (t1r * sd(t1)^2) ) )
			pp <- 2 * pt(-abs(tt), df = t1r - 1)

			if(alphatest) {

				if(pp > alphalevel) { #checks if predicted falls within prediction interval for the predictors
					within <- "Cannot Exclude"
				}
				else within <- "Excluded"
			}
			if(!alphatest) {
				if(temp2p <= pm1[3] && temp2p >= pm1[2]) { #checks if predicted falls within prediction interval for the predictors
					within <- "Cannot Exclude"
				}
				else within <- "Excluded"
			}
			return(data.frame(temp1[1],temp1[2],temp1[3],temp2[1],temp2[2],temp2[3],round(rsqr1, digits = 3),t1r,format(pp, scientific = F),within,stringsAsFactors=FALSE))

		}
	
		sortlist <- split(sort, 1:nrow(sort))

		op <- system.time ( hera1 <- mclapply(FUN = myfunregs, X = sortlist, mc.cores = no_cores, mc.preschedule = TRUE) )
		print(op)
		hera1 = as.data.frame(data.table::rbindlist(hera1))
	
		rm(is.uniques) #making the environment clean again
		rm(unique.models)
		rm(unique.t1ls)


	}

     print("Statistical association comparisons completed.")
	names(hera1) <- c("ID","Side","Element","ID","Side","Element","RSquared", "Sample","p-value","Result")
	
	if(plotme) {
		plotres <- plotme(sortdata = sort, refdata = ref, splitn = splitn, predlevel = predlevel, ttype = "reg", testtype = testtype)
	}
	else plotres <- NULL

	if(!stdout) {
    	 print("File generation has started.")
		if(oo[2]) {
			suppressMessages(library(foreach))
			not_excluded <- hera1[hera1$Result == "Cannot Exclude",][,-9]
			temp1 <- unique(as.character(not_excluded[,1]))
			temp2 <- unique(as.character(not_excluded[,4]))
			unique_IDs <- unique(c(temp1,temp2))

			cl <- makeCluster(no_cores)
			registerDoSNOW(cl)
			clusterExport(cl, "not_excluded", envir=environment())
			foreach(i = unique_IDs) %dopar% {
				library(stargazer) #ugh
				if(any(as.character(not_excluded[,1]) == i)) {
					stargazer(not_excluded[as.character(not_excluded[,1]) == i,], type = 'text', out = i, summary = FALSE, rownames = FALSE, title = paste("Potential associations not excluded with specimen: ", i, sep=""))
				}
				if(any(as.character(not_excluded[,4]) == i)) {
					stargazer(not_excluded[as.character(not_excluded[,4]) == i,], type = 'text', out = i, summary = FALSE, rownames = FALSE, title = paste("Potential associations not excluded with specimen: ", i, sep=""))
				}
				sink(as.character(i), append = TRUE, split = FALSE)
				cat('\nDate: ', strftime(Sys.time(), "%Y-%m-%d %H:%M:%S"), 'Analyst___________', ' Initials___________') 
				cat('\nFor Official Use Only') 
				sink()	
			}
			stopCluster(cl)
		}

		if(oo[1]) {
			write.csv(hera1[hera1$Result == "Cannot Exclude",][,-9], file = "not-excluded-list.csv", row.names=FALSE, col.names = TRUE)
			write.csv(hera1[hera1$Result == "Excluded",][,-9], file = "excluded-list.csv",row.names=FALSE, col.names = TRUE)
		}
	}
	gc()
	setwd(workingdir)
     print("File generation has completed.")
	enableJIT(0)

	if(nrow(hera1) == 1) {
		return(list(direc,hera1[hera1$Result == "Cannot Exclude",], hera1[hera1$Result == "Excluded",], plotres))
	}
	else return(list(direc,hera1[hera1$Result == "Cannot Exclude",][,-9], hera1[hera1$Result == "Excluded",][,-9], plotres))
}