#' reg.test Input Function
#' Function to produce combinations for associating elements with regression
#'
#' @param ref Reference data
#' @param sort Sorted data for comparison
#' @param splitn The index location of each bone types measurements. Used internally from reg.input()
#' @param prediction_interval Specifies the prediction interval 
#' @param sessiontempdir Specifies temporary directory for analytical session 
#' @param output_options C(TRUE,FALSE) First logic specifies excel output, second specifies plot output
#' @param threads The number of threads to use
#' @param test If true, PCA-CCA-Regression, if false, Simple Linear Regression 
#' @param alphalevel The alpha level for exclusion
#' @param pca Specifies the number of principal components to use
#'
#' @keywords reg.test
#' @export
#' @examples 
#' reg.multitest()

reg.multitest <- function(sort = NULL, ref = NULL, splitn = NULL, prediction_interval = 0.90, sessiontempdir = NULL, output_options = c(TRUE,FALSE), threads = 1, test = TRUE, alphatest = TRUE, alphalevel = 0.05, pca = NULL) {	

	alphalevel
	prediction_interval
	alphatest
	test
	threads
	pca
	output_options
	sessiontempdir
    
     print("Statistical comparisons started")	
	options(stringsAsFactors = FALSE)    

	options(warn = -1) #disables warnings
	options(as.is = TRUE)
	if(is.na(sort) || is.null(sort)) {return(NULL)} #input san
	if(is.na(ref) || is.null(ref)) {return(NULL)} #input san
	
	workingdir = getwd()

	direc <- OsteoSort:::analytical_temp_space(output_options, sessiontempdir) #creates temporary space 

	
	if(test) { #true PCA CCA regression
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
				ident <- identical(x, c(temp1n, temp2n, temp1[2], temp2[2]))
				return(ident) 
			})

			index <- match(TRUE,output1) #index of model if exists

			if(is.na(index)) {
				t1 <- ref[temp1n]# reference
				t2 <- ref[temp2n]
				t1r <- nrow(t1)
				B1PCAt <- prcomp(t1) #PCA one
				B2PCAt <- prcomp(t2) #PCA two

				if(!is.null(pca)) {
					if(pca >= 1) {
						npca1 <- ncol(B1PCAt$x)
						npca2 <- ncol(B2PCAt$x)

						if(npca1 >= pca) {
							B1PCA <- as.matrix(B1PCAt$x[,c(1:pca)]) #PC scores
						}
						else {
							B1PCA <- B1PCAt$x #PC scores use all if specified number is greater than what exists
						}
						
						if(npca2 >= pca) {
							B2PCA <- as.matrix(B2PCAt$x[,c(1:pca)]) #PC scores
						}
						else {
							B2PCA <- B2PCAt$x #PC scores
						}

					}
					if(pca < 1) {
						S1 <- summary(B1PCAt)
						S2 <- summary(B2PCAt)
						for(jj in 1:ncol(S1$importance)){
							if(S1$importance[3,jj] >= pca) {
								B1PCA <- as.matrix(B1PCAt$x[,c(1:jj)]) #PC scores
								break
							}
						}
						for(jj in 1:ncol(S2$importance)){
							if(S2$importance[3,jj] >= pca) {
								B2PCA <- as.matrix(B2PCAt$x[,c(1:jj)]) #PC scores
								break
							}
						}
					}
				}
				else {
					B1PCA <- B1PCAt$x #PC scores
					B2PCA <- B2PCAt$x #PC scores
				}

			
				cmodel1 <- CCA::cc(B1PCA, B2PCA) #CCA model
				score1 <- cmodel1$scores$xscores[,1] #takes first variate y 
				score2 <- cmodel1$scores$yscores[,1] #takes first variate x

				model1 <- lm(score2~score1) #linear model
			
				is.unique[[length(is.unique)+1]] <<- c(temp1n,temp2n, temp1[2], temp2[2]) #cache me outside 
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

				if(!is.null(pca)) {
					if(pca >= 1) {
						npca1 <- ncol(B1PCAt$x)
						npca2 <- ncol(B2PCAt$x)

						if(npca1 >= pca) {
							B1PCA <- as.matrix(B1PCAt$x[,c(1:pca)]) #PC scores
						}
						else {
							B1PCA <- B1PCAt$x #PC scores use all if specified number is greater than what exists
						}
						
						if(npca2 >= pca) {
							B2PCA <- as.matrix(B2PCAt$x[,c(1:pca)]) #PC scores
						}
						else {
							B2PCA <- B2PCAt$x #PC scores
						}

					}
					if(pca < 1) {
						S1 <- summary(B1PCAt)
						S2 <- summary(B2PCAt)
						for(jj in 1:ncol(S1$importance)){
							if(S1$importance[3,jj] >= pca) {
								B1PCA <- as.matrix(B1PCAt$x[,c(1:jj)]) #PC scores
								break
							}
						}
						for(jj in 1:ncol(S2$importance)){
							if(S2$importance[3,jj] >= pca) {
								B2PCA <- as.matrix(B2PCAt$x[,c(1:jj)]) #PC scores
								break
							}
						}
					}
				}
				else {
					B1PCA <- B1PCAt$x #PC scores
					B2PCA <- B2PCAt$x #PC scores
				}



			}
			
			rsqr1 <- summary(model1)$r.squared

			temp2p <- data.frame(t(as.numeric(as.matrix(temp2[-1][-1][-1]))))
			temp1p <- data.frame(t(as.numeric(as.matrix(temp1[-1][-1][-1]))))
		
			names(temp2p) <- temp2n
			names(temp1p) <- temp1n		
		
			if(!is.null(pca)) {
				if(pca >= 1) {
					npca1 <- ncol(B1PCAt$x)
					npca2 <- ncol(B2PCAt$x)

					if(npca1 >= pca) {
						temp1p <- as.data.frame(predict(B1PCAt, temp1p))[c(1:pca)]
					}
					else {
						temp1p <- as.data.frame(predict(B1PCAt, temp1p))
					}
					
					if(npca2 >= pca) {
						temp2p <- as.data.frame(predict(B2PCAt, temp2p))[c(1:pca)]
					}
					else {
						temp2p <- as.data.frame(predict(B2PCAt, temp2p))
					}

				}
				if(pca < 1) {
					S1 <- summary(B1PCAt)
					S2 <- summary(B2PCAt)
					for(jj in 1:ncol(S1$importance)){
						if(S1$importance[3,jj] >= pca) {
							temp1p <- as.data.frame(predict(B1PCAt, temp1p))[c(1:jj)]
							break
						}
					}
					for(jj in 1:ncol(S2$importance)){
						if(S2$importance[3,jj] >= pca) {
							temp2p <- as.data.frame(predict(B2PCAt, temp2p))[c(1:jj)]
							break
						}
					}
				}
			}
			else {
				temp1p <- as.data.frame(predict(B1PCAt, temp1p))
				temp2p <- as.data.frame(predict(B2PCAt, temp2p))
			}
		
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
		
			pm1 <- predict(model1, newdata = data.frame(score1 = as.numeric(df1)), interval = "prediction", level = prediction_interval)
			tt <- abs(pm1[1] - df2) / ( summary.lm((model1))$sigma * sqrt( 1+(1/length(cmodel1$scores$xscores[,1])) + ((df1 - mean(cmodel1$scores$xscores[,1]))^2) / (length(cmodel1$scores$xscores[,1]) * sd(cmodel1$scores$xscores[,1])^2) ) )
			pp <- 2 * pt(-abs(as.numeric(tt)), df = length(cmodel1$scores$xscores[,1]) - 2)

			if(alphatest) {
				if(pp > alphalevel) { #checks if predicted falls within prediction interval for the predictors
					within <- "Cannot Exclude"
				}
				else within <- "Excluded"
			}

			if(!alphatest) {
				if(df2 <= pm1[3] && df2 >= pm1[2]) { #checks if predicted falls within prediction interval for the predictors
					within <- "Cannot Exclude"
				}
				else within <- "Excluded"
			}

			if(output_options[2]) {
				lmp1 <- predict(model1, interval = "prediction", level = prediction_interval)
				score1 <- cmodel1$scores$xscores[,1]
				score2 <- cmodel1$scores$yscores[,1]
				no_return_value <- OsteoSort:::output_function(hera1=list(temp1[1], temp2[1], score1, score2, df2, df1, lmp1, score1), method="exclusion", type="plot2")
			}

			return(data.frame(temp1[1],temp1[2],temp1[3],temp2[1],temp2[2],temp2[3],paste(c(temp1n, temp2n), collapse = " "),length(c(temp1n, temp2n)),round(rsqr1, digits = 3),t1r,round(pp, digits=5),within, stringsAsFactors=FALSE))

		}
	
		sortlist <- split(sort, 1:nrow(sort))


		if(Sys.info()[['sysname']] == "Windows") {
			cl <- makeCluster(threads)
			clusterExport(cl, list("ref", "splitn", "alphalevel", "prediction_interval", "alphatest", "output_options", "is.unique", "unique.model", "unique.pca1", "unique.pca2", "unique.cca", "unique.t1r"), envir = environment())
			clusterEvalQ(cl, { library(CCA) })
			op <- system.time ( hera1 <- parLapply(cl=cl, fun = myfunreg, X = sortlist) )
			print(op)
			stopCluster(cl)
		}
		else {
			op <- system.time ( hera1 <- mclapply(FUN = myfunreg, X = sortlist, mc.cores = threads, mc.preschedule = TRUE) )
			print(op)
		}
		hera1 = as.data.frame(data.table::rbindlist(hera1))
	
		rm(is.unique) #making the environment clean again
		rm(unique.model)
		rm(unique.pca1)
		rm(unique.pca2)
		rm(unique.cca)
		rm(unique.t1l)
	}

	if(!test) { #false simple regression
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
				ident <- identical(x, c(temp1n, temp2n, temp1[2], temp2[2]))
				return(ident) 
			})

			index <- match(TRUE,output1) #index of model if exists
				t1 <- log(rowSums(ref[temp1n]))# reference
				t2 <- log(rowSums(ref[temp2n]))
			if(is.na(index)) {


				t1r <- length(t1)

				model1 <- lm(t2~t1) #linear model
			
				is.uniques[[length(is.uniques)+1]] <<-   c(temp1n,temp2n, temp1[2], temp2[2]) #cache me outside 
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

			pm1 <- predict(model1, newdata = data.frame(t1 = as.numeric(temp1p)), interval = "prediction", level = prediction_interval)
			tt <- abs(pm1[1] - temp2p) / ( summary.lm((model1))$sigma * sqrt( 1+(1/t1r) + ((temp1p - mean(t1))^2) / (t1r * sd(t1)^2) ) )
			pp <- 2 * pt(-abs(tt), df = t1r - 2)

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
			
			if(output_options[2]) {
				lmp1 <- predict(model1, interval = "prediction", level = prediction_interval)
				no_return_value <- OsteoSort:::output_function(hera1=list(temp1[1], temp2[1], t2, t1, temp2p, temp1p, lmp1, t1), method="exclusion", type="plot2")
			}


			return(data.frame(temp1[1],temp1[2],temp1[3],temp2[1],temp2[2],temp2[3],paste(c(temp1n, temp2n), collapse = " "),length(c(temp1n, temp2n)),round(rsqr1, digits = 3),t1r,round(pp, digits=5),within,stringsAsFactors=FALSE))

		}
	
		sortlist <- split(sort, 1:nrow(sort))

		if(Sys.info()[['sysname']] == "Windows") {
			cl <- makeCluster(threads)
			clusterExport(cl, list("ref", "splitn", "alphalevel", "prediction_interval", "alphatest", "output_options", "is.uniques", "unique.models", "unique.t1rs"), envir = environment())
			op <- system.time ( hera1 <- parLapply(cl=cl, fun = myfunregs, X = sortlist) )
			print(op)
			stopCluster(cl)

		}
		else {
			op <- system.time ( hera1 <- mclapply(FUN = myfunregs, X = sortlist, mc.cores = threads, mc.preschedule = TRUE) )
			print(op)
		}
		hera1 = as.data.frame(data.table::rbindlist(hera1))
	
		rm(is.uniques) #making the environment clean again
		rm(unique.models)
		rm(unique.t1ls)


	}

	names(hera1) <- c("id","Side","Element","id","Side","Element","Measurements","# of measurements","RSquared", "Sample","p-value","Result")

	if(output_options[1]) {
		no_return_value <- OsteoSort:::output_function(hera1, method="exclusion", type="csv")
	}

	gc()
	setwd(workingdir)
	options(stringsAsFactors = TRUE) #restore default R  
     print("Statistical comparisons completed")
	return(list(direc,hera1[hera1$Result == "Cannot Exclude",], hera1[hera1$Result == "Excluded",]))
}