#' reg.test Input Function
#' Function to produce combinations for associating elements with regression
#' @param sort data to be sorted 
#' @keywords reg.test
#' @export
#' @examples 
#' reg.multitest()

reg.multitest <- function(sort = NULL, ref = NULL, splitn = NULL, predlevel = 0.90, stdout = FALSE, sessiontempdir = NULL, a = FALSE, oo = c(TRUE,FALSE), corlevel = 0.5) {	    
     print("Statistical association comparisons have started.")
	library(parallel)
	library(foreach)
	library(doSNOW)
	require(compiler)
	library(earth)
	library(RcppArmadillo)
	enableJIT(3)

	if(detectCores() > 1) {no_cores <- round(detectCores() /2)}
	if(detectCores() == 1) {no_cores <- 1}
	
	options(warn = -1) #disables warnings
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
		if(a) {
			setwd(sessiontempdir)
			direc <- NULL
		}
	}

	#use random name, assign(), eval(as.symbol()) for global lists? 
	#would avoid environmental scoping problem
	
	is.unique <<- list()
	unique.model <<- list()
	
	hera1 <- apply(sort, 1, function(x) {

		temp1 <- na.omit(x[seq(from = splitn[1]+1, to = splitn[2])])
		temp1n <- names(temp1[-1][-1][-1]) #captures measurement names
		temp2 <- na.omit(x[seq(from = 1, to = splitn[1])])
		temp2n <- names(temp2[-1][-1][-1]) #captures measurement names

		t1 <- as.data.frame(ref[temp1n])# reference
		t2 <- as.data.frame(ref[temp2n])
		output1 <- lapply(is.unique, function(x) { 
			ident <- identical(x, unique(c(temp1n, temp2n)))
			return(ident) 
		})

		index <- match(TRUE,output1)
		
		B1PCAt <- prcomp(t1)
		B2PCAt <- prcomp(t2)

		B1PCA <- B1PCAt$x 
		B2PCA <- B2PCAt$x
			
		t <- as.data.frame(cor(B1PCA, B2PCA, use="complete.obs"))
			
		predictednames <- c()
		for(i in colnames(t)) {
		    if(any(t[,i] > corlevel)) {
			   predictednames <- c(predictednames, i)
		    }
		}

		predictornames <- c()
		for(i in rownames(t)) {
		    if(any(t[i,]  > corlevel)) {
			   predictornames <- c(predictornames, i)
			   }
		}
		
		
		if(length(predictednames) < 1 || length(predictornames) < 1) {
			return(c(temp1[1], temp1[2], temp1[3], temp2[1], temp2[2], temp2[3], RSquare = 0, Excluded="Not enough correlation", Sample_size = nrow(t1)))
		}

		if(is.na(index)) {
			if(length(predictornames) > 1) { B1PCA <- rowSums(B1PCA[,predictornames])}
			if(length(predictednames) > 1) { B2PCA <- rowSums(B2PCA[,predictednames])}
			if(length(predictornames) == 1) { B1PCA <- B1PCA[,predictornames]}
			if(length(predictednames) == 1) { B2PCA <- B2PCA[,predictednames]}
			
			names(B1PCA) <- predictornames
			names(B2PCA) <- predictednames

			df1 <- as.data.frame(cbind(B1PCA, B2PCA))
			model1 <- lm(B2PCA ~ B1PCA, data = df1)

			is.unique[[length(is.unique)+1]] <<-   unique(c(temp1n,temp2n))
			unique.model[[length(unique.model)+1]] <<- model1
		}
		else model1 <- unique.model[[index]]
		
	   
		rsqr1 <- summary(model1)$r.squared


		temp2p <- as.data.frame(t(as.numeric(temp2[-1][-1][-1])))
		temp1p <- as.data.frame(t(as.numeric(temp1[-1][-1][-1])))
		
		names(temp2p) <- temp2n
		names(temp1p) <- temp1n		
		
		temp1p <- as.data.frame(predict(B1PCAt, temp1p))
		temp2p <- as.data.frame(predict(B2PCAt, temp2p))

		if(length(predictednames) > 1) {predicted <- sum(temp2p[,predictednames])}
		if(length(predictornames) > 1) {predictors <- sum(temp1p[,predictornames])}
		if(length(predictednames) <= 1) {predicted <- temp2p[,predictednames]}
		if(length(predictornames) <= 1) {predictors <- temp1p[,predictornames]}

		names(predictors) <- predictornames
		names(predicted) <- predictednames

		pm1 <- predict(model1, newdata = data.frame(B1PCA = predictors), interval="prediction", level = predlevel) #prediction interval based on the lm from model1
		
		
		
		if(predicted <= pm1[3] && predicted >= pm1[2]) { #checks if predicted falls within prediction interval for the predictors
			within <- "Cannot Exclude"
		}
		else within <- "Excluded"
print(within)
		#temp2 <- as.data.frame(t(temp2)) #converts temp2 to dataframe for $ operator
		return(c(temp1[1], temp1[2], temp1[3], temp2[1], temp2[2], temp2[3], RSquare = round(rsqr1, digits = 3), Excluded=within, Sample_size = nrow(t1)))

	})
	
     print("Statistical association comparisons completed.")
     print("File generation has started.")
	hera1 <- as.data.frame(t(hera1))

	if(!stdout) {		
		if(oo[2]) {
			not_excluded <<- hera1[hera1$Ex == "Cannot Exclude",][,-8]
			temp1 <<- unique(as.character(not_excluded[,1]))
			temp2 <<- unique(as.character(not_excluded[,4]))
			unique_IDs <<- unique(c(temp1,temp2))

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
			write.csv(hera1[hera1$Ex == "Cannot Exclude",][,-8], file = "not-excluded-list.csv", row.names=FALSE, col.names = TRUE)
			write.csv(hera1[hera1$Ex == "Excluded",][,-8], file = "excluded-list.csv",row.names=FALSE, col.names = TRUE)
		}
	}

	setwd(workingdir)
     print("File generation has completed.")
	enableJIT(0)


	if(nrow(hera1) == 1) {
		return(list(direc,hera1[hera1$Ex == "Cannot Exclude",], hera1[hera1$Ex == "Excluded",]))
	}
	if(nrow(hera1) > 1) {
		return(list(direc,hera1[hera1$Ex == "Cannot Exclude",][,-8], hera1[hera1$Ex == "Excluded",][,-8]))
	}
}