#' antestat.regtest Input Function
#'
#' Function to produce a p-value evaluating the strength of evidence for comparing an antemortem stature to postmortem measurement
#'
#' @param antemortem_stature The antemortem stature for comparison
#' @param postmortem_measurement The postmortem measurement for comparison
#' @param prediction_interval The prediction interval level
#' @param tails The number of tails for the t-statistic
#' @param output_options C(TRUE,FALSE) First logic specifies excel output, second specifies plot output
#' @param sessiontempdir Specifies temporary directory for analytical session 
#'
#' @keywords antestat
#' @export
#' @examples 
#' antestat.regtest()

antestat.regtest <- function(sort = NULL, ref = NULL, sessiontempdir = NULL, output_options = c(FALSE,FALSE), prediction_interval = 0.95, tails = 2, alphalevel = 0.05, alphatest = TRUE, cores = 1) {
     print("Antemortem stature to postmortem measurement comparisons have started.")	
	enableJIT(3)

	options(warn = -1) #disables warnings
	options(as.is = TRUE)
	if(is.na(ref) || is.null(ref)) {return(NULL)} #input san
	if(is.na(sort) || is.null(sort)) {return(NULL)} #input san

	workingdir = getwd()
	direc <- OsteoSort:::analytical_temp_space(output_options, sessiontempdir) #creates temporary space 

	#reference regression model
     measurement <- ref[,1]
	stature <- ref[,2]
	lm1 <- lm(stature~measurement)
	nref <- length(measurement) #reference size

	myfunante <- function(X){
		pm1m <- predict(lm1, newdata = data.frame(measurement = as.numeric(X[6])), interval = "prediction", level = prediction_interval)

		tt <- abs(round(pm1m[1,1], digits=2) - X[2]) / ( summary.lm((lm1))$sigma * sqrt( 1+(1/nref) + ((X[6] - mean(measurement))^2) / (nref * sd(measurement)^2) ) )
		tt <- tt[,1] #wtf why is this required Why a data.frame conversion? 

		pp <- tails * pt(-abs(tt), df = nref - 2)

		if(alphatest) {
			if(pp > alphalevel) { #checks if predicted falls within prediction interval for the predictors
				within <- "Cannot Exclude"
			}
			else within <- "Excluded"
		}
		if(!alphatest) {
			if(X[2] <= pm1m[1,3] && X[2] >= pm1m[1,2]) { #checks if predicted falls within prediction interval for the predictors
				within <- "Cannot Exclude"
			}
			else within <- "Excluded"
		}

		if(output_options[2]) {
			jpeg(paste("graph",X[1],"-", X[3],".jpg",sep=''),height = 400, width = 400)
			dev.control('enable')
			plot(stature,measurement, xlab = "Stature", ylab = "Measurement")
			points(pm1m[1,1],X[6],col="blue",pch=16, cex=1.5)
			points(X[2],X[6],col="red",pch=16, cex=1.5)
			df1 <- rbind(pm1m[1,1], X[2])
			df2 <- rbind(X[6], X[6])
			matlines(df1, df2, col=c("red"), lty=2)
			lmp1 <- predict(lm1, interval = "prediction", level = prediction_interval)
			matlines(lmp1[,1], measurement, col=c("red"))
			matlines(lmp1[,2], measurement, col=c("blue"), lty = 4)
			matlines(lmp1[,3], measurement, col=c("blue"), lty = 4)
			dev.off()
		}
		data.frame(X[1],X[2],X[3],X[4],X[5], X[6], round(tt, digits = 2), round(pp, digits=2), nref, round(pm1m[1,2], digits=2), round(pm1m[1,1], digits=2), round(pm1m[1,3], digits=2), round(summary(lm1)$r.squared, digits = 2), within, stringsAsFactors = FALSE)
	}

	sortlist <- split(sort, 1:nrow(sort))

	if(Sys.info()[['sysname']] == "Windows") {
		op <- system.time ( hera1m <- lapply(FUN = myfunante, X = sortlist) )
		print(op)
	}
	else {
		op <- system.time ( hera1m <- mclapply(FUN = myfunante, X = sortlist, mc.cores = cores, mc.preschedule = TRUE) )
		print(op)
	}

	hera1m = as.data.frame(data.table::rbindlist(hera1m))
	colnames(hera1m) <- c("id","antemortem_stature", "id","side","element","postmortem_measurement", "t-statistic", "p-value","sample_size", "lower_PI","point_estimate","upper_PI", "Rsquared", "Result")
	print("Antemortem stature to postmortem measurement comparisons have completed.")	

	if(output_options[1]) {
		print("File generation has started.")
		no_return_value <- OsteoSort:::output_function(hera1m)
		print("File generation has completed.")
	}

	gc()
	setwd(workingdir)

	return(list(direc,hera1m[hera1m$Result == "Cannot Exclude",], hera1m[hera1m$Result == "Excluded",]))
}
