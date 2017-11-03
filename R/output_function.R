#' output_excel function
#'
#' @param results
#' Only used to call internally
#'

output_function <- function(hera1, method = "exclusion", type = "csv") {

	if(method == "exclusion") {
		if(type == "csv") {
			if(nrow(hera1[hera1$Result == "Cannot Exclude",]) > 0) {
				write.csv(hera1[hera1$Result == "Cannot Exclude",], file = "not-excluded-list.csv", row.names=FALSE, col.names = TRUE)
			}
			if(nrow(hera1[hera1$Result == "Excluded",]) > 0) {
				write.csv(hera1[hera1$Result == "Excluded",], file = "excluded-list.csv",row.names=FALSE, col.names = TRUE)
			}
		}
		if(type == "plot") {
			jpeg(paste("graph",hera1[[1]],"-",hera1[[2]],".jpg",sep=''),height = 400, width = 400)
			dev.control('enable')	
			hist(x = hera1[[3]], xlab = "", main = NULL)
			abline(v = hera1[[4]], lty = 2, col="darkred")
			dev.off()
		}
		if(type == "plot2") {
				jpeg(paste("graph",hera1[[1]],"-",hera1[[2]],".jpg",sep=''),height = 400, width = 400)
				dev.control('enable')
				plot(hera1[[3]],hera1[[4]], xlab = "", ylab = "")
				points(hera1[[5]],hera1[[6]],col="blue",pch=16)
				matlines(hera1[[7]][,1], hera1[[3]], col=c("red"))
				matlines(hera1[[7]][,2], hera1[[3]], col=c("blue"), lty = 4)
				matlines(hera1[[7]][,3], hera1[[3]], col=c("blue"), lty = 4)
				dev.off()
		}
		if(type == "plot3") {
			jpeg(paste("graph",hera1[[1]],"-", hera1[[2]],".jpg",sep=''),height = 400, width = 400)
			dev.control('enable')
			plot(hera1[[3]],hera1[[4]], xlab = "Stature", ylab = "Measurement")
			points(hera1[[5]],hera1[[6]],col="blue",pch=16, cex=1.5)
			points(hera1[[7]],hera1[[6]],col="red",pch=16, cex=1.5)
			df1 <- rbind(hera1[[5]], hera1[[7]])
			df2 <- rbind(hera1[[6]], hera1[[6]])
			matlines(df1, df2, col=c("red"), lty=2)
			matlines(hera1[[8]][,1], hera1[[4]], col=c("red"))
			matlines(hera1[[8]][,2], hera1[[4]], col=c("blue"), lty = 4)
			matlines(hera1[[8]][,3], hera1[[4]], col=c("blue"), lty = 4)
			dev.off()
		}
	}
	if(method == "2D") {
		if(type == "csv-res") {
			write.csv(hera1, file = "potential-matches.csv", row.names=FALSE, col.names=TRUE)
		}
		if(type == "csv-all") {
			write.csv(hera1, file = "all-distances.csv", row.names=FALSE, col.names=TRUE)
		}
		if(type == "coord") {
			writetps(hera1, file = "Coordinates.tps")
		}
		if(type == "plot") {
			if(!is.list(hera1)) {
				jpeg(filename="registration.jpg", width = 400, height = 400)
				dev.control('enable')	
				plot(apply(hera1, c(1,2), mean), col="white", xlim=c(min(hera1),max(hera1)), ylim=c(max(hera1),min(hera1)), xlab="", ylab="")
				for(a in 1:dim(hera1)[3]) {
					points(hera1[,,a], col=OsteoSort:::add.alpha(a,0.3))	
				}
				points(apply(hera1, c(1,2), mean), col="black", bg="blue", pch=23)
				dev.off()
			}
			if(is.list(hera1)) {
				for(i in seq(from = 2, to = length(hera1), by=2)) {
					jpeg(filename=paste(names(hera1)[[i]], "-", names(hera1)[[i-1]], ".jpg",sep=""), width = 400, height = 400)
					dev.control('enable')	
					plot(hera1[[i]], col="red", xlab="", ylab="")
					points(hera1[[i-1]], col="blue")	
					dev.off()
				}
			}
		}
	}
	if(method == "OS") {
		if(type == "csv") {
			write.csv(hera1[[1]], file=hera1[[2]], row.names=FALSE)
		}
		if(type == "plot") {
			jpeg(paste("graph",".jpg",sep=''),height = 400, width = 400)
			dev.control('enable')	
			hist(x = as.numeric(hera1[[1]]), xlab = hera1[[2]], main = NULL)
			abline(v = hera1[[3]], lty = 2, col="darkred")
			abline(v = hera1[[4]], lty = 2, col="darkblue")
			abline(v = hera1[[5]], lty = 2, col="darkblue")
			if(!hera1[[8]]) {
				abline(v = hera1[[6]], lty = 2, col="black")
				abline(v = hera1[[7]], lty = 2, col="black")
			}
			dev.off()
		}
	}
}