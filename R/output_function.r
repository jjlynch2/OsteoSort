#' Internal function for output_options
#'
#' @param hera1 The data to be ploted
#' @param method The analytical method to be plotted
#' @param type The type of output
#' @param return_plot if TRUE plots are displayed rather than written to the analytical directory
#'
#' @examples
#' output_function()

output_function <- function(hera1, method = "exclusion", type = "csv", return_plot = FALSE) {
	print("Writing output files")
	par(mgp=c(3,3,0))
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
			if(!return_plot) {
				jpeg(paste("graph",hera1[[1]],"-",hera1[[2]],".jpg",sep=''),height = 600, width = 600)
				dev.control('enable')
			}	
			hist(x = hera1[[3]], xlab = "", main = NULL, ylab="", cex.axis=1.7)
			abline(v = hera1[[4]], lty = 2, col="darkred", lwd=3,)

			if(!return_plot) {dev.off()}
		}
		if(type == "plot2") {
				if(!return_plot) {
					jpeg(paste("graph",hera1[[1]],"-",hera1[[2]],".jpg",sep=''),height = 600, width = 600)
					dev.control('enable')
				}

				plot(hera1[[3]],hera1[[4]], xlab = "", ylab = "", pch=1, cex.axis=1.7); box(lwd=2)
				points(hera1[[5]],hera1[[6]],col="blue",pch=16, cex=2)
				lines(hera1[[8]],hera1[[7]][,1],  col=c("red"), lwd=2,lty=1)
				lines(hera1[[8]],hera1[[7]][,2],  col=c("blue"),lwd=2,lty=1)
				lines(hera1[[8]], hera1[[7]][,3], col=c("blue"),lwd=2,lty=1)

				if(!return_plot) { dev.off() }
		}
		if(type == "plot3") {
			if(!return_plot) {
				jpeg(paste("graph",hera1[[1]],"-", hera1[[2]],".jpg",sep=''),height = 600, width = 600)
				dev.control('enable')
			}

			plot(hera1[[3]],hera1[[4]], xlab = "", ylab = "", pch=1, cex.axis=1.7); box(lwd=2)
			points(hera1[[7]],hera1[[5]],col="blue",pch=16, cex=2)
			points(hera1[[7]],hera1[[6]],col="red",pch=16, cex=2)

			lines(hera1[[3]], hera1[[8]][,1], col=c("red"),lwd=2,lty=1)
			lines(hera1[[3]], hera1[[8]][,2],col=c("blue"),lwd=2,lty=1)
			lines(hera1[[3]], hera1[[8]][,3], col=c("blue"),lwd=2,lty=1)

			if(!return_plot) { dev.off()}
		}
	}
	if(method == "3D") {
		if(type == "csv-res") {
			write.csv(hera1, file = "potential-matches.csv", row.names=FALSE, col.names=TRUE)
		}
		if(type == "csv-all") {
			write.csv(hera1, file = "all-distances.csv", row.names=FALSE, col.names=TRUE)
		}
		if(type == "coord") {
			writetps(hera1, file = "Coordinates.tps")
		}
		if(type == "dot") {
			jpeg(paste("dotchart",hera1[1,1],".jpg",sep=''),height = 1200, width = 300)
			dev.control('enable')

			a <- hera1
			b <- hera1[-1,]
			c <- hera1[1,]
			stripchart(as.numeric(a[,3]) ~ a[,1], vertical = TRUE, method = "jitter", pch = 16, ylab ="", xlab = a[1,1], col = "white", cex.axis = 1.7, cex.main=1.7, cex.lab=1.7)

			stripchart(as.numeric(b[,3]) ~ b[,1], vertical = TRUE, method = "jitter", pch = 16, col = "black", add = TRUE, cex = 2)

stripchart(as.numeric(c[3]) ~ c[1], vertical = TRUE, method = "jitter", pch = 16, col = "dodgerblue", add = TRUE, cex = 2)
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
				if(!return_plot) {	
					jpeg(filename="registration.jpg", height = 600)
					dev.control('enable')	
				}

				plot(apply(hera1, c(1,2), mean), col="white", xlim=c(min(hera1),max(hera1)), ylim=c(max(hera1),min(hera1)), xlab="", ylab="", cex.axis=1.3)
				for(a in 1:dim(hera1)[3]) {
					points(hera1[,,a], col=OsteoSort:::add.alpha(a,0.3))	
				}
				points(apply(hera1, c(1,2), mean), col="black", bg="blue", pch=23)

				if(!return_plot) { dev.off() }
			}
			if(is.list(hera1)) {
				for(i in seq(from = 2, to = length(hera1), by=2)) {
					if(!return_plot) {
						jpeg(filename=paste(names(hera1)[[i]], "-", names(hera1)[[i-1]], ".jpg",sep=""), height = 600)
						dev.control('enable')	
					}

					plot(hera1[[i]], col="red", xlab="", ylab="", cex.axis=1.3)
					points(hera1[[i-1]], col="blue")	

					if(!return_plot) { dev.off()}
				}
			}
		}
	}
	if(method == "OS") {
		if(type == "csv") {
			write.csv(hera1[[1]], file=hera1[[2]], row.names=FALSE)
		}
		if(type == "plot") {
			if(!return_plot) {
				jpeg(paste("graph",".jpg",sep=''),height = 600, width = 600)
				dev.control('enable')	
			}

			hist(x = as.numeric(hera1[[1]]), xlab = "", main = NULL, ylab="", cex.axis=1.7, )
			abline(v = hera1[[3]], lty = 2, lwd=3, col="darkred")
			abline(v = hera1[[4]], lty = 2, lwd=3, col="darkblue")
			abline(v = hera1[[5]], lty = 2, lwd=3, col="darkblue")
			if(!hera1[[8]]) {
				abline(v = hera1[[6]], lty = 2, lwd=3,  col="black")
				abline(v = hera1[[7]], lty = 2, lwd=3,  col="black")
			}

			if(!return_plot) {dev.off()}
		}
	}
	print("Output files written")
}