#' Internal function for output_options
#'
#' @param hera1 The data to be ploted
#' @param method The analytical method to be plotted
#' @param type The type of output
#'
#' @examples
#' output_function()

output_function <- function(hera1, method = "exclusion", type = "csv") {
	print("Writing output files")
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
			ptemp <- qplot(hera1[[3]], geom="histogram", xlab="", ylab="", col = I("grey"), fill = I("#126a8f")) + geom_vline(xintercept = hera1[[4]], linetype = "dashed", color="#ea6011") + theme_minimal() + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20))
			ggsave(paste("graph",hera1[[1]],"-",hera1[[2]],".jpg",sep=''), plot = ptemp, device = "jpeg", dpi = 300)
		}
		if(type == "plot2") {
			d <- data.frame(x = hera1[[3]], y = hera1[[4]])
			ptemp <- ggplot(d, aes(x=x, y=y)) + theme_minimal() + geom_point(col = I("grey"), size = 2) + labs(x = hera1[[1]], y = hera1[[2]]) + theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20))
			OLS = lm(hera1[[4]] ~ hera1[[3]])
			pm1 <- predict(OLS, interval = "prediction", level = 1-hera1[[7]])
			ptemp <- ptemp + geom_line(aes(y = pm1[,1]), linetype = "dashed", color = "#ea6011")
			ptemp <- ptemp + geom_line(aes(y = pm1[,2]), linetype = "dashed", color = "black")
			ptemp <- ptemp + geom_line(aes(y = pm1[,3]), linetype = "dashed", color = "black")
			ptemp <- ptemp + geom_point(x = hera1[[5]], y = hera1[[6]], col = "#126a8f", size = 4)
			ggsave(paste("graph",hera1[[1]],"-",hera1[[2]],".jpg",sep=''), plot = ptemp, device = "jpeg", dpi = 300)
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
				heram <- apply(hera1, c(1,2), mean)
				d <- data.frame(x = heram[,1], y = heram[,2])
				ptemp <- ggplot(d, aes(x=x, y=y)) + theme_minimal() + geom_point(col = I("white"), size = 1) + labs(x = "", y = "") + xlim(c(min(hera1), max(hera1))) + ylim(c(max(hera1), min(hera1)))
				for(a in 1:dim(hera1)[3]) {
					ptemp <- ptemp + geom_point(x = hera1[,1,a], y = hera1[,2,a], col = OsteoSort:::add.alpha(a,0.3), size = 3)
				}
				ptemp <- ptemp + geom_point(x = heram[,1], y = heram[,2], col = I("black"), size = 4) + labs(x="", y="")
				ggsave(paste("Registration",".jpg",sep=''), plot = ptemp, device = "jpeg", dpi = 300)
			}
			if(is.list(hera1)) {
				for(i in seq(from = 2, to = length(hera1), by=2)) {
					tempa <- rbind(hera1[[i]], hera1[[i-1]])
					d <- data.frame(x = tempa[,1], y = tempa[,2])
					Specimens <- c(rep(names(hera1)[[i]], nrow(hera1[[i]])), rep(names(hera1)[[i-1]], nrow(hera1[[i-1]])))
					ptemp <- ggplot(d, aes(x = x, y = y, color = Specimens)) + theme_minimal() + geom_point(size = 3) 
					ggsave(paste(names(hera1)[[i]], "-", names(hera1)[[i-1]], ".jpg",sep=""), plot = ptemp, device = "jpeg", dpi = 300)
				}
			}
		}
	}
	if(method == "OS") {
		if(type == "csv") {
			write.csv(hera1[[1]], file=hera1[[2]], row.names=FALSE)
		}
		if(type == "plot") {
			ptemp <- qplot(as.numeric(hera1[[1]]), geom="histogram", xlab="", ylab="", col = I("grey"), fill = I("#126a8f")) + theme_minimal() + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20))
			ptemp <- ptemp + geom_vline(xintercept = hera1[[3]], linetype = "dashed", color="#ea6011") + geom_vline(xintercept = hera1[[4]], linetype = "dashed", color="darkgrey") + geom_vline(xintercept = hera1[[5]], linetype = "dashed", color="darkgrey") 
			if(!hera1[[8]]) {
				ptemp <- ptemp + geom_vline(xintercept = hera1[[6]], linetype = "dashed", color="black") + geom_vline(xintercept = hera1[[7]], linetype = "dashed", color="black") 
			}
			ggsave(paste("graph",hera1[[1]],"-",hera1[[2]],".jpg",sep=''), plot = ptemp, device = "jpeg", dpi = 300)
		}
	}
	print("Output files written")
}
