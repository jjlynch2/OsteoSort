output_function <- function(hera1 = NULL, rejected = NULL, options = NULL, method = "exclusion", cora_data = NULL, type = "csv", uln = NULL, fpath = NULL) {
	if(method == "options") {
		write.csv(options, file = paste(fpath,"/settings.csv",sep=""), row.names=FALSE, col.names = TRUE)
	}
	if(method == "exclusion") {
		if(type == "cora") {
			measurement_means <- hera1[[2]]
			measurement_sd <- hera1[[3]]
			hera1 <- hera1[[1]]
			#pv <- packageVersion("OsteoSort")
			compare_method <- "OsteoSort"
			if(options[7]) {
				compare_method_settings <- paste('"{',"'version': '",pv,"',",
	                                                "'reference': '",options[1],"',",
	                                                "'distribution': '","z","',",
	                                                "'alpha': ",options[2],'}"', sep="")
			} else {
				opt1 <- opt2 <- opt3 <- c()
				if(options[3] == "TRUE") {
					opt1 <- paste("'absolute_mean': '",options[3],"',",sep="")
				}
				if(options[4] == "TRUE") {
					opt2 <- paste("'zero_mean': '",options[4],"',",sep="")
				}
				if(options[5] == "TRUE") {
					opt3 <-paste("'box_cox': '",options[5],"',",sep="")
				}
				compare_method_settings <- paste('"{',"'version': '",pv,"',",
	                                                "'reference': '",options[1],"',",
	                                                "'distribution': '","t","',",
	                                                "'alpha': ",options[2],",",
	                                                opt1, opt2, opt3,
	                                                "'tails': '",options[6],"'",'}"', sep="")
			}
			se1 <- cora_data[na.omit(match(hera1[,1], cora_data[,2])), 1:7]
			se2 <- cora_data[na.omit(match(hera1[,4], cora_data[,2])), 1:6]
			colnames(se1) <- c("se_id","se_skeletal_element","se_accession_number","se_provenance1","se_provenance2","se_designator","skeletal_bone")
			colnames(se2) <- c("pair_id","pair_skeletal_element","pair_accession_number","pair_provenance1","pair_provenance2","pair_designator")
			hera1[hera1$result == "Excluded",12] <- "yes"
			hera1[hera1$result == "Cannot Exclude",12] <- "no"
			hera1 <- cbind(se1, se2, hera1[,-c(1:6)], elimination_reason = "", elimination_date = "")
			hera1[hera1$result == "yes",]$elimination_reason <- c("statistical")
			hera1[hera1$result == "yes",]$elimination_date <- c(paste(as.POSIXct(format(Sys.time()), tz="GMT")))
			measurements_used <- gsub(" ", "': True, '", x = paste('"{',"'", hera1[,14], sep=""))
			measurements_used <- substr(measurements_used,1,nchar(measurements_used)-3)
			measurements_used <- paste(measurements_used, '}"',sep="")
			measurements <- strsplit(hera1[,14], " ")
			num_measurements <- lengths(measurements)
			hera1 <- hera1[,-c(14)]
			if(options[7]) {
				measurement_means <- substr(measurement_means,1,nchar(measurement_means)-1)
				measurement_sd <- substr(measurement_sd,1,nchar(measurement_sd)-1)
				measurement_means <- paste('"{', measurement_means, '}"', sep="")
				measurement_sd <- paste('"{', measurement_sd, '}"', sep="")
			} else {
				measurement_means <- ""
				measurement_sd <- ""
			}
			new_df <- data.frame(cbind(hera1[,1], hera1[,8],hera1[2:6],hera1[9:13],hera1[7],
								  compare_method,
								  compare_method_settings,
								  hera1$sample,
								  hera1$p_value,
								  hera1$result,
								  num_measurements, 
								  hera1$mean,
								  hera1$sd,
								  measurements_used, 
								  measurement_means, 
								  measurement_sd,
								  hera1$elimination_reason,
								  hera1$elimination_date

					),stringsAsFactors = FALSE
			)
			colnames(new_df) <- c("se_id","pair_id","se_skeletal_element","se_accession_number","se_provenance1","se_provenance2","se_designator","pair_skeletal_element","pair_accession_number","pair_provenance1","pair_provenance2","pair_designator","bonename","compare_method","compare_method_settings","sample_size","pvalue","excluded","num_measurements","mean","sd","measurements_used","measurement_means","measurement_sd","elimination_reason","elimination_date")
			write.table(new_df, file = paste(fpath,"/CoRA_Osteometric_Sorting_Results_Import.csv",sep=""),row.names=FALSE, col.names = TRUE, sep=",", quote=FALSE)
		}
		if(type == "csv") {
			if(nrow(hera1[hera1$result == "Cannot Exclude",]) > 0) {
				write.csv(hera1[hera1$result == "Cannot Exclude",], file = paste(fpath,"/not-excluded-list.csv",sep=""), row.names=FALSE, col.names = TRUE)
			}
			if(nrow(hera1[hera1$result == "Excluded",]) > 0) {
				write.csv(hera1[hera1$result == "Excluded",], file = paste(fpath,"/excluded-list.csv",sep=""),row.names=FALSE, col.names = TRUE)
			}
			if(!is.null(rejected)) {
				if(nrow(rejected) > 1) {
					write.csv(rejected, file = paste(fpath,"/rejected-list.csv",sep=""),row.names=FALSE, col.names = TRUE)
				}
			}
		}
		if(type == "csv2") {
			if(!is.null(hera1)) {
				if(nrow(hera1[hera1$result == "Cannot Exclude",]) > 0) {
					write.csv(hera1[hera1$result == "Cannot Exclude",], file = paste(fpath,"/not-excluded-selected-list.csv",sep=""), row.names=FALSE, col.names = TRUE)
				}
				if(nrow(hera1[hera1$result == "Excluded",]) > 0) {
					write.csv(hera1[hera1$result == "Excluded",], file = paste(fpath,"/excluded-selected-list.csv",sep=""),row.names=FALSE, col.names = TRUE)
				}
			}
			if(!is.null(rejected)) {
				if(nrow(rejected) > 1) {
					write.csv(rejected, file = paste(fpath,"/rejected-selected-list.csv",sep=""),row.names=FALSE, col.names = TRUE)
				}
			}
		}
		if(type == "plot") {
			ptemp <- qplot(hera1[[3]], geom="histogram", xlab="", ylab="", col = I("grey"), fill = I("#126a8f")) + geom_vline(xintercept = hera1[[4]], linetype = "dashed", color="#ea6011", size=1) + theme_minimal() + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20))
			ggsave(paste(fpath,"/graph",hera1[[1]],"-",hera1[[2]],".jpg",sep=''), plot = ptemp, device = "jpeg", dpi = 300, bg="white")
		}
		if(type == "plot2") {
			d <- data.frame(x = hera1[[3]], y = hera1[[4]])
			ptemp <- ggplot(d, aes(x=x, y=y)) + theme_minimal() + geom_point(col = I("grey"), size = 2) + labs(x = hera1[[1]], y = hera1[[2]]) + theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20))
			OLS = lm(hera1[[4]] ~ hera1[[3]])
			pm1 <- predict(OLS, interval = "prediction", level = 1-hera1[[7]])
			ptemp <- ptemp + geom_line(aes(y = pm1[,1]), linetype = "dashed", color = "#ea6011", size=1)
			ptemp <- ptemp + geom_line(aes(y = pm1[,2]), linetype = "dashed", color = "black", size=1)
			ptemp <- ptemp + geom_line(aes(y = pm1[,3]), linetype = "dashed", color = "black", size=1)
			ptemp <- ptemp + geom_point(x = hera1[[5]], y = hera1[[6]], col = "#126a8f", size = 4)
			ggsave(paste(fpath,"/graph",hera1[[1]],"-",hera1[[2]],".jpg",sep=''), plot = ptemp, device = "jpeg", dpi = 300, bg="white")
		}
	}
}
