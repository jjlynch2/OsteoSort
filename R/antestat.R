#' antestat Input Function
#'
#' Function to produce a p-value evaluating the strength of evidence for comparing an antemortem stature to postmortem measurement
#'
#' @param antemortem_stature The antemortem stature for comparison
#' @param postmortem_measurement The postmortem measurement for comparison
#' @param bone The bone type to compare
#' @param prediction_interval The prediction interval level
#' @param data The population data to use for modeling
#' @param tails The number of tails for the t-statistic
#' @param output_options C(TRUE,FALSE) First logic specifies excel output, second specifies plot output
#' @param sessiontempdir Specifies temporary directory for analytical session 
#' @param metric Specifies millimeters (mm), centimeters (cm), or inches (in) for stature
#'
#' @keywords antestat
#' @export
#' @examples 
#' antestat()

antestat <- function(antemortem_stature = NULL, postmortem_measurement = NULL, sessiontempdir = NULL, output_options = c(TRUE,TRUE), bone = "Femur", prediction_interval = 0.95, population = "Trotter-any-male", tails = 2, metric = "mm") {
     print("Antemortem stature to postmortem measurement comparison has started.")	
	workingdir = getwd()
	direc <- OsteoSort:::analytical_temp_space(output_options, sessiontempdir) #creates temporary space 
	p1 <- NULL
	#cleans up parameters
	bone <- tolower(bone)
	if(bone == "humerus") {bonemeasurement <- "Hum_01"}
	if(bone == "radius") {bonemeasurement <- "Rad_01"}
	if(bone == "ulna") {bonemeasurement <- "Uln_01"}
	if(bone == "femur") {bonemeasurement <- "Fem_01"}
	if(bone == "tibia") {bonemeasurement <- "Tib_01"}
	if(bone == "fibula") {bonemeasurement <- "Fib_01"}

	if(population == "DPAA-any-male") {
		K <- read.table(system.file("extdata", "dpaa_pc", package = "OsteoSort"), header = TRUE, sep=",", stringsAsFactors = FALSE)
	}
	if(population == "DPAA-white-male") {
		K <- read.table(system.file("extdata", "dpaa_pc", package = "OsteoSort"), header = TRUE, sep=",", stringsAsFactors = FALSE)
		K <- K[K$Anc == "W",]
	}
	if(population == "DPAA-black-male") {
		K <- read.table(system.file("extdata", "dpaa_pc", package = "OsteoSort"), header = TRUE, sep=",", stringsAsFactors = FALSE)
		K <- K[K$Anc == "B",]
	}
	if(population == "Trotter-any-male") {
		K <- read.table(system.file("extdata", "trot_pc", package = "OsteoSort"), header = TRUE, sep=",", stringsAsFactors = FALSE)
	}
	if(population == "Trotter-white-male") {
		K <- read.table(system.file("extdata", "trot_pc", package = "OsteoSort"), header = TRUE, sep=",", stringsAsFactors = FALSE)
		K <- K[K$Anc == "W",]
	}
	if(population == "Trotter-black-male") {
		K <- read.table(system.file("extdata", "trot_pc", package = "OsteoSort"), header = TRUE, sep=",", stringsAsFactors = FALSE)
		K <- K[K$Anc == "B",]
	}
	if(population == "20th-FStat-any") {
		K <- read.table(system.file("extdata", "fdb_pc", package = "OsteoSort"), header = TRUE, sep=",", stringsAsFactors = FALSE)
	}
	if(population == "20th-FStat-any-male") {
		K <- read.table(system.file("extdata", "fdb_pc", package = "OsteoSort"), header = TRUE, sep=",", stringsAsFactors = FALSE)
		K <- K[K$Sex == "M",]	
	}
	if(population == "20th-FStat-white-male") {
		K <- read.table(system.file("extdata", "fdb_pc", package = "OsteoSort"), header = TRUE, sep=",", stringsAsFactors = FALSE)
		K <- K[K$Anc == "W",]
		K <- K[K$Sex == "M",]
	}
	if(population == "20th-FStat-black-male") {
		K <- read.table(system.file("extdata", "fdb_pc", package = "OsteoSort"), header = TRUE, sep=",", stringsAsFactors = FALSE)
		K <- K[K$Anc == "B",]
		K <- K[K$Sex == "M",]
	}
	if(population == "20th-FStat-any-female") {
		K <- read.table(system.file("extdata", "fdb_pc", package = "OsteoSort"), header = TRUE, sep=",", stringsAsFactors = FALSE)
		K <- K[K$Sex == "F",]
	}
	if(population == "20th-FStat-white-female") {
		K <- read.table(system.file("extdata", "fdb_pc", package = "OsteoSort"), header = TRUE, sep=",", stringsAsFactors = FALSE)
		K <- K[K$Anc == "W",]	
		K <- K[K$Sex == "F",]
	}
	if(population == "20th-FStat-black-female") {
		K <- read.table(system.file("extdata", "fdb_pc", package = "OsteoSort"), header = TRUE, sep=",", stringsAsFactors = FALSE)
		K <- K[K$Anc == "B",]	
		K <- K[K$Sex == "F",]
	}

	#antemortem stature regression
	K <- data.frame(cbind(K[[bonemeasurement]], K$Stature))
	K <- K[rowSums(is.na(K)) != 1,]
     measurement <- K[,1]
	stature <- K[,2]

	if(metric == "mm") {stature <- stature * 10}
	if(metric == "in") {stature <- stature / 2.54}


	lm1 <- lm(stature~measurement) #reference regression model
	pm1 <- predict(lm1, newdata = data.frame(measurement = postmortem_measurement), interval = "prediction", level = prediction_interval)
	nref <- length(measurement) #reference size
	tt <- abs(round(pm1[1], digits=2) - antemortem_stature) / ( summary.lm((lm1))$sigma * sqrt( 1+(1/nref) + ((postmortem_measurement - mean(measurement))^2) / (nref * sd(measurement)^2) ) )
	pp <- tails * pt(-abs(tt), df = nref - 2)

	res <- data.frame(antemortem_stature, postmortem_measurement, round(tt, digits = 2), round(pp, digits=2), nref, round(pm1[2], digits=2), round(pm1[1], digits=2), round(pm1[3], digits=2))
	colnames(res) <- c("antemortem_stature", "postmortem_measurement", "t-statistic", "p-value","sample_size", "lower_PI","point_estimate","upper_PI")
	

     print("Antemortem stature to postmortem measurement comparison has completed.")	

	print("File generation has started.")
	if(output_options[2]) {
		jpeg(paste("graph",".jpg",sep=''),height = 800, width = 800)
		dev.control('enable')
		
		plot(stature,measurement, xlab = "Stature", ylab = "Measurement")
	
		points(pm1[1],postmortem_measurement,col="blue",pch=16)
	
		lmp1 <- predict(lm1, interval = "prediction", level = prediction_interval)

		matlines(lmp1[,1], measurement, col=c("red"))
		matlines(lmp1[,2], measurement, col=c("blue"), lty = 4)
		matlines(lmp1[,3], measurement, col=c("blue"), lty = 4)

		p1 <- recordPlot()
		dev.off()
	}

	if(output_options[1]) {
		write.csv(res, file = "results.csv", row.names=FALSE, col.names=TRUE)
	}

	gc()
	setwd(workingdir)
     print("File generation has completed.")


	return(list(direc,res,p1))
}
