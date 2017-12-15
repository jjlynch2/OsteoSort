#' antestat.input Input Function
#'
#' Function to generate combinations of antemortem stature to postmortem measurements for use with antestat.regtest.R
#'
#' @param antemortem_stature The antemortem stature for comparison
#' @param postmortem_measurement The postmortem measurement for comparison
#' @param bone The bone type to compare
#' @param population The population data to use for modeling
#' @param metric Specifies millimeters (mm), centimeters (cm), or inches (in) for stature
#'
#' @keywords antestat
#' @export
#' @examples 
#' antestat.input()

antestat.input <- function(antemortem_stature = NULL, postmortem_measurement = NULL, bone = "Femur", population = "Trotter-any-male", metric = "cm") {
	print("Import and reference generation started")
	options(stringsAsFactors = FALSE) 

	options(warn = -1) #disables warnings
	options(as.is = TRUE)
	if(is.na(antemortem_stature) || is.null(antemortem_stature)) {return(NULL)} #input san
	if(is.na(postmortem_measurement) || is.null(postmortem_measurement)) {return(NULL)} #input san	
	
	antemortem_stature <- data.frame(antemortem_stature, stringsAsFactors = FALSE)
	postmortem_measurement <- data.frame(postmortem_measurement, stringsAsFactors = FALSE)

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
	K <- data.frame(cbind(K[[bonemeasurement]], K$Stature), stringsAsFactors = FALSE)
	K <- K[rowSums(is.na(K)) != 1,]

	#converts stature prior to creating models
	if(metric == "mm") {K[,2] <- K[,2] * 10}
	if(metric == "in") {K[,2] <- K[,2] / 2.54}

	K[,2] <- round(K[,2], digits =2) #round conversion

	#postmortem_measurement data.frame clean up
	postmortem_measurement$Element <- tolower(postmortem_measurement$Element) #lower case bone name
	postmortem_measurement <- postmortem_measurement[postmortem_measurement$Element == bone,] #sort by bone
	
	postmortem_measurement <- cbind.data.frame(postmortem_measurement$id, postmortem_measurement$Side, postmortem_measurement$Element, postmortem_measurement[[bonemeasurement]], stringsAsFactors = FALSE) #id, Side, element, and measurement
	postmortem_measurement <- postmortem_measurement[rowSums(is.na(postmortem_measurement)) != 1,] #remove NA rows
	postmortem_measurement[,4] <- as.numeric(postmortem_measurement[,4])
	#antemortem_stature data.frame cleanup
	antemortem_stature <- antemortem_stature[rowSums(is.na(antemortem_stature)) != 1,] #remove NA rows

	#creates combination of antemortem to postmortem data
	nvars1 <- nrow(unique(antemortem_stature))
	nvars2 <- nrow(unique(postmortem_measurement))
	indices <- expand.grid(1:nvars1, 1:nvars2)
	sort <- cbind.data.frame(antemortem_stature[indices[,1],], postmortem_measurement[indices[,2],], stringsAsFactors = FALSE)
	colnames(sort) <- c("id","Stature","id","Side","Element",bonemeasurement)
	colnames(K) <- c("id","Stature")
	gc()

	options(stringsAsFactors = TRUE) #restore default R  
     print("Import and reference generation completed")
	return(list(sort, K))
}
