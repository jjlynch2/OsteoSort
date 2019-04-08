#' Setup Julia Environment
#' 
#' @keywords JuliaSetup()
#' @export
#' @examples
#' JuliaSetup()

JuliaSetup <- function(cores = NULL, recall = FALSE) {

	if (!recall) {
		julia <- JuliaCall::julia_setup()
		pkg = c("Pkg","Statistics","Distributed","SharedArrays", "Optim", "Rmath")
		for(i in pkg) {
			JuliaCall::julia_install_package_if_needed(i)
		}
	} #avoids setup again if only changing cores

	julia_source(system.file("jl", "Set_Procs.jl", package = "OsteoSort"))
	if(is.null(cores)) {
		julia_call("Set_Procs", (OsteoSort::detectCores()-1), OsteoSort::detectCores())
	}
	else julia_call("Set_Procs", cores, OsteoSort::detectCores())

	julia_source(system.file("jl", "library.jl", package = "OsteoSort"))
	julia_source(system.file("jl", "Hausdorff.jl", package = "OsteoSort"))
	julia_source(system.file("jl", "Pair_match_t_test.jl", package = "OsteoSort"))
	julia_source(system.file("jl", "Box_Cox.jl", package = "OsteoSort"))
	julia_source(system.file("jl", "Euclidean_Distance_Operations.jl", package = "OsteoSort"))

	JV <- JuliaCall:::julia_line(c("-e", "print(VERSION)"), stdout=TRUE)

	return(JV)
}