#' Setup Julia Environment
#' 
#' @keywords JuliaSetup()
#' @export
#' @examples
#' JuliaSetup()

JuliaSetup <- function(add_cores = 1, remove_cores = FALSE, libraries = FALSE, source = FALSE, recall_libraries = FALSE) {

	if (libraries) {
		julia <- JuliaCall::julia_setup()
		pkg = c("Pkg","Statistics","Distributed","SharedArrays", "Optim", "Rmath")
		for(i in pkg) {
			print(paste("Loading Julia package: ", i, sep=""))
			JuliaCall::julia_install_package_if_needed(i)
			JuliaCall::julia_library(i)
		}
		julia_source(system.file("jl", "cores.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "t_test_plot_MC.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "t_test_MC.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "regression_MC.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "Euclidean_Distance_Operations_MC.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "Hausdorff_MC.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "z_test_MC.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "regression_plot_MC.jl", package = "OsteoSort"))
	}

	sycores <- detectCores()
	jcores <- julia_call("nprocs")

	if(add_cores > jcores && add_cores <= sycores) {
		julia_call("add_cores", add_cores - jcores)
	}
	if(add_cores < jcores) {
		julia_call("clean_cores")
		julia_call("add_cores", add_cores-1)
	}
	if(remove_cores) {
		julia_call("clean_cores")
	}

	if (source) {
		julia_source(system.file("jl", "library.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "t_test_WC.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "z_test_WC.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "Box_Cox_WC.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "regression_WC.jl", package = "OsteoSort"))
		julia_source(system.file("jl", "Euclidean_Distance_Operations_WC.jl", package = "OsteoSort"))
	}

	if(recall_libraries) {
		julia_source(system.file("jl", "library.jl", package = "OsteoSort"))
	}

	JV <- JuliaCall:::julia_line(c("-e", "print(VERSION)"), stdout=TRUE)

	return(JV)
}