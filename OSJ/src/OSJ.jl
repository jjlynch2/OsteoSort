module OSJ

#pulls depends into scope
using Statistics
using GLM
using Rmath
using Optim

#include osteometric sorting code
include("t_test.jl")
include("t_test_cache.jl")
include("t_test_plot.jl")
include("z_test.jl")
include("yeojohnson.jl")
include("regression.jl")
include("regression_helpers.jl")
include("regression_plot.jl")

#export function calls
export ZTEST
export TTEST
export TTESTA
export TTESTB
export TTESTM
export TTESTAB
export TTESTAM
export TTESTBM
export TTESTABM
export TTESTAB_plot
export TTESTA_plot
export TTESTB_plot
export TTEST_plot
export REGSL
export REGSL_plot

end # module OSJ
