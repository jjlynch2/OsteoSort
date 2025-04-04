using RCall
using GLM
using Rmath
using Optim
using Statistics
using Suppressor
using Pkg
using OSJ

left = rand(20,4)
right = rand(20,4)
ref_left = rand(20,4)
ref_right = rand(20,4)
tails = 2

#plot functions
TTESTAB_plot(left, right, ref_left, ref_right)
TTESTA_plot(left, right, ref_left, ref_right)
TTEST_plot(left, right, ref_left, ref_right)
TTESTB_plot(left, right, ref_left, ref_right)

#various ttest
TTESTABM(left, right, ref_left, ref_right, tails)
TTESTAM(left, right, ref_left, ref_right, tails)
TTESTBM(left, right, ref_left, ref_right, tails)
TTESTAB(left, right, ref_left, ref_right, tails)
TTESTA(left, right, ref_left, ref_right, tails)
TTESTB(left, right, ref_left, ref_right, tails)
TTESTM(left, right, ref_left, ref_right, tails)
TTEST(left, right, ref_left, ref_right, tails)

#regression
REGSL(left, right, ref_left, ref_right)
REGSL_plot(left, right, ref_left, ref_right)