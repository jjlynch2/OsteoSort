#Current development status for 1.2.6rc1
# Installation
Requires [**Rtools**](https://cran.r-project.org/bin/windows/Rtools/) to build on Windows

Requires [**Xcode**](https://developer.apple.com/xcode/) to build on OSX

To install directly from GitHub use the devtools R package:

`install.packages("devtools")`

`library(devtools)`

`install_github("jjlynch2/OsteoSort", ref="v1.2.6rc1")`

# Depends
* compiler
* Morpho
* parallel
* pixmap
* jpeg
* CCA
* data.table
* Rcpp
* RcppParallel
* car

# Changes for OsteoSort version : 1.2.6rc1

## Minor changes:
* Updated imports for DESC
* Adusted default scale option for 2D outline extraction

## Moderate changes:
* Added Boxcox transformation for t-tests
* Added dependency on car
* Added option to select number of Principal Components for regression analysis in reg.multitest()
* Added option (default) to select number of Principal Components based on cumulative variance 

## Major changes:
* Added input.3d() and match.3d() functions for importing and pair-matching 3D data respectively
* Added rotation(), translation(), and mean_shape() functions