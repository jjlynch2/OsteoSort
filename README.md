# Installation
Requires [**Rtools**](https://cran.r-project.org/bin/windows/Rtools/) to build on Windows
Requires [**Xcode**](https://developer.apple.com/xcode/) to build on OSX

To install directly from GitHub use the devtools R package:

`require(devtools)`
`install_github("jjlynch2/OsteoSort", ref = "v1.2.5")`

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

# Changes for OsteoSort version : 1.2.5

## Minor changes:
* Added ByteCompile: yes on install
* Adjusted articulation t-test default options

## Moderate changes:
* Removed plyr as a dependency
* Adjusted population specific Trotter corrections for antemortem stature strength association

