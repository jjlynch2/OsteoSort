## OsteoSort 1.3.0

## Installation
```javascript
require("devtools")
install_github("jjlynch2/OsteoSort", ref="v1.3.0")
library(OsteoSort)
OsteoSort()
```

## R Dependencies
* pixmap
* jpeg
* Morpho
* DT
* shiny
* htmltools
* zip
* rgl
* ClusterR
* JuliaCall
* ggplot2
* shinyalerts
* ggrepel
* networkD3
* network
* sna
* grid
* dplyr

## Julia Dependencies
* Pkg
* Statistics
* Distributed
* SharedArrays
* Optim
* Rmath

## Changes for OsteoSort version 1.3.0
* The Julia programming language is now integrated as the new analytical platform.
* Reference data can be imported and exported.
* The user interface is now dynamic and generates based on the reference data and user input.
* The ability to check for updates on GitHub has been implemented.
* Z-transform method is now implemeneted.
* Three-dimensional point cloud antimere sorting is now supported.
* Two-dimensional photograph antimere sorting now uses pairwise registration.



## Other Windows Dependencies
* Julia must be in your Windows environrment PATH to run.

## Known Issues
* Some versions of JuliaCall require RCall to be manually rebuilt in Julia.
* Some newer versions of R may require Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true") to avoid warnings being converted to errors during installation.

