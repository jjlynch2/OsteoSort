## OsteoSort 1.3.1

## Installation
```javascript
require("devtools")
install_github("jjlynch2/OsteoSort", ref="v1.3.1")
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

## Changes for OsteoSort version 1.3.1
* Bug fix: the independent and dependent mean and standard deviations were switched for probability calculation

## Other Windows Dependencies
* Julia must be in your Windows environrment PATH to run.

## Known Issues
* Some versions of JuliaCall require RCall to be manually rebuilt in Julia.
* Some newer versions of R may require Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true") to avoid warnings being converted to errors during installation.

