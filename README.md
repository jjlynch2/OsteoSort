## OsteoSort 1.3.0

## Installation
```javascript
install.packages("devtools")
library(devtools)
install_github("jjlynch2/OsteoSort", ref="v1.3.0")
library(OsteoSort)
OsteoSort()
```

## R Dependencies
* pixmap            Converting images to pixmaps
* jpeg              Converting pixmaps to greyscale
* Morpho            Iterative Closest Point
* DT                Data tables in Shiny
* shiny             GUI
* htmltools         GUI extended features
* zip               Archiving results
* rgl               Displaying and digitizing three-dimensional data
* ClusterR          K-means clustering
* JuliaCall         Integrate R with Julia

## Julia Dependencies
* Pkg               Package management
* Statistics        Basic statistical operations
* Distributed       Distributed parallel processing support
* SharedArrays      Shared arrays for returning data from multiple cores
* Optim             Optimize min/max for use with boxcox
* Rmath             R-style function calls

## Changes for OsteoSort version : 1.3.0

## Other Windows Dependencies
* Julia must be in your Windows environrment PATH to run.

## Known Issues
* A bug exists in the StatsModels package version 0.6.0 in Julia, which prevents RCall from precompiling. This bug breaks JuliaCall in R. Downgrading the package to version 0.5.0 will allow QA3D to run properly.
* Some versions of JuliaCall require RCall to be manually rebuilt in Julia.