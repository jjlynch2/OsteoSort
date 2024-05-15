## OsteoSort 1.3.2

## Changes
OsteoSort 1.3.2 will be the final release of OsteoSort with support for 2D and 3D data. This should be finalized within the next few weeks. The 3D osteometric sorting method will be pulled into a new project and released as an independent tool (OS3D) due to increased complexity. The 2D osteomertic sorting method will be deprecated.

Future iterations of OsteoSort (1.4.x) will include a cloud-based environment (freely accessible with registered accounts) and instructions on deploying a docker container. That release will support the traditional metric-based osteometric sorting methods. This is anticpated to be released sometime next year.

## OsteoSort 1.3.2 (development) Installation
```javascript
require("devtools")
install_github("jjlynch2/OsteoSort")
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
* shinydashboard
* shinyWidgets

## Julia Dependencies
* Pkg
* Statistics
* Distributed
* SharedArrays
* Optim
* Rmath

## Other Windows Dependencies
* Julia must be in your Windows environrment PATH to run.

## Known Issues
* Some systems require the RCall package in Julia to be rebuilt. 
```javascript
using Pkg
Pkg.build("RCall")
```
