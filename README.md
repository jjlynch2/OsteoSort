## OsteoSort 1.4.0

## Changes

## Installation
```sh
docker build -t osteosort .
docker run -d -p 3838:3838 osteosort
```

## R Dependencies
* DT
* shiny
* htmltools
* zip
* JuliaCall
* ggplot2
* shinyalerts
* grid
* dplyr

## Julia Dependencies
* Statistics
* Optim
* Rmath
* GLM
* RCall
* Suppressor
* OSJ (local OsteoSort package)

## Citation
Lynch, J.J. 2025 OsteoSort. Computerized Osteometric Sorting. Version 1.4.0. Defense POW/MIA Accounting Agency, Offutt AFB, NE.

## TODO
1. Verify z-test
2. Add cache for size in z-test? see ttestab cache example
3. Clean up allocated arrays outside of nested loops in z-test
4. Fix deprecated argument in GLM package
5. Verify that the session temp is deleting correctly. Could be a tmp perm issue.
6. Verify the memory is releasing. Could be reactive statements causing this. The app timeout should help with this issue
7. Change UI to be an assemble style for reference
    a. May need a new config for measurements to use in constructing the UI. That way it occurs prior to the database call.
8. Add calls for postgres db
9. Rebuild JSON CoRA OsteoSort API (waiting on Sachin for this)