## OsteoSort 1.4.0

## Changes
This is a beta version of 1.4.0.

## Installation
```sh
git clone https://github.com/OsteoSort
docker build -t osteosort .
docker run -d -p 4001:3838 osteosort
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
1. Fix deprecated argument in GLM package
2. Change UI for reference
    a. May need a new config for measurements to use in constructing the UI. That way it occurs prior to the database call.
3. Add calls for postgres db
4. Rebuild JSON CoRA API