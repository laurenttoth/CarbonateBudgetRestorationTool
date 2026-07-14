# Carbonate Budget Restoration Tool

This repository contains a Shiny app and carbonate-budget data for sites in the Florida Reef Tract. It is designed to aid reef restoration practitioners in identifying ideal sites, species balances, and outplant strategies to achieve restoration goals.

The Carbonate Budget Restoration Tool was adapted by Connor M. Jenkins at the U.S. Geological Survey St. Petersburg Coastal and Marine Science Center from Alice Webb's Reef Persistence Tool. Adaptation conceptualized and guided by Lauren T. Toth (USGS) and John T. Morris (NOAA).

## Installation

To install the packages required to use the Carbonate Budget Restoration Tool, run the following line in an R console:

```r
install.packages(c('rsconnect','shiny','bslib','shinydashboard','ggplot2','dplyr','tidyr','leaflet','shinythemes','leaflegend','ggplot2','tidyverse','ggforce','png','RCurl','jpeg','sf','magrittr','maps','reshape2','RColorBrewer','plotly','geojsonio','shinyWidgets','shinyjs','shinyBS','here','readxl','tidyr','dplyr'))
```

If the package installation times out, adjust the timeout setting. For example, to increase the timeout from the default 60 seconds to 120 seconds:

```r
options(timeout=120)
```

## Usage

Open `app.R` in RStudio and run:

```r
shiny::runApp()
```
