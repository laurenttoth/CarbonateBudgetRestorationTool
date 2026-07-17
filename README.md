# Carbonate Budget Restoration Tool

This repository contains a Shiny app and carbonate-budget data for sites in the Florida Reef Tract. It is designed to aid reef restoration practitioners in identifying ideal sites, species balances, and outplant strategies to achieve restoration goals.

The Carbonate Budget Restoration Tool was adapted by Connor M. Jenkins at the U.S. Geological Survey St. Petersburg Coastal and Marine Science Center from Alice Webb's Reef Persistence Tool. Adaptation conceptualized and guided by Lauren T. Toth (USGS) and John T. Morris (NOAA).

## Installation

To install the packages required to use the Carbonate Budget Restoration Tool, run the following line in an R console:

```r
install.packages(c('rsconnect','shiny','bslib','shinydashboard','dashboardthemes','ggplot2','dplyr','tidyr','leaflet','shinythemes','leaflegend','ggplot2','tidyverse','ggforce','png','RCurl','jpeg','sf','magrittr','maps','reshape2','RColorBrewer','plotly','geojsonio','shinyWidgets','shinyjs','shinyBS','here','readxl','tidyr','dplyr','jsonlite'))
```

If the package installation times out, adjust the timeout setting. For example, to increase the timeout from the default 60 seconds to 120 seconds:

```r
options(timeout=120)
```

## Usage

### Launching the app

Open `app.R` in RStudio and run:

```r
shiny::runApp()
```

Keep the R console open to see messages, warnings, and errors from the tool.

Note: some users may see excessive `file.info()` warnings, which can be safely ignored. To run the app with these warnings silenced, open `launch_app_quiet.R` in RStudio and run it as source (default shortcut: Ctrl+Shift+S).

Alternatively, open an R console and run:

```r
source("path/to/launch_app_quiet.R")
```

(Replace path/to/launch_app_quiet.R with the actual path to where launch_app_quiet.R is saved. Note that filepaths in R must use forward-slash ("/") or double-backslash ("\\\\") separators.)

### Using the Interface

NCRMP reef survey data from 2014-2024 is displayed on the map on the "Home" tab. Select a reef site and view its metadata by clicking on a point.

Adjust point size with the +/- control on the bottom right of the map.

Use the `☰` button in the header bar to show/hide the navigation sidebar. Click on the sidebar tabs to navigate between pages.

## Artificial Intelligence Disclosure

Claude Opus 4.8 was employed in July 2026 to convert the original Shiny bootstrapPage logic to dashboardPage logic. All code was reviewed, tested, and validated by the authors to ensure correctness and reproducibility. Any use of trade, firm, or product names is for descriptive purposes only and does not imply endorsement by the U.S. Government.
