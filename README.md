# Carbonate Budget Restoration Tool

This repository contains a Shiny app and carbonate-budget data for sites in the Florida Reef Tract. It is designed to aid reef restoration practitioners in identifying ideal sites, species balances, and outplant strategies to achieve restoration goals.

The Carbonate Budget Restoration Tool was adapted by Connor M. Jenkins at the U.S. Geological Survey St. Petersburg Coastal and Marine Science Center from Alice Webb's Reef Persistence Tool. Adaptation conceptualized and guided by Dr. Lauren T. Toth (USGS) and Dr. John Morris (NOAA).

## Installation

To install the packages required to use the Carbonate Budget Restoration Tool, run the following line in an R console:

```r
install.packages(c('rsconnect','shiny','bslib','shinydashboard','dashboardthemes',
                   'ggplot2','dplyr','tidyr','leaflet','shinythemes','leaflegend',
                   'ggplot2','tidyverse','ggforce','png','RCurl','jpeg','sf','magrittr',
                   'maps','reshape2','RColorBrewer','plotly','geojsonio','shinyWidgets',
                   'shinyjs','shinyBS','here','readxl','writexl','tidyr','dplyr','jsonlite'))
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

Note: some users may see excessive `file.info()` and/or `unknown aesthetics: text` warnings, which can be safely ignored. To run the app with these warnings silenced, open `launch_app_quiet.R` in RStudio and run it as source (default shortcut: `Ctrl+Shift+S`).

Alternatively, open an R console and run:

```r
source("path/to/launch_app_quiet.R")
```

(Replace path/to/launch_app_quiet.R with the actual path to where launch_app_quiet.R is saved. Note that filepaths in R must use forward-slash ("/") or double-backslash ("\\\\") separators.)

### Using the Interface

Use the `☰` button in the header bar to show/hide the navigation sidebar. Click on the sidebar tabs to navigate between pages.

#### Reef Site Map

Use this tab to view National Coral Reef Monitoring Program (NCRMP) reef survey data from 2014-2024. Select a reef site and view its metadata by clicking on a point.

By default, sites are symbolized according to their reef accretion potential (RAP). The legend is displayed in the bottom left corner.

Aspects of the map display can be manipulated with the collapsible `Map Controls` section in the upper right:  

- Use the `Target Percent-Cover Increase` slider to simulate a hard-coral percent-cover increase of the given amount at the NCRMP sites. Halo symbology will be added around the site points denoting their "Restoration Potential" (i.e., whether restoration results in a transition in the site's calcium carbonate budget).

- Use the `Filter by` group to filter the data points by survey year or habitat.

- Use the `Symbolize by` group to symbolize the site points by reef state (erosion, stasis, or growth) or gross bioerosion.

- Use the `Point size` +/- control to adjust the size of the points.

#### Restoration Planning

Use this tab to simulate a restoration effort at a reef site. Follow these steps to run a simulation:

1. Genus- or species-level coral cover survey data at the target site is required to begin the simulation. This cover is used as the baseline assemblage for the simulation. There are two ways to enter survey data:

    **(a) File upload:** A `Baseline_Cover_TEMPLATE.xlsx` Excel workbook has been included in the `data` folder of this repository to aid in data entry. See the `README` sheet of this workbook for more information. For an example of a complete data file, see `Baseline_Cover_EXAMPLE.xlsx`, also included in the `data` folder.  

    Fill out the template, save it under a new name, and load it using the `Load from file` input in the `Baseline Cover` section. Input parameters will be populated automatically based on the contents of the uploaded file.

    A copy of the most recently uploaded baseline cover file will be cached in a `cache` folder created where `app.R` is stored. The cached file is automatically re-uploaded on the next launch. Use the `Clear cache` button to delete the cached file (the original file will be unaffected).

    **(b) Create from scratch:** Use the inputs to name the site and designate its area, subregion, habitat, and baseline cover. Use the `Save baseline` button to save the scratch inputs in an `.xlsx` file which can be uploaded to the app in a subsequent session.

2. Designate the target post-restoration percent-cover using the sliders in the `Restoration Mix` section. When a target percent-cover is designated for a species, the number of outplants required to meet the target in the given scenario is calculated and displayed beneath the slider.

3. Manipulate additional restoration variables by using the sliders and text inputs in the `Restoration Parameters` section:  

    **Avg. outplant diameter:** The average diameter of the outplants, in centimeters.

    **Avg. outplant cost:** The average cost of each outplant.

    **Restoration horizon:** The number of years post-restoration by when the target percent-cover should be reached.

    **Simulation duration:** The number of years post-restoration that the simulation should last. Can exceed the `Restoration horizon` so the long-term effect of the restoration plan may be observed.

    **Bleaching Scenario:**

    **Degree-Heating Weeks:** The cumulative heat stress expected each year, in degree-heating weeks.

    **Events / 5 years:** The number of bleaching events expected per five years.

4. View the simulation and its predicted cost in the `Projected Reef Accretion Potential (RAP)` timeline.

5. After building the scenario, save it by scrolling to the bottom of the page and using the `Save Scenario` section. Enter the name of the project and scenario, and click `Save`. The scenario will be saved as `{project}__{scenario}.json` in an automatically-generated `scenarios` folder wherever app.R is stored.  

    Saved scenarios' filenames may be edited, but retain the double-underscore between the project and scenario labels. The program uses this convention to automatically recognize and differentiate projects and scenarios.

#### Scenario Comparison

Use this tab to compare scenarios created in the `Restoration Planning` tab.

The app will automatically detect scenarios saved in the `scenarios` folder. Use the `Project name` dropdown to switch between projects, if more than one is present. By default, the first discovered project is loaded in the dropdown, and all of that project's scenarios are enabled for comparison. Toggle the scenarios on and off as desired.

Use the `Refresh list` button to re-scan the `scenarios` folder and refresh the available projects and scenarios.

Use the `Download report` button to download a `.csv` file which summarizes the selected scenarios. A record is created for each scenario in the report.

An `Impact Summary` is displayed for each enabled scenario. Click the carat at the top-right of a Summary to collapse it.

#### Restoration Monitoring

Use this tab to monitor an ongoing restoration effort using observed coral-cover and bioerosion data.

Without observed data, a basic simulation of a restoration effort at an NCRMP site can be "monitored". Growth is modeled as a linear regression between the original percent-cover and the target percent-cover calculated from the target percent-cover increase selected on the `Reef Site Map`. Click a site on the map to select it for this simulated monitoring, or use the `Select site` dropdown in the `Inputs` section of the `Restoration Monitoring` tab.

Use the `Upload coral cover data` and `Upload bioerosion data` to use observed data for monitoring, if available. Report the observed data by filling out the `Restoration_Monitoring_TEMPLATE.xlsx` and `Bioerosion_TEMPLATE.xlsx` included in the `data` folder of the repository. See the `README` sheets of these workbook files for more information on data entry. See `Restoration_Monitoring_EXAMPLE.xlsx` and `Bioerosion_EXAMPLE.xlsx` for examples of a complete set of monitoring observation data.

If the observed reports include data for more than one site, use the `Select site` dropdown to select the site to monitor. 

A comparison between the Baseline and Restored coral cover, carbonate budget, and reef accretion potential is displayed in the `Baseline vs. Restored Impact` section.

The observed data are used to calculate the site's reef accretion potential over time, which is graphed on the timeline in the `Reef Accretion Potential` section.

#### About this App

Contains summary, background, author, source, and methodological information.

## Artificial Intelligence Disclosure

Claude Opus 4.8 was employed in July 2026 to convert the original Shiny bootstrapPage logic to dashboardPage logic, and to assist in creating the app layout and connecting widgets to their intended functions. All code was reviewed, tested, and validated by the authors to ensure correctness and reproducibility. Any use of trade, firm, or product names is for descriptive purposes only and does not imply endorsement by the U.S. Government.

## Recommended Citation

Jenkins, C.M., Toth, L.T., and Morris, J., 2026, Carbonate Budget Restoration Tool Version 1.0: U.S. Geological Survey software release, [DOI placeholder].