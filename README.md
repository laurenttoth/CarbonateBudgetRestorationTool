# Reef Persistence Tool

## <https://cjenkins-usgs.shinyapps.io/carbonate-budget-restoration-tool/>

This repository contains a Shiny app and carbonate-budget data for sites in the Florida Reef Tract. It is designed to aid reef restoration practitioners in identifying ideal sites, species balances, and outplant strategies to achieve restoration goals.

The Reef Persistence Tool was adapted by Connor M. Jenkins at the U.S. Geological Survey St. Petersburg Coastal and Marine Science Center from Alice Webb's original Reef Persistence Tool. Adaptation conceptualized and guided by Dr. Lauren T. Toth (USGS) and Dr. John Morris (NOAA).

## Usage

Use the `☰` button in the header bar to show/hide the navigation sidebar. Click on the sidebar tabs to navigate between pages.

If the display is poorly scaled, adjust the zoom level in the browser (if using the website) or RStudio (if running locally).

### Reef Site Map

Use this tab to view National Coral Reef Monitoring Program (NCRMP) reef survey data from 2014-2024. Select a reef site and view its metadata by clicking on a point.

By default, sites are symbolized according to their reef accretion potential (RAP). The legend is displayed in the bottom left corner.

If baseline cover data has been uploaded in the `Outplanting Scenarios` tab, the site(s) will be added to the map. The uploaded site points are 50% larger than the NCRMP site points. The site selected in the `Outplanting Scenarios` tab is 25% larger than the rest of the uploaded site points.

Aspects of the map display can be manipulated with the collapsible `Map Controls` section in the upper right:  

- Use the `Target Percent-Cover Increase` slider to simulate a hard-coral percent-cover increase of the given amount at the NCRMP sites. Halo symbology will be added around the site points denoting their `Restored Reef Status` (i.e., the hypothetical post-restoration reef state). If applicable, the selected uploaded site will be symbolized with a `Restoration Potential` halo according to the current restoration scenario.

- Use the `Filter by` group to filter the data points by habitat, survey year, bioerosion, and coral cover.

  - Use the `Habitat` dropdown to select which habitats to display.

  - Use the `Survey year`, `Gross bioerosion`, and `Current coral cover` double-ended sliders to restrict the range of their respective values. Sites whose associated value is outside of the specified range will not be drawn on the map. `Survey year` includes all available years; `Gross bioerosion` and `Current coral cover` have been split into five natural-breaks bins.

- Use the `Symbolize by` group to symbolize the site points and uploaded reef points by reef accretion potential, reef state (erosion, stasis, or growth) or gross bioerosion.

- Use the `Show named reefs` checkbox to show/hide named reef sites. When enabled, reef-site points and polygons will be displayed and labeled according to the sites' names. There are hundreds of named sites, so they are only displayed on closer zoom levels for legibility when enabled. Site point locations were provided by the Florida Keys National Marine Sanctuary (FKNMS). Polygons representing additional named reef sites were retrieved from Florida Fish & Wildlife Conservation Commission (FWC) mooring buoy data. These polygons do not necessarily delineate the geography of the reef sites (though the locations are accurate).

- Use the `Point size` +/- control to adjust the size of the points.

### Management Interventions

This tab group contains tabs designed to aid restoration managers in constructing, comparing, and monitoring restoration strategies.

Expand and collapse this group using the chevron icon.

#### Outplanting Scenarios

Use this tab to simulate a restoration effort at a reef site. Follow these steps to run a simulation:

1. Genus- or species-level coral cover survey data at the target site is required to begin the simulation. This cover is used as the baseline assemblage for the simulation. There are two ways to enter survey data:  

    **(a) File upload:** In the Restoration Scenario box, use the `↓ Template` button to download a baseline cover template Excel workbook to aid in data entry. See the `README` sheet of this workbook for more information.

    - For an example of a complete data file, see `Baseline_Cover_EXAMPLE.xlsx`, included in the `www` folder of the repository. Automatically upload this example data by clicking the `↑ Example` button.

    - **IMPORTANT: Do not edit the `*_TEMPLATE.xlsx` files in the `www` folder.** These are the master copies. Always use the `↓ Template` button to retrieve a fresh template.

    - Fill out the template, save it under a new name, and load it using the `Load .xlsx` input. Input parameters will be populated automatically based on the contents of the uploaded file.

    - A copy of the most recently uploaded baseline cover file is cached in a `cache` folder created where `app.R` is stored. The cached data can be re-uploaded by clicking the `↑Cache` button. Use the `Clear cache` button to delete the cached file (the original file will be unaffected).

    **(b) Create from scratch:** Use the inputs to name the site and designate its location, area, subregion, habitat, and baseline cover. Use the `Save baseline` button to save the scratch inputs in an `.xlsx` file which can be uploaded to the app in a subsequent session. Once the saved from-scratch `.xlsx` is uploaded, its data will be cached.

2. Use the inputs in the `Restoration Mix` section of the `Restoration Scenario` box to designate the desired outcome, or the resources available for the restoration effort. There are two modes which drive the restoration simulation:  

    - **(a) Target cover:** When a target percent-cover is provided for a species, the number of outplants required to meet the target in the given scenario is calculated and displayed in the appropriate Outplants cell.  

    - **(b) Outplant count:** When an initial outplant count is provided, the simulation will estimate the projected cover that may be achieved using that number of outplants. The count-driven simulation has two submodes:  

      - **(i) Single-year (default):** Assumes a single outplanting effort at Year 0.

      - **(ii) Multi-year:** Allows for multiple outplanting efforts with unique cost, count, and diameter prarameters during the simulation. To use this submode, enter a comma-delineated list of extra years in the `Additional outplanting years` input box alongside the grid headers. If an `Outplants` count is provided for a species in the `Restoration Mix`, that species' record will turn into a dropdown containing additional input rows for the extra outplanting years. Click the chevron next to the species' `Baseline cover` cell to expand and collapse the record.  

        - **Note:** this submode **does not work** for species with a user-input `Target cover` cell. The only way to calculate the number of outplants required to reach a target cover percentage is to assume a single outplanting effort.

    Use the `Reset targets` button to clear all values in the `Restoration Mix` except the `Baseline cover`.

3. Manipulate additional restoration variables by using the other input cells in the `Restoration Mix` grid:  

    - **Avg. outplant diameter:** The average starting diameter of the outplants, in centimeters.

    - **Avg. outplant cost:** The average cost of each outplant, in dollars.

    - **Outplants per cluster (optional):** The number of outplants in a cluster, if restoring using a clustered-outplant method. If blank or <= 1, each outplant is considered its own colony. Otherwise, the cluster is the colony, with a diameter calculated as follows:

    ```r
    plants_in_cluster_diam <- ceiling(sqrt(opc))
    cluster_diam <- (plants_in_cluster_diam * colony_diam) +
                    (0.5 * (plants_in_cluster_diam - 1))  
    ```

    where `opc` is "outplants per cluster". A half-centimeter gap is included between each fragment in the cluster.

4. Define the target-achievement year, and the duration of the simulation:

   - **Restoration horizon:** The number of years post-restoration by when the target percent-cover should be achieved.

   - **Simulation duration:** The number of years post-restoration that the simulation should last. Can exceed the `Restoration horizon` so the long-term effect of the restoration plan may be observed.

5. Account for coral bleaching mortality and growth stress by manipulating variables in the `Bleaching Scenario` box:

   - **Degree-Heating Weeks:** The cumulative heat stress expected each year, in degree-heating weeks.

   - **Events / 5 years:** The number of bleaching events expected per five years.

6. Run the simulation by clicking the `Simulate` button. Optionally, enable `Reactive simulation` to automatically run the simulation whenever any input value is changed.

7. View the results: The projected percent cover for each species at the end of the simulation is reported in the `Final cover (%)` grid column.  

    Scroll down to view the simulation's growth results and predicted cost in the `Projected Reef Accretion Potential (RAP)` timeline.

    Hover on a representative timeline point to update the `Restored` card readout with the summary values for that year.

    Save the timeline as a .png file by selecting the camera icon on the far left of the control banner which appears when hovering over the graph area.

8. Optionally, save the result of the constructed scenario. Enter the name of the project and scenario in the `Project name` and `Scenario name` inputs. Click `Save result`. The scenario will be saved as `{project}__{scenario}.json` in the `Output folder`.  

    A suggested scenario name is automatically constructed using the input values, using the following convention:  

    ```r
    "{dominant species agricode}_{bleaching frequency}B_{degree-heating weeks}DHW_{restoration horizon}_{simulation duration}"
    ```  

    e.g.:

    ```r
    "Acer_2B_16DHW_10_20"
    ```

    for a scenario whose dominant species is *Acropora cervicornis*, with 2 bleaching events every 5 years at 16 dgree-heating weeks, and a restoration horizon of 10 years, simulated for 20 years.

    **IMPORTANT: Existing scenarios' filenames may be edited, but the double-underscore between the project and scenario labels must be retained.** The app uses this convention to automatically recognize and differentiate projects and scenarios.

    Toggle between saved scenarios' projection timelines by uploading the .json file(s) in the Projected Reef Accretion Potential timeline box and selecting one of the uploaded scenarios from the adjacent dropdown. Multiple scenarios can be uploaded simultaneously. The scenarios can also be uploaded in the `Scenario Comparison` tab to explore and compare their outcome metrics.  

#### Scenario Comparison

Use this tab to compare outcome metrics and population dynamics between scenarios created in the `Outplanting Scenarios` tab.

Use the `Upload scenarios` button to select one or more saved scenario .json files. Multiple files can be uploaded at one time. Toggle the uploaded scenarios on and off as desired using the `Scenarios` checklist.

If the original color palette for the scenarios is difficult to discern, use the `Generate palette` button to create a new random palette of high-contrast pastels with the current `Palette seed`. Reuse a palette seed to recreate a previous palette.

A summary table is displayed to the right of the Scenario Selection box. Use the `Download report` button to download this table as a `.csv` file.

Filter the scenarios according to their values by using the filter boxes beneath each column's name. For a numeric field, click in the filter box to display a double-ended slider which can be used to restric the field's range. Multuple filters will be honored. When a scenario is filtered out of the table, it is also disabled in the "Scenarios" checklist. Re-enable the scenario in the checklist to add it back to the table and comparison charts.

This tab contains several visualizations in separate, collapsible boxes. Use the `-` button on the right side of the box header to collapse a box, and the `+` button to expand it. Each visualization can be saved as a .png file by selecting the camera icon on the far left of the control banner which appears when hovering over the graph area.

Per-scenario project cost, return on investment, and restored reef accretion potential are displayed as bar charts in the `Total Metrics` box.

A per-species colony-count timeline for a selected scenario is displayed in the `Population Timeline` box. Baseline and outplanted populations are tracked separately. Select a scenario row in the `Comparison Table` to be represented in this timeline.

Per-scenario Simpson diversity index over time is displayed in the `Diversity Timline` box.

Per-scenario calcifying species distribution is displayed in the `Species Distribution` box as a series of pie charts at representative years.

#### Restoration Monitoring

Use this tab to monitor an ongoing restoration effort using observed coral-cover and bioerosion data.

Without observed data, a basic simulation of a restoration effort at an NCRMP site can be "monitored". Growth is modeled as a linear regression between the original percent-cover and the target percent-cover calculated from the target percent-cover increase selected on the `Reef Site Map`. Click a site on the map to select it for this simulated monitoring, or use the `Select site` dropdown in the `Inputs` section of the `Restoration Monitoring` tab.

Use the `Coral cover .xlsx` and `Bioerosion .xlsx` to submit observed data for monitoring, if available. Use the `↑Example` and `↓Template` buttons to upload example data or download a template, respectively. See the `README` sheets of these workbook files for more information on data entry. See `Restoration_Monitoring_EXAMPLE.xlsx` and `Bioerosion_EXAMPLE.xlsx` in the repository's `www` folder for examples of a complete set of monitoring observation data.

If the observed reports include data for more than one site, use the `Select site` dropdown to select the site to monitor.

A comparison between the Baseline and Restored coral cover, carbonate budget, and reef accretion potential is displayed in the `Baseline vs. Restored Impact` section.

The observed data are used to calculate the site's reef accretion potential over time, which is graphed on the timeline in the `Reef Accretion Potential` section.

#### Calcifier Data

This tab contains a searchable, sortable, filterable table with all available species- and genus-level growth, calcification, and mortality data fed into the simulation.

### About this App

Contains summary, background, author, source, and methodological information.

## Local Setup (optional)

**Note: This process is only necessary to run the Reef Persistence Tool app locally. For easier access, click the link at the top of this document to launch the app immediately in the web browser.**

### Installation

To install the packages required to use the Reef Persistence Tool, use the `renv` package to rebuild the tool's environment. If `renv` is not installed, open an R console and run:

```r
install.packages("renv")
```

Then rebuild the environment from the included `renv.lock` file:

```r
renv::restore("path/to/tool")
```

(Replace "path/to/tool" with the actual path to the folder where the Reef Persistence Tool `app.R` is stored.)

Alternatively, run the following line in an R console:

```r
install.packages(c("Rtools", "rsconnect", "shiny", "bslib", "shinydashboard", "shinythemes",
                   "ggplot2", "dplyr", "tidyr", "leaflet", "leaflegend", "jsonlite",
                   "tidyverse", "ggforce", "png", "RCurl", "jpeg", "sf", "magrittr",
                   "maps", "reshape2", "RColorBrewer", "plotly", "geojsonio", "shinyWidgets",
                   "shinyjs", "shinyBS", "here", "readxl", "writexl", "tidyr", "dplyr", "DT", "terra=1.9-0"))
```

If the package installation times out, adjust the timeout setting. For example, to increase the timeout from the default 60 seconds to 120 seconds:

```r
options(timeout=120)
```

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

## Artificial Intelligence Disclosure

Claude Opus 4.8 was employed in July - September 2026 to convert the original Shiny bootstrapPage logic to dashboardPage logic, and to assist in organizing the app layout and connecting widgets to their intended functions. All code was reviewed, tested, and validated by the authors to ensure correctness and reproducibility. Any use of trade, firm, or product names is for descriptive purposes only and does not imply endorsement by the U.S. Government.

## Recommended Citation

Jenkins, C.M., Toth, L.T., and Morris, J., 2026, Reef Persistence Tool Version 1.0: U.S. Geological Survey software release, [DOI placeholder].
