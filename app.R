# Carbonate Budget Restoration Tool ----

# Adapted by Connor M. Jenkins at the U.S. Geological Survey St. Petersburg Coastal and Marine Science Center
# from Alice Webb's Reef Persistence Tool. Adaptation conceptualized and guided by Lauren T. Toth (USGS) and John T. Morris (NOAA).

# Call Packages ----
library(rsconnect)
library(shiny)
library(bslib)
library(shinydashboard)
library(dashboardthemes) # (optional to use dark theme)
library(ggplot2)
library(dplyr)
library(tidyr)
library(leaflet)
library(shinythemes)
library(leaflegend)
library(tidyverse)
library(ggforce)
library(png)
library(RCurl)
library(jpeg)
library(sf)
library(magrittr)
library(maps)
library(reshape2)
library(RColorBrewer)
library(plotly)
library(geojsonio)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(here)
library(readxl)
library(jsonlite)
library(later)

# Enable automatic reloading of the app when code changes are detected
options(shiny.autoreload = TRUE)

# year vector
cv_dates <- as.data.frame(c(2019:2100))
colnames(cv_dates) <- c("year")

# call data for world map
world_data <- ggplot2::map_data("world")
worldcountry <- fortify(world_data)

# # Mote sites
# triangle_sites <- read.csv(here("data", "Mote_sites.csv"))
mote_cover <- read.csv(here("data", "Mote_cover.csv"))
travis_rates <- read.csv(here("data", "Travis_rates.csv"))
bioerosion <- read.csv(here("data", "Bioerosion.csv"))

# Ingest NCRMP carbonate budget data
df <- read.csv(here("data", "NCRMP_CarbonateBudgets_2014_to_2024.csv"))

# Create unique site IDs in case PRIMARY_SAMPLE_UNIT is reused/not unique
df$site_id <- paste(df$YEAR, df$SUB_REGION, df$PRIMARY_SAMPLE_UNIT, sep = "_")

sites <- sort(df$site_id)

# Ingest regions polygon shapefile
regions_sf <- sf::st_read(here("data", "regions", "regions.shp"), quiet = TRUE)

# Ensure geographic CRS (WGS84) so it aligns with the leaflet basemap
regions_sf <- sf::st_transform(regions_sf, 4326)

# Pastel palette keyed to the Region field
region_levels <- sort(unique(regions_sf$Region))
pastel_colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "Pastel1"))(length(region_levels))
region_pal <- colorFactor(pastel_colors, domain = region_levels)


# Ingest user-input baseline cover data
base_cover_df <- read_excel(here("data", "Baseline_cover_TEMPLATE.xlsx"), sheet = "Coral Cover input")
taxa <- read_excel(here("data", "Baseline_cover_TEMPLATE.xlsx"), sheet = "Taxa")
taxa <- taxa$Taxon

# Calculate reef accretion potential
df$rap <- df$net_G / 2.9 / (1 - 0.6265)

# Linear regression: relationship between percent cover and RAP ----
# Used on the Home tab to translate a target percent-cover increase into a
# projected ("restored") RAP for each site.
cover_rap_lm <- lm(rap ~ hardCoral_PrctCvr, data = df)
cover_rap_slope <- unname(coef(cover_rap_lm)["hardCoral_PrctCvr"])

# Directory for saved restoration scenarios (Scenario Comparison tab)
scenario_dir <- here("scenarios")
if (!dir.exists(scenario_dir)) dir.create(scenario_dir, showWarnings = FALSE)

# Shared restoration species list (used across multiple tabs) ----
restoration_species_global <- c(
  "Acropora palmata", "Acropora cervicornis", "Montastraea cavernosa",
  "Orbicella faveolata", "Colpophyllia natans", "Porites astreoides",
  "Siderastrea siderea", "Stephanocoenia intersepta",
  "Diploria labyrinthiformis", "Solenastrea bournoni"
)

# Filter choices for the Home-tab map controls ----
year_choices <- sort(unique(df$YEAR))
habitat_choices <- sort(unique(df$HABITAT_TYPE))

# White-to-red palettes for the "Symbolize by" numeric options ----
# Each clamped 0 -> field max.
make_wr_pal <- function(field, rev=FALSE) {
  colorNumeric(
    palette = colorRampPalette(c("white", "red"))(100),
    domain = c(0, max(df[[field]], na.rm = TRUE)),
    reverse = rev
  )
}

# Color palette for RAP symbology
at <- c(-8, -6, -4, -2, 0, 2, 4, 6, 8)
colors <- c("darkred", "red", "orange", "yellow", "white", "#0099FF", "#0033FF", "darkblue", "#000066")
num_pal <- colorNumeric(colors, domain = at)

pal_rap        <- num_pal
pal_parrotfish <- make_wr_pal("parrotfish_G")
pal_gross      <- make_wr_pal("grossE_G")

# Reversed palettes for legend displays
num_pal_rev <- colorNumeric(colors, domain = at, reverse = TRUE)
pal_parrotfish_rev <- make_wr_pal("parrotfish_G", rev = TRUE)
pal_gross_rev  <- make_wr_pal("grossE_G", rev = TRUE)

# Categorical palette for Reef State (budget_State is categorical) ----
state_levels <- sort(unique(as.character(df$budget_State)))

# Reef State: original Blue / Yellow / Orange status colors
state_colors <- c("Growth" = "#0099FF", "Stasis" = "#FFFF99", "Erosion" = "#FF6600")
num_pal_state <- colorFactor(unname(state_colors), domain = names(state_colors))
# Shiny User Interface ----
# Converted from bootstrapPage/navbarPage to shinydashboard::dashboardPage

## Header ----
header <- dashboardHeader(
  title = "Carbonate Budget Restoration Tool",
  titleWidth = 380
)

## Sidebar ----
sidebar <- dashboardSidebar(
  width = 230,
  sidebarMenu(
    id = "nav",
    menuItem("Home", tabName = "home", icon = icon("map")),
    menuItem("Restoration Planning", tabName = "restoration", icon = icon("seedling")),
    menuItem("Scenario Comparison", tabName = "scenarios", icon = icon("scale-balanced")),
    menuItem("Restoration Monitoring", tabName = "monitoring", icon = icon("chart-column")),
    menuItem("About this Site", tabName = "about", icon = icon("circle-info"))
  )
)

## Body ----
body <- dashboardBody(
  useShinyjs(),
  # shinyDashboardThemes(theme = 'grey_dark'), # change dashboard theme (optional. reactive?)

  # Tag Setup ----
  tags$head(
    includeHTML(here("gtag.html")),
    includeCSS(here("styles.css")),
    # Preserve custom background color (optional)
    tags$style(HTML("
      .content-wrapper, .right-side { background-color: #BFDADA; }
      .custom-absolute-panel { z-index: 9999; }
      /* .box { color: #000; } */
      /* Full-bleed map on the Home tab */
      .home-map-outer {
        position: absolute; top: 0; left: 0; right: 0; bottom: 0;
        overflow: hidden; padding: 0;
      }
      /* Map Controls floating panel */
      .map-controls-panel {
        position: absolute; top: 160px; right: 10px; z-index: 1000;
        width: 280px; background: rgba(255,255,255,0.92);
        border-radius: 8px; box-shadow: 0 1px 6px rgba(0,0,0,0.3);
      }
      .map-controls-header {
        cursor: pointer; padding: 8px 12px; font-weight: bold;
        background: #3c8dbc; color: white; border-radius: 8px 8px 0 0;
        display: flex; justify-content: space-between; align-items: center;
      }
      .map-controls-body { padding: 10px 12px; max-height: 60vh; overflow-y: auto; }
      .map-controls-body .form-group { margin-bottom: 10px; }
    "))
  ),

  tabItems(
    # Home Tab ----
    tabItem(
      tabName = "home",
      div(
        class = "home-map-outer",
        leafletOutput("mymap", width = "100%", height = "100%"),
        # Right-aligned data caption overlaid on the top-right corner of the map
        tags$div(
          style = "position: absolute; top: 45px; right: 10px;
          z-index: 1000; display: flex; gap: 8px;",
          tags$li(
            class = "dropdown",
            tags$span(
              style = "color: white; line-height: 50px; margin-right: 15px; font-size: 18px;
                       text-shadow: -1px -1px 0 black, 1px -1px 0 black,
                                    -1px 1px 0 black, 1px 1px 0 black;
                                    ",
              "Displaying 2014-2024 NCRMP data"
            )
          )
        ),
        # USGS & NOAA logos
        tags$div(
          style = "position: absolute; top: 90px; right: 10px;
                   z-index: 1000; display: flex; gap: 8px;",

          tags$img(src = "usgsLogo.png", style = "height: 60px;"),
          tags$img(src = "noaaLogo.png", style = "height: 60px;")
        ),

        # Map Controls: vertically-collapsible box below the logos
        tags$div(
          class = "map-controls-panel",
          tags$div(
            class = "map-controls-header",
            onclick = "var b=document.getElementById('map_controls_body'); b.style.display = (b.style.display==='none') ? 'block' : 'none';",
            tags$span("Map Controls"),
            tags$span(icon("chevron-down"))
          ),
          tags$div(
            id = "map_controls_body",
            class = "map-controls-body",

            # Target Percent-Cover Increase slider
            sliderInput("target_cover_increase", "Target Percent-Cover Increase",
              min = 0, max = 30, value = 0, step = 5, post = "%", width = "100%"
            ),

            # Restoration Potential legend (halo meanings)
            # tags$div(
            #   style = "margin: 4px 0 10px 0;",
            #   tags$strong("Restoration Potential"),
            #   tags$div(style = "display:flex; align-items:center; gap:6px; margin-top:4px;",
            #     tags$span(style = "width:14px; height:14px; border:3px solid #1f78ff; border-radius:50%; display:inline-block;"),
            #     tags$span("Was Growing Anyway")
            #   ),
            #   tags$div(style = "display:flex; align-items:center; gap:6px;",
            #     tags$span(style = "width:14px; height:14px; border:3px solid #33a02c; border-radius:50%; display:inline-block;"),
            #     tags$span("Growth Resumed")
            #   ),
            #   tags$div(style = "display:flex; align-items:center; gap:6px;",
            #     tags$span(style = "width:14px; height:14px; border:3px solid #ffcc00; border-radius:50%; display:inline-block;"),
            #     tags$span("Erosion Mitigated")
            #   ),
            #   tags$div(style = "display:flex; align-items:center; gap:6px;",
            #     tags$span(style = "width:14px; height:14px; background:rgba(150,150,150,0.5); border-radius:50%; display:inline-block;"),
            #     tags$span("No Return")
            #   )
            # ),

            # Filter group: Year + Habitat dropdown checkboxes
            tags$strong("Filter"),
            shinyWidgets::dropdownButton(
              inputId = "filter_year_dd",
              label = "Year",
              circle = FALSE, width = "100%", status = "default",
              checkboxGroupInput("filter_year", NULL,
                choices = year_choices, selected = year_choices
              )
            ),
            tags$div(style = "height:6px;"),
            shinyWidgets::dropdownButton(
              inputId = "filter_habitat_dd",
              label = "Habitat",
              circle = FALSE, width = "100%", status = "default",
              checkboxGroupInput("filter_habitat", NULL,
                choices = habitat_choices, selected = habitat_choices
              )
            ),

            tags$hr(),

            # Symbolize by: exclusive radio buttons
            radioButtons("symbolize_by", "Symbolize by:",
              choices = c(
                "Reef Accretion Potential" = "rap",
                "Reef State"               = "budget_State",
                "Parrotfish Bioerosion"    = "parrotfish_G",
                "Gross Bioerosion"         = "grossE_G"
              ),
              selected = "rap"
            ),

            tags$hr(),

            # Point-size stepper (moved into Map Controls)
            tags$div(
              style = "font-size: 13px; margin-bottom: 4px; color: #333;",
              "Point size"
            ),
            tags$div(
              style = "display: flex; align-items: center; gap: 8px;",
              actionButton("point_size_down", "\u2212", class = "btn-sm"),
              textOutput("point_size_label", inline = TRUE),
              actionButton("point_size_up", "+", class = "btn-sm")
            )
          )
        )
      )
    ),

    # Restoration Planning Tab ----
    tabItem(
      tabName = "restoration",
      # Vertical layout: horizontal input row on top, timeline on the bottom
      fluidRow(
        # --- Input element 1: Baseline cover (subsumed from Baseline Input) ---
        column(
          width = 4,
          shinydashboard::box(
            title = "Baseline Cover",
            width = 12, status = "primary", solidHeader = TRUE,

            # Load from file
            fileInput("baseline_upload", "Load from file (.xlsx)",
              accept = c(".xlsx")
            ),

            numericInput(
              "site_area_m2",
              label = "Site area (m\u00b2)",
              value = 100, min = 0, step = 1
            ),
            selectInput(
              "habitat_choice",
              label = "Habitat",
              choices = c("\u2014 Select habitat \u2014" = "", "Inshore", "Offshore"),
              selected = ""
            ),
            selectizeInput(
              "baseline_species",
              "select your species:",
              choices = sort(unique(taxa)),
              multiple = TRUE,
              options = list(maxItems = 12, placeholder = "Select species...")
            ),
            uiOutput("baseline_cover_inputs"),

            tags$hr(),

            # Scenario save controls (retained)
            textInput("scenario_project", "Project name", value = ""),
            textInput("scenario_name", "Scenario name", value = ""),
            actionButton("save_scenario", "Save scenario", icon = icon("floppy-disk"))
          )
        ),

        # --- Input element 2: Restoration mix (subsumed from Baseline Input) ---
        column(
          width = 4,
          shinydashboard::box(
            title = "Restoration Mix",
            width = 12, status = "success", solidHeader = TRUE,
            div(tags$strong("Set target restoration cover (%) for each species:")),
            uiOutput("restoration_sliders")
          )
        ),

        # --- Input element 3: outplant + bleaching parameters ---
        column(
          width = 4,
          shinydashboard::box(
            title = "Restoration Parameters",
            width = 12, status = "warning", solidHeader = TRUE,

            # Outplant parameters (vertical)
            numericInput("outplant_size", "Average outplant size (cm)",
              value = 5, min = 1, max = 100, step = 0.1
            ),
            numericInput("outplant_cost", "Average outplant cost ($)",
              value = 10, min = 1, max = 100, step = 0.01
            ),

            # Bleaching scenario (vertical, red outline)
            tags$fieldset(
              style = "border: 2px solid #d9534f; border-radius: 6px;
                       padding: 10px; margin-top: 12px;",
              tags$legend(
                style = "width: auto; font-size: 15px; font-weight: bold;
                         color: #d9534f; padding: 0 6px;",
                "Bleaching Scenario"
              ),
              sliderInput("dhw", "Degree-Heating Weeks",
                min = 8, max = 24, value = 8, step = 1
              ),
              sliderInput("bleach_events", "Events / 5 years",
                min = 0, max = 5, value = 0, step = 1
              )
            )
          )
        )
      ),

      # --- Timeline (bottom) ---
      fluidRow(
        column(
          width = 12,
          shinydashboard::box(
            title = "Projected reef accretion potential (12 years)",
            width = 12, status = "info", solidHeader = TRUE,
            plotly::plotlyOutput("restoration_timeline", height = "400px")
          )
        )
      )
    ),

    # Scenario Comparison Tab ----
    # Sidebar: project (single select), scenario (multi select from saved .json),
    #          download report (.csv)
    # Main:    "Year 10 Outcome Summary" -> cost bar, ROI bar, RAP/Elev scatter
    tabItem(
      tabName = "scenarios",
      fluidRow(
        # Sidebar (left)
        column(
          width = 3,
          shinydashboard::box(
            title = "Scenario Selection", width = 12,
            status = "primary", solidHeader = TRUE,
            selectInput("sc_project", "Project name", choices = NULL),
            checkboxGroupInput("sc_scenarios", "Scenarios", choices = NULL),
            actionButton("sc_refresh", "Refresh list", icon = icon("rotate")),
            br(), br(),
            downloadButton("sc_download_csv", "Download report (.csv)")
          )
        ),
        # Main content (right)
        column(
          width = 9,
          tags$h2("Outcome Summary: Year 10",
            style = "text-align:center; color:black; font-weight:bold;"
          ),
          fluidRow(
            column(
              width = 6,
              shinydashboard::box(
                title = "Project Cost", width = 12,
                status = "info", solidHeader = TRUE,
                plotOutput("sc_cost_bar", height = "300px")
              )
            ),
            column(
              width = 6,
              shinydashboard::box(
                title = "Return on Investment", width = 12,
                status = "info", solidHeader = TRUE,
                plotOutput("sc_roi_bar", height = "300px")
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              shinydashboard::box(
                title = "Carbonate Budget: RAP & Elevation Gain", width = 12,
                status = "success", solidHeader = TRUE,
                plotOutput("sc_scatter", height = "350px")
              )
            )
          )
        )
      )
    ),

    # Restoration Monitoring Tab ----
    #   Sidebar: upload coral cover, upload bioerosion, select site, download report
    #   Main:    baseline vs restored impact (cover/budget/accretion + summary),
    #            timeline of RAP over 10 yrs with SLR reference lines
    tabItem(
      tabName = "monitoring",
      fluidRow(
        # Sidebar (left)
        column(
          width = 3,
          shinydashboard::box(
            title = "Inputs", width = 12, status = "primary", solidHeader = TRUE,
            fileInput("upload_cover", "Upload coral cover data",
              accept = c(".csv", ".xlsx")
            ),
            fileInput("upload_bioerosion", "Upload bioerosion data",
              accept = c(".csv", ".xlsx")
            ),
            selectizeInput("monitoring_selected_site", "Select site",
              choices = NULL,
              options = list(placeholder = "Select a site...")
            ),
            br(),
            downloadButton("cc_download_report", "Download report")
          )
        ),
        # Main content (right)
        column(
          width = 9,
          # Baseline vs restored impact
          shinydashboard::box(
            title = "Baseline vs. Restored Impact", width = 12,
            status = "info", solidHeader = TRUE,
            fluidRow(
              column(
                width = 4,
                tags$h4("Baseline", style = "text-align:center; font-weight:bold;"),
                valueBoxOutput("cc_baseline_cover", width = NULL),
                valueBoxOutput("cc_baseline_budget", width = NULL),
                valueBoxOutput("cc_baseline_rap", width = NULL)
              ),
              column(
                width = 4,
                tags$h4("Restored", style = "text-align:center; font-weight:bold;"),
                valueBoxOutput("cc_restored_cover", width = NULL),
                valueBoxOutput("cc_restored_budget", width = NULL),
                valueBoxOutput("cc_restored_rap", width = NULL)
              ),
              column(
                width = 4,
                tags$h4("Impact summary", style = "text-align:center; font-weight:bold;"),
                div(
                  style = "background:#f7f7f7; border:1px solid #ddd;
                           border-radius:6px; padding:12px; min-height:180px;",
                  htmlOutput("cc_impact_summary")
                )
              )
            )
          ),
          # Timeline
          shinydashboard::box(
            title = "Reef Accretion Potential over 10 Years", width = 12,
            status = "success", solidHeader = TRUE,
            plotly::plotlyOutput("cc_timeline", height = "350px")
          )
        )
      )
    ),

    # "About this Site" Tab ----
    tabItem(
      tabName = "about",
      shinydashboard::box(
        width = 12, status = "primary", solidHeader = FALSE,
        tags$div(
          tags$h4("Aim"),
          "The aim of this site is to provide a predictive tool for decision makers to assess regional responses under future climate change
            and to evaluate the potential impact of local initiatives to mitigate effects of ocean acidification and warming.The modelling
            approach that is used to built projections in this interactive tool is described in ",
          tags$a(href = "https://www.nature.com/articles/s41598-022-26930-4", "an article,"), "published in Scientific reports.",
          tags$br(), tags$br(), tags$h4("Background"),
          "For reef framework to persist, constructional processes by corals and other calcifers need
           to outpace loss due to physical, chemical, and biological erosion. This balance is both delicate and
           dynamic and is currently threatened by the effects of ocean warming and acidifcation.

           Although the protection and recovery of ecosystem functions are at the center of most restoration
           and conservation programs, decision makers are limited by the lack of predictive tools to forecast
           habitat persistence under diferent emission scenarios.",
          tags$br(), tags$br(),
          "The Carbonate Budget Restoration Tool will enable decision makers to evaluate impact of local restoraton initiatives on reef habitat persistence in the context of climate change.",
          tags$br(), tags$br(), tags$h4("Code"),
          "Code and input data used to generate this Shiny mapping tool are available on Github.",
          tags$br(), tags$br(), tags$h4("Sources"),
          tags$br(), tags$br(), tags$h4("Authors"),
          "Dr Alice Webb,Atlantic Oceanographic and Meteorological Laboratory, Ocean Chemistry and Ecosystem Division, NOAA, USA;", tags$br(),
          "Geography, College of Life and Environmental Sciences, University of Exeter, UK", tags$br(),
          "Patrick Kiel, Atlantic Oceanographic andMeteorological Laboratory, Ocean Chemistry and Ecosystem Division,NOAA, Miami, Florida, USA;", tags$br(),
          "Cooperative Institute for Marine and Atmospheric Studies, University of Miami, USA", tags$br(),
          "Mike Jankulak, Atlantic Oceanographic andMeteorological Laboratory, Ocean Chemistry and Ecosystem Division,NOAA, Miami, Florida, USA;", tags$br(),
          "Cooperative Institute for Marine and Atmospheric Studies, University of Miami, USA", tags$br(),
          "Dr Ian Enochs, Atlantic Oceanographic andMeteorological Laboratory, Ocean Chemistry and Ecosystem Division,NOAA, USA", tags$br(),
          tags$br(), tags$br(), tags$h4("Contact"),
          "alice.webb@noaa.gov", tags$br(), tags$br(),
          tags$img(src = "noaaLogo.png", width = "150px", height = "150px")
        )
      )
    )
  )
)

ui <- dashboardPage(
  skin = "black",
  header,
  sidebar,
  body
)

# Shiny Server ----
server <- function(input, output, session) {
  # define reactVal to store coordinates
  reef_name       <- reactiveVal()
  reef_year       <- reactiveVal(2019)
  initial_budget  <- reactiveVal(NULL)

  # Register large site lists server-side (performance)
  updateSelectizeInput(session, "monitoring_selected_site",
    choices = unique(df$site_id), server = TRUE
  )

  # Point size state, adjusted by the +/- stepper (clamped 2-15)
  point_size <- reactiveVal(4)

  observeEvent(input$point_size_up, {
    point_size(min(point_size() + 1, 15))
  })
  observeEvent(input$point_size_down, {
    point_size(max(point_size() - 1, 2))
  })


  output$point_size_label <- renderText({
    point_size()
  })

  filtered_df <- reactive({
    df |>
      filter(site_id == input$selected_site)
  })

  ## Home map: filtered + restoration-projected data ----
  # Applies the Year/Habitat checkbox filters and computes restored_rap +
  # halo classification from the target percent-cover-increase slider.
  map_data_reactive <- reactive({
    d <- df

    # Year / Habitat filters (checkbox groups); empty selection => no sites
    yr_sel  <- input$filter_year
    hab_sel <- input$filter_habitat
    if (is.null(yr_sel))  yr_sel  <- character(0)
    if (is.null(hab_sel)) hab_sel <- character(0)
    d <- d[d$YEAR %in% yr_sel & d$HABITAT_TYPE %in% hab_sel, , drop = FALSE]
    if (nrow(d) == 0) return(d)

    # Projected RAP from the target cover increase, via the regression slope
    inc <- if (is.null(input$target_cover_increase)) 0 else input$target_cover_increase
    d$restored_rap <- d$rap + cover_rap_slope * inc

    # Halo / fill classification (only meaningful when inc > 0)
    # Blue  = was already growing (baseline rap > 0.5)
    # Green = transitioned from <=-0.5 to >=0.5
    # Yellow= transitioned from <=-0.5 to (-0.5, 0.5)
    # Gray  = still <=-0.5
    classify <- function(base, restored) {
      if (base > 0.5) {
        "blue"
      } else if (base < 0.5 && restored >= 0.5) {
        "darkgreen"
      } else if (base >= -0.5 && base < 0.5 && restored >= 0.5) {
        "limegreen"
      } else if (base <= -0.5 && restored > -0.5 && restored < 0.5) {
        "yellow"
      } else if (restored <= -0.5) {
        "gray"
      } else {
        NA_character_
      }
    }
    d$halo <- mapply(classify, d$rap, d$restored_rap)
    d
  })

  # Initialize leaflet map ----
  output$mymap <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) |>
      addProviderTiles(providers$Esri.WorldImagery,
        options = providerTileOptions(attribution = 'Map data &copy; <a href="https://www.esri.com/">Esri</a>')
      ) |>
      setView(lng = -80.6097, lat = 25, zoom = 8) |>

      # Region polygons, pastel fill at 75% transparency
      addPolygons(
        data        = regions_sf,
        fillColor   = ~ region_pal(Region),
        fillOpacity = 0.45,
        color       = "white",
        weight      = 1,
        opacity     = 0.8,
        label       = ~Region
      ) |>

      # Static control: White text instruction
      addControl(
        html = "<div
                  style='
                    font-size: 22px;
                    font-weight: bold;
                    color: white;
                    text-shadow:
                      -1px -1px 0 black,
                      1px -1px 0 black,
                      -1px 1px 0 black,
                      1px 1px 0 black;'>
                  Click on a site to<br> find out more</div>",
        position = "bottomright",
        className = "map-title"
      )
  })

  # Add / update NCRMP markers, halos, and legend ----
  observe({
    d <- map_data_reactive()
    field <- input$symbolize_by
    inc <- if (is.null(input$target_cover_increase)) 0 else input$target_cover_increase

    proxy <- leafletProxy("mymap") |>
      clearGroup("ncrmp") |>
      clearGroup("halo") |>
      clearControls()

    if (is.null(d) || nrow(d) == 0) {
      return(proxy)
    }

    # Choose fill color + legend per the selected symbolize-by field
    if (field == "budget_State") {
      fill_cols <- num_pal_state(as.character(d$budget_State))
    } else if (field == "rap") {
      fill_cols <- num_pal(d$rap)
    } else {
      pal <- switch(field,
        "parrotfish_G" = pal_parrotfish,
        "grossE_G" = pal_gross
      )
      fill_cols <- pal(pmax(0, d[[field]]))
    }

    # In RAP mode, gray-out "No Return" sites at 50% transparency
    fill_opacity <- rep(0.85, nrow(d))
    if (field == "rap" && inc > 0) {
      gray_idx <- which(d$halo == "gray")
      fill_cols[gray_idx] <- "gray"
      fill_opacity[gray_idx] <- 0.5
    }

    # Draw halos first (underneath) when slider is active
    if (inc > 0) {
      halo_cols <- c(blue = "#1f78ff", green = "#33a02c", yellow = "#ffcc00")
      hd <- d[d$halo %in% names(halo_cols), , drop = FALSE]
      if (nrow(hd) > 0) {
        proxy <- proxy |>
          addCircleMarkers(
            data = hd,
            lng = ~LON_DEGREES, lat = ~LAT_DEGREES,
            radius = point_size() + 2,
            weight = 0,
            fillColor = unname(halo_cols[hd$halo]),
            fillOpacity = 0.9,
            stroke = FALSE,
            group = "halo"
          )
      }
    }

    # Main site markers
    proxy <- proxy |>
      addCircleMarkers(
        data = d,
        lng = ~LON_DEGREES, lat = ~LAT_DEGREES,
        radius = point_size(),
        weight = 1,
        color = "black",
        fillColor = fill_cols,
        fillOpacity = fill_opacity,
        stroke = TRUE,
        group = "ncrmp",
        popup = ~ paste0(
          "<a style='cursor: pointer' onclick='Shiny.setInputValue(\"linkClickPlanning\", Math.random())'>",
          "<span style='font-size: 20px; color: black;'>NCREMP Site: ", site_id, "</span>",
          "</a>",
          "<table style='font-size: 14px; border-collapse: collapse; margin-top: 6px;'>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Habitat:</td>",
          "<td style='padding: 2px 0;'>", HABITAT_TYPE, "</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Survey year:</td>",
          "<td style='padding: 2px 0;'>", YEAR, "</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Current coral cover:</td>",
          "<td style='padding: 2px 0;'>", round(hardCoral_PrctCvr, 1), "%</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Parrotfish bioerosion:</td>",
          "<td style='padding: 2px 0;'>", round(parrotfish_G, 2), "kg CaCO\u00b3/yr</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Gross bioerosion:</td>",
          "<td style='padding: 2px 0;'>", round(grossE_G, 2), "kg CaCO\u00b3/yr</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Reef accr. potential:</td>",
          "<td style='padding: 2px 0;'>", round(rap, 2), " mm/yr</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Projected RAP:</td>",
          "<td style='padding: 2px 0;'>", round(restored_rap, 2), " mm/yr</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Current status:</td>",
          "<td style='padding: 2px 0;'>", budget_State, "</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Water depth:</td>",
          "<td style='padding: 2px 0;'>", round(AVG_DEPTH, 1), " m</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Coordinates:</td>",
          "<td style='padding: 2px 0;'>", round(LON_DEGREES, 5), ", ", round(LAT_DEGREES, 5), "</td></tr>",
          "</table>"
        )
      )

    # Legend for Restoration Potential
    rest_colors = c("Growth  → Growth"  = "#1f78ff",
                    "Erosion → Growth"  = "darkgreen",
                    "Stasis  → Growth"  = "limegreen",
                    "Erosion → Stasis"  = "yellow",
                    "Erosion → Erosion" = "gray")

    proxy <- proxy |>
      addLegend("bottomleft",
        colors = unname(rest_colors),
        labels = names(rest_colors),
        title = "Restoration Potential",
        opacity = 1
      )

    # Legend for the selected symbolize-by field
    if (field == "budget_State") {
      proxy <- proxy |>
        addLegend("bottomleft",
          colors = unname(state_colors), labels = names(state_colors),
          title = "Reef Status", opacity = 1
        )
    } else if (field == "rap") {
      proxy <- proxy |>
        addLegendNumeric(
          pal = num_pal_rev,
          title = HTML("Reef<br/>accretion<br/>potential<br/>(mm/yr)"),
          shape = "stadium", values = at,
          fillOpacity = 1, decreasing = TRUE,
          position = "bottomleft"
        )
    } else {
      pal <- switch(field,
        "parrotfish_G" = pal_parrotfish_rev,
        "grossE_G" = pal_gross_rev
      )
      ttl <- switch(field,
        "parrotfish_G" = "Parrotfish<br/>bioerosion<br/>(kg CaCO\u00b3/yr)",
        "grossE_G" = "Gross<br/>bioerosion<br/>(kg CaCO\u00b3/yr)"
      )
      proxy <- proxy |>
        addLegend("bottomleft",
          pal = pal,
          values = c(0, max(df[[field]], na.rm = TRUE)),
          title = HTML(ttl), opacity = 1,
          labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
        )
    }

    proxy
  })

  # capture the selected reef name for the restoration tab
  observeEvent(input$mymap_marker_click, {
    click <- input$mymap_marker_click

    reef_name(df |>
                filter(LAT_DEGREES == click$lat & LON_DEGREES == click$lng) |>
                pull(site_id) |>
                unique())

    print(paste("Selected reef:", reef_name()))

    # Sync the monitoring-tab site picker to the clicked marker
    updateSelectizeInput(session, "monitoring_selected_site", selected = reef_name())
  })

  # change the tab when the map popup hyperlink is clicked
  observeEvent(input$linkClickPlanning, {
    updateTabItems(session, inputId = "nav", selected = "restoration")
  })

  ## ---------------------------------------------------------------------------
  ## Baseline cover: dynamic per-species inputs + upload auto-populate ----
  ## ---------------------------------------------------------------------------

  # Holds Taxon -> Percent_Cover from the last upload
  uploaded_covers <- reactiveVal(NULL)

  # Baseline cover: load from uploaded .xlsx (Coral Cover input sheet)
  observeEvent(input$baseline_upload, {
    req(input$baseline_upload)

    up <- tryCatch(
      readxl::read_excel(input$baseline_upload$datapath, sheet = "Coral Cover input"),
      error = function(e) {
        showNotification(paste("Could not read sheet:", e$message), type = "error")
        NULL
      }
    )
    req(up)

    # Site area: default 100, overridden by Site_Area_m2 if present
    area_val <- if ("Site_Area_m2" %in% names(up) && any(!is.na(up$Site_Area_m2))) {
      up$Site_Area_m2[!is.na(up$Site_Area_m2)][1]
    } else {
      100
    }
    updateNumericInput(session, "site_area_m2", value = area_val)

    # Habitat
    if ("Habitat" %in% names(up) && any(!is.na(up$Habitat))) {
      updateSelectInput(session, "habitat_choice",
        selected = up$Habitat[!is.na(up$Habitat)][1]
      )
    }

    # Species selection from the Taxon field
    if ("Taxon" %in% names(up)) {
      sp <- unique(up$Taxon[!is.na(up$Taxon)])
      updateSelectizeInput(session, "baseline_species", selected = sp)

      # Stash covers so the dynamic numericInputs can pick them up once rendered
      uploaded_covers(setNames(
        round(as.numeric(up$Percent_Cover[match(sp, up$Taxon)]), 2),
        sp
      ))
    }
  })

  # Dynamic per-species numericInputs, seeded from any uploaded covers
  output$baseline_cover_inputs <- renderUI({
    req(input$baseline_species)
    sp <- input$baseline_species
    covers <- uploaded_covers()
    tagList(lapply(sp, function(s) {
      id <- paste0("base_", gsub("[^A-Za-z0-9]", "_", s))
      val <- if (!is.null(covers) && s %in% names(covers) && !is.na(covers[[s]])) {
        covers[[s]]
      } else {
        0
      }
      numericInput(id, label = s, value = val, min = 0, max = 100, step = 0.1)
    }))
  })

  # Fixed restoration species sliders (Restoration mix box)
  restoration_species <- c(
    "Acropora cervicornis",  "Acropora palmata",
    "Colpophyllia natans",   "Diploria labyrinthiformis",
    "Montastraea cavernosa", "Orbicella faveolata",
    "Porites astreoides",    "Porites porites",
    "Pseudodiploria spp.",   "Siderastrea siderea",
    "Solenastrea bournoni",  "Stephanocoenia intersepta"
  )

  output$restoration_sliders <- renderUI({
    sliders <- lapply(restoration_species, function(s) {
      id <- paste0("rest_slider_", gsub("[^A-Za-z0-9]", "_", s))
      sliderInput(id, label = s, min = 0, max = 20, value = 0, step = 0.5, post = "%")
    })

    half <- ceiling(length(sliders) / 2)

    fluidRow(
      column(6, tagList(sliders[1:half])),
      column(6, tagList(sliders[(half + 1):length(sliders)]))
    )
  })

  # Seed restoration-mix sliders from matching baseline_species values,
  # once per baseline-species change. Hands-off afterward.
  observeEvent(input$baseline_species, {
    sel <- input$baseline_species          # captured in reactive context
    # brief defer so the dynamic base_ inputs exist before we read them
    later::later(function() {
      for (s in restoration_species) {
        if (s %in% sel) {
          base_id <- paste0("base_", gsub("[^A-Za-z0-9]", "_", s))
          rest_id <- paste0("rest_slider_", gsub("[^A-Za-z0-9]", "_", s))
          isolate({
            updateSliderInput(session, rest_id, value = .safe_num(input[[base_id]]))
          })
        }
      }
    }, delay = 0.2)
  }, ignoreNULL = FALSE)

  ## ---------------------------------------------------------------------------
  ## Restoration Planning: baseline / restored metrics + plotly timeline ----
  ## ---------------------------------------------------------------------------

  # Reactive store of RAP values (shared with Monitoring tab)
  rap_values <- reactiveValues(
    baseline = NULL,
    restored = NULL
  )

  .safe_num <- function(x) {
    if (is.null(x) || is.na(x)) 0 else as.numeric(x)
  }

  # Baseline cover & carbonate budget from the entered/uploaded data
  baseline_metrics <- reactive({
    sp <- if (is.null(input$baseline_species)) character(0) else input$baseline_species
    ids <- paste0("base_", gsub("[^A-Za-z0-9]", "_", sp))
    covers <- sapply(ids, function(id) .safe_num(input[[id]]))
    total_cover <- sum(covers, na.rm = TRUE)

    rates <- as.numeric(travis_rates$rate[match(sp, travis_rates$Species)])
    net_budget <- sum(covers * rates / 100, na.rm = TRUE)

    row <- bioerosion[bioerosion$Location == input$habitat_choice,
      c("AVG_PARROTFISH", "AVG_URCHIN", "AVG_MICROBIOEROSION"), drop = FALSE]
    erosion <- if (nrow(row)) sum(as.numeric(row[1, ]), na.rm = TRUE) else 0
    budget <- net_budget - erosion

    rap_values$baseline <- budget / 2.9 / (1 - 0.6265)

    list(cover = total_cover, budget = budget, rap = rap_values$baseline)
  })

  # Restored (target) cover & carbonate budget
  restored_metrics <- reactive({
    b <- baseline_metrics()
    slider_ids <- paste0("rest_slider_", gsub("[^A-Za-z0-9]", "_", restoration_species))
    rest_vals <- sapply(slider_ids, function(id) .safe_num(input[[id]]))
    rest_rates <- as.numeric(travis_rates$rate[match(restoration_species, travis_rates$Species)])
    net_rest <- sum(rest_vals * rest_rates / 100, na.rm = TRUE)

    total_cover <- b$cover + sum(rest_vals, na.rm = TRUE)
    budget <- b$budget + net_rest

    rap_values$restored <- budget / 2.9 / (1 - 0.6265)

    list(cover = total_cover, budget = budget, rap = rap_values$restored)
  })

  # Per-year projection (12 yrs). >>> STUB MATH — replace later <<<
  timeline_df <- reactive({
    b <- baseline_metrics()
    r <- restored_metrics()
    years <- 0:12

    # Placeholder: linear ramp from baseline to restored over first ~5 yrs,
    # then flat. Bleaching (dhw, bleach_events) and outplant knobs unused for now.
    frac <- pmin(years / 5, 1)
    rap    <- b$rap    + frac * (r$rap    - b$rap)
    cover  <- b$cover  + frac * (r$cover  - b$cover)
    budget <- b$budget + frac * (r$budget - b$budget)

    data.frame(Year = years, RAP = rap, Cover = cover, Budget = budget)
  })

  output$restoration_timeline <- plotly::renderPlotly({
    d <- timeline_df()
    b <- baseline_metrics()
    pips <- d[d$Year %in% c(1, 5, 10), ]

    # Year 0 baseline annotation text
    y0_label <- paste0(
      "Baseline (Year 0)<br>Cover: ", round(b$cover, 1), "%",
      "<br>Budget: ", round(b$budget, 2), " kg/m\u00b2/yr"
    )

    p <- ggplot(d, aes(Year, RAP)) +
      geom_line(color = "forestgreen", linewidth = 1.2) +
      geom_point(
        data = pips,
        aes(Year, RAP, text = paste0(
          "Year ", Year,
          "<br>Projected cover: ", round(Cover, 1), "%",
          "<br>Projected budget: ", round(Budget, 2), " kg/m\u00b2/yr"
        )),
        size = 4, color = "forestgreen"
      ) +
      scale_x_continuous(breaks = 0:12, limits = c(0, 12)) +
      labs(x = "Year", y = "Reef accretion potential (mm/yr)") +
      theme_minimal(base_size = 14)

    plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(
        annotations = list(
          list(
            x = 0, y = b$rap, text = y0_label,
            showarrow = TRUE, arrowhead = 0, ax = 40, ay = -40,
            align = "left", bgcolor = "white", bordercolor = "#ccc",
            font = list(size = 11)
          )
        )
      )
  })

  ## ---------------------------------------------------------------------------
  ## Save scenario (from Restoration Planning tab) ----
  ## ---------------------------------------------------------------------------
  observeEvent(input$save_scenario, {
    validate(need(nzchar(input$scenario_project), "Enter a project name."))
    validate(need(nzchar(input$scenario_name), "Enter a scenario name."))

    r <- restored_metrics()
    b <- baseline_metrics()

    total_cover <- r$cover
    restored_rap <- r$rap
    baseline_rap <- b$rap

    # Simple illustrative cost / ROI model driven by added cover + outplant cost
    added_cover <- r$cover - b$cover
    cost <- added_cover * .safe_num(input$outplant_cost) * 100 # placeholder unit cost
    elev_gain_10yr <- restored_rap * 10 # mm over 10 years
    roi <- if (cost > 0) (elev_gain_10yr / cost) * 1000 else 0

    scenario <- list(
      project = input$scenario_project,
      scenario = input$scenario_name,
      habitat = input$habitat_choice,
      site_area_m2 = .safe_num(input$site_area_m2),
      total_cover = total_cover,
      baseline_rap = baseline_rap,
      restored_rap = restored_rap,
      outplant_size = .safe_num(input$outplant_size),
      outplant_cost = .safe_num(input$outplant_cost),
      dhw = .safe_num(input$dhw),
      bleach_events = .safe_num(input$bleach_events),
      cost = cost,
      roi = roi,
      elev_gain_10yr = elev_gain_10yr,
      saved = as.character(Sys.time())
    )

    fname <- file.path(
      scenario_dir,
      paste0(
        gsub("[^A-Za-z0-9]", "_", input$scenario_project), "__",
        gsub("[^A-Za-z0-9]", "_", input$scenario_name), ".json"
      )
    )
    write_json(scenario, fname, auto_unbox = TRUE, pretty = TRUE)

    showNotification(
      paste0("Saved scenario '", input$scenario_name, "' under project '", input$scenario_project, "'."),
      type = "message"
    )
  })

  ## ---------------------------------------------------------------------------
  ## Restoration Monitoring tab ----
  ## ---------------------------------------------------------------------------

  # Uploaded coral-cover data (overrides df when present). Accepts .csv or .xlsx.
  uploaded_monitoring_cover <- reactive({
    f <- input$upload_cover
    if (is.null(f)) {
      return(NULL)
    }
    ext <- tolower(tools::file_ext(f$name))
    tryCatch(
      if (ext == "xlsx") {
        readxl::read_excel(f$datapath)
      } else {
        read.csv(f$datapath, stringsAsFactors = FALSE)
      },
      error = function(e) {
        showNotification(paste("Could not read cover file:", e$message), type = "error")
        NULL
      }
    )
  })

  # Uploaded bioerosion data (overrides bioerosion when present)
  uploaded_monitoring_bioerosion <- reactive({
    f <- input$upload_bioerosion
    if (is.null(f)) {
      return(NULL)
    }
    ext <- tolower(tools::file_ext(f$name))
    tryCatch(
      if (ext == "xlsx") {
        readxl::read_excel(f$datapath)
      } else {
        read.csv(f$datapath, stringsAsFactors = FALSE)
      },
      error = function(e) {
        showNotification(paste("Could not read bioerosion file:", e$message), type = "error")
        NULL
      }
    )
  })

  # Helper: baseline metrics for the selected NCRMP site (upload overrides df)
  cc_baseline_vals <- reactive({
    req(input$monitoring_selected_site)

    up <- uploaded_monitoring_cover()
    if (!is.null(up) && "site_id" %in% names(up) &&
        input$monitoring_selected_site %in% up$site_id) {
      dat <- up[up$site_id == input$monitoring_selected_site, , drop = FALSE][1, ]
    } else {
      dat <- df |> filter(site_id == input$monitoring_selected_site) |> slice(1)
    }

    list(
      cover  = dat$hardCoral_PrctCvr,
      budget = dat$net_G,
      rap    = if (!is.null(dat$rap) && !is.na(dat$rap)) dat$rap else dat$net_G / 2.9 / (1 - 0.6265)
    )
  })

  # Helper: restored metrics. Uses the interactive restored RAP from the
  # Restoration Planning tab if available, else falls back to baseline.
  cc_restored_vals <- reactive({
    base <- cc_baseline_vals()
    restored_rap <- if (!is.null(rap_values$restored)) rap_values$restored else base$rap
    restored_budget <- if (!is.null(rap_values$restored)) {
      restored_rap * 2.9 * (1 - 0.6265)
    } else {
      base$budget
    }
    # Restored cover from the planning-tab restoration mix, else baseline
    r <- restored_metrics()
    restored_cover <- if (!is.null(r$cover) && r$cover > 0) r$cover else base$cover
    list(cover = restored_cover, budget = restored_budget, rap = restored_rap)
  })

  output$cc_baseline_cover <- renderValueBox({
    valueBox(paste0(round(cc_baseline_vals()$cover, 1), " %"),
      "Baseline coral cover",
      icon = icon("percent"), color = "green"
    )
  })
  output$cc_baseline_budget <- renderValueBox({
    valueBox(paste0(round(cc_baseline_vals()$budget, 2), " kg/m\u00b2/yr"),
      "Baseline carbonate budget",
      icon = icon("balance-scale"), color = "blue"
    )
  })
  output$cc_baseline_rap <- renderValueBox({
    valueBox(paste0(round(cc_baseline_vals()$rap, 2), " mm/yr"),
      "Baseline reef accretion",
      icon = icon("chart-line"), color = "aqua"
    )
  })

  output$cc_restored_cover <- renderValueBox({
    valueBox(paste0(round(cc_restored_vals()$cover, 1), " %"),
      "Restored coral cover",
      icon = icon("plus-circle"), color = "olive"
    )
  })
  output$cc_restored_budget <- renderValueBox({
    valueBox(paste0(round(cc_restored_vals()$budget, 2), " kg/m\u00b2/yr"),
      "Restored carbonate budget",
      icon = icon("balance-scale"), color = "blue"
    )
  })
  output$cc_restored_rap <- renderValueBox({
    valueBox(paste0(round(cc_restored_vals()$rap, 2), " mm/yr"),
      "Restored reef accretion",
      icon = icon("chart-line"), color = "teal"
    )
  })

  # Impact summary text box
  output$cc_impact_summary <- renderUI({
    b <- cc_baseline_vals()
    r <- cc_restored_vals()
    d_cover  <- r$cover - b$cover
    d_budget <- r$budget - b$budget
    d_rap    <- r$rap - b$rap
    arrow <- function(x) if (x > 0) "\u25B2" else if (x < 0) "\u25BC" else "\u2013"
    HTML(paste0(
      "<p><b>Site:</b> ", input$monitoring_selected_site, "</p>",
      "<p>", arrow(d_cover), " Coral cover change: <b>",
      sprintf("%+.1f", d_cover), " %</b></p>",
      "<p>", arrow(d_budget), " Carbonate budget change: <b>",
      sprintf("%+.2f", d_budget), " kg/m\u00b2/yr</b></p>",
      "<p>", arrow(d_rap), " Reef accretion change: <b>",
      sprintf("%+.2f", d_rap), " mm/yr</b></p>",
      "<hr>",
      "<p>", if (r$rap >= 4) {
        "Restored accretion keeps pace with current sea-level rise."
      } else {
        "Restored accretion still falls short of current sea-level rise."
      }, "</p>"
    ))
  })

  # Timeline: RAP over 10 years with SLR reference lines (plotly)
  output$cc_timeline <- plotly::renderPlotly({
    b <- cc_baseline_vals()
    r <- cc_restored_vals()
    years <- 0:10
    tl <- data.frame(
      Year = rep(years, 2),
      RAP  = c(rep(b$rap, length(years)), rep(r$rap, length(years))),
      Scenario = rep(c("Baseline", "Restored"), each = length(years))
    )

    p <- ggplot(tl, aes(Year, RAP, color = Scenario,
                        group = Scenario,
                        text = paste0(
                          Scenario, "<br>Year: ", Year,
                          "<br>RAP: ", round(RAP, 2), " mm/yr"
                        ))) +
      geom_line(linewidth = 1.2) +
      geom_hline(yintercept = 4, linetype = "dashed", color = "deepskyblue3") +
      geom_hline(yintercept = 40, linetype = "dashed", color = "#0b3d91") +
      scale_color_manual(values = c("Baseline" = "lightgreen", "Restored" = "forestgreen")) +
      labs(x = "Year", y = "Reef accretion potential (mm/yr)", color = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top")

    plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(
        legend = list(orientation = "h", x = 0, y = 1.1),
        annotations = list(
          list(x = 0.5, y = 4, text = "Current SLR (4 mm/yr)",
               showarrow = FALSE, xanchor = "left", yshift = 10,
               font = list(color = "deepskyblue3")),
          list(x = 0.5, y = 40, text = "Future SLR (40 mm/yr)",
               showarrow = FALSE, xanchor = "left", yshift = 10,
               font = list(color = "#0b3d91"))
        )
      )
  })

  # Download report for the Restoration Monitoring tab
  output$cc_download_report <- downloadHandler(
    filename = function() {
      paste0("carbonate_report_", input$monitoring_selected_site, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      b <- cc_baseline_vals()
      r <- cc_restored_vals()
      out <- data.frame(
        Site = input$monitoring_selected_site,
        Metric = c("Coral cover (%)", "Carbonate budget (kg/m2/yr)", "Reef accretion (mm/yr)"),
        Baseline = c(b$cover, b$budget, b$rap),
        Restored = c(r$cover, r$budget, r$rap)
      )
      out$Change <- out$Restored - out$Baseline
      write.csv(out, file, row.names = FALSE)
    }
  )

  ## ---------------------------------------------------------------------------
  ## Scenario Comparison tab ----
  ## ---------------------------------------------------------------------------

  # Read all saved scenario .json files
  all_scenarios <- reactive({
    input$sc_refresh
    input$save_scenario # refresh after a save
    files <- list.files(scenario_dir, pattern = "\\.json$", full.names = TRUE)
    if (length(files) == 0) {
      return(data.frame())
    }
    rows <- lapply(files, function(f) {
      s <- tryCatch(fromJSON(f), error = function(e) NULL)
      if (is.null(s)) {
        return(NULL)
      }
      as.data.frame(s, stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
  })

  # Populate the project selector
  observe({
    sc <- all_scenarios()
    projects <- if (nrow(sc)) sort(unique(sc$project)) else character(0)
    updateSelectInput(session, "sc_project", choices = projects)
  })

  # Populate the scenario multi-select based on chosen project
  observe({
    sc <- all_scenarios()
    req(input$sc_project)
    scen <- if (nrow(sc)) sort(unique(sc$scenario[sc$project == input$sc_project])) else character(0)
    updateCheckboxGroupInput(session, "sc_scenarios", choices = scen)
  })

  # Filtered scenarios for plotting
  sc_selected <- reactive({
    sc <- all_scenarios()
    req(nrow(sc) > 0, input$sc_project, input$sc_scenarios)
    sc[sc$project == input$sc_project & sc$scenario %in% input$sc_scenarios, ]
  })

  # Project cost bar
  output$sc_cost_bar <- renderPlot({
    d <- sc_selected()
    validate(need(nrow(d) > 0, "Select one or more scenarios."))
    ggplot(d, aes(x = scenario, y = cost, fill = scenario)) +
      geom_col() +
      labs(x = NULL, y = "Project cost ($)") +
      scale_fill_brewer(palette = "Blues") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))
  })

  # ROI bar
  output$sc_roi_bar <- renderPlot({
    d <- sc_selected()
    validate(need(nrow(d) > 0, "Select one or more scenarios."))
    ggplot(d, aes(x = scenario, y = roi, fill = scenario)) +
      geom_col() +
      labs(x = NULL, y = "ROI (mm elevation per $1k)") +
      scale_fill_brewer(palette = "Greens") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))
  })

  # RAP & elevation-gain scatter
  output$sc_scatter <- renderPlot({
    d <- sc_selected()
    validate(need(nrow(d) > 0, "Select one or more scenarios."))
    ggplot(d, aes(x = restored_rap, y = elev_gain_10yr, color = scenario)) +
      geom_point(size = 4) +
      geom_text(aes(label = scenario), show.legend = FALSE, vjust = -1) +
      geom_vline(xintercept = 4, linetype = "dashed", color = "deepskyblue3") +
      labs(
        x = "Restored reef accretion (mm/yr)",
        y = "Elevation gain over 10 yr (mm)",
        color = NULL
      ) +
      theme_minimal(base_size = 14)
  })

  # Download the selected scenarios as a .csv report
  output$sc_download_csv <- downloadHandler(
    filename = function() {
      paste0("scenario_comparison_", Sys.Date(), ".csv")
    },
    content = function(file) {
      d <- sc_selected()
      write.csv(d, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)