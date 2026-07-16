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

# Ingest user-input baseline cover data
base_cover_df <- read_excel(here("data", "Baseline_cover_TEMPLATE.xlsx"), sheet = "Coral Cover input")
taxa <- read_excel(here("data", "Baseline_cover_TEMPLATE.xlsx"), sheet = "Taxon list")
taxa <- taxa$Taxon

# Calculate reef accretion potential
df$rap <- df$net_G / 2.9 / (1 - 0.6265)

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
    menuItem("Baseline Input", tabName = "baseline", icon = icon("pen-to-square")),
    menuItem("Coral Cover & Bioerosion", tabName = "coralcover", icon = icon("chart-column")),
    menuItem("Restoration Planning", tabName = "restoration", icon = icon("seedling")),
    menuItem("Scenario Comparison", tabName = "scenarios", icon = icon("scale-balanced")),
    menuItem("About this Site", tabName = "about", icon = icon("circle-info"))
  )
)

"#BFDADA"

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
    "))
  ),

  tabItems(
# Home Tab ----
    tabItem(
      tabName = "home",
      div(
        class = "home-map-outer",
        leafletOutput("mymap", width = "100%", height = "100%"),
        # Right-aligned logos + data caption, kept from the original navbar-custom block
        # Logos overlaid on the top-right corner of the map
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
        tags$div(
          style = "position: absolute; top: 90px; right: 10px;
                   z-index: 1000; display: flex; gap: 8px;",

          tags$img(src = "usgsLogo.png", style = "height: 60px;"),
          tags$img(src = "noaaLogo.png", style = "height: 60px;")
        )
      )
    ),

    # Baseline Input Tab ----
    tabItem(
      tabName = "baseline",
      fluidRow(
        column(
          width = 3,
          shinydashboard::box(
            title = "Baseline cover",
            width = 12, status = "primary", solidHeader = TRUE,

            # Site area and habitat
            numericInput(
              "site_area_m2",
              label = "Site area (m\u00b2)",
              value = NA, min = 0, step = 1
            ),
            selectInput(
              "habitat_choice",
              label = "Habitat",
              choices = c("\u2014 Select habitat \u2014" = "", "Inshore", "Offshore"),
              selected = ""
            ),

            # Existing species selector + dynamic inputs
            selectizeInput(
              "baseline_species",
              "select your species:",
              choices = sort(unique(taxa)),
              multiple = TRUE,
              options = list(maxItems = 12, placeholder = "Select species...")
            ),
            uiOutput("baseline_cover_inputs")
          )
        ),
        column(
          width = 3,
          shinydashboard::box(
            title = "Restoration mix",
            width = 12, status = "success", solidHeader = TRUE,
            div(tags$strong("Set target restoration cover (%) for each species:")),
            uiOutput("restoration_sliders")
          )
        ),
        column(
          width = 2,
          valueBoxOutput("baseline_cover_box", width = NULL),
          valueBoxOutput("baseline_budget", width = NULL)
        ),
        column(
          width = 2,
          valueBoxOutput("restored_cover_box", width = NULL),
          valueBoxOutput("restored_budget", width = NULL)
        )
      )
    ),

    # Coral Cover & Bioerosion Tab ----
    # Filled in per commented guidance:
    #   Sidebar: upload coral cover, upload bioerosion, select site, download report
    #   Main:    baseline vs restored impact (cover/budget/accretion + summary),
    #            timeline of RAP over 10 yrs with SLR reference lines
    tabItem(
      tabName = "coralcover",
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
            selectInput("cc_selected_site", "Select site",
              choices = unique(df$site_id)
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
            title = "Baseline vs. restored impact", width = 12,
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
            title = "Reef accretion potential over 10 years", width = 12,
            status = "success", solidHeader = TRUE,
            plotOutput("cc_timeline", height = "350px")
          )
        )
      )
    ),

    # Restoration Planning Tab ----
    tabItem(
      tabName = "restoration",
      sidebarLayout(

        # sidebar panel (left column)
        sidebarPanel(
          width = 5,
          pickerInput("selected_site", "Select Site", choices = unique(df$site_id)),
          br(),
          tags$h4("Adjust coral cover for restoration",
            style = "color: #3c8dbc; font-weight: bold; font-size: 20px;"
          ),
          uiOutput("coral_slider_ui"),
          br(),
          div(
            actionButton("Restoration_info", "More info", icon = icon("info")),
            hidden(div(
              id = "Restoration_info_text", class = "hidden-text",
              "The corals accompanied by a shaded slider bar come from your selected site but aren\u2019t candidates for restoration.
               If a species that can be restored has a percentage above zero, that species already occurs at the site, and additional planting is possible.",
              style = "color:white"
            ))
          ),
          br(),
          # Allow the user to persist the current scenario for later comparison
          textInput("scenario_project", "Project name", value = ""),
          textInput("scenario_name", "Scenario name", value = ""),
          actionButton("save_scenario", "Save scenario", icon = icon("floppy-disk"))
        ),

        # Main panel (right column)
        mainPanel(
          width = 7,

          # Section headers
          fluidRow(
            column(
              width = 4,
              tags$h4("Baseline", style = "text-align: center; font-weight: bold; font-size: 22px;")
            ),
            column(
              width = 4,
              tags$h4("Restored", style = "text-align: center; font-weight: bold; font-size: 22px;")
            )
          ),

          # Value boxes
          fluidRow(
            column(
              width = 4,
              valueBoxOutput("baseline_coral_cover", width = NULL),
              valueBoxOutput("baseline_budget_box", width = NULL),
              valueBoxOutput("baseline_RAP_box", width = NULL),
              br(),
              tags$h3("Baseline Construction vs. Erosion",
                style = "text-align:center; font-size: 1.5vw;"
              ),
              plotOutput("baseline_pie", width = "100%", height = "350px")
            ),
            column(
              width = 4,
              valueBoxOutput("total_coral_added", width = NULL),
              valueBoxOutput("restored_budget_box", width = NULL),
              valueBoxOutput("restored_RAP_box", width = NULL),
              br(),
              tags$h3("Restored Construction vs. Erosion",
                style = "text-align:center; font-size: 1.5vw;"
              ),
              plotOutput("restored_pie", width = "100%", height = "350px")
            ),
            column(
              width = 4,
              tags$h4("Sea level rise and reef growth",
                style = "text-align:center; font-size: 1.5vw;"
              ),
              plotOutput("slr_circle", width = "100%", height = "400px")
            )
          ),
          fluidRow(
            column(
              width = 12, align = "left",
              tags$div(
                style = "display: inline-flex; gap: 10px; padding-top: 0px;",
                tags$div(
                  style = "display: flex; align-items: center;",
                  tags$div(style = "width: 15px; height: 15px; background-color: #104E8B; margin-right: 6px;"),
                  tags$span("Net Calcification")
                ),
                tags$div(
                  style = "display: flex; align-items: center;",
                  tags$div(style = "width: 15px; height: 15px; background-color: #663399; margin-right: 6px;"),
                  tags$span("Parrotfish")
                ),
                tags$div(
                  style = "display: flex; align-items: center;",
                  tags$div(style = "width: 15px; height: 15px; background-color: #5E819D; margin-right: 6px;"),
                  tags$span("Urchins")
                ),
                tags$div(
                  style = "display: flex; align-items: center;",
                  tags$div(style = "width: 15px; height: 15px; background-color: #CC0099; margin-right: 6px;"),
                  tags$span("BioSponges")
                ),
                tags$div(
                  style = "display: flex; align-items: center;",
                  tags$div(style = "width: 15px; height: 15px; background-color: #184b52; margin-right: 6px;"),
                  tags$span("Microborers")
                )
              )
            )
          )
        )
      )
    ),

    # Scenario Comparison Tab ----
    # Filled in per commented guidance:
    #   Sidebar: project (single select), scenario (multi select from saved .json),
    #            download report (.csv)
    #   Main:    "Year 10 Outcome Summary" -> cost bar, ROI bar, RAP/Elev scatter
    tabItem(
      tabName = "scenarios",
      fluidRow(
        # Sidebar (left)
        column(
          width = 3,
          shinydashboard::box(
            title = "Scenario selection", width = 12,
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
                title = "Project cost", width = 12,
                status = "info", solidHeader = TRUE,
                plotOutput("sc_cost_bar", height = "300px")
              )
            ),
            column(
              width = 6,
              shinydashboard::box(
                title = "Return on investment", width = 12,
                status = "info", solidHeader = TRUE,
                plotOutput("sc_roi_bar", height = "300px")
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              shinydashboard::box(
                title = "Carbonate budget: RAP & elevation gain", width = 12,
                status = "success", solidHeader = TRUE,
                plotOutput("sc_scatter", height = "350px")
              )
            )
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
# Color palette for site circles on home page
at <- c(-8, -6, -4, -2, 0, 2, 4, 6, 8)
colors <- c("darkred", "red", "orange", "yellow", "white", "#0099FF", "#0033FF", "darkblue", "#000066")
num_pal <- colorNumeric(colors, domain = at)
num_pal_rev <- colorNumeric(colors, domain = at, reverse = TRUE)

# make data frame reactive
server <- function(input, output, session) {
  # define reactVal to store coordinates
  reef_name       <- reactiveVal()
  reef_year       <- reactiveVal(2019)
  # reef_adaptation <- reactiveVal(0)
  initial_budget  <- reactiveVal(NULL)

  slider_ids <- reactive({
    req(input$selected_site)

    site_data <- mote_cover |>
      filter(Site == input$selected_site, Class == "HC", !is.na(months12)) |>
      pull(Species)

    # Make save IDs using make.names
    make.names(site_data)
  })

  observeEvent(input$plot_date, {
    reef_year(input$plot_date)
    updateSliderTextInput(session, "plot_date2", selected = reef_year())
  })

  # Observe changes in the second slider and update the first slider
  observeEvent(input$plot_date2, {
    reef_year(input$plot_date2)
    updateSliderTextInput(session, "plot_date", selected = reef_year())
  })

  observeEvent(input$plot_date2, {
    reef_year(input$plot_date2)
  })

  filtered_df <- reactive({
    df |>
      filter(site_id == input$selected_site)
  })

  # Initialize leaflet map ----
  output$mymap <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) |>
      addProviderTiles(providers$Esri.WorldImagery,
        options = providerTileOptions(attribution = 'Map data &copy; <a href="https://www.esri.com/">Esri</a>')
      ) |>
      setView(lng = -80.6097, lat = 25, zoom = 8) |>

      # Static legend for carbonate budget
      addLegendNumeric(
        pal = num_pal_rev,
        title = HTML("Reef<br/>accretion<br/>potential<br/>(mm/yr)"),
        shape = "stadium",
        values = at,
        fillOpacity = 10,
        decreasing = TRUE,
        position = "bottomleft"
      ) |>

      # Static legend for reef status
      addLegend("bottomleft",
        colors  = c("#0099FF", "#FFFF99", "#FF6600"),
        labels  = c("Growing", "Stasis", "Eroding"),
        title   = HTML("<span style='font-size: 16px;'>Reef Status</span>"),
        opacity = 1
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

  # Add NCRMP data to map ----
  observe({
    leafletProxy("mymap", data = df) |>
      addCircleMarkers(
        lng    = ~LON_DEGREES,
        lat    = ~LAT_DEGREES,
        radius = 6,
        weight = 2,
        color  = "black",
        fillColor   = ~ num_pal(rap),
        fillOpacity = 0.8,
        stroke      = TRUE,
        popup       = ~ paste0(
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
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Reef accr. potential:</td>",
          "<td style='padding: 2px 0;'>", round(rap, 2), " mm/yr</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Current status:</td>",
          "<td style='padding: 2px 0;'>", budget_State, "</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Water depth:</td>",
          "<td style='padding: 2px 0;'>", round(AVG_DEPTH, 1), " m</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Coordinates:</td>",
          "<td style='padding: 2px 0;'>", round(LON_DEGREES, 5), ", ", round(LAT_DEGREES, 5), "</td></tr>",
          "</table>"
        )
      )
  })

  # capture the selected reef name for the reef characteristics tab
  observeEvent(input$mymap_marker_click, {
    click <- input$mymap_marker_click

    reef_name(df |>
                filter(LAT_DEGREES == click$lat & LON_DEGREES == click$lng) |>
                pull(site_id) |>
                unique())

    print(paste("Selected reef:", reef_name()))

    updateSelectInput(session, "selectReef", selected = reef_name())
  })

  # capture the selected reef name for the restoration tab
  observeEvent(input$mymap_marker_click, {
    click <- input$mymap_marker_click

    reef_name(df |>
                filter(LAT_DEGREES == click$lat & LON_DEGREES == click$lng) |>
                pull(site_id) |>
                unique())

    updateSelectInput(session, "selectReef2", selected = reef_name())

    # Also sync the restoration-tab site picker to the clicked marker
    updatePickerInput(session, "selected_site", selected = reef_name())
  })

  # text appears
  observeEvent(input$more_info, {
    toggle("more_info_text")
  })

  observeEvent(input$more_info2, {
    toggle("more_info2_text")
  })

  # change the tab when the hyperlink is clicked
  # (dashboardSidebar uses updateTabItems with menu tabNames)
  observeEvent(input$linkClickReef, {
    updateTabItems(session, inputId = "nav", selected = "restoration")
  })

  observeEvent(input$linkClickPlanning, {
    updateTabItems(session, inputId = "nav", selected = "restoration")
  })

  ## Sliders ----

  output$coral_slider_ui <- renderUI({
    req(input$selected_site)

    site_data <- mote_cover |>
      filter(Site == input$selected_site, Class == "HC", !is.na(months12)) |>
      select(Species, Cover = months12)

    restoration_species <- restoration_species_global

    all_species <- union(site_data$Species, restoration_species)
    non_restoration_species <- setdiff(site_data$Species, restoration_species)
    ordered_species <- c(non_restoration_species, restoration_species)

    sliders <- lapply(ordered_species, function(species) {
      cover <- site_data$Cover[site_data$Species == species]
      init_value <- if (length(cover) > 0) cover else 0
      is_restoration <- species %in% restoration_species
      id <- paste0("site_slider_", gsub(" ", "_", species))

      row_style <- "padding: 6px; margin-bottom: 8px; color: white;"
      init_value <- if (length(cover) > 0) round(cover, 2) else 0
      slider <- sliderInput(
        inputId = id,
        label = NULL,
        min = init_value,
        max = 50,
        value = init_value,
        step = 0.1,
        post = "%",
        width = "100%"
      )

      if (!is_restoration) {
        slider <- shinyjs::disabled(slider)
      }

      div(
        style = row_style,
        fluidRow(
          column(5, tags$strong(tags$em(style = "color: inherit;", species))),
          column(7, slider)
        )
      )
    })

    half <- ceiling(length(sliders) / 2)
    fluidRow(
      column(6, sliders[1:half]),
      column(6, sliders[(half + 1):length(sliders)])
    )
  })

  output$reef_status_box <- renderValueBox({
    if (is.null(initial_budget())) {
      valueBox(
        value = "Your reef is ...",
        subtitle = "Reef status",
        icon = icon("question-circle"),
        color = "blue"
      )
    } else {
      budget <- initial_budget()
      if (budget < -1) {
        valueBox(
          value = "Your reef is eroding",
          subtitle = "Reef status",
          icon = icon("fire"),
          color = "red"
        )
      } else if (budget >= -1 && budget <= 1) {
        valueBox(
          value = "Your reef is in stasis",
          subtitle = "Reef status",
          icon = icon("pause-circle"),
          color = "yellow"
        )
      } else {
        valueBox(
          value = "Your reef is growing",
          subtitle = "Reef status",
          icon = icon("leaf"),
          color = "green"
        )
      }
    }
  })

  ## Restoration Planning ----
  observeEvent(input$Restoration_info, {
    toggle("Restoration_info_text")
  })

  ## Baseline Carbonate Budget Value Box ----
  output$baseline_budget_box <- renderValueBox({
    req(input$selected_site)

    coral_data <- mote_cover |>
      filter(Site == input$selected_site, Class == "HC", !is.na(months12)) |>
      select(Species, Cover = months12, Location)

    result <- coral_data |>
      left_join(travis_rates, by = "Species") |>
      left_join(bioerosion, by = "Location") |>
      mutate(Contribution = Cover * rate / 100)

    net_budget <- sum(result$Contribution, na.rm = TRUE)
    erosion_total <- result |>
      dplyr::distinct(HABITAT_TYPE, AVG_PARROTFISH, AVG_URCHIN, AVG_MICROBIOEROSION) |>
      dplyr::summarise(total = AVG_PARROTFISH + AVG_URCHIN + AVG_MICROBIOEROSION) |>
      dplyr::pull(total)

    total_budget <- net_budget - erosion_total

    valueBox(
      value = tags$p(
        paste0(round(total_budget, 2), " kg/m\u00b2/yr"),
        style = "font-size: 2.1vw;"
      ),
      subtitle = tags$p(
        "Baseline Carbonate Budget",
        style = "font-size:1.1vw;"
      ),
      icon = icon("balance-scale"),
      color = "blue",
      width = 12
    )
  })

  rap_values <- reactiveValues(
    baseline = NULL,
    restored = NULL
  )

  ## Baseline RAP Box ----
  output$baseline_RAP_box <- renderValueBox({
    req(input$selected_site)

    coral_data <- mote_cover |>
      filter(Site == input$selected_site, Class == "HC", !is.na(months12)) |>
      select(Species, Cover = months12, Location)

    result <- coral_data |>
      left_join(travis_rates, by = "Species") |>
      left_join(bioerosion, by = "Location") |>
      mutate(Contribution = Cover * rate / 100)

    net_budget <- sum(result$Contribution, na.rm = TRUE)

    bio_vals <- result |>
      dplyr::distinct(HABITAT_TYPE, AVG_PARROTFISH, AVG_URCHIN, AVG_MICROBIOEROSION) |>
      dplyr::slice(1) |>
      tidyr::replace_na(list(PF = 0, Urchins = 0, BioSponges = 0, Micro = 0))

    pf <- bio_vals$PF
    urchins <- bio_vals$Urchins
    bio_sponges <- bio_vals$BioSponges

    rap_values$baseline <- (net_budget + ((-pf * 0.25) - (urchins + bio_sponges) * 0.5)) / (2.89 * (1 - 0.558))

    valueBox(
      value = tags$p(paste0(round(rap_values$baseline, 2), " mm/yr"),
        style = "font-size: 2.1vw;"
      ),
      subtitle = tags$p("Baseline Reef Accretion",
        style = "font-size:1.1vw;"
      ),
      icon = icon("chart-line"),
      color = "aqua",
      width = 12
    )
  })

  ## Restored Carbonate Budget Value Box ----
  output$restored_budget_box <- renderValueBox({
    req(input$selected_site)

    site_data <- mote_cover |>
      filter(Site == input$selected_site, Class == "HC", !is.na(months12)) |>
      select(Species)

    restoration_species <- restoration_species_global

    all_species <- union(site_data$Species, restoration_species)
    ordered_species <- c(setdiff(site_data$Species, restoration_species), restoration_species)

    slider_ids <- paste0("site_slider_", gsub(" ", "_", ordered_species))

    covers <- sapply(slider_ids, function(id) {
      val <- input[[id]]
      if (is.null(val)) {
        return(0)
      }
      return(as.numeric(val))
    })

    total_cover <- sum(covers, na.rm = TRUE)

    if (total_cover > 100) {
      return(valueBox(
        value = tags$p("\u2014",
          style = "font-size: 2.1vw;"
        ),
        subtitle = tags$p("Restored Carbonate Budget",
          style = "font-size:1.1vw;"
        ),
        icon = icon("balance-scale"),
        color = "blue",
        width = 12
      ))
    }

    coral_data <- data.frame(
      Species = ordered_species,
      Cover = covers
    )

    result <- coral_data |>
      left_join(travis_rates, by = "Species") |>
      mutate(Contribution = Cover * rate / 100)

    net_budget <- sum(result$Contribution, na.rm = TRUE)

    site_loc <- mote_cover |>
      filter(Site == input$selected_site) |>
      distinct(Location) |>
      slice(1)

    bio_vals <- left_join(site_loc, bioerosion, by = "Location")
    pf <- ifelse(is.na(bio_vals$PF), 0, bio_vals$PF)
    urchins <- ifelse(is.na(bio_vals$Urchins), 0, bio_vals$Urchins)
    bio_sponges <- ifelse(is.na(bio_vals$BioSponges), 0, bio_vals$BioSponges)
    micro <- ifelse(is.na(bio_vals$Micro), 0, bio_vals$Micro)

    erosion_total <- pf + urchins + bio_sponges + micro

    total_budget <- net_budget - erosion_total

    valueBox(
      value = tags$p(paste0(round(total_budget, 2), " kg/m\u00b2/yr"),
        style = "font-size: 2.1vw;"
      ),
      subtitle = tags$p("Restored Carbonate Budget",
        style = "font-size:1.2vw;"
      ),
      icon = icon("balance-scale"),
      color = "blue",
      width = 12
    )
  })

  ## Restored RAP Box ----
  output$restored_RAP_box <- renderValueBox({
    req(input$selected_site)

    site_data <- mote_cover |>
      filter(Site == input$selected_site, Class == "HC", !is.na(months12)) |>
      select(Species)

    restoration_species <- restoration_species_global

    all_species <- union(site_data$Species, restoration_species)
    ordered_species <- c(setdiff(site_data$Species, restoration_species), restoration_species)

    slider_ids <- paste0("site_slider_", gsub(" ", "_", ordered_species))

    covers <- sapply(slider_ids, function(id) {
      val <- input[[id]]
      if (is.null(val)) {
        return(0)
      }
      return(as.numeric(val))
    })

    total_cover <- sum(covers, na.rm = TRUE)

    if (total_cover > 100) {
      return(valueBox(
        value = tags$p("\u2014",
          style = "font-size: 2.1vw;"
        ),
        subtitle = tags$p("Restored Reef Accretion",
          style = "font-size: 1.1vw;"
        ),
        icon = icon("chart-line"),
        color = "teal",
        width = 12
      ))
    }
    site_loc <- mote_cover |>
      filter(Site == input$selected_site) |>
      distinct(Location) |>
      slice(1)

    coral_data <- data.frame(
      Species = ordered_species,
      Cover = covers,
      Location = site_loc$Location[1]
    )

    result <- coral_data |>
      left_join(travis_rates, by = "Species") |>
      left_join(bioerosion, by = "Location") |>
      mutate(Contribution = as.numeric(Cover) * as.numeric(rate) / 100)

    net_budget <- sum(result$Contribution, na.rm = TRUE)

    bio_vals <- result |>
      dplyr::distinct(Location, PF, Urchins, BioSponges, Micro) |>
      dplyr::slice(1)

    pf <- as.numeric(bio_vals$PF)
    urchins <- as.numeric(bio_vals$Urchins)
    bio_sponges <- as.numeric(bio_vals$BioSponges)
    rap_values$restored <- (net_budget + ((-pf * 0.25) - (urchins + bio_sponges) * 0.5)) / (2.89 * (1 - 0.558))

    valueBox(
      value = tags$p(paste0(round(rap_values$restored, 2), " mm/yr"),
        style = "font-size: 2.1vw;"
      ),
      subtitle = tags$p("Restored Reef Accretion",
        style = "font-size: 1.1vw;"
      ),
      icon = icon("chart-line"),
      color = "teal",
      width = 12
    )
  })

  ## Baseline Coral Cover ----
  output$baseline_coral_cover <- renderValueBox({
    req(input$selected_site)

    total_cover <- mote_cover |>
      dplyr::filter(
        Site == input$selected_site,
        Class == "HC",
        !is.na(months12)
      ) |>
      dplyr::summarise(total = sum(months12, na.rm = TRUE)) |>
      dplyr::pull(total)

    if (length(total_cover) == 0 || is.na(total_cover)) {
      total_cover <- 0
    }
    total_cover_rounded <- round(total_cover, 2)

    valueBox(
      value = tags$p(paste0(sprintf("%.1f", total_cover_rounded), " %"),
        style = "font-size: 2.1vw;"
      ),
      subtitle = tags$p("Baseline Coral Cover",
        style = "font-size: 1.1vw;"
      ),
      icon = icon("percent"),
      color = "green"
    )
  })

  ## Restored Coral Cover ----
  output$total_coral_added <- renderValueBox({
    req(input$selected_site)

    site_data <- mote_cover |>
      filter(Site == input$selected_site, Class == "HC", !is.na(months12)) |>
      select(Species)

    restoration_species <- restoration_species_global

    all_species <- union(site_data$Species, restoration_species)
    ordered_species <- c(setdiff(site_data$Species, restoration_species), restoration_species)

    slider_ids <- paste0("site_slider_", gsub(" ", "_", ordered_species))

    values <- sapply(slider_ids, function(id) {
      val <- input[[id]]
      if (is.null(val)) {
        return(0)
      }
      return(as.numeric(val))
    })

    total_cover <- sum(values, na.rm = TRUE)
    total_cover_rounded <- round(total_cover, 2)

    if (total_cover_rounded > 100) {
      valueBox(
        value = tags$p(">100%",
          style = "font-size: 2.1vw;"
        ),
        subtitle = tags$p("Total Target Cover",
          style = "font-size:1.1vw;"
        ),
        icon = icon("exclamation-triangle"),
        color = "red",
        width = 12
      )
    } else {
      valueBox(
        value = tags$p(paste0(sprintf("%.1f", total_cover_rounded), " %"),
          style = "font-size: 2vw;"
        ),
        subtitle = tags$p("Total Target Cover",
          style = "font-size:1vw;"
        ),
        icon = icon("plus-circle"),
        color = "olive",
        width = 12
      )
    }
  })

  ## Baseline Pie ----
  output$baseline_pie <- renderPlot({
    req(input$selected_site)

    coral_data <- mote_cover |>
      filter(Site == input$selected_site, Class == "HC", !is.na(months12)) |>
      select(Species, Cover = months12) |>
      left_join(travis_rates, by = "Species") |>
      mutate(Contribution = Cover * rate / 100)

    net_calcification <- sum(coral_data$Contribution, na.rm = TRUE)

    site_location <- mote_cover |>
      filter(Site == input$selected_site) |>
      pull(Location) |>
      unique()

    bio_data <- bioerosion |>
      filter(Location == site_location)

    pf <- bio_data$PF[1]
    urchins <- bio_data$Urchins[1]
    biosponges <- bio_data$BioSponges[1]
    micro <- bio_data$Micro[1]

    mpiedat <- data.frame(
      variable = c("Net Calcification", "Parrotfish", "Urchins", "BioSponges", "Microborers"),
      value = c(net_calcification, pf, urchins, biosponges, micro)
    )

    pie_colors <- c(
      "Net Calcification" = "dodgerblue4",
      "Parrotfish" = "#663399",
      "Urchins" = "#5E819D",
      "BioSponges" = "#CC0099",
      "Microborers" = "#184b52"
    )

    ggplot(mpiedat, aes(x = "", y = value, fill = variable)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = pie_colors) +
      theme_void() +
      theme(
        legend.position = "none"
      ) +
      guides(fill = guide_legend(title.position = "none", title.hjust = 0, nrow = 2))
  })

  ## Restored Pie ----
  output$restored_pie <- renderPlot({
    req(input$selected_site)

    ## SLR Circle ----
    output$slr_circle <- renderPlot({
      library(ggplot2)

      # params
      present_mm <- 4
      future_mm <- 40
      scale_max <- 100
      r_circle <- 3
      fill_cut_mm <- 5
      blue_fill <- "deepskyblue3"

      mm_to_y <- function(mm) (mm / scale_max) * r_circle
      max_y <- r_circle - 0.05
      y_restored_rap <- min(mm_to_y(rap_values$restored), max_y)
      max_y_baseline <- r_circle - 0.05
      y_baseline_rap <- min(mm_to_y(rap_values$baseline), max_y_baseline)

      angle <- seq(0, 2 * pi, length.out = 720)
      circle_df <- data.frame(
        x = r_circle * cos(angle),
        y = r_circle * sin(angle)
      )

      y_cut <- mm_to_y(fill_cut_mm)

      lower_edge <- subset(circle_df, y <= y_cut)
      lower_fill <- rbind(
        lower_edge,
        data.frame(x = rev(lower_edge$x), y = rep(y_cut, nrow(lower_edge)))
      )

      upper_edge <- subset(circle_df, y >= y_cut)
      upper_fill <- rbind(
        upper_edge,
        data.frame(x = rev(upper_edge$x), y = rep(y_cut, nrow(upper_edge)))
      )

      draw_horizontal <- function(y_val, col) {
        if (abs(y_val) <= r_circle) {
          x_half <- sqrt(r_circle^2 - y_val^2)
          geom_segment(aes(x = -x_half, xend = x_half, y = y_val, yend = y_val),
            color = col, linewidth = 1
          )
        } else {
          NULL
        }
      }

      y_present <- mm_to_y(present_mm)
      y_future <- mm_to_y(future_mm)

      wave_x <- seq(-r_circle, r_circle, length.out = 1000)

      wave_y_present_center <- mm_to_y(present_mm)
      wave_y_present <- wave_y_present_center + 0.04 * sin(10 * wave_x)
      inside_present <- wave_x^2 + wave_y_present^2 <= r_circle^2
      wave_present_df <- data.frame(x = wave_x[inside_present], y = wave_y_present[inside_present])

      wave_y_future_center <- mm_to_y(future_mm)
      wave_y_future <- wave_y_future_center + 0.05 * sin(10 * wave_x)
      inside_future <- wave_x^2 + wave_y_future^2 <= r_circle^2
      wave_future_df <- data.frame(x = wave_x[inside_future], y = wave_y_future[inside_future])

      ggplot() +
        geom_polygon(data = lower_fill, aes(x, y), fill = blue_fill, color = NA) +
        geom_polygon(data = upper_fill, aes(x, y), fill = "aliceblue", color = NA) +
        geom_path(data = wave_present_df, aes(x, y), color = blue_fill, linewidth = 1) +
        geom_path(data = wave_future_df, aes(x, y), color = "#0b3d91", linewidth = 1) +
        annotate("text",
          x = 1.5, y = y_present + 0.15,
          label = paste0("Current SLR (", present_mm, " mm/yr)"),
          size = 4.5, color = blue_fill
        ) +
        annotate("text",
          x = 1.3, y = y_future + 0.15,
          label = paste0("Future SLR (", future_mm, " mm/yr)"),
          size = 4.5, color = "#0b3d91"
        ) +
        draw_horizontal(y_baseline_rap, "lightgreen") +
        draw_horizontal(y_restored_rap, "forestgreen") +
        annotate("text",
          x = r_circle,
          y = y_baseline_rap - 0.2,
          label = "Baseline RAP",
          color = "lightgreen",
          size = 4.5,
          hjust = 3.2
        ) +
        annotate("text",
          x = r_circle,
          y = y_restored_rap + 0.2,
          label = "Restored RAP",
          color = "forestgreen",
          size = 4.5,
          hjust = 3.1
        ) +
        coord_fixed() +
        theme_void()
    })

    site_data <- mote_cover |>
      filter(Site == input$selected_site, Class == "HC", !is.na(months12)) |>
      select(Species)

    restoration_species <- restoration_species_global

    all_species <- union(site_data$Species, restoration_species)
    ordered_species <- c(setdiff(site_data$Species, restoration_species), restoration_species)

    slider_ids <- paste0("site_slider_", gsub(" ", "_", ordered_species))
    values <- sapply(slider_ids, function(id) {
      val <- input[[id]]
      if (is.null(val)) {
        return(0)
      }
      as.numeric(val)
    })

    cover_df <- data.frame(Species = ordered_species, Cover = values)

    calc_df <- cover_df |>
      left_join(travis_rates, by = "Species") |>
      mutate(Contribution = Cover * rate / 100)

    net_calcification <- sum(calc_df$Contribution, na.rm = TRUE)

    site_location <- mote_cover |>
      filter(Site == input$selected_site) |>
      pull(Location) |>
      unique()

    bio_data <- bioerosion |>
      filter(Location == site_location)

    pf <- bio_data$PF[1]
    urchins <- bio_data$Urchins[1]
    biosponges <- bio_data$BioSponges[1]
    micro <- bio_data$Micro[1]

    mpiedat <- data.frame(
      variable = c("Net Calcification", "Parrotfish", "Urchins", "BioSponges", "Microborers"),
      value = c(net_calcification, pf, urchins, biosponges, micro)
    )

    pie_colors <- c(
      "Net Calcification" = "dodgerblue4",
      "Parrotfish" = "#663399",
      "Urchins" = "#5E819D",
      "BioSponges" = "#CC0099",
      "Microborers" = "#184b52"
    )

    ggplot(mpiedat, aes(x = "", y = value, fill = variable)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = pie_colors) +
      theme_void() +
      theme(
        legend.position = "none"
      ) +
      guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 2))
  })

  ## Box1 ----
  output$stateBox <- renderValueBox({
    reef_to_use <- reef_name()

    dat <- df |> filter(site_id == reef_to_use)

    reef_state <- if (dat$net_G > 0.9) {
      "GROWING"
    } else if (dat$net_G < -0.1) {
      "ERODING"
    } else {
      "IN STASIS"
    }

    box_color <- switch(reef_state,
      "GROWING" = "blue",
      "ERODING" = "red",
      "IN STASIS" = "yellow",
      "blue"
    )

    valueBox(
      value = tags$p(
        paste("Your reef is", reef_state),
        style = "font-size: 2vw; font-weight: bold"
      ),
      subtitle = "Reef status",
      icon = icon("sliders"),
      color = box_color
    )
  })

  output$coverBox <- renderValueBox({
    dat <- df |> filter(site_id == reef_name())
    valueBox(
      value = tags$p(paste0(round(dat$hardCoral_PrctCvr), "%"), style = "font-size: 2vw;"),
      "Coral cover",
      icon = icon("chart-pie"),
      color = "purple"
    )
  })

  output$carbonateBox <- renderValueBox({
    dat <- df |> filter(site_id == reef_name())
    valueBox(
      value = tags$p(HTML(paste0(round(dat$net_G, 1), "kg/m", tags$sup("2"), "/year")), style = "font-size: 2vw;"),
      "Carbonate budget",
      icon = icon("scale-balanced"),
      color = "blue"
    )
  })

  output$rapBox <- renderValueBox({
    dat <- df |> filter(site_id == reef_name())
    valueBox(
      value = tags$p(paste0(round(dat$rap, 1), "mm/year"), style = "font-size: 2vw;"),
      "Vertical reef growth",
      icon = icon("layer-group"),
      color = "blue"
    )
  })

  output$State <- renderText({
    dat <- df |> filter(site_id == reef_name())

    if ((dat$net_G) > 0.9) {
      paste("The reef is", "growing.")
    } else if (dat$net_G < -0.1) {
      paste("The reef is", "eroding.")
    } else {
      paste("The reef is", "neither growing or eroding.")
    }
  })

  output$Carbonate <- renderText({
    dat <- df |> filter(site_id == reef_name())

    paste(
      "The net carbonate budget equals",
      round(dat$net_G), "kg/m2/year."
    )
  })

  output$Cover <- renderText({
    dat <- df |> filter(site_id == reef_name())

    paste(
      "Coral cover is ",
      round(dat$hardCoral_PrctCvr), "%."
    )
  })

  ## Schematic: Accretion vs. SLR (Box2) ----
  output$SLRmetrics <- renderText({
    reef_depth <- df |>
      filter(site_id == reef_name()) |>
      pull(AVG_DEPTH)

    if (reef_depth < 0) {
      paste0(
        "The natural wall of the reef is now \n",
        abs(round(reef_depth)),
        " mm deeper compared to 2019."
      )
    } else {
      paste0(
        "The natural wall of the reef is now \n",
        abs(round(reef_depth)),
        " mm shallower compared to 2019."
      )
    }
  })

  ## Box3 ----
  output$myImage <- renderImage(
    {
      dat <- df |> filter(site_id == reef_name())
      piedat <- dat[c("hardCoral_G", "macrobioerosion_G", "microbioerosion_G", "cca_G")]
      mpiedat <- melt(piedat, id.vars = NULL)

      mpiedat$category <- ifelse(mpiedat$variable %in% c("hardCoral_G", "cca_G"), "Constructors", "Destroyers")

      mpiedat$variable <- factor(mpiedat$variable, levels = c("hardCoral_G", "cca_G", "macrobioerosion_G", "microbioerosion_G"))

      plot <- ggplot(mpiedat, aes(x = "", y = value, fill = variable)) +
        geom_bar(stat = "identity", width = 1, color = "white", position = "stack") +
        coord_polar("y", start = 0) +
        scale_fill_manual(values = c("Coral" = "#663399", "CCA" = "#CC0099", "Macro" = "#7d9dbc", "Micro" = "#184b52")) +
        labs(fill = "") +
        theme_void() +
        theme(
          legend.position = "bottom",
          legend.title = element_text(color = "white", size = 12, face = "bold"),
          legend.text = element_text(color = "white", size = 9),
          legend.box = "vertical"
        ) +
        guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))

      outfile <- tempfile(fileext = ".png")

      png(outfile,
        width = 200 * 8,
        height = 200 * 8,
        res = 72 * 8, bg = "transparent"
      )
      print(plot)
      dev.off()
      list(
        src = outfile,
        contentType = "image/png",
        height = "80%",
        width = "80%"
      )
    },
    deleteFile = TRUE
  )

  # Reactive expression to compute the percentage
  reactive_percentage <- reactive({
    avg_value <- (input$slider1 + input$slider2 + input$slider3)
    paste0(round(avg_value), "%")
  })

  output$restoredCoral <- renderValueBox({
    valueBox(
      value = tags$p(
        reactive_percentage(),
        style = "font-size: 2.5vw;"
      ),
      subtitle = "total coral cover was added",
      icon = icon("leaf"),
      color = "green"
    )
  })

  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(head(data, input$maxrows), row.names = FALSE)
    options(orig)
  })

  ## Restoration Planning 2 (Baseline Input tab) ----
  output$baseline_cover_inputs <- renderUI({
    req(input$baseline_species)
    sp <- input$baseline_species
    tagList(lapply(sp, function(s) {
      id <- paste0("base_", gsub("[^A-Za-z0-9]", "_", s))
      numericInput(id, label = s, value = 0, min = 0, max = 100, step = 0.1)
    }))
  })

  # Fixed restoration species sliders
  restoration_species <- c(
    "Acropora palmata", "Acropora cervicornis",
    "Montastraea cavernosa", "Orbicella faveolata",
    "Colpophyllia natans", "Porites astreoides",
    "Siderastrea siderea", "Stephanocoenia intersepta",
    "Diploria labyrinthiformis", "Solenastrea bournoni",
    "Pseudodiploria spp."
  )

  output$restoration_sliders <- renderUI({
    sliders <- lapply(restoration_species, function(s) {
      id <- paste0("rest_slider_", gsub("[^A-Za-z0-9]", "_", s))
      sliderInput(id, label = s, min = 0, max = 100, value = 0, step = 0.5, post = "%")
    })

    half <- ceiling(length(sliders) / 2)

    fluidRow(
      column(6, tagList(sliders[1:half])),
      column(6, tagList(sliders[(half + 1):length(sliders)]))
    )
  })

  output$baseline_cover_box <- shinydashboard::renderValueBox({
    sp <- input$baseline_species %||% character(0)
    ids <- paste0("base_", gsub("[^A-Za-z0-9]", "_", sp))

    totals <- sapply(ids, function(id) {
      val <- input[[id]]
      if (is.null(val)) 0 else as.numeric(val)
    })

    total_cover <- sum(totals, na.rm = TRUE)

    shinydashboard::valueBox(
      value = tags$p(paste0(round(total_cover, 1), "%"),
        style = "font-size: 2vw;"
      ),
      subtitle = tags$p("Baseline cover",
        style = "font-size: 1vw;"
      ),
      icon = icon("layer-group", class = "fa-3x"),
      color = if (total_cover > 100) "red" else "blue",
      width = 12
    )
  })

  .safe_num <- function(x) {
    if (is.null(x) || is.na(x)) 0 else as.numeric(x)
  }

  output$restored_cover_box <- shinydashboard::renderValueBox({
    sp_base <- if (is.null(input$baseline_species)) character(0) else input$baseline_species
    base_ids <- paste0("base_", gsub("[^A-Za-z0-9]", "_", sp_base))
    base_vals <- sapply(base_ids, function(id) .safe_num(input[[id]]))
    baseline_total <- sum(base_vals, na.rm = TRUE)

    restoration_species <- restoration_species_global
    slider_ids <- paste0("rest_slider_", gsub("[^A-Za-z0-9]", "_", restoration_species))
    slider_vals <- sapply(slider_ids, function(id) .safe_num(input[[id]]))
    restoration_total <- sum(slider_vals, na.rm = TRUE)

    restored_total <- baseline_total + restoration_total

    shinydashboard::valueBox(
      value = tags$p(paste0(round(restored_total, 1), "%"),
        style = "font-size: 2vw;"
      ),
      subtitle = tags$p("Total Target Cover",
        style = "font-size: 1vw;"
      ),
      icon = icon("leaf"),
      color = if (restored_total > 100) "red" else "olive",
      width = 12
    )
  })

  output$baseline_budget <- shinydashboard::renderValueBox({
    sp <- if (is.null(input$baseline_species)) character(0) else input$baseline_species
    ids <- paste0("base_", gsub("[^A-Za-z0-9]", "_", sp))
    covers <- sapply(ids, function(id) {
      v <- input[[id]]
      if (is.null(v)) 0 else as.numeric(v)
    })

    rates <- as.numeric(travis_rates$rate[match(sp, travis_rates$Species)])
    net_budget <- sum(covers * rates / 100, na.rm = TRUE)

    row <- bioerosion[bioerosion$Location == input$habitat_choice, c("AVG_PARROTFISH", "AVG_URCHIN", "AVG_MICROBIOEROSION"), drop = FALSE]
    total_erosion <- if (nrow(row)) sum(as.numeric(row[1, ]), na.rm = TRUE) else 0

    shinydashboard::valueBox(
      value = tags$p(paste0(round(net_budget - total_erosion, 2), " kg/m\u00b2/yr"),
        style = "font-size: 2vw;"
      ),
      subtitle = tags$p("Baseline carbonate budget",
        style = "font-size: 1vw;"
      ),
      icon = icon("balance-scale"),
      color = "blue",
      width = 12
    )
  })

  output$restored_budget <- shinydashboard::renderValueBox({
    sp <- if (is.null(input$baseline_species)) character(0) else input$baseline_species
    ids <- paste0("base_", gsub("[^A-Za-z0-9]", "_", sp))
    base_vals <- sapply(ids, function(id) {
      v <- input[[id]]
      if (is.null(v)) 0 else as.numeric(v)
    })
    base_rates <- as.numeric(travis_rates$rate[match(sp, travis_rates$Species)])
    net_base <- sum(base_vals * base_rates / 100, na.rm = TRUE)

    restoration_species <- c(
      "Acropora palmata", "Acropora cervicornis",
      "Montastraea cavernosa", "Orbicella faveolata",
      "Colpophyllia natans", "Porites astreoides",
      "Siderastrea siderea", "Stephanocoenia intersepta",
      "Diploria labyrinthiformis", "Solenastrea bournoni",
      "Pseudodiploria spp."
    )
    slider_ids <- paste0("rest_slider_", gsub("[^A-Za-z0-9]", "_", restoration_species))
    rest_vals <- sapply(slider_ids, function(id) {
      v <- input[[id]]
      if (is.null(v)) 0 else as.numeric(v)
    })
    rest_rates <- as.numeric(travis_rates$rate[match(restoration_species, travis_rates$Species)])
    net_rest <- sum(rest_vals * rest_rates / 100, na.rm = TRUE)

    row <- bioerosion[bioerosion$HABITAT_TYPE == input$habitat_choice, c("AVG_PARROTFISH", "AVG_URCHIN", "AVG_MICROBIOEROSION"), drop = FALSE]
    total_erosion <- if (nrow(row)) sum(as.numeric(row[1, ]), na.rm = TRUE) else 0

    restored_budget <- (net_base + net_rest) - total_erosion

    shinydashboard::valueBox(
      value = tags$p(paste0(round(restored_budget, 2), " kg/m\u00b2/yr"),
        style = "font-size: 2vw;"
      ),
      subtitle = tags$p("Restored carbonate budget",
        style = "font-size: 1vw;"
      ),
      icon = icon("balance-scale"),
      color = "olive",
      width = 12
    )
  })

  ## ---------------------------------------------------------------------------
  ## Coral Cover & Bioerosion tab (newly implemented) ----
  ## ---------------------------------------------------------------------------

  # Helper: baseline metrics for a given NCRMP site row
  cc_baseline_vals <- reactive({
    req(input$cc_selected_site)
    dat <- df |> filter(site_id == input$cc_selected_site) |> slice(1)
    list(
      cover  = dat$hardCoral_PrctCvr,
      budget = dat$net_G,
      rap    = dat$rap
    )
  })

  # Helper: restored metrics. If no restoration has been set on the
  # Restoration Planning tab, restored == baseline. Restored RAP/budget
  # are pulled from the reactive rap_values / a simple uplift assumption.
  cc_restored_vals <- reactive({
    base <- cc_baseline_vals()
    # Use the interactive restored RAP if available, else fall back to baseline
    restored_rap <- if (!is.null(rap_values$restored)) rap_values$restored else base$rap
    restored_budget <- if (!is.null(rap_values$restored)) {
      restored_rap * 2.9 * (1 - 0.6265)
    } else {
      base$budget
    }
    # Restored cover: baseline plus any target sliders on the planning tab
    site_data <- mote_cover |>
      filter(Site == input$selected_site, Class == "HC", !is.na(months12)) |>
      select(Species)
    ordered_species <- c(setdiff(site_data$Species, restoration_species_global), restoration_species_global)
    slider_ids <- paste0("site_slider_", gsub(" ", "_", ordered_species))
    covers <- sapply(slider_ids, function(id) {
      val <- input[[id]]
      if (is.null(val)) 0 else as.numeric(val)
    })
    restored_cover <- sum(covers, na.rm = TRUE)
    if (restored_cover == 0) restored_cover <- base$cover
    list(
      cover  = restored_cover,
      budget = restored_budget,
      rap    = restored_rap
    )
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
      "<p><b>Site:</b> ", input$cc_selected_site, "</p>",
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

  # Timeline: RAP over 10 years with SLR reference lines
  output$cc_timeline <- renderPlot({
    b <- cc_baseline_vals()
    r <- cc_restored_vals()
    years <- 0:10
    tl <- data.frame(
      Year = rep(years, 2),
      RAP  = c(rep(b$rap, length(years)), rep(r$rap, length(years))),
      Scenario = rep(c("Baseline", "Restored"), each = length(years))
    )
    ggplot(tl, aes(Year, RAP, color = Scenario)) +
      geom_line(linewidth = 1.2) +
      geom_hline(yintercept = 4, linetype = "dashed", color = "deepskyblue3") +
      geom_hline(yintercept = 40, linetype = "dashed", color = "#0b3d91") +
      geom_hline(yintercept = b$rap * 0 + max(df$rap, na.rm = TRUE),
        linetype = "dotted", color = "grey40"
      ) +
      annotate("text", x = 0.5, y = 4 + 1, label = "Current SLR (4 mm/yr)",
        color = "deepskyblue3", hjust = 0, size = 4
      ) +
      annotate("text", x = 0.5, y = 40 + 1, label = "Future SLR (40 mm/yr)",
        color = "#0b3d91", hjust = 0, size = 4
      ) +
      scale_color_manual(values = c("Baseline" = "lightgreen", "Restored" = "forestgreen")) +
      labs(x = "Year", y = "Reef accretion potential (mm/yr)", color = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top")
  })

  # Download report for the Coral Cover & Bioerosion tab
  output$cc_download_report <- downloadHandler(
    filename = function() {
      paste0("carbonate_report_", input$cc_selected_site, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      b <- cc_baseline_vals()
      r <- cc_restored_vals()
      out <- data.frame(
        Site = input$cc_selected_site,
        Metric = c("Coral cover (%)", "Carbonate budget (kg/m2/yr)", "Reef accretion (mm/yr)"),
        Baseline = c(b$cover, b$budget, b$rap),
        Restored = c(r$cover, r$budget, r$rap)
      )
      out$Change <- out$Restored - out$Baseline
      write.csv(out, file, row.names = FALSE)
    }
  )

  ## ---------------------------------------------------------------------------
  ## Save scenario (from Restoration Planning tab) ----
  ## ---------------------------------------------------------------------------
  observeEvent(input$save_scenario, {
    req(input$selected_site)
    validate(need(nzchar(input$scenario_project), "Enter a project name."))
    validate(need(nzchar(input$scenario_name), "Enter a scenario name."))

    site_data <- mote_cover |>
      filter(Site == input$selected_site, Class == "HC", !is.na(months12)) |>
      select(Species)
    ordered_species <- c(setdiff(site_data$Species, restoration_species_global), restoration_species_global)
    slider_ids <- paste0("site_slider_", gsub(" ", "_", ordered_species))
    covers <- sapply(slider_ids, function(id) {
      val <- input[[id]]
      if (is.null(val)) 0 else as.numeric(val)
    })

    total_cover <- sum(covers, na.rm = TRUE)
    restored_rap <- if (!is.null(rap_values$restored)) rap_values$restored else NA
    baseline_rap <- if (!is.null(rap_values$baseline)) rap_values$baseline else NA

    # Simple illustrative cost / ROI model driven by added cover
    added_cover <- total_cover
    cost <- added_cover * 1000 # $ per % cover (placeholder unit cost)
    elev_gain_10yr <- restored_rap * 10 # mm over 10 years
    roi <- if (cost > 0) (elev_gain_10yr / cost) * 1000 else 0

    scenario <- list(
      project = input$scenario_project,
      scenario = input$scenario_name,
      site = input$selected_site,
      total_cover = total_cover,
      baseline_rap = baseline_rap,
      restored_rap = restored_rap,
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
  ## Scenario Comparison tab (newly implemented) ----
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
      geom_text(aes(label = scenario), show.legend = FALSE) +
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