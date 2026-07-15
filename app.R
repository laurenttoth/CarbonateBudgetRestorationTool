# Carbonate Budget Restoration Tool ----

# Adapted by Connor M. Jenkins at the U.S. Geological Survey St. Petersburg Coastal and Marine Science Center
# from Alice Webb's Reef Persistence Tool. Adaptation conceptualized and guided by Lauren T. Toth (USGS) and John T. Morris (NOAA).

# Call Packages ----
library(rsconnect)
library(shiny)
library(bslib)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(leaflet)
library(shinythemes)
library(leaflegend)
library(ggplot2)
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

# Enable automatic reloading of the app when code changes are detected
options(shiny.autoreload = TRUE)

# year vector
cv_dates <- as.data.frame(c(2019:2100))
colnames(cv_dates) <- c("year")

# # call data for world map
# world_data <- ggplot2::map_data("world")
# worldcountry <- fortify(world_data)

# # import Cheeca peojections and simulated data
# data <- read.csv(here("data", "shiny_all_fake_projections.csv"))

# # import relative contribution data
# rel <- read.csv(here("data", "relative.csv"))

# # import coral cover data
# coral_cover_s <- read.csv(here("data", "coralcoverS.csv"))

# # import SLR data for all sites
# reef_data <- read.csv(here("data", "SLR_all_sites.csv"))

# # import SLR data for Cheeca and the image output
# slr_2019_2100 <- read.csv(here("data", "SLR_2019_2100.csv"))

# # High resolution from cm to mm
# slr_2019_2100$hr_cesmmm <- slr_2019_2100$HR_CESM * 10

# # Mote sites
# triangle_sites <- read.csv(here("data", "Mote_sites.csv"))
mote_cover <- read.csv(here("data", "Mote_cover.csv"))
travis_rates <- read.csv(here("data", "Travis_rates.csv"))
bioerosion <- read.csv(here("data", "Bioerosion.csv"))

# # merge all data frame
# data <- merge(data, coral_cover_s, by = c("Time", "variable", "Scenario"))
# data <- merge(data, slr_2019_2100, by = c("Time"))
# data <- merge(data, rel, by = c("Time", "variable", "Scenario"))

# # name columns
# scenario <- data$Scenario
# adaptation <- data$variable
# num <- data$Time
# ncc <- data$ncc # net calcium carbonate
# sdncc <- data$sdncc
# rap <- data$RAP # reef accretion potential
# sdrap <- data$sdRAP
# ah <- data$AH
# sdah <- data$sdAH
# site <- data$Site
# lat <- data$lat
# long <- data$long
# coralcover <- data$CoralCover
# slr <- data$SLR_HR
# accslr <- data$hr_cesmmm
# ah10 <- data$AH + 15
# coral <- data$perHC
# macro <- data$perBBS
# micro <- data$permicro
# cca <- data$perSCP

# # recreate data_frame
# df <- data.frame(
#   num, ncc, sdncc, rap, sdrap, ah, sdah, coralcover, adaptation, scenario,
#   site, lat, long, slr, accslr, ah10, coral, macro, micro, cca
# )


# Re-writing data imports:
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

# Shiny User Interface ----
ui <- bootstrapPage(
  title = "Carbonate Budget Restoration Tool",
  useShinyjs(),
  # use this in non shinydashboard app
  setBackgroundColor(color = "#f8fffb"),
  useShinyjs(),

  # Tag Setup ----
  tags$head(includeHTML(here("gtag.html"))),
  tags$style(HTML("
      /* Style for the specific red demonstration text */
      .navbar-custom h3 {
        float: right;
        margin: 5px 10px; /* Adjust margins as needed */
        color: white;
        font-size: 1.5vw; /* Responsive text size */
        line-height: 45px; /* Aligns with the logo height */
      }
      .navbar-custom img {
        height: 45px; /* Fixed height for the logo */
        vertical-align: middle;
        margin-left: 10px;
      }
      .navbar-custom a {
        text-decoration: none;
        color: #FFFFFF;
      }
      .custom-absolute-panel {
      z-index: 9999;  /* High z-index to prevent overlap */
    }
    ")),

  navbarPage(
    theme = shinytheme("flatly"), collapsible = TRUE, id = "nav",

    tags$div(
      class = "navbar-custom",
      HTML('<h3 style="
                float: right;
                margin: 5px auto;
                height: 45px;
                position: absolute;
                right: 200px;
                top: 5px;
                color: white;
                text-shadow:
                  -1px -1px 0 black,
                  1px -1px 0 black,
                  -1px 1px 0 black,
                  1px 1px 0 black;">
              Displaying 2014-2024 NCRMP data</h3> 
            <img src="noaaLogo.png" style="
              float: right;
              margin: 0px auto;
              height: 45px;
              position: absolute;
              right: 10px;
              top: 7px;"> 
            <img src="usgsLogo.png" style="
              float: right;
              margin: 0px auto;
              height: 45px;
              position: absolute;
              right: 70px;
              top: 7px;"> 
            <a style="
              text-decoration: none;
              cursor: default;
              color: #FFFFFF;
              text-shadow:
                -1px -1px 0 black,
                1px -1px 0 black,
                -1px 1px 0 black,
                1px 1px 0 black;"
            class="active" 
            href="#">Carbonate Budget Restoration Tool</ ></a>'),
      id = "nav",
    ),

    # Home Tab ----
    tabPanel(
      "Home",
      div(
        class = "outer",
        tags$head(includeCSS(here("styles.css"))),
        leafletOutput("mymap", width = "100%", height = "100%"),
        tags$h4("In construction"),


        # absolutePanel(
        #   id = "controls", class = "panel panel-default",
        #   top = 50, right = 300, width = 365, fixed = FALSE,
        #   draggable = FALSE, height = "auto",

        #   span(tags$i(h2("LOOK INTO THE FUTURE")), style = "color:#045a8d"),
        #   sliderTextInput("plot_date",
        #     label = h4("Choose your year"),
        #     choices = seq(from = min(cv_dates$year), to = max(cv_dates$year), by = 1),
        #     selected = min(cv_dates$year),
        #     grid = FALSE,
        #     animate = animationOptions(interval = 2000, loop = FALSE)
        #   ),


        #   actionButton("more_info", "More info", icon = icon("info")),
        #   hidden(div(
        #     id = "more_info_text", class = "hidden-text", "Carbonate budget projections are used as a metric for reef persistence. A carbonate budget
        #                                                    represents the summation of all processes contributing to calcification and bioerosion on a reef.
        #                                                    When the budget is positive, building capacity outweighs loss due to biological, chemical and physical erosion,
        #                                                    the reef is growing. If negative, the reef is in a state of net loss, the reef is flattening. We project these rates
        #                                                    into the future using site-specific climatic projections and species-specific relationships between 
        #                                                    rates of calcification and erosion and ocean acidification and temperature.",
        #     style = "color:#045a8d"
        #   ))
        # ),


        # absolutePanel(
        #   id = "controls", class = "panel panel-default",
        #   top = 50, right = 10, width = 270, fixed = FALSE,
        #   draggable = TRUE, height = "auto",

        #   span(tags$i(h2("LETS'S GO GREEN")), style = "color:#045a8d"),


        #   sliderInput("home_adaptation",
        #     label = h5("Change bleaching tolerance (in \u00B0C)"),
        #     min = 0, max = 2, step = 0.25, value = 0
        #   ),


        #   radioButtons("home_scenario",
        #     label = h4("Choose emission scenario"),
        #     choices = list("Reduce gas emission" = "SSP2_4.5", "Business as usual" = "SSP5_8.5"),
        #     selected = "SSP5_8.5",
        #     inline = FALSE,
        #     width = "100%"
        #   ),
        #   actionButton("more_info2", "More info", icon = icon("info")),
        #   hidden(div(
        #     id = "more_info2_text", class = "hidden-text", "The two scenarios presented here refer to SSP5-8.5 
        #                                                     (Buisness as usual) which is the pathway that represents current 
        #                                                     rates of emissions and emissions growth. It is considered a
        #                                                     “worst case scenario” and it assumes there is no climate policy or 
        #                                                     that policy is not effective. SSP2-4.5 (reduced emissions) is a highly
        #                                                     ambitious but still possible scenario. It is considered a “middle of the road” 
        #                                                     pathway whith intermediate CO2 emissions peaking in 2040 and gradually declining towards 2100.",
        #     style = "color:#045a8d"
        #   ))
        # )
      )
    ),

    # Reef Characteristics Tab ----
    tabPanel(
      "Reef Characteristics",
      value = "reef",
      tags$style("
        #controls {
          background-color: white;
          opacity: 0.6;
        }
        #controls:hover{
          opacity: 1;
        }
               "),
      tags$style(HTML("
        .well {
          background-color: #141c44; /* Pink background color */
          color: white; /* Blue text color */
        }

      ")),

      # Sidebar layout: Reef Selection ----
      sidebarLayout(
        sidebarPanel(
          width = 3,
          pickerInput("selectReef",
            "Select Your Reef",
            label = tags$span(style = "font-size: 20px;
                                       color: #00CC99;
                                       font-weight: bold;",
                                    "Select Your Reef"),
            choices = sites),
          br(),
          tags$style(HTML("#chosenReef {
          font-size: 18px; /* Adjust the font size as needed */
              color: white;     /* Set text color to green */
        }
      ")),
          textOutput("chosenReef"),
          # imageOutput("photo", width = "100%", height = "100%"),
          sliderTextInput("plot_date2",
            selected = min(cv_dates$year),
            label = h4("Look into the future"),
            choices = seq(from = min(cv_dates$year), to = max(cv_dates$year), by = 1),
            grid = FALSE,
            animate = animationOptions(interval = 3000, loop = FALSE)
          ),
          # radioButtons("reef_scenario",
          #   label = h4("Let's go green"),
          #   choices = list("Reduce gas emissions" = "SSP2_4.5",
          #                  "Business as usual" = "SSP5_8.5"),
          #   selected = "SSP5_8.5",
          #   inline = FALSE,
          #   width = "100%"
          # )
        ),

        # Reef Characteristics: Output illustrations ----
        fluidRow(
          column(
            3,
            valueBoxOutput("stateBox", width = NULL),
            textOutput("SLRmetrics") |>
              tagAppendAttributes(style = "text-align:center;font-weight:bold"),
            imageOutput("myImageSLR"),
          ),
          column(
            3,
            box(
              width = NULL, align = "center", collapsible = FALSE, title = tagList(
                div("Construction vs. Erosion",
                    style = "font-size: 24px;
                             font-weight: bold;
                             margin-bottom: 10px;"),
                div("Percentage contribution of constructional forces (coral and calcifying algae) and erosional processes (micro- and macro-erosion)",
                    style = "font-size: 1vw;
                             color: gray;")
              ), background = "navy", solidHeader = FALSE,
              collapsed = FALSE, imageOutput("myImage", width = "22vw", height = "auto")
            ),
            box(
              width = NULL, title = tagList(
                div("Sea level rise projections", style = "font-size: 24px; font-weight: bold;")
              ), collapsible = FALSE, background = "navy",
              collapsed = FALSE,
              plotOutput("slr_curve", height = "200px", width = "100%")
            ),
          ),
          column(
            2,
            valueBoxOutput("coverBox", width = NULL),
            valueBoxOutput("carbonateBox", width = NULL),
            valueBoxOutput("rapBox", width = NULL),
            valueBoxOutput("slrBox", width = NULL)
          )
        )
      )
    ),

    # Restoration & Adaptation Tab ----
    tabPanel(
      "Restoration & Adaptation",
      value = "reef2",
      tags$style("
        #controls {
          background-color: white;
          opacity: 0.6;
        }
        #controls:hover{
          opacity: 1;
        }
               "),
      tags$style(HTML("
        .well {
          background-color: #141c44; /* Pink background color */
          color: white; /* Blue text color */
        }
      ")),
      sidebarLayout(
        sidebarPanel(
          width = 3,


          pickerInput("selectReef2",
            "Select Your Reef",
            label = tags$span(style = "font-size: 20px; color: #00CC99; font-weight: bold;", "Select Your Reef"),
            choices = sites),
          br(),
          tags$style(HTML("#chosenReef2 {
          font-size: 18px; /* Adjust the font size as needed */
        }
      ")),
          textOutput("chosenReef2"),
          # imageOutput("photo2", width = "100%", height = "100%"),
          sliderTextInput("plot_date3",
            selected = min(cv_dates$year),
            label = h4("Look into the future"),
            choices = seq(from = min(cv_dates$year), to = max(cv_dates$year), by = 1),
            grid = FALSE,
            animate = animationOptions(interval = 3000, loop = FALSE)
          ),
          # radioButtons("reef_scenario2",
          #   label = h4("Let's go green"),
          #   choices = list("Reduce gas emissions" = "SSP2_4.5", "Business as usual" = "SSP5_8.5"),
          #   selected = "SSP5_8.5",
          #   inline = FALSE,
          #   width = "100%"
          # )
        ), mainPanel(
          box(
            div("RESTORATION", style = "text-align: center;font-size: 24px; font-weight: bold;"),
            solidHeader = FALSE,
            div(h2("Plant corals over the next 20 years to increase percentage coral cover at your site", style = "text-align: center;font-size: 20px;font-weight: bold;color: #337ab7;")),
            width = 8, # Full width of the page (12 out of 12 columns)
            height = "auto",

            # Use fluidRow to arrange the three images and sliders in a row
            fluidRow(
              # First Image and Slider
              column(
                4,
                div(
                  style = "text-align: center;",
                  tags$h4("Branching")
                ),
                div(
                  style = "text-align: center;",
                  tags$img(src = "images/structure.png", height = "auto", width = "67%")
                ),
                div(
                  style = "display: flex; justify-content: flex-end;",
                  sliderTextInput(
                    inputId = "slider1",
                    label = NULL,
                    choices = c(0, 1, 3, 7, 10, 15, 20),
                    grid = TRUE,
                    selected = 0,
                    post = "%",
                    width = "80%"
                  )
                ),
                div(
                  style = "text-align: center;",
                  actionButton("Branching_info", "More info", icon = icon("info"), style = " color: white; border: none;"),
                  hidden(div(
                    id = "Branching_info_text", class = "hidden-text", "Branching and plating corals grow rapidly, forming large,
                    tree-like colonies that provide structural complexity, effectively shading out competitors for light. These 
                    corals are highly susceptible to breakage during storms and suffer high mortality rates following temperature
                    anomalies. This sensitivity means they can only be dominant in ideal environments.",
                    style = "color:#045a8d"
                  ))
                )
              ),

              # Second Image and Slider
              column(
                4,
                div(
                  style = "text-align: center;",
                  tags$h4("Builders")
                ),
                div(
                  style = "text-align:  center;",
                  tags$img(src = "images/builder.png", height = "auto", width = "70%")
                ),
                div(
                  style = "display: flex; justify-content: flex-end;",
                  sliderTextInput(
                    inputId = "slider2",
                    label = NULL,
                    choices = c(0, 1, 3, 7, 10, 15, 20),
                    grid = TRUE,
                    selected = 0,
                    post = "%",
                    width = "80%",
                  )
                ),
                div(
                  style = "text-align: center;",
                  actionButton("Builders_info", "More info", icon = icon("info"), style = "color: white; border: none;"),
                  hidden(div(
                    id = "Builders_info_text", class = "hidden-text", "Domed colonies with moderate growth rates that can reach large colony sizes.
                                This group includes corals such as Orbicella spp., Montastrea cavernosa and brain corals. These reef builders represent a generalist stress tolerant
                                           life-history strategy that can do well in habitats where competition is limited by low levels of stress.",
                    style = "color:#045a8d"
                  ))
                )
              ),

              # Third Image and Slider
              column(
                4,
                div(
                  style = "text-align: center;",
                  tags$h4("Weedy")
                ),
                div(
                  style = "text-align: center;",
                  tags$img(src = "images/weedy.png", height = "auto", width = "60%")
                ),
                div(
                  style = "display: flex; justify-content: flex-end;",
                  sliderTextInput(
                    inputId = "slider3",
                    label = NULL,
                    choices = c(0, 1, 3, 7, 10, 15, 20),
                    grid = TRUE,
                    selected = 0,
                    post = "%",
                    width = "80%"
                  )
                ),
                div(
                  style = "text-align: center;",
                  actionButton("Weedy_info", "More info", icon = icon("info"), style = "color: white; border: none;"),
                  hidden(div(
                    id = "Weedy_info_text", class = "hidden-text", "Small corals with brooding reproduction, fast growth rates, high population turnover
                            that can opportunistically colonise recently disturbed habitats. \u2018Weedy\u2019 species include Porites astreoides and Siderastrea spp.
                                       are more likely to be \u2018winners\u2019 and persist in unfavourable and disturbed environments.",
                    style = "color:#045a8d"
                  ))
                )
              )
            )
          ),
          box(
            div(
              style = "text-align: center;",
              div("ADAPTATION", style = "font-size: 24px; font-weight: bold;")
            ),
            div(
              style = "text-align: center;",
              tags$img(src = "images/adapted3.png", height = "auto", width = "20%"),
              tags$img(src = "images/sun.png", height = "auto", width = "45%")
            ),
            solidHeader = FALSE,
            width = 4, # Full width of the page (12 out of 12 columns)
            height = "auto",

            # Use fluidRow to arrange the three images and sliders in a row
            fluidRow(
              # First Image and Slider
              column(
                12,
                div(
                  style = "text-align: center;",
                  h2("Increase bleaching tolerance of all corals", style = "font-size: 20px; font-weight: bold; color: #337ab7;")
                ),
                sliderInput("home_adaptation2",
                  label = NULL, post = "\u00B0C",
                  min = 0, max = 2, step = 0.25, value = 0
                )
              )
            ),
            div(
              style = "text-align: center;",
              actionButton("Adaptation_info", "More info", icon = icon("info"), style = "color: white; border: none;"),
              hidden(div(
                id = "Adaptation_info_text", class = "hidden-text", "To explore the potential effects of coral thermal adaptation on reef
                              persistence, the bleaching threshold of all corals is increased up to 2\u00B0C by increments of 0.25\u00B0C.
                              By adding adaptation to your scenario, bleaching is delayed in time.   ",
                style = "color:#045a8d"
              ))
            )
          ),
          fluidRow(
            tabBox(
              id = "tabset1", width = 5,
              tabPanel("Vertical Growth", plotOutput("rapPlot", width = "auto", height = "300px")),
              tabPanel("with Sea Level Rise", plotOutput("wSLRPlot", width = "auto", height = "300px"))
            ),
            column(
              6,
              valueBoxOutput("restoredCoral", width = 5)
            ),
            column(
              6,
              valueBoxOutput("x_value_at_y0", width = 5)
            ),
            column(
              6,
              valueBoxOutput("withSLR", width = 5)
            )
          )
        )
      )
    ),
    # Planning Restoration Tab ----
    tabPanel(
      "Mote Sites Restoration",
      value = "Planning Restoration",
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
              id = "Restoration_info_text", class = "hidden-text", "The corals accompanied by a shaded slider bar come from your selected site but aren\u2019t candidates for restoration.
                                         If a species that can be restored has a percentage above zero, that species already occurs at the site, and additional planting is possible.",
              style = "color:white"
            ))
          )
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

              # Title like Baseline and Restored
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
    # Planning Restoration Tab 2 ----
    # --- UI ---
    tabPanel(
      "Planning Restoration",
      fluidRow(
        column(
          width = 3,
          shinydashboard::box(
            title = "Baseline cover",
            width = 12, status = "primary", solidHeader = TRUE,

            # NEW: site area and habitat
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
              choices = sort(unique(taxa)), # assumes available in global
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

    # Data Tab ----
    tabPanel(
      "Data",
      tags$h4("Data not availabe at this time"),
      numericInput("maxrows", "Rows to show", 25),
      verbatimTextOutput("rawtable"),
      downloadButton("downloadCsv", "Download as CSV"), tags$br(), tags$br(),
      "Projections for Cheeca Rocks can be found ", tags$a(
        href = "https://www.nature.com/articles/s41598-022-26930-4",
        "here."
      )
    ),

    # "About this Site" Tab ----
    tabPanel(
      "About this Site",
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



# Shiny Server ----
# Color palette for site circles on home page
at <- c(-8, -6, -4, -2, 0, 2, 4, 6, 8)
colors = c("darkred", "red", "orange", "yellow", "white", "#0099FF", "#0033FF", "darkblue", "#000066")
num_pal <- colorNumeric(colors, domain = at)
num_pal_rev <- colorNumeric(colors, domain = at, reverse = TRUE)
# num_pal <- colorBin("RdYlBu",  bins = at, domain = at)

# make data frame reactive
server <- function(input, output, session) {
  # define reactVal to store coordinates
  reef_name       <- reactiveVal()
  reef_year       <- reactiveVal(2019)
  # reef_adaptation <- reactiveVal(0)
  reef_scenario   <- reactiveVal("SSP5_8.5")
  initial_budget  <- reactiveVal(NULL)

  slider_ids <- reactive({
    req(input$selected_site)

    site_data <- mote_cover |>
      filter(Site == input$selected_site, Class == "HC", !is.na(months12)) |>
      pull(Species)

    # Make save IDs using make.names
    make.names(site_data)
  })


  # update the reef panel
  # output$chosenReef <- renderText({
  #   paste(
  #     input$selectReef, "reef looks like this:"
  #   )
  # })
  # output$chosenReef2 <- renderText({
  #   paste(
  #     input$selectReef2, "reef looks like this:"
  #   )
  # })

  # change inputSlider from 2nd tab when slider from 1st tab is changed

  # observeEvent(input$home_adaptation, {
  #   reef_adaptation(input$home_adaptation)
  #   updateSliderInput(session, "reef_adaptation", value = reef_adaptation())
  # })
  observeEvent(input$plot_date, {
    reef_year(input$plot_date)
    updateSliderTextInput(session, "plot_date2", selected = reef_year())
  })
  # observeEvent(input$home_scenario, {
  #   reef_scenario(input$home_scenario)
  #   updateRadioButtons(session, "reef_scenario", selected = reef_scenario())
  # })
  # Observe changes in the second slider and update the first slider
  observeEvent(input$plot_date2, {
    reef_year(input$plot_date2)

    updateSliderTextInput(session, "plot_date", selected = reef_year())
  })
  # Observe changes in the second set of radio buttons and update the first set
  # observeEvent(input$reef_scenario, {
  #   reef_scenario(input$reef_scenario) # Update the state with the second set's value
  #   updateRadioButtons(session, "home_scenario", selected = reef_scenario())
  # })
  # input slider from second tab changes Circle shematic
  # observeEvent(input$reef_adaptation, {
  #   reef_adaptation(input$reef_adaptation)
  #   #  updateSliderInput(session, "reef_adaptation", value=reef_adaptation())
  # })
  observeEvent(input$plot_date2, {
    reef_year(input$plot_date2)

    # updateSliderTextInput(session, "plot_date2", selected=reef_year())
  })
  # observeEvent(input$reef_scenario, {
  #   reef_scenario(input$reef_scenario)
  #   #  updateRadioButtons(session, "reef_scenario", selected=reef_scenario())
  # })


  filtered_df <- reactive({
    df |>
      # filter(num %in% reef_year()) |>
      # filter(scenario %in% reef_scenario()) |>
      # filter(adaptation %in% reef_adaptation())
      filter(site_id == input$selected_site)
  })


  # df$reef_depth <- ah10 - accslr

  # df$rd_scaled <- df$reef_depth / 100
  # df$rheight <- df$ah10 / 100

  # Initialize leaflet map ----
  output$mymap <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$Esri.WorldImagery,
        options = providerTileOptions(attribution = 'Map data &copy; <a href="https://www.esri.com/">Esri</a>')
      ) |>
      setView(lng = -80.6097, lat = 25, zoom = 8) |>
      # Completely static legend for Carbonate budget
      addLegendNumeric(
        pal = num_pal_rev,
        title = HTML("Reef<br/>accretion<br/>potential<br/>(mm/yr)"),
        shape = "stadium",
        values = at,
        fillOpacity = 10,
        decreasing = TRUE,
        position = "bottomleft"
      ) |>

      # Static elements like the title and reef status legend
      addLegend("bottomleft",
        colors  = c("#0099FF", "#FFFF99", "#FF6600"),
        labels  = c("Growing", "Stasis", "Eroding"),
        title   = HTML("<span style='font-size: 16px;'>Reef Status</span>"),
        opacity = 1
      ) |>

      # Static control: White text title - "CARBONATE BUDGET RESTORATION TOOL"
      # We don't need this. Title is in the ribbon bar.
      # addControl(
      #   html = "<div 
      #             style='
      #               font-size: 42px;
      #               font-weight: bold;
      #               color:white;
      #               text-shadow:
      #                 -1px -1px 0 black,
      #                 1px -1px 0 black,
      #                 -1px 1px 0 black,
      #                 1px 1px 0 black;'>
      #             CARBONATE BUDGET <br>RESTORATION TOOL</div>",
      #   position = "topleft",
      #   className = "map-title"
      # ) |>

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

  # Dynamic map update without touching the legend or static controls
  # observe({
  #   leafletProxy("mymap", data = filtered_df()) |>
  #     clearShapes() |> # Clear previous circles
  #     addCircles(
  #       lng = ~long, lat = ~lat, weight = 40,
  #       popup = ~ paste(
  #         "<a style='cursor: pointer' onclick='Shiny.onInputChange(\"linkClickReef\", Math.random())'>",
  #         "<span style='font-size: 20px;'>", site, "</span>",
  #         "</a>",
  #         "<br/><span style='font-size: 14px;'>Coral cover: ", round(coralcover, 1), "%</span>",
  #         "<br/><span style='font-size: 14px;'>Carbonate budget: ", round(ncc, 1), " kg/m", tags$sup("2"), "/year</span>"
  #       ),
  #       radius = 1,
  #       color = ~ num_pal(ncc), opacity = 0.8
  #     )

  #   # No need to clear or re-add the static legend
  # })

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
        popup       = ~ paste0( # Changed from lines of text to a table
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
    # create object for clicked polygon
    click <- input$mymap_marker_click

    # find the matching reef name by the clicked coordinates
    reef_name(df |>
                filter(LAT_DEGREES == click$lat & LON_DEGREES == click$lng) |>
                pull(site_id) |>
                unique())

    print(paste("Selected reef:", reef_name()))

    # update the input w/ the selected reef
    updateSelectInput(session,
      "selectReef",
      selected = reef_name()
    )
  })


  # capture the selected reef name for the restoration tab
  observeEvent(input$mymap_marker_click, {
    # create object for clicked polygon
    click <- input$mymap_marker_click

    # find the matching reef name by the clicked coordinates
    reef_name(df |>
                filter(LAT_DEGREES == click$lat & LON_DEGREES == click$lng) |>
                pull(site_id) |>
                unique())


    # update the input w/ the selected reef
    updateSelectInput(session,
      "selectReef2",
      selected = reef_name()
    )
  })

  # text appears
  observeEvent(input$more_info, {
    toggle("more_info_text")
  })

  observeEvent(input$more_info2, {
    toggle("more_info2_text")
  })

  # change the tab when the hyperlink is clicked
  observeEvent(input$linkClickReef, {
    updateTabsetPanel(session, inputId = "nav", selected = "reef")
  })

  observeEvent(input$linkClickPlanning, {
    updateTabsetPanel(session, inputId = "nav", selected = "Planning Restoration")
  })
  ## Sliders ----

  output$coral_slider_ui <- renderUI({
    req(input$selected_site)

    site_data <- mote_cover |>
      filter(Site == input$selected_site, Class == "HC", !is.na(months12)) |>
      select(Species, Cover = months12)

    restoration_species <- c(
      "Acropora palmata", "Acropora cervicornis", "Montastraea cavernosa",
      "Orbicella faveolata", "Colpophyllia natans", "Porites astreoides",
      "Siderastrea siderea", "Stephanocoenia intersepta", "Diploria labyrinthiformis", "Solenastrea bournoni"
    )

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

  ## Planning Restoration ----
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
      # value = paste0(round(total_budget, 2), " kg/m\u00b2/yr"),
      # subtitle = "Baseline Carbonate Budget",
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
      select(Species, Cover = months12, Location) # keep Location for the join

    result <- coral_data |>
      left_join(travis_rates, by = "Species") |>
      left_join(bioerosion, by = "Location") |> # adds PF, Urchins, BioSponges, Micro
      mutate(Contribution = Cover * rate / 100)

    # Total production across species
    net_budget <- sum(result$Contribution, na.rm = TRUE)

    # Pull the single row of erosion values for this site/location
    bio_vals <- result |>
      dplyr::distinct(HABITAT_TYPE, AVG_PARROTFISH, AVG_URCHIN, AVG_MICROBIOEROSION) |>
      dplyr::slice(1) |>
      tidyr::replace_na(list(PF = 0, Urchins = 0, BioSponges = 0, Micro = 0))

    pf <- bio_vals$PF
    urchins <- bio_vals$Urchins
    bio_sponges <- bio_vals$BioSponges
    # Micro available as bio_vals$Micro if you need it

    # RAP formula (mm/yr)
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

    restoration_species <- c(
      "Acropora palmata", "Acropora cervicornis", "Montastraea cavernosa",
      "Orbicella faveolata", "Colpophyllia natans", "Porites astreoides",
      "Siderastrea siderea", "Stephanocoenia intersepta", "Diploria labyrinthiformis", "Solenastrea bournoni"
    )

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

    ## --- minimal additions start ---
    # Get the site's Location once
    site_loc <- mote_cover |>
      filter(Site == input$selected_site) |>
      distinct(Location) |>
      slice(1)

    # Look up erosion components and sum them (defaults to 0 if missing)
    bio_vals <- left_join(site_loc, bioerosion, by = "Location")
    pf <- ifelse(is.na(bio_vals$PF), 0, bio_vals$PF)
    urchins <- ifelse(is.na(bio_vals$Urchins), 0, bio_vals$Urchins)
    bio_sponges <- ifelse(is.na(bio_vals$BioSponges), 0, bio_vals$BioSponges)
    micro <- ifelse(is.na(bio_vals$Micro), 0, bio_vals$Micro)

    erosion_total <- pf + urchins + bio_sponges + micro
    ## --- minimal additions end ---

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

    restoration_species <- c(
      "Acropora palmata", "Acropora cervicornis", "Montastraea cavernosa",
      "Orbicella faveolata", "Colpophyllia natans", "Porites astreoides",
      "Siderastrea siderea", "Stephanocoenia intersepta", "Diploria labyrinthiformis", "Solenastrea bournoni"
    )

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
      Location = site_loc$Location[1] # ADDED
    )

    result <- coral_data |>
      left_join(travis_rates, by = "Species") |>
      left_join(bioerosion, by = "Location") |>
      mutate(Contribution = as.numeric(Cover) * as.numeric(rate) / 100) # tiny numeric guard

    net_budget <- sum(result$Contribution, na.rm = TRUE)

    # ADDED: pull PF/Urchins/BioSponges once from the joined result (coerce numeric)
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
      color = "green" # Always teal
    )
  })

  ## Restored Coral Cover ----
  output$total_coral_added <- renderValueBox({
    req(input$selected_site)

    site_data <- mote_cover |>
      filter(Site == input$selected_site, Class == "HC", !is.na(months12)) |>
      select(Species)

    restoration_species <- c(
      "Acropora palmata", "Acropora cervicornis", "Montastraea cavernosa",
      "Orbicella faveolata", "Colpophyllia natans", "Porites astreoides",
      "Siderastrea siderea", "Stephanocoenia intersepta", "Diploria labyrinthiformis", "Solenastrea bournoni"
    )

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

    # Net calcification calculation
    coral_data <- mote_cover |>
      filter(Site == input$selected_site, Class == "HC", !is.na(months12)) |>
      select(Species, Cover = months12) |>
      left_join(travis_rates, by = "Species") |>
      mutate(Contribution = Cover * rate / 100)

    net_calcification <- sum(coral_data$Contribution, na.rm = TRUE)

    # Get site location
    site_location <- mote_cover |>
      filter(Site == input$selected_site) |>
      pull(Location) |>
      unique()

    # Bioerosion data
    bio_data <- bioerosion |>
      filter(Location == site_location)

    pf <- bio_data$PF[1]
    urchins <- bio_data$Urchins[1]
    biosponges <- bio_data$BioSponges[1]
    micro <- bio_data$Micro[1]

    # Pie data
    mpiedat <- data.frame(
      variable = c("Net Calcification", "Parrotfish", "Urchins", "BioSponges", "Microborers"),
      value = c(net_calcification, pf, urchins, biosponges, micro)
    )

    # Define matching custom colors
    pie_colors <- c( # 663399,#184b52
      "Net Calcification" = "dodgerblue4", # Coral
      "Parrotfish" = "#663399", # CCA
      "Urchins" = "#5E819D", # Macro
      "BioSponges" = "#CC0099", # Micro
      "Microborers" = "#184b52" # Optional gray #CC0099
    )

    # Plot
    ggplot(mpiedat, aes(x = "", y = value, fill = variable)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = pie_colors) +
      #   labs(fill = "", title = "Baseline Construction vs. Erosion") +  # Title here) +
      theme_void() +
      theme(
        # plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "dodgerblue3"),
        legend.position = "none" # <-- Hide legend here
      ) +
      guides(fill = guide_legend(title.position = "none", title.hjust = 0, nrow = 2))
  })

  ## Restored Pie ----
  output$restored_pie <- renderPlot({
    req(input$selected_site)

    ## SLR Circle ----
    # in server.R / server function
    # SERVER
    output$slr_circle <- renderPlot({
      library(ggplot2)

      # params
      present_mm <- 4
      future_mm <- 40
      scale_max <- 100 # mm/yr at circle edge
      r_circle <- 3
      fill_cut_mm <- 5
      blue_fill <- "deepskyblue3"

      mm_to_y <- function(mm) (mm / scale_max) * r_circle
      max_y <- r_circle - 0.05
      y_restored_rap <- min(mm_to_y(rap_values$restored), max_y)
      max_y_baseline <- r_circle - 0.05
      y_baseline_rap <- min(mm_to_y(rap_values$baseline), max_y_baseline)

      # circle path 
      angle <- seq(0, 2 * pi, length.out = 720)
      circle_df <- data.frame(
        x = r_circle * cos(angle),
        y = r_circle * sin(angle)
      )

      # cutoff height 
      y_cut <- mm_to_y(fill_cut_mm)

      # lower (blue) fill
      lower_edge <- subset(circle_df, y <= y_cut)
      lower_fill <- rbind(
        lower_edge,
        data.frame(x = rev(lower_edge$x), y = rep(y_cut, nrow(lower_edge)))
      )

      # upper (white) fill
      upper_edge <- subset(circle_df, y >= y_cut)
      upper_fill <- rbind(
        upper_edge,
        data.frame(x = rev(upper_edge$x), y = rep(y_cut, nrow(upper_edge)))
      )

      ## solid line positions and truncation 
      draw_horizontal <- function(y_val, col) {
        if (abs(y_val) <= r_circle) {
          x_half <- sqrt(r_circle^2 - y_val^2)
          geom_segment(aes(x = -x_half, xend = x_half, y = y_val, yend = y_val),
            color = col, linewidth = 1
          )
        } else {
          NULL # skip if outside circle
        }
      }

      y_present <- mm_to_y(present_mm)
      y_future <- mm_to_y(future_mm)


      # Wavy line for Current SLR (present_mm)
      wave_x <- seq(-r_circle, r_circle, length.out = 1000)

      wave_y_present_center <- mm_to_y(present_mm)
      wave_y_present <- wave_y_present_center + 0.04 * sin(10 * wave_x) # tweak amp/freq if needed
      inside_present <- wave_x^2 + wave_y_present^2 <= r_circle^2
      wave_present_df <- data.frame(x = wave_x[inside_present], y = wave_y_present[inside_present])

      # Wavy line for Future SLR (future_mm)
      wave_y_future_center <- mm_to_y(future_mm)
      wave_y_future <- wave_y_future_center + 0.05 * sin(10 * wave_x)
      inside_future <- wave_x^2 + wave_y_future^2 <= r_circle^2
      wave_future_df <- data.frame(x = wave_x[inside_future], y = wave_y_future[inside_future])


      ggplot() +
        # fill halves
        geom_polygon(data = lower_fill, aes(x, y), fill = blue_fill, color = NA) +
        geom_polygon(data = upper_fill, aes(x, y), fill = "aliceblue", color = NA) +
        # truncated lines
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
          x = r_circle, # a bit inside right edge
          y = y_baseline_rap - 0.2, # slightly below the line
          label = "Baseline RAP",
          color = "lightgreen",
          size = 4.5,
          hjust = 3.2
        ) +

        # Restored RAP label (dark green line)
        annotate("text",
          x = r_circle,
          y = y_restored_rap + 0.2,
          label = "Restored RAP",
          color = "forestgreen",
          size = 4.5,
          hjust = 3.1
        ) +

        # circle outline in blue
        #   geom_path(data = circle_df, aes(x, y), linewidth = 1.2, color = blue_fill) +
        coord_fixed() +
        theme_void()
    })


    # Get species list from site and restoration group
    site_data <- mote_cover |>
      filter(Site == input$selected_site, Class == "HC", !is.na(months12)) |>
      select(Species)

    restoration_species <- c(
      "Acropora palmata", "Acropora cervicornis", "Montastraea cavernosa",
      "Orbicella faveolata", "Colpophyllia natans", "Porites astreoides",
      "Siderastrea siderea", "Stephanocoenia intersepta", "Diploria labyrinthiformis", "Solenastrea bournoni"
    )

    all_species <- union(site_data$Species, restoration_species)
    ordered_species <- c(setdiff(site_data$Species, restoration_species), restoration_species)

    # Get slider values
    slider_ids <- paste0("site_slider_", gsub(" ", "_", ordered_species))
    values <- sapply(slider_ids, function(id) {
      val <- input[[id]]
      if (is.null(val)) {
        return(0)
      }
      as.numeric(val)
    })

    cover_df <- data.frame(Species = ordered_species, Cover = values)

    # Calculate net calcification
    calc_df <- cover_df |>
      left_join(travis_rates, by = "Species") |>
      mutate(Contribution = Cover * rate / 100)

    net_calcification <- sum(calc_df$Contribution, na.rm = TRUE)

    # Get site location
    site_location <- mote_cover |>
      filter(Site == input$selected_site) |>
      pull(Location) |>
      unique()

    # Bioerosion data (same for restored)
    bio_data <- bioerosion |>
      filter(Location == site_location)

    pf <- bio_data$PF[1]
    urchins <- bio_data$Urchins[1]
    biosponges <- bio_data$BioSponges[1]
    micro <- bio_data$Micro[1]

    # Build pie chart data
    mpiedat <- data.frame(
      variable = c("Net Calcification", "Parrotfish", "Urchins", "BioSponges", "Microborers"),
      value = c(net_calcification, pf, urchins, biosponges, micro)
    )

    # Custom colors
    pie_colors <- c(
      "Net Calcification" = "dodgerblue4",
      "Parrotfish" = "#663399",
      "Urchins" = "#5E819D",
      "BioSponges" = "#CC0099",
      "Microborers" = "#184b52"
    )

    # Plot
    ggplot(mpiedat, aes(x = "", y = value, fill = variable)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = pie_colors) +
      #  labs(fill = "", title = "Restored Construction vs. Erosion") +
      theme_void() +
      theme(
        #   plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "dodgerblue3"),
        legend.position = "none" # <-- Hide legend here
      ) +
      guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 2))
  })


  ## Box1 ----

  # Render the valueBox with dynamic color
  output$stateBox <- renderValueBox({
    # Get the selected reef from the pickerInput
    selected_reef <- input$selectReef

    # Determine which reef to use: either the selected reef or the reef_name() from tab one
    # reef_to_use <- if (!is.null(selected_reef) && selected_reef != "") {
    #   selected_reef
    # } else {
    #   reef_name()
    # }
    reef_to_use <- reef_name()

    # Filter the data for the selected reef
    dat <- df |> filter(site_id == reef_to_use)

    # Determine reef state
    reef_state <- if (dat$net_G > 0.9) {
      "GROWING"
    } else if (dat$net_G < -0.1) {
      "ERODING"
    } else {
      "IN STASIS"
    }

    # Determine color based on reef state
    box_color <- switch(reef_state,
      "GROWING" = "blue",
      "ERODING" = "red",
      "IN STASIS" = "yellow",
      "blue" # Default color if none match
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

  ## Aliiiiiiiiiiiiiiiiiiiiiiiiiiiiiice ----
  # observeEvent(input$bt2, {
  #   updateBox("box2", action = "toggle")
  # })




  output$coverBox <- renderValueBox({
    dat <- df |> filter(site_id == reef_name())
    valueBox(
      value = tags$p(paste0(round(dat$hardCoral_PrctCvr), "%"), style = "font-size: 2vw;"),
      "Coral cover",
      icon = icon("chart-pie"),
      color = "purple", # blue
    )
  })


  output$carbonateBox <- renderValueBox({
    dat <- df |> filter(site_id == reef_name())
    valueBox(
      value = tags$p(HTML(paste0(round(dat$net_G, 1), "kg/m", tags$sup("2"), "/year")), style = "font-size: 2vw;"),
      "Carbonate budget",
      icon = icon("scale-balanced"),
      color = "blue",
    )
  })

  output$rapBox <- renderValueBox({
    dat <- df |> filter(site_id == reef_name())
    valueBox(
      value = tags$p(paste0(round(dat$rap, 1), "mm/year"), style = "font-size: 2vw;"),
      "Vertical reef growth",
      icon = icon("layer-group"),
      color = "blue",
    )
  })

  # output$slrBox <- renderValueBox({
  #   dat <- df |> filter(site_id == reef_name())
  #   valueBox(
  #     value = tags$p(paste0(round(dat$SLR, 1), "mm/year"), style = "font-size: 2vw;"),
  #     "Sea level rise",
  #     icon = icon("house-flood-water"),
  #     color = "teal",
  #   )
  # })




  # update the reef panel
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
  # output$seaL <- renderText({
  #   dat <- df |> filter(site_id == reef_name())

  #   paste(
  #     "Sea level is increasing by",
  #     round(dat$SLR, 1), "mm/year."
  #   )
  # })


  # filtered_slr <- reactive({
  #   reef_data |> filter(site_id == input$selectReef)
  # })
  # output$slr_curve <- renderPlot({
  #   ggplot(filtered_slr(), aes(x = Time, y = cumsum(HR / 10))) + # put a cumsum wrapper around y
  #     geom_line(colour = "#00d4d4") +
  #     geom_point(aes(size = ifelse(Time == input$plot_date2, 6, 3)),
  #       alpha = 0.8, colour = "#00d4d4"
  #     ) +
  #     ylab("Sea level rise (mm)") +
  #     xlab("Year") +
  #     theme_bw() +
  #     theme(
  #       legend.title = element_blank(), legend.position = "", plot.title = element_text(size = 10),
  #       plot.margin = margin(5, 12, 5, 5),
  #       axis.text = element_text(size = 12, color = "white"), # Set axis text color to white
  #       axis.title = element_text(size = 14, color = "white"),
  #       axis.line = element_line(color = "white", linewidth = 1),
  #       panel.grid = element_blank(), # Remove major grid lines
  #       panel.border = element_blank(),
  #       panel.background = element_rect(fill = "#141c44", linewidth = 0), # Set panel background to transparent
  #       plot.background = element_rect(fill = "#141c44", linewidth = 0)
  #     )
  # })

  # ggplot(filtered_slr(), aes(x = Time, y = cumsum(HR/10)))

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



  # output$myImageSLR <- renderImage(
  #   {
  #     path <- here("www", "images", "silhouette2.png")
  #     img <- readPNG(path, native = TRUE)

  #     # generate plot
  #     dat <- df |> filter(site_id == reef_name())

  #     if (dat$AVG_DEPTH < 0) {
  #       reef <- tibble(
  #         deg = 0:360,
  #         r = 3,
  #         x = r * cos((deg * pi) / 180),
  #         y = r * sin((deg * pi) / 180)
  #       ) |>
  #         filter(y <= dat$rd_scaled)

  #       reef1 <- tibble(
  #         deg = 0:360,
  #         r = 3,
  #         x1 = r * cos((deg * pi) / 180),
  #         y1 = r * sin((deg * pi) / 180)
  #       ) |>
  #         filter(y1 >= (((dat$SLR - 0.4576675) / (3.284522 - 0.4576675)) + 1.5))



  #       p1 <- ggplot() +
  #         geom_circle(aes(
  #           x0 = 0,
  #           y0 = 0,
  #           r = 3
  #         ),
  #         fill = "deepskyblue3", color = "white", # 5b8899#43accb
  #         # linewidth = 2,
  #         inherit.aes = FALSE
  #         ) +
  #         geom_ribbon(
  #           data = reef,
  #           aes(x, ymin = y, ymax = dat$rd_scaled),
  #           fill = "black", color = "black", linewidth = 0.5
  #         ) +
  #         geom_ribbon(
  #           data = reef1,
  #           aes(x1, ymin = y1, ymax = (((dat$SLR - 0.4576675) / (3.284522 - 0.4576675)) + 1.5 + 0.03 * sin(10 * x1))),
  #           fill = "aliceblue", color = "white", linewidth = 0.5
  #         ) +
  #         geom_segment(aes(x = -2.8, xend = 2.8, y = 0.95, yend = 0.95),
  #           linetype = "dotted", linewidth = 0.8, color = "#2c383e"
  #         ) +
  #         annotation_raster(img,
  #           xmin = -3.7, # Make it cover more area along x-axis
  #           xmax = 3.8, # Same for xmax
  #           ymin = dat$rd_scaled - 1.5, # Adjust ymin to make it larger vertically
  #           ymax = dat$rd_scaled + 2.5
  #         ) +
  #         theme_void() +
  #         # annotate("text", x = 0, y = 3.9,size=4, label = paste0("The natural wall of the reef is now \n",
  #         #                                                  abs(round(dat$reef_depth)),
  #         #                                                  " mm deeper compared to 2019.")) +
  #         annotate("text", x = 2.3, y = 0.95, label = "reef height\n in 2019", size = 2.7, colour = "1d2529") +
  #         geom_circle(aes(
  #           x0 = 0,
  #           y0 = 0,
  #           r = 3
  #         ),
  #         color = "white",
  #         #  linewidth = 2,
  #         inherit.aes = FALSE
  #         ) +
  #         coord_fixed()


  #       outfile1 <- tempfile(fileext = ".png")

  #       # Generate the PNG
  #       png(outfile1,
  #         width = 300 * 8,
  #         height = 300 * 8,
  #         res = 72 * 8
  #       )
  #       print(p1)
  #       dev.off()
  #       list(
  #         src = outfile1,
  #         contentType = "image/png",
  #         height = "auto",
  #         width = "100%"
  #       )
  #     } else {
  #       reef <- tibble(
  #         deg = 0:360,
  #         r = 3,
  #         x = r * cos((deg * pi) / 180),
  #         y = r * sin((deg * pi) / 180)
  #       ) |>
  #         filter(y >= dat$rd_scaled)

  #       reef1 <- tibble(
  #         deg = 0:360,
  #         r = 3,
  #         x1 = r * cos((deg * pi) / 180),
  #         y1 = r * sin((deg * pi) / 180)
  #       ) |>
  #         filter(y1 >= (((dat$SLR - 0.4576675) / (3.284522 - 0.4576675)) + 1.5))

  #       p2 <- ggplot() +
  #         geom_circle(aes(
  #           x0 = 0,
  #           y0 = 0,
  #           r = 3
  #         ),
  #         fill = "black", color = "white",
  #         #  linewidth = 2,
  #         inherit.aes = FALSE
  #         ) +
  #         geom_ribbon(
  #           data = reef,
  #           aes(x, ymin = dat$rd_scaled, ymax = y),
  #           fill = "deepskyblue3", color = "white", linewidth = 0.5
  #         ) + # 3ba3bf#43accb
  #         geom_ribbon(
  #           data = reef1,
  #           aes(x1, ymin = (((dat$SLR - 0.4576675) / (3.284522 - 0.4576675)) + 1.5 + 0.03 * sin(10 * x1)), ymax = y1),
  #           fill = "aliceblue", color = "white", linewidth = 0.5
  #         ) +
  #         geom_segment(aes(x = -2.8, xend = 2.8, y = 0.95, yend = 0.95),
  #           linetype = "dotted", linewidth = 0.8, color = "#2c383e"
  #         ) +
  #         geom_segment(
  #           data = reef, aes(x = -x, xend = x, y = dat$rd_scaled, yend = dat$rd_scaled),
  #           linewidth = 0.8, color = "black"
  #         ) +
  #         annotate("text", x = 2.3, y = 0.95, label = "reef height\n in 2019", size = 3, colour = "#1d2529") +
  #         annotation_raster(img,
  #           xmin = -3.7, # Make it cover more area along x-axis
  #           xmax = 3.8, # Same for xmax
  #           ymin = dat$rd_scaled - 1.5, # Adjust ymin to make it larger vertically
  #           ymax = dat$rd_scaled + 2.5
  #         ) +
  #         theme_void() +
  #         geom_circle(aes(
  #           x0 = 0,
  #           y0 = 0,
  #           r = 3
  #         ),
  #         color = "white",
  #         inherit.aes = FALSE
  #         ) +
  #         coord_fixed()
  #       outfile2 <- tempfile(fileext = ".png")

  #       # Generate the PNG
  #       png(outfile2,
  #         width = 300 * 8,
  #         height = 300 * 8,
  #         res = 72 * 8
  #       )
  #       print(p2)
  #       dev.off()
  #       list(
  #         src = outfile2,
  #         contentType = "image/png",
  #         height = "auto",
  #         width = "100%"
  #       )
  #     }
  #   },
  #   deleteFile = TRUE
  # )

  ## Box3 ----
  # Plot the data ----
  output$myImage <- renderImage(
    {
      # generate plot
      dat <- df |> filter(site_id == reef_name())
      piedat <- dat[c("hardCoral_G", "macrobioerosion_G", "microbioerosion_G", "cca_G")]
      mpiedat <- melt(piedat, id.vars = NULL)

      # new column 'category' to group the data into Constructors and Destroyers
      mpiedat$category <- ifelse(mpiedat$variable %in% c("hardCoral_G", "cca_G"), "Constructors", "Destroyers")

      # Reorder the levels of 'variable' in your data frame
      mpiedat$variable <- factor(mpiedat$variable, levels = c("hardCoral_G", "cca_G", "macrobioerosion_G", "microbioerosion_G"))

      plot <- ggplot(mpiedat, aes(x = "", y = value, fill = variable)) +
        geom_bar(stat = "identity", width = 1, color = "white", position = "stack") +
        coord_polar("y", start = 0) +
        scale_fill_manual(values = c("Coral" = "#663399", "CCA" = "#CC0099", "Macro" = "#7d9dbc", "Micro" = "#184b52")) +
        labs(fill = "") + # Set the legend title here
        theme_void() +
        theme(
          legend.position = "bottom", # Position legend at the bottom
          legend.title = element_text(color = "white", size = 12, face = "bold"), # Customize the legend title appearance
          legend.text = element_text(color = "white", size = 9),
          legend.box = "vertical" # Arrange legend items vertically
        ) +
        guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) # Center-align and position title on top

      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext = ".png")

      # Generate the PNG
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




  # Load the appropriate photo for the selected site
  # output$photo <- renderImage(
  #   {
  #     if (input$selectReef == "Cheeca Rocks") {
  #       filename <- normalizePath(here("www", "images", "cheeca.jpg"))
  #     }
  #     if (input$selectReef == "La Parguera") {
  #       filename <- normalizePath(here("www", "images", "parguera2.jpg"))
  #     }
  #     if (input$selectReef == "Flower Garden Banks") {
  #       filename <- normalizePath(here("www", "images", "flower.jpg"))
  #     }
  #     if (input$selectReef == "Saint Croix") {
  #       filename <- normalizePath(here("www", "images", "croix.jpg"))
  #     }
  #     if (input$selectReef == "Saint Thomas") {
  #       filename <- normalizePath(here("www", "images", "thomas.jpg"))
  #     }
  #     if (input$selectReef == "Dry Tortugas") {
  #       filename <- normalizePath(here("www", "images", "tortugas.jpg"))
  #     }
  #     list(
  #       src = filename,
  #       height = "auto",
  #       width = "100%"
  #     )
  #   },
  #   deleteFile = FALSE
  # )

  # output$photo2 <- renderImage(
  #   {
  #     if (input$selectReef2 == "Cheeca Rocks") {
  #       filename <- normalizePath(here("www", "images", "cheeca.jpg"))
  #     }
  #     if (input$selectReef2 == "La Parguera") {
  #       filename <- normalizePath(here("www", "images", "parguera2.jpg"))
  #     }
  #     if (input$selectReef2 == "Flower Garden Banks") {
  #       filename <- normalizePath(here("www", "images", "flower.jpg"))
  #     }
  #     if (input$selectReef2 == "Saint Croix") {
  #       filename <- normalizePath(here("www", "images", "croix.jpg"))
  #     }
  #     if (input$selectReef2 == "Saint Thomas") {
  #       filename <- normalizePath(here("www", "images", "thomas.jpg"))
  #     }
  #     if (input$selectReef2 == "Dry Tortugas") {
  #       filename <- normalizePath(here("www", "images", "tortugas.jpg"))
  #     }
  #     list(
  #       src = filename,
  #       height = "auto",
  #       width = "100%"
  #     )
  #   },
  #   deleteFile = FALSE
  # )

  # filtered_data <- reactive({
  #   dat |>
  #     filter(
  #       # Scenario == input$reef_scenario2,
  #       # variable == input$home_adaptation2,
  #       site_id == input$selectReef2
  #     )
  # })

  # filtered_slr2 <- reactive({
  #   reef_data |> filter(Site == input$selectReef2)
  # })


  # output$rapPlot <- renderPlot({
  #   x_value <- x_when_negative()
  #   p <- ggplot(filtered_data(), aes(x = Time, y = rap)) +
  #     geom_line(colour = "white") +
  #     geom_point(aes(size = ifelse(Time == input$plot_date3, 6, 3)),
  #       alpha = 0.8, colour = "white"
  #     ) +
  #     ylab("vertical reef growth (mm/year)") +
  #     xlab("Year") +
  #     theme_bw() +
  #     theme(
  #       legend.title = element_blank(), legend.position = "", plot.title = element_text(size = 10),
  #       plot.margin = margin(5, 12, 5, 5),
  #       axis.text = element_text(size = 12, color = "white"), # Set axis text color to white
  #       axis.title = element_text(size = 14, color = "white"),
  #       axis.line = element_line(color = "white"),
  #       panel.grid.major = element_blank(), # Remove major grid lines
  #       panel.grid.minor = element_blank(),
  #       panel.background = element_rect(fill = "#141c44"), # Set panel background to transparent
  #       plot.background = element_rect(fill = "#141c44")
  #     ) +
  #     geom_hline(yintercept = 0, colour = "blue", linetype = "dashed")

  #   if (!is.na(x_value)) {
  #     p <- p + geom_vline(xintercept = x_value, linetype = "dashed", color = "#DD4B39")
  #   }

  #   p
  # })

  # output$wSLRPlot <- renderPlot({
  #   ggplot(filtered_data(), aes(x = Time, y = RAP)) +
  #     geom_line(colour = "white") +
  #     geom_point(aes(size = ifelse(Time == input$plot_date3, 6, 3)),
  #       alpha = 0.8, colour = "white"
  #     ) +
  #     geom_line(aes(x = Time, y = HR / 10), data = filtered_slr2(), colour = "#00d4d4") +
  #     ylab("vertical reef growth & SLR (mm/year)") +
  #     xlab("Year") +
  #     theme_bw() +
  #     theme(
  #       legend.title = element_blank(), legend.position = "", plot.title = element_text(size = 10),
  #       plot.margin = margin(5, 12, 5, 5),
  #       axis.text = element_text(size = 12, color = "white"), # Set axis text color to white
  #       axis.title = element_text(size = 14, color = "white"),
  #       axis.line = element_line(color = "white"),
  #       panel.grid.major = element_blank(), # Remove major grid lines
  #       panel.grid.minor = element_blank(),
  #       panel.background = element_rect(fill = "#141c44"), # Set panel background to transparent
  #       plot.background = element_rect(fill = "#141c44")
  #     ) +
  #     geom_hline(yintercept = 0, colour = "blue", linetype = "dashed")
  # })



  # Calculate the x-value where y = 0
  # x_when_negative <- reactive({
  #   df <- filtered_data()
  #   # Find the index where RAP changes sign
  #   index <- which(diff(sign(df$rap)) < 0)

  #   if (length(index) == 0) {
  #     return(NA) # No negative crossing
  #   }

  #   # Take the first interval where RAP becomes negative
  #   i <- index[1]

  #   # Linear interpolation between points
  #   x0 <- df$Time[i]
  #   x1 <- df$Time[i + 1]
  #   y0 <- df$rap[i]
  #   y1 <- df$rap[i + 1]

  #   # Linear interpolation formula to find exact x where RAP = 0
  #   x_when_negative <- x0 - (y0 * (x1 - x0) / (y1 - y0))

  #   return(x_when_negative)
  # })



  # # Render the valueBox with x value where y=0
  # output$x_value_at_y0 <- renderValueBox({
  #   x_value <- x_when_negative()
  #   box_color <- ifelse(!is.na(x_value), "red", "blue")
  #   valueBox(
  #     value = tags$p(
  #       ifelse(is.na(x_value), "> 2100", round(x_value, 0)),
  #       style = "font-size: 2.2vw;" # Adjust the font size based on viewport width
  #     ),
  #     subtitle = tags$p(
  #       "Transition year to reef net erosion"
  #     ),
  #     icon = icon("exclamation-triangle"),
  #     color = box_color
  #   )
  # })


  # Reactive expression to compute the percentage
  reactive_percentage <- reactive({
    # Example calculation: average of slider values
    avg_value <- (input$slider1 + input$slider2 + input$slider3)
    paste0(round(avg_value), "%")
  })

  # Render the valueBox
  output$restoredCoral <- renderValueBox({
    valueBox(
      value = tags$p(
        reactive_percentage(),
        style = "font-size: 2.5vw;" # Adjust the font size based on viewport width
      ),
      subtitle = "total coral cover was added",
      icon = icon("leaf"),
      color = "green"
    )
  })

  # filtered_time_value <- reactive({
  #   filtered_data()
  #   filtered_slr2()

  #   # Ensure both datasets have the same Time range
  #   merged_data <- merge(filtered_data(), filtered_slr2(), by = "Time")

  #   # Find the first Time where HR > RAP
  #   condition_met <- merged_data[which(merged_data$HR / 10 > merged_data$RAP), ]

  #   if (nrow(condition_met) > 0) {
  #     return(condition_met$Time[1]) # Return the first occurrence
  #   } else {
  #     return(NA) # Return NA if no condition is met
  #   }
  # })

  # observeEvent(input$Adaptation_info, {
  #   toggle("Adaptation_info_text")
  # })

  # observeEvent(input$Branching_info, {
  #   toggle("Branching_info_text")
  # })
  # observeEvent(input$Builders_info, {
  #   toggle("Builders_info_text")
  # })
  # observeEvent(input$Weedy_info, {
  #   toggle("Weedy_info_text")
  # })

  # output$withSLR <- renderValueBox({
  #   time_value <- filtered_time_value()

  #   if (!is.na(time_value)) {
  #     valueBox(
  #       value = tags$p(
  #         time_value,
  #         style = "font-size: 2.2vw; max-width: 100%; word-wrap: break-word;" # Adjusted font size and added word wrap
  #       ),
  #       subtitle = tags$p(
  #         "Year reef growth stops matching sea level rise"
  #       ),
  #       icon = icon("house-flood-water"),
  #       color = "teal"
  #     )
  #   } else {
  #     valueBox(
  #       value = tags$p(
  #         ">2100",
  #         style = "font-size: 2.2vw; max-width: 100%; word-wrap: break-word;" # Adjusted font size and added word wrap
  #       ),
  #       subtitle = tags$p(
  #         "Year reef growth stops matching sea level rise"
  #       ),
  #       icon = icon("house-flood-water"),
  #       color = "teal"
  #     )
  #   }
  # })


  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(head(data, input$maxrows), row.names = FALSE)
    options(orig)
  })

  ## Restoration Planning 2 ----
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
    "Diploria labyrinthiformis", "Solenastrea bournoni", # use exact name you prefer
    "Pseudodiploria spp."
  )

  # Build list of sliderInput()s
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

  # Sum all numericInputs created for baseline cover and show in a valueBox
  output$baseline_cover_box <- shinydashboard::renderValueBox({
    # IDs were created as base_<sanitized species name>
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

  # helper to read inputs safely
  .safe_num <- function(x) {
    if (is.null(x) || is.na(x)) 0 else as.numeric(x)
  }

  output$restored_cover_box <- shinydashboard::renderValueBox({
    # baseline total (same IDs you used: base_<sanitized species>)
    sp_base <- if (is.null(input$baseline_species)) character(0) else input$baseline_species
    base_ids <- paste0("base_", gsub("[^A-Za-z0-9]", "_", sp_base))
    base_vals <- sapply(base_ids, function(id) .safe_num(input[[id]]))
    baseline_total <- sum(base_vals, na.rm = TRUE)

    # restoration total (sliders: slider_<sanitized species>)
    restoration_species <- c(
      "Acropora palmata", "Acropora cervicornis", "Montastraea cavernosa",
      "Orbicella faveolata", "Colpophyllia natans", "Porites astreoides",
      "Siderastrea siderea", "Stephanocoenia intersepta",
      "Diploria labyrinthiformis", "Solenastrea bournoni"
    )
    slider_ids <- paste0("rest_slider_", gsub("[^A-Za-z0-9]", "_", restoration_species))
    slider_vals <- sapply(slider_ids, function(id) .safe_num(input[[id]]))
    restoration_total <- sum(slider_vals, na.rm = TRUE)

    # restored cover = baseline + restoration
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
    # species chosen + their baseline % covers
    sp <- if (is.null(input$baseline_species)) character(0) else input$baseline_species
    ids <- paste0("base_", gsub("[^A-Za-z0-9]", "_", sp))
    covers <- sapply(ids, function(id) {
      v <- input[[id]]
      if (is.null(v)) 0 else as.numeric(v)
    })

    # net production = sum( cover * rate / 100 ), matching rates from travis_rates
    rates <- as.numeric(travis_rates$rate[match(sp, travis_rates$Species)])
    net_budget <- sum(covers * rates / 100, na.rm = TRUE)

    # total erosion from bioerosion for selected habitat (Inshore/Offshore)
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
    # "baseline" part
    sp <- if (is.null(input$baseline_species)) character(0) else input$baseline_species
    ids <- paste0("base_", gsub("[^A-Za-z0-9]", "_", sp))
    base_vals <- sapply(ids, function(id) {
      v <- input[[id]]
      if (is.null(v)) 0 else as.numeric(v)
    })
    base_rates <- as.numeric(travis_rates$rate[match(sp, travis_rates$Species)])
    net_base <- sum(base_vals * base_rates / 100, na.rm = TRUE)

    # "restoration" part
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

    # erosion (by selected habitat)
    row <- bioerosion[bioerosion$HABITAT_TYPE == input$habitat_choice, c("AVG_PARROTFISH", "AVG_URCHIN", "AVG_MICROBIOEROSION"), drop = FALSE]
    total_erosion <- if (nrow(row)) sum(as.numeric(row[1, ]), na.rm = TRUE) else 0

    # restored carbonate budget
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
  # value =tags$p( ">100%",
  #                style = "font-size: 2.1vw;"
  # ),
  # subtitle = tags$p("Total Target Cover",
  #                   style = "font-size:1.1vw;"
  # ),
}
shinyApp(ui, server)
