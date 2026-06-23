## ------------------------------
##  DHW-proportional cover-loss mortality (Webb et al. 2023 derived)
##   Calculated as: (ASB Loss % / 8 DHW) = % cover change per 1 DHW
## ------------------------------
use_dhw_cover_mortality <- TRUE   # Switch to TRUE for smooth responses

dhw_slope_lookup_FK <- tibble::tribble(
  ~taxon,            ~slope_pct_per_DHW,
  "Acropora",         -2.00, # Conservative estimate
  "Agaricia",         -1.00, # Based on morph similarity to Porites
  "Orbicella",        -0.85, # (6.8% / 8)
  "Montastraea",      -1.56, # (12.5% / 8)
  "Colpophyllia",     -0.89, # (7.1% / 8)
  "Porites",          -1.37, # (11.0% / 8)
  "Siderastrea",      -0.21, # (1.7% / 8)
  "Pseudodiploria",   -0.80, # Generic brain coral estimate
  "Diploria",         -0.80,
  "Stephanocoenia",   -0.50,
  "Madracis",         -0.50,
  "Millepora",        -0.50
)

dhw_slope_default_pct_per_DHW <- -0.80
dhw_mortality_cap <- 0.95


## ------------------------------
## Bleaching mortality settings (Webb et al. 2023)
## ------------------------------
use_bleaching_mortality <- FALSE
bleach_threshold_MB  <- 4
bleach_threshold_ASB <- 8

bleach_loss_lookup <- tibble::tribble(
  ~taxon,                    ~loss_ASB, ~loss_MB,
  "Orbicella",               0.068,     0.023,
  "Siderastrea",             0.017,     0.006,
  "Porites",                 0.110,     0.037,
  "Colpophyllia",            0.071,     0.024,
  "Montastraea",             0.125,     0.042
)

bleach_loss_default_ASB <- 0.05
bleach_loss_default_MB  <- 0.03
