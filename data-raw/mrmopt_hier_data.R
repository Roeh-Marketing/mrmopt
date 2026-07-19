## Simulate mrmopt_hier_data: a hierarchical sub-channel media dataset
##
## A single "TV" channel with two levels of hierarchy:
##   subtype (Broadcast, Cable, Streaming) → partner (2–3 per subtype)
##
## 8 partners x 104 weeks = 832 rows.
## All partners share a Gompertz response curve family with unit-level
## variation in steepness (b), ceiling (d), and midpoint (e). The floor (c)
## is shared across all units.
##
## Run this script to regenerate data/mrmopt_hier_data.rda.

library(dplyr)
library(purrr)
library(tibble)

set.seed(3917)

n_weeks <- 104
start_date <- as.Date("2023-01-02")
weeks <- seq(start_date, by = "week", length.out = n_weeks)

gompertz <- function(x, b, c, d, e) {
  c + (d - c) * exp(-exp(b * (x - e)))
}

# --- Partner definitions ---
# Each partner has its own spend scale and slight curve parameter variation.
# The hierarchy: subtype → partner. Partners within a subtype share more
# similar parameters than partners across subtypes.

partners <- list(

  # --- Broadcast: large spend, high ceiling ---
  list(
    subtype    = "Broadcast",
    partner    = "Network A",
    spend_mean = 35000,
    spend_sd   = 7000,
    params     = list(b = -0.00008, c = 40, d = 700, e = 32000),
    noise_sd   = 120
  ),
  list(
    subtype    = "Broadcast",
    partner    = "Network B",
    spend_mean = 28000,
    spend_sd   = 5500,
    params     = list(b = -0.00010, c = 40, d = 550, e = 26000),
    noise_sd   = 100
  ),
  list(
    subtype    = "Broadcast",
    partner    = "Network C",
    spend_mean = 22000,
    spend_sd   = 4500,
    params     = list(b = -0.00009, c = 40, d = 480, e = 21000),
    noise_sd   = 90
  ),

  # --- Cable: moderate spend, moderate ceiling ---
  list(
    subtype    = "Cable",
    partner    = "Cable One",
    spend_mean = 15000,
    spend_sd   = 3500,
    params     = list(b = -0.00014, c = 40, d = 320, e = 14000),
    noise_sd   = 70
  ),
  list(
    subtype    = "Cable",
    partner    = "Cable Two",
    spend_mean = 12000,
    spend_sd   = 3000,
    params     = list(b = -0.00016, c = 40, d = 260, e = 11000),
    noise_sd   = 60
  ),

  # --- Streaming: lower spend, different saturation dynamics ---
  list(
    subtype    = "Streaming",
    partner    = "Stream Alpha",
    spend_mean = 10000,
    spend_sd   = 2500,
    params     = list(b = -0.00020, c = 40, d = 220, e = 9000),
    noise_sd   = 55
  ),
  list(
    subtype    = "Streaming",
    partner    = "Stream Beta",
    spend_mean = 8000,
    spend_sd   = 2000,
    params     = list(b = -0.00022, c = 40, d = 180, e = 7500),
    noise_sd   = 45
  ),
  list(
    subtype    = "Streaming",
    partner    = "Stream Gamma",
    spend_mean = 6000,
    spend_sd   = 1500,
    params     = list(b = -0.00025, c = 40, d = 140, e = 5500),
    noise_sd   = 40
  )
)

simulate_partner <- function(partner_def, weeks) {
  n <- length(weeks)

  # Spend: truncated normal (no negative spend)
  spend <- pmax(
    rnorm(n, mean = partner_def$spend_mean, sd = partner_def$spend_sd),
    partner_def$spend_mean * 0.1
  )
  spend <- round(spend)

  # Response: Gompertz evaluated at spend + noise, floored at 0
  p <- partner_def$params
  conversions <- gompertz(spend, b = p$b, c = p$c, d = p$d, e = p$e) +
    rnorm(n, mean = 0, sd = partner_def$noise_sd)
  conversions <- pmax(round(conversions), 0L)

  tibble(
    subtype     = partner_def$subtype,
    partner     = partner_def$partner,
    week        = weeks,
    spend       = spend,
    conversions = as.integer(conversions)
  )
}

mrmopt_hier_data <- map_dfr(partners, simulate_partner, weeks = weeks) |>
  mutate(
    subtype = factor(subtype, levels = c("Broadcast", "Cable", "Streaming")),
    partner = factor(partner)
  )

usethis::use_data(mrmopt_hier_data, overwrite = TRUE)
