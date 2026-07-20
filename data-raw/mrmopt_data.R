## Simulate mrmopt_data: a realistic multi-channel media spend dataset
##
## 5 channels x 104 weeks (2 years) of weekly spend and conversions.
## Each channel has its own spend scale, response curve shape, and noise level.
## Run this script to regenerate data/mrmopt_data.rda.

library(dplyr)
library(tidyr)
library(purrr)

set.seed(6241)

n_weeks <- 104
start_date <- as.Date("2023-01-02")
weeks <- seq(start_date, by = "week", length.out = n_weeks)

# Gompertz curve: c + (d - c) * exp(-exp(b * (x - e)))
gompertz <- function(x, b, c, d, e) {
  c + (d - c) * exp(-exp(b * (x - e)))
}

# Logistic curve: c + (d - c) / (1 + exp(b * (x - e)))
logistic <- function(x, b, c, d, e) {
  c + (d - c) / (1 + exp(b * (x - e)))
}

# Log-logistic curve: c + (d - c) / (1 + exp(b * (log(x) - log(e))))
log_logistic <- function(x, b, c, d, e) {
  c + (d - c) / (1 + exp(b * (log(x) - log(e))))
}

# --- Channel definitions ---
# spend_mean / spend_sd: weekly spend distribution (dollars)
# curve / params: response curve type and parameters
# noise_sd: additive noise on conversions

channels <- list(

  # b values are on the raw spend scale (dollars).
  # For standard forms (gompertz, logistic), b ≈ -6 / (3 * spend_sd)
  # gives a realistic transition width spanning ~3 SDs of spend.
  # Log-logistic operates on log(spend), so b stays in that scale.

  `Paid Search` = list(
    spend_mean = 45000,
    spend_sd   = 8000,
    curve      = gompertz,
    # b: steepness, c: floor conversions, d: ceiling conversions, e: midpoint spend
    params     = list(b = -0.00020, c = 80, d = 950, e = 42000),
    noise_sd   = 50
  ),

  `Paid Social` = list(
    spend_mean = 28000,
    spend_sd   = 6000,
    curve      = logistic,
    params     = list(b = -0.00033, c = 50, d = 620, e = 26000),
    noise_sd   = 100
  ),

  `Display` = list(
    spend_mean = 18000,
    spend_sd   = 4000,
    curve      = gompertz,
    params     = list(b = -0.00050, c = 20, d = 340, e = 16000),
    noise_sd   = 75
  ),

  `Online Video` = list(
    spend_mean = 35000,
    spend_sd   = 9000,
    curve      = logistic,
    params     = list(b = -0.00022, c = 60, d = 780, e = 33000),
    noise_sd   = 150
  ),

  `TV` = list(
    spend_mean = 80000,
    spend_sd   = 15000,
    curve      = log_logistic,
    # Log-logistic: b operates on log(spend) scale, no adjustment needed
    params     = list(b = -4.0, c = 100, d = 1800, e = 75000),
    noise_sd   = 225
  )
)

simulate_channel <- function(ch_name, ch_def, weeks) {
  n <- length(weeks)

  # Spend: truncated normal (no negative spend)
  spend <- pmax(
    rnorm(n, mean = ch_def$spend_mean, sd = ch_def$spend_sd),
    ch_def$spend_mean * 0.1
  )
  spend <- round(spend)

  # Response: curve evaluated at spend + noise, floored at 0
  p <- ch_def$params
  conversions <- ch_def$curve(spend, b = p$b, c = p$c, d = p$d, e = p$e) +
    rnorm(n, mean = 0, sd = ch_def$noise_sd)
  conversions <- pmax(round(conversions), 0L)

  tibble::tibble(
    channel     = ch_name,
    week        = weeks,
    spend       = spend,
    conversions = as.integer(conversions)
  )
}

mrmopt_data <- map2_dfr(
  names(channels),
  channels,
  ~ simulate_channel(.x, .y, weeks)
) |>
  mutate(channel = factor(channel, levels = names(channels)))

usethis::use_data(mrmopt_data, overwrite = TRUE)
