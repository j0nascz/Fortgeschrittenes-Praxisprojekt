# =============================================================================
# helpers.R
# Packages, plot settings and helper functions shared by all analysis scripts.
#
# Do not run this on its own. Every analysis script starts with:
#   source("R/helpers.R")
#   source("R/data_prep.R")
# =============================================================================

library(haven)
library(logistf)
library(nnet)
library(ggplot2)

dir.create("figures", showWarnings = FALSE)
dir.create("output",  showWarnings = FALSE)


# -----------------------------------------------------------------------------
# Plot settings
# -----------------------------------------------------------------------------
theme_set(theme_minimal(base_size = 16))

col_blue  <- "#1f4e79"
col_red   <- "#c0504d"
col_green <- "#2e8b57"

# Save a ggplot as a PNG for the slides
save_slide <- function(plot, name, width = 10, height = 6.5) {
  ggsave(file.path("figures", paste0(name, ".png")), plot,
         width = width, height = height, dpi = 300, bg = "white")
}


# -----------------------------------------------------------------------------
# Data helpers
# -----------------------------------------------------------------------------

# "Yes"/1 -> 1, "No"/0 -> 0, NA stays NA
to01 <- function(x) {
  x <- as.character(as_factor(x))
  ifelse(is.na(x), NA, ifelse(x %in% c("Yes", "yes", "1", "Ja", "ja"), 1, 0))
}


# -----------------------------------------------------------------------------
# Model helpers
# -----------------------------------------------------------------------------

# Covariate sets used in the models
cov_full  <- "log(EuroScore2) + BMI + Duration_of_cardiopulmonary_bypass_minutes + SBP"
cov_small <- "log(EuroScore2) + Duration_of_cardiopulmonary_bypass_minutes"  # few events (Firth)

# Fit a logistic model (glm or Firth) and return the OR of one exposure.
#   outcome : name of the 0/1 outcome column
#   expo    : name of the exposure column
#   cov     : covariate string (e.g. cov_full)
#   oral    : TRUE = also adjust for days on exclusive oral feeding
#   firth   : TRUE = Firth's penalized logistic regression (logistf)
# Returns c(OR, lower, upper, Events)
fit_or <- function(outcome, expo, cov, oral, data, firth = FALSE) {
  rhs <- paste(expo, cov, sep = " + ")
  if (oral) rhs <- paste(rhs, "days_onexclusive_oral_feeding", sep = " + ")
  f <- as.formula(paste(outcome, "~", rhs))
  events <- sum(data[[outcome]], na.rm = TRUE)

  if (firth) {
    m <- logistf(f, data = data)
    i <- which(names(coef(m)) == expo)
    c(OR = unname(exp(coef(m)[i])), lower = unname(exp(m$ci.lower[i])),
      upper = unname(exp(m$ci.upper[i])), Events = events)
  } else {
    m  <- glm(f, family = binomial, data = data)
    ci <- exp(confint.default(m)[expo, ])
    c(OR = unname(exp(coef(m)[expo])), lower = unname(ci[1]), upper = unname(ci[2]),
      Events = events)
  }
}

# Format an OR with its CI as text, e.g. "0.99 (0.87-1.12)"
fmt_or <- function(or, lower, upper) sprintf("%.2f (%.2f-%.2f)", or, lower, upper)
