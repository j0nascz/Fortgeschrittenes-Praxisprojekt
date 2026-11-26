# =============================================================================
# data_prep.R
# Reads the raw SPSS file and makes the general adjustments needed by all
# analyses. Creates in the workspace:
#   dat   all patients (n = 293)
#
# Anything only one analysis needs (subsets, outcome groups, ...) is created
# in that analysis script, not here.
#
# Needs helpers.R first (for to01). Every analysis script starts with:
#   source("R/helpers.R")
#   source("R/data_prep.R")
# =============================================================================

if (!exists("to01")) source("R/helpers.R")

dat <- read_sav("data/raw/Datenbank_Praktikum_FINAL.sav")


# -----------------------------------------------------------------------------
# Binary outcomes (0/1)
# -----------------------------------------------------------------------------
dat$AKI_new           <- to01(dat$modified_new_AKI_any_day_after_day4)  # Outcome A (primary)
dat$AKI_on_day_5_or_6 <- to01(dat$modified_new_AKI_on_day_5_or_6)       # Outcome B (AKI on day 5-6)
dat$AKI_worst         <- to01(dat$AKI_any_sensitivity_worst_case)       # Worst case: AKI or death
dat$died              <- to01(dat$died_in_ICU)


# -----------------------------------------------------------------------------
# Quick check
# -----------------------------------------------------------------------------
cat(sprintf("Patients: %d | new AKI: %d | AKI day 5-6: %d | died in ICU: %d\n",
            nrow(dat),
            as.integer(sum(dat$AKI_new, na.rm = TRUE)),
            as.integer(sum(dat$AKI_on_day_5_or_6, na.rm = TRUE)),
            as.integer(sum(dat$died, na.rm = TRUE))))
