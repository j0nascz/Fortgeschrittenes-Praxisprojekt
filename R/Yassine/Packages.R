
# ============================================================
# Required packages — Fortgeschrittenes Praxisprojekt
# ============================================================

# List all packages used in the project
packages <- c(
  # Data import and manipulation
  "haven",
  "dplyr",
  "tidyr",
  
  # Data visualization
  "ggplot2",
  "scales",
  "patchwork",
  "grid",
  
  # Statistical modeling
  "mgcv",
  "splines",
  "sandwich",
  "lmtest",
  
  # Survival and competing-risks analysis
  "survival",
  "cmprsk"
)

# Install missing packages (only when necessary)
missing_packages <- setdiff(
  packages,
  rownames(installed.packages())
)

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load all packages
invisible(
  lapply(packages, library, character.only = TRUE)
)

message("All required packages loaded successfully.")
