library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sandwich)
library(lmtest)
library(dplyr)
library(mgcv)      # GAM
library(sandwich)  # Robuste Standardfehler
library(lmtest)    # Koeffiziententests
library(survival)
library(cmprsk)


# Install packages once (if needed)
packages <- c(
  "dplyr",
  "ggplot2",
  "mgcv",
  "sandwich",
  "lmtest",
  "splines",
  "survival",
  "cmprsk"
)

new_packages <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(new_packages) > 0) install.packages(new_packages)

# Load packages
library(dplyr)
library(ggplot2)
library(mgcv)
library(sandwich)
library(lmtest)
library(splines)
library(survival)
library(cmprsk)
library(scales)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
