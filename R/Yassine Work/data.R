data <- read_sav("data/raw/Datenbank Praktikum FINAL.sav")

data <- data %>%
  select(
    PatientID,
    EuroScore2,
    ES2_Quart,
    ES2_Median,
    BMI,
    BMI_Median,
    BMI_Quart,
    Duration_of_cardiopulmonary_bypass_minutes,
    Duration_of_CPB_Quart,
    Duration_of_CPB_Median,
    SBP,
    SBP_Quart,
    days_onexclusive_oral_feeding,
    Protein_intake__d1_4,
    Protein_intake_per_kg_on_day4,
    AKI_any,
    AKI_any_early,
    AKI_any_sensitivity_worst_case,
    died_in_ICU
  ) %>% mutate(
    died_in_ICU = ifelse(died_in_ICU == "Yes", 1, 0)
  ) %>%
  relocate(
    Duration_of_cardiopulmonary_bypass_minutes,
    Duration_of_CPB_Quart,
    Duration_of_CPB_Median,
    .after = ES2_Median
  )

View(data)
