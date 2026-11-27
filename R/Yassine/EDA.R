# ============================================================================
# EXPLORATORY DATA ANALYSIS: ARTIFICIAL PROTEIN INTAKE AND AKI
# ============================================================================
# Input:  data (already imported and prepared)
# Setup: Run your separate package-loading script before this file.
# Required packages: dplyr, tidyr, ggplot2, scales, patchwork
#
# Outcomes (as checked in your dataset):
#   AKI_any       = primary AKI outcome after ICU day 4 (0/1)
#   AKI_any_early = secondary AKI outcome on ICU days 5-6 (0/1)
# These labels rely on your prior column-equivalence checks.
#
# This script does NOT modify data or export patient-level information.
# Colour scheme:
#   Zero artificial protein = red (#EF4444)
#   Positive artificial protein = blue (#3B82F6)
#   AKI = red; No AKI = blue; Missing = grey
# ============================================================================


# 1. DATA STRUCTURE AND OUTCOME CODING ---------------------------------------

print(dim(data))
print(names(data))
str(data)

id_check <- data %>%
  summarise(
    n_patients = n(),
    n_unique_IDs = n_distinct(PatientID, na.rm = TRUE),
    n_missing_IDs = sum(is.na(PatientID)),
    n_duplicate_IDs = sum(duplicated(PatientID[!is.na(PatientID)]))
  )
print(id_check)

# Inspect frequency tables; each AKI variable should contain 0, 1, or NA.
print(table(data$AKI_any, useNA = "ifany"))
print(table(data$AKI_any_early, useNA = "ifany"))
print(table(data$AKI_any_sensitivity_worst_case, useNA = "ifany"))
print(table(data$died_in_ICU, useNA = "ifany"))

stopifnot(
  all(is.na(data$AKI_any) | data$AKI_any %in% c(0, 1)),
  all(is.na(data$AKI_any_early) | data$AKI_any_early %in% c(0, 1))
)


# 2. MISSING VALUES ----------------------------------------------------------

missing_data <- data %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Missing_n"
  ) %>%
  mutate(Missing_percent = 100 * Missing_n / nrow(data)) %>%
  arrange(desc(Missing_n))

print(missing_data, n = Inf)

if (any(missing_data$Missing_n > 0)) {
  p_missing <- missing_data %>%
    filter(Missing_n > 0) %>%
    ggplot(aes(x = reorder(Variable, Missing_percent),
               y = Missing_percent)) +
    geom_col(fill = "#64748B", width = 0.7) +
    geom_text(
      aes(label = sprintf("n = %d (%.1f%%)", Missing_n, Missing_percent)),
      hjust = -0.12, size = 3.5
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.4))) +
    labs(
      title = "Missing Values by Variable",
      subtitle = paste("Total patients:", nrow(data)),
      x = NULL,
      y = "Missing observations (%)"
    ) +
    theme_minimal()
  
  print(p_missing)
} else {
  message("No values coded as NA were found.")
}


# 3. STUDY SUMMARY AND DESCRIPTIVE STATISTICS --------------------------------

study_summary <- data %>%
  summarise(
    n_patients = n(),
    n_primary_evaluable = sum(!is.na(AKI_any)),
    n_AKI_after4 = sum(AKI_any == 1, na.rm = TRUE),
    n_secondary_evaluable = sum(!is.na(AKI_any_early)),
    n_AKI_day56 = sum(AKI_any_early == 1, na.rm = TRUE),
    n_AKI_worst_case = sum(AKI_any_sensitivity_worst_case == 1,
                           na.rm = TRUE),
    n_ICU_deaths = sum(died_in_ICU == "Yes", na.rm = TRUE),
    n_protein_evaluable = sum(!is.na(Protein_intake__d1_4)),
    n_zero_protein = sum(Protein_intake__d1_4 == 0, na.rm = TRUE),
    pct_zero_protein = 100 * mean(Protein_intake__d1_4 == 0,
                                  na.rm = TRUE)
  )
print(study_summary)

continuous_vars <- c(
  "Protein_intake__d1_4",
  "EuroScore2",
  "BMI",
  "Duration_of_cardiopulmonary_bypass_minutes",
  "SBP",
  "days_onexclusive_oral_feeding"
)

# Use safe summaries: return NA for variables with no observed values.
safe_mean   <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
safe_sd     <- function(x) if (sum(!is.na(x)) < 2) NA_real_ else sd(x, na.rm = TRUE)
safe_median <- function(x) if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)
safe_q     <- function(x, p) if (all(is.na(x))) NA_real_ else
  as.numeric(quantile(x, p, na.rm = TRUE))
safe_min   <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
safe_max   <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

descriptive_table <- data %>%
  select(all_of(continuous_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    n = sum(!is.na(Value)),
    Mean = safe_mean(Value),
    SD = safe_sd(Value),
    Median = safe_median(Value),
    Q1 = safe_q(Value, 0.25),
    Q3 = safe_q(Value, 0.75),
    Min = safe_min(Value),
    Max = safe_max(Value),
    .groups = "drop"
  ) %>%
  mutate(across(c(Mean, SD, Median, Q1, Q3, Min, Max), ~ round(.x, 2)))

print(descriptive_table)


# 4. ARTIFICIAL PROTEIN DISTRIBUTION -----------------------------------------

protein_data <- data %>%
  filter(!is.na(Protein_intake__d1_4)) %>%
  mutate(
    Protein_group = factor(
      if_else(Protein_intake__d1_4 == 0, "Zero", "Positive"),
      levels = c("Zero", "Positive")
    )
  )

protein_counts <- protein_data %>%
  count(Protein_group, .drop = FALSE) %>%
  mutate(Percentage = round(100 * n / sum(n), 1))
print(protein_counts)

p1 <- ggplot(protein_data, aes(x = Protein_intake__d1_4)) +
  geom_histogram(bins = 30, fill = "#3B82F6", color = "white") +
  labs(
    title = "Distribution of Artificial Protein Intake",
    x = "Mean artificial protein intake (g/kg/day), ICU days 1–4",
    y = "Number of patients"
  ) +
  theme_minimal()
print(p1)

if (any(protein_data$Protein_intake__d1_4 > 0)) {
  p2 <- protein_data %>%
    filter(Protein_intake__d1_4 > 0) %>%
    ggplot(aes(x = Protein_intake__d1_4)) +
    geom_histogram(bins = 25, fill = "#3B82F6", color = "white") +
    labs(
      title = "Distribution of Positive Artificial Protein Intake",
      x = "Mean artificial protein intake (g/kg/day), ICU days 1–4",
      y = "Number of patients"
    ) +
    theme_minimal()
  print(p2)
}


# 5. FEEDING DURATION AND PROTEIN OVERLAP ------------------------------------

feeding_data <- protein_data %>%
  filter(!is.na(days_onexclusive_oral_feeding),
         days_onexclusive_oral_feeding %in% 0:4)

feeding_summary <- feeding_data %>%
  group_by(days_onexclusive_oral_feeding) %>%
  summarise(
    n = n(),
    n_zero_protein = sum(Protein_intake__d1_4 == 0),
    percent_zero = 100 * mean(Protein_intake__d1_4 == 0),
    median_protein = median(Protein_intake__d1_4),
    max_protein = max(Protein_intake__d1_4),
    .groups = "drop"
  )
print(feeding_summary)

p3 <- ggplot(
  feeding_data,
  aes(x = factor(days_onexclusive_oral_feeding,
                 levels = 0:4),
      y = Protein_intake__d1_4)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#EF4444") +
  geom_jitter(width = 0.15, height = 0, alpha = 0.45,
              size = 1.8, color = "#3B82F6") +
  labs(
    title = "Artificial Protein Intake by Oral Feeding Duration",
    x = "Days of exclusive oral feeding",
    y = "Mean artificial protein intake (g/kg/day)"
  ) +
  theme_minimal()
print(p3)


# 6. OBSERVED AKI AND ARTIFICIAL PROTEIN -------------------------------------

aki_data <- protein_data %>%
  filter(!is.na(AKI_any)) %>%
  mutate(AKI_status = factor(AKI_any, levels = c(0, 1),
                             labels = c("No AKI", "AKI")))

aki_summary <- aki_data %>%
  group_by(AKI_status) %>%
  summarise(
    n = n(),
    mean_protein = mean(Protein_intake__d1_4),
    median_protein = median(Protein_intake__d1_4),
    Q1 = as.numeric(quantile(Protein_intake__d1_4, 0.25)),
    Q3 = as.numeric(quantile(Protein_intake__d1_4, 0.75)),
    percent_zero = 100 * mean(Protein_intake__d1_4 == 0),
    .groups = "drop"
  )
print(aki_summary)

p4 <- ggplot(aki_data,
             aes(x = AKI_status, y = Protein_intake__d1_4,
                 fill = AKI_status)) +
  geom_boxplot(width = 0.45,
               outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.12, alpha = 0.3, size = 1.3) +
  labs(
    title = "Artificial Protein Intake by AKI Status",
    x = "AKI after ICU day 4",
    y = "Mean artificial protein intake (g/kg/day)"
  ) +
  scale_fill_manual(values = c("No AKI" = "#3B82F6",
                               "AKI" = "#EF4444")) +
  theme_minimal() +
  theme(legend.position = "none")
print(p4)

risk_data <- aki_data %>%
  group_by(Protein_group) %>%
  summarise(
    n = n(),
    AKI_events = sum(AKI_any == 1),
    AKI_risk = mean(AKI_any == 1),
    .groups = "drop"
  )
print(risk_data)

p5 <- ggplot(risk_data,
             aes(x = Protein_group, y = AKI_risk,
                 fill = Protein_group)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = sprintf("%.1f%%\n(n=%d)", 100 * AKI_risk, n)),
    vjust = -0.3, size = 3.4
  ) +
  scale_fill_manual(values = c("Zero" = "#EF4444",
                               "Positive" = "#3B82F6")) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, 1, 0.2)) +
  coord_cartesian(ylim = c(0, 1.1), clip = "off") +
  labs(
    title = "Observed AKI Proportion by Artificial Protein Intake",
    x = "Artificial protein intake, ICU days 1–4",
    y = "Observed AKI proportion"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
print(p5)


# 7. AKI PROPORTIONS BY FEEDING DAYS + PATIENT COUNT TABLE -------------------
# Uses complete cases for AKI, protein intake, and feeding days.
# Counts in the sixth panel therefore refer to the patients in these plots,
# not necessarily every patient in the original dataset.

aki_feeding_data <- aki_data %>%
  filter(!is.na(days_onexclusive_oral_feeding),
         days_onexclusive_oral_feeding %in% 0:4) %>%
  mutate(Oral_days = factor(days_onexclusive_oral_feeding,
                            levels = 0:4,
                            labels = paste("Oral feeding:", 0:4, "days")))

aki_feeding_risk <- aki_feeding_data %>%
  group_by(Oral_days, Protein_group, .drop = FALSE) %>%
  summarise(
    n = n(),
    AKI_events = sum(AKI_any == 1),
    AKI_risk = if_else(n > 0, AKI_events / n, NA_real_),
    .groups = "drop"
  )
print(aki_feeding_risk)

patient_counts <- aki_feeding_risk %>%
  select(Oral_days, Protein_group, n) %>%
  pivot_wider(names_from = Protein_group,
              values_from = n, values_fill = 0) %>%
  mutate(Total = Zero + Positive)
print(patient_counts)

make_aki_panel <- function(day) {
  panel_label <- paste("Oral feeding:", day, "days")
  panel_data <- aki_feeding_risk %>%
    filter(as.character(Oral_days) == panel_label, n > 0)
  
  ggplot(panel_data,
         aes(x = Protein_group, y = AKI_risk,
             fill = Protein_group)) +
    geom_col(width = 0.6) +
    geom_text(
      aes(label = sprintf("%.1f%%\n(n=%d)", 100 * AKI_risk, n)),
      vjust = -0.3, size = 3.2
    ) +
    scale_fill_manual(values = c("Zero" = "#EF4444",
                                 "Positive" = "#3B82F6"),
                      drop = FALSE) +
    scale_x_discrete(limits = c("Zero", "Positive"), drop = FALSE) +
    scale_y_continuous(labels = scales::percent,
                       breaks = seq(0, 1, 0.2)) +
    coord_cartesian(ylim = c(0, 1.12), clip = "off") +
    labs(title = panel_label, x = NULL, y = "AKI proportion") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 11, face = "bold")
    )
}

aki_panels <- lapply(0:4, make_aki_panel)

# Convert the count table into long format for a ggplot table panel.
table_data <- patient_counts %>%
  mutate(Day = as.integer(Oral_days) - 1L) %>%
  select(Day, Zero, Positive, Total) %>%
  pivot_longer(c(Zero, Positive, Total),
               names_to = "Group", values_to = "Count") %>%
  mutate(
    x = match(Group, c("Zero", "Positive", "Total")) + 1L,
    y = 5 - Day
  )

p_table <- ggplot() +
  annotate("rect", xmin = 0.5, xmax = 4.5,
           ymin = 5.5, ymax = 6.5, fill = "#E5E7EB") +
  annotate("text", x = 1:4, y = 6,
           label = c("Oral days", "Zero", "Positive", "Total"),
           fontface = "bold", size = 3.0) +
  geom_text(data = table_data,
            aes(x = x, y = y, label = Count, color = Group),
            size = 3.6, show.legend = FALSE) +
  geom_text(data = data.frame(Day = 0:4),
            aes(x = 1, y = 5 - Day, label = Day), size = 3.6) +
  geom_hline(yintercept = 0.5:5.5,
             color = "#D1D5DB", linewidth = 0.3) +
  scale_color_manual(values = c("Zero" = "#EF4444",
                                "Positive" = "#3B82F6",
                                "Total" = "#374151")) +
  coord_cartesian(xlim = c(0.5, 4.5),
                  ylim = c(0.5, 6.5), expand = FALSE) +
  labs(title = "Patient Counts (n)") +
  theme_void() +
  theme(plot.title = element_text(size = 11, face = "bold"))

p_aki_feeding <- patchwork::wrap_plots(
  c(aki_panels, list(p_table)), ncol = 3
) +
  patchwork::plot_annotation(
    title = "Observed AKI Proportion by Artificial Protein Intake",
    subtitle = "Stratified by Days of Exclusive Oral Feeding"
  )
print(p_aki_feeding)


# 8. CLINICAL VARIABLES BY AKI STATUS ---------------------------------------

clinical_data <- data %>%
  select(AKI_any, EuroScore2, BMI,
         Duration_of_cardiopulmonary_bypass_minutes, SBP) %>%
  filter(!is.na(AKI_any)) %>%
  pivot_longer(-AKI_any, names_to = "Variable", values_to = "Value") %>%
  mutate(AKI_status = factor(AKI_any, levels = c(0, 1),
                             labels = c("No AKI", "AKI")))

p6 <- ggplot(clinical_data,
             aes(x = AKI_status, y = Value, fill = AKI_status)) +
  geom_boxplot(alpha = 0.7, na.rm = TRUE) +
  facet_wrap(~ Variable, scales = "free_y") +
  scale_fill_manual(values = c("No AKI" = "#3B82F6",
                               "AKI" = "#EF4444")) +
  labs(
    title = "Clinical Characteristics by AKI Status",
    x = "AKI after ICU day 4",
    y = "Observed value"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
print(p6)

# ============================================================================
# END OF EDA
# ============================================================================
