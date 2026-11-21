library(haven)
library(dplyr)
library(ggplot2)
library(patchwork)

data <- read_sav("data/raw/Datenbank_Praktikum_FINAL.sav")


# stratified mode

# seperate models for each group

models <- lapply(0:4, function(d) {
  glm(
    AKI_any ~ Protein_intake__d1_4 +
      EuroScore2 +
      BMI +
      Duration_of_cardiopulmonary_bypass_minutes +
      SBP,
    data = subset(data, days_onexclusive_oral_feeding == d),
    family = binomial(link = "logit")
  )
})

names(models) <- paste0("Oral feeding days = ", 0:4)

# display results of the models

summary(models[[1]])  # 0 Tage
summary(models[[2]])  # 1 Tag
summary(models[[3]])  # 2 Tage
summary(models[[4]])  # 3 Tage
summary(models[[5]])  # 4 Tage

# compare or

library(broom)
library(dplyr)

results <- bind_rows(
  lapply(1:5, function(i) {
    tidy(models[[i]], conf.int = TRUE, exponentiate = TRUE) %>%
      filter(term == "Protein_intake__d1_4") %>%
      mutate(group = names(models)[i])
  })
)

results %>%
  select(group, estimate, conf.low, conf.high, p.value)


# group sizes

data %>%
  group_by(days_onexclusive_oral_feeding) %>%
  summarise(
    n = n(),
    AKI_cases = sum(AKI_any == 1, na.rm = TRUE),
    no_AKI = sum(AKI_any == 0, na.rm = TRUE),
    .groups = "drop"
  )

data %>%
  filter(days_onexclusive_oral_feeding == 4) %>%
  count(Protein_intake__d1_4, AKI_any)


# stratified mode

# seperate models for each group

models <- lapply(0:4, function(d) {
  glm(
    AKI_any ~ Protein_intake_per_kg_on_day4 +
      EuroScore2 +
      BMI +
      Duration_of_cardiopulmonary_bypass_minutes +
      SBP,
    data = subset(data, days_onexclusive_oral_feeding == d),
    family = binomial(link = "logit")
  )
})

names(models) <- paste0("Oral feeding days = ", 0:4)

# display results of the models

summary(models[[1]])  # 0 Tage
summary(models[[2]])  # 1 Tag
summary(models[[3]])  # 2 Tage
summary(models[[4]])  # 3 Tage
summary(models[[5]])  # 4 Tage

# compare or

library(broom)
library(dplyr)

results <- bind_rows(
  lapply(1:5, function(i) {
    tidy(models[[i]], conf.int = TRUE, exponentiate = TRUE) %>%
      filter(term == "Protein_intake_per_kg_on_day4") %>%
      mutate(group = names(models)[i])
  })
)

results %>%
  select(group, estimate, conf.low, conf.high, p.value)


# group sizes

data %>%
  group_by(days_onexclusive_oral_feeding) %>%
  summarise(
    n = n(),
    AKI_cases = sum(AKI_any == 1, na.rm = TRUE),
    no_AKI = sum(AKI_any == 0, na.rm = TRUE),
    .groups = "drop"
  )
