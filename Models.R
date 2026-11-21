library(haven)
library(dplyr)
library(ggplot2)
library(patchwork)

data <- read_sav("data/raw/Datenbank_Praktikum_FINAL.sav")

# Logistic Regression
# Model A

model_a <- glm(
  AKI_any ~ Protein_intake__d1_4,
  data = data,
  family = binomial(link = "logit")
)

summary(model_a)

# Modell B – Präoperative und körperbezogene Faktoren
model_b <- glm(AKI_any ~ Protein_intake__d1_4 +
  EuroScore2 + BMI,
  data = data,
  family = binomial(link = "logit")
)

summary(model_b)

# Modell C – Zusätzlich perioperative Faktoren
model_c<- glm(AKI_any ~ Protein_intake__d1_4 +
  EuroScore2 + BMI + SBP +
  Duration_of_cardiopulmonary_bypass_minutes,
  data = data,
  family = binomial(link = "logit")
)

summary(model_c)

# model D zusaätzlich ernährungssituation
model_d<- glm(
    AKI_any ~ Protein_intake__d1_4 +
    EuroScore2 +
    BMI +
    Duration_of_cardiopulmonary_bypass_minutes +
    SBP +
    days_onexclusive_oral_feeding,
  data = data,
  family = binomial(link = "logit")
)

summary(model_d)

# Plots

# Unadjusted Model

library(ggplot2)

# Vorhergesagte Wahrscheinlichkeiten und Konfidenzintervalle
pred_unadjusted <- data.frame(
  Protein_intake__d1_4 = seq(
    min(data$Protein_intake__d1_4, na.rm = TRUE),
    max(data$Protein_intake__d1_4, na.rm = TRUE),
    length.out = 200
  )
)

pred_unadjusted$pred <- predict(
  model_unadjusted,
  newdata = pred_unadjusted,
  type = "link",
  se.fit = TRUE
)$fit

pred_unadjusted$se <- predict(
  model_unadjusted,
  newdata = pred_unadjusted,
  type = "link",
  se.fit = TRUE
)$se.fit

# 95%-Konfidenzintervall auf der Wahrscheinlichkeitsskala
pred_unadjusted$prob <- plogis(pred_unadjusted$pred)

pred_unadjusted$lower <- plogis(
  pred_unadjusted$pred - 1.96 * pred_unadjusted$se
)

pred_unadjusted$upper <- plogis(
  pred_unadjusted$pred + 1.96 * pred_unadjusted$se
)

# Plot
plot_unadjusted <- ggplot() +
  geom_jitter(
    data = data,
    aes(x = Protein_intake__d1_4, y = AKI_any),
    height = 0.04,
    width = 0,
    alpha = 0.4
  ) +
  geom_ribbon(
    data = pred_unadjusted,
    aes(
      x = Protein_intake__d1_4,
      ymin = lower,
      ymax = upper
    ),
    fill = "steelblue",
    alpha = 0.2
  ) +
  geom_line(
    data = pred_unadjusted,
    aes(x = Protein_intake__d1_4, y = prob),
    color = "steelblue",
    linewidth = 1.2
  ) +
  labs(
    title = "Unadjusted Logistic Regression",
    x = "Protein intake (g/kg/day)",
    y = "Probability of AKI"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

plot_unadjusted

# Adjusted Model

# Vorhersagedaten erstellen
pred_adjusted <- data.frame(
  Protein_intake__d1_4 = seq(
    min(data$Protein_intake__d1_4, na.rm = TRUE),
    max(data$Protein_intake__d1_4, na.rm = TRUE),
    length.out = 200
  ),
  EuroScore2 = mean(data$EuroScore2, na.rm = TRUE),
  BMI = mean(data$BMI, na.rm = TRUE),
  Duration_of_cardiopulmonary_bypass_minutes =
    mean(data$Duration_of_cardiopulmonary_bypass_minutes, na.rm = TRUE),
  SBP = mean(data$SBP, na.rm = TRUE),
  days_onexclusive_oral_feeding =
    mean(data$days_onexclusive_oral_feeding, na.rm = TRUE)
)

# Vorhersage inklusive Standardfehler
pred_result <- predict(
  model_adjusted,
  newdata = pred_adjusted,
  type = "link",
  se.fit = TRUE
)

pred_adjusted$prob <- plogis(pred_result$fit)

pred_adjusted$lower <- plogis(
  pred_result$fit - 1.96 * pred_result$se.fit
)

pred_adjusted$upper <- plogis(
  pred_result$fit + 1.96 * pred_result$se.fit
)

# Plot
plot_adjusted <- ggplot() +
  geom_jitter(
    data = data,
    aes(x = Protein_intake__d1_4, y = AKI_any),
    height = 0.04,
    width = 0,
    alpha = 0.4
  ) +
  geom_ribbon(
    data = pred_adjusted,
    aes(
      x = Protein_intake__d1_4,
      ymin = lower,
      ymax = upper
    ),
    fill = "darkorange",
    alpha = 0.2
  ) +
  geom_line(
    data = pred_adjusted,
    aes(x = Protein_intake__d1_4, y = prob),
    color = "darkorange",
    linewidth = 1.2
  ) +
  labs(
    title = "Adjusted Logistic Regression",
    subtitle = "Confounders held constant at their mean values",
    x = "Protein intake (g/kg/day)",
    y = "Probability of AKI"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

plot_adjusted

# both plots



plot_unadjusted + plot_adjusted













# Sensitivity Analysis

model_unadjusted_sens <- glm(
  AKI_any_sensitivity_worst_case ~ Protein_intake__d1_4,
  data = data,
  family = binomial(link = "logit")
)

summary(model_unadjusted_sens)

model_adjusted_sens <- glm(
  AKI_any_sensitivity_worst_case ~ Protein_intake__d1_4 +
    EuroScore2 +
    BMI +
    Duration_of_cardiopulmonary_bypass_minutes +
    SBP +
    days_onexclusive_oral_feeding,
  data = data,
  family = binomial(link = "logit")
)

summary(model_adjusted_sens)


# odds ratio models with confidence intervalls

exp(cbind(
  OR = coef(model_unadjusted),
  confint(model_unadjusted)
))

exp(cbind(
  OR = coef(model_adjusted),
  confint(model_adjusted)
))

# relation oral feedin protein intake

cor(
  data$Protein_intake__d1_4,
  data$days_onexclusive_oral_feeding,
  use = "complete.obs",
  method = "pearson"
)




# schrittweise adjustierung

model_1 <- glm(
  AKI_any ~ Protein_intake__d1_4,
  data = data,
  family = binomial
)

model_2 <- glm(
  AKI_any ~ Protein_intake__d1_4 +
    EuroScore2 + BMI,
  data = data,
  family = binomial
)

model_3 <- glm(
  AKI_any ~ Protein_intake__d1_4 +
    EuroScore2 + BMI +
    Duration_of_cardiopulmonary_bypass_minutes +
    SBP,
  data = data,
  family = binomial
)

model_4 <- glm(
  AKI_any ~ Protein_intake__d1_4 +
    EuroScore2 + BMI +
    Duration_of_cardiopulmonary_bypass_minutes +
    SBP +
    days_onexclusive_oral_feeding,
  data = data,
  family = binomial
)






