library(haven)
library(dplyr)
library(ggplot2)
library(patchwork)

data <- read_sav("data/raw/Datenbank_Praktikum_FINAL.sav")

# Logistic Regression
# unadjusted model

model_unadjusted <- glm(
  AKI_any ~ Protein_intake__d1_4,
  data = data,
  family = binomial(link = "logit")
)

summary(model_unadjusted)

# adjusted model
model_adjusted<- glm(
    AKI_any ~ Protein_intake__d1_4 +
    EuroScore2 +
    BMI +
    Duration_of_cardiopulmonary_bypass_minutes +
    SBP +
    days_onexclusive_oral_feeding,
  data = data,
  family = binomial(link = "logit")
)

summary(model_adjusted)

# Plots

# Unadjusted Model

ggplot(data, aes(x = Protein_intake__d1_4, y = AKI_any)) +
  geom_jitter(
    width = 0,
    height = 0.04,
    alpha = 0.4,
    size = 2
  ) +
  stat_smooth(
    method = "glm",
    method.args = list(family = binomial(link = "logit")),
    se = TRUE,
    color = "blue",
    linewidth = 1
  ) +
  labs(
    title = "Unadjusted association between protein intake and AKI",
    x = "Average artificial protein intake (g/kg/day)",
    y = "Probability of AKI"
  ) +
  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    limits = c(-0.05, 1.05)
  ) +
  theme_minimal()




# adjusted model


# Prediction data
prediction_data <- data.frame(
  Protein_intake__d1_4 = seq(
    min(data$Protein_intake__d1_4, na.rm = TRUE),
    max(data$Protein_intake__d1_4, na.rm = TRUE),
    length.out = 100
  ),
  EuroScore2 = mean(data$EuroScore2, na.rm = TRUE),
  BMI = mean(data$BMI, na.rm = TRUE),
  Duration_of_cardiopulmonary_bypass_minutes =
    mean(data$Duration_of_cardiopulmonary_bypass_minutes, na.rm = TRUE),
  SBP = mean(data$SBP, na.rm = TRUE),
  days_onexclusive_oral_feeding =
    mean(data$days_onexclusive_oral_feeding, na.rm = TRUE)
)

# Predictions on the log-odds scale, including standard errors
pred <- predict(
  model_adjusted,
  newdata = prediction_data,
  type = "link",
  se.fit = TRUE
)

# Convert predictions and confidence limits to probabilities
prediction_data$predicted <- plogis(pred$fit)
prediction_data$lower <- plogis(pred$fit - 1.96 * pred$se.fit)
prediction_data$upper <- plogis(pred$fit + 1.96 * pred$se.fit)

# Plot
ggplot() +
  geom_jitter(
    data = data,
    aes(x = Protein_intake__d1_4, y = AKI_any),
    width = 0,
    height = 0.04,
    alpha = 0.35,
    size = 2
  ) +
  geom_ribbon(
    data = prediction_data,
    aes(
      x = Protein_intake__d1_4,
      ymin = lower,
      ymax = upper
    ),
    fill = "grey30",
    alpha = 0.2
  ) +
  geom_line(
    data = prediction_data,
    aes(x = Protein_intake__d1_4, y = predicted),
    color = "blue",
    linewidth = 1
  ) +
  labs(
    title = "Adjusted association between protein intake and AKI",
    subtitle = "Confounders held constant at their mean values",
    x = "Average artificial protein intake (g/kg/day)",
    y = "Predicted probability of AKI"
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
  ) +
  theme_minimal()






