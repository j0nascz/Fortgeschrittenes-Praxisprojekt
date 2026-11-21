library(haven)
library(dplyr)
library(ggplot2)

data <- read_sav("data/raw/Datenbank_Praktikum_FINAL.sav")

# A: density plots

# 1. Euroscore

ggplot(data, aes(x = EuroScore2)) +
  geom_density(fill = "steelblue", alpha = 0.4, na.rm = TRUE) +
  labs(
    title = "Distribution of EuroScore2",
    x = "EuroScore2",
    y = "Density"
  ) +
  theme_minimal()

ggplot(data, aes(x = EuroScore2)) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white",
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of EuroScore2",
    x = "EuroScore2",
    y = "Number of patients"
  ) +
  theme_minimal()

# 2. BMI

ggplot(data, aes(x = BMI)) +
  geom_density(fill = "steelblue", alpha = 0.4, na.rm = TRUE) +
  labs(
    title = "Distribution of BMI",
    x = "BMI (kg/m²)",
    y = "Density"
  ) +
  theme_minimal()

ggplot(data, aes(x = BMI)) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white",
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of BMI",
    x = "BMI (kg/m²)",
    y = "Number of patients"
  ) +
  theme_minimal()

# 3. Duration of cardioplulmonary bypass

ggplot(data, aes(x = Duration_of_cardiopulmonary_bypass_minutes)) +
  geom_density(fill = "steelblue", alpha = 0.4, na.rm = TRUE) +
  labs(
    title = "Distribution of Cardiopulmonary Bypass Duration",
    x = "Duration (minutes)",
    y = "Density"
  ) +
  theme_minimal()

ggplot(data, aes(x = Duration_of_cardiopulmonary_bypass_minutes)) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white",
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of Cardiopulmonary Bypass Duration",
    x = "Duration (minutes)",
    y = "Number of patients"
  ) +
  theme_minimal()

# 4. SBP

ggplot(data, aes(x = SBP)) +
  geom_density(fill = "steelblue", alpha = 0.4, na.rm = TRUE) +
  labs(
    title = "Distribution of Systolic Blood Pressure",
    x = "SBP (mmHg)",
    y = "Density"
  ) +
  theme_minimal()

ggplot(data, aes(x = SBP)) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white",
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of Systolic Blood Pressure",
    x = "SBP (mmHg)",
    y = "Number of patients"
  ) +
  theme_minimal()

# 6. Protein_intake__d1_4

ggplot(data, aes(x = Protein_intake__d1_4)) +
  geom_density(
    fill = "steelblue",
    alpha = 0.4,
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of Protein Intake (Days 1–4)",
    x = "Average artificial protein intake (g/kg/day)",
    y = "Density"
  ) +
  theme_minimal()


ggplot(data, aes(x = Protein_intake__d1_4)) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white",
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of Protein Intake (Days 1–4)",
    x = "Average artificial protein intake (g/kg/day)",
    y = "Number of patients"
  ) +
  theme_minimal()

# B. Histogram – exclusive oral feeding days

# 5. Days of exclusive oral feeding

ggplot(data, aes(x = days_onexclusive_oral_feeding)) +
  geom_histogram(
    binwidth = 1,
    boundary = -0.5,
    fill = "steelblue",
    color = "white",
    na.rm = TRUE
  ) +
  scale_x_continuous(breaks = 0:4) +
  labs(
    title = "Distribution of Days with Exclusive Oral Feeding",
    x = "Number of days",
    y = "Number of patients"
  ) +
  theme_minimal()


# C. Bar charts – binary outcome variables
# 6. AKI_any

ggplot(data, aes(x = factor(AKI_any))) +
  geom_bar(fill = "steelblue", na.rm = TRUE) +
  scale_x_discrete(
    breaks = c("0", "1"),
    labels = c("No AKI", "AKI")
  ) +
  labs(
    title = "Occurrence of AKI",
    x = "AKI status",
    y = "Number of patients"
  ) +
  theme_minimal()

# 7. AKI_any_early

ggplot(data, aes(x = factor(AKI_any_early))) +
  geom_bar(fill = "steelblue", na.rm = TRUE) +
  scale_x_discrete(
    breaks = c("0", "1"),
    labels = c("No early AKI", "Early AKI")
  ) +
  labs(
    title = "Occurrence of Early AKI",
    x = "Early AKI status",
    y = "Number of patients"
  ) +
  theme_minimal()


# Plots protein intake per group

# Protein_intake__d1_4 per days_onexclusive_oral_feeding
plot(
  data$days_onexclusive_oral_feeding,
  data$Protein_intake__d1_4,
  xlab = "Days of exclusive oral feeding",
  ylab = "Average artificial protein intake",
  main = "Protein intake vs. oral feeding"
)

# Scatterplot
plot(
  data$days_onexclusive_oral_feeding,
  data$Protein_intake__d1_4,
  xlab = "Days of exclusive oral feeding",
  ylab = "Average artificial protein intake (g/kg/day)",
  main = "Protein intake vs. oral feeding",
  xaxt = "n",
  pch = 16,
  col = rgb(0, 0, 0, 0.25)
)

axis(1, at = 0:4)

# Overlay boxplots
boxplot(
  Protein_intake__d1_4 ~ days_onexclusive_oral_feeding,
  data = data,
  add = TRUE,
  at = 0:4,
  boxwex = 0.35,
  outline = FALSE,
  col = rgb(0, 0.4, 0.8, 0.25),
  border = "blue"
)

# Protein_intake__d1_4 per ES2_Quart
plot(
  data$ES2_Quart,
  data$Protein_intake__d1_4,
  xlab = "Days of exclusive oral feeding",
  ylab = "Average artificial protein intake",
  main = "Protein intake vs. oral feeding"
)



# Protein_intake__d1_4 per BMI_Quart,
plot(
  data$BMI_Quart,
  data$Protein_intake__d1_4,
  xlab = "Days of exclusive oral feeding",
  ylab = "Average artificial protein intake",
  main = "Protein intake vs. oral feeding"
)

# Protein_intake__d1_4 per Duration_of_CPB_Quart,
plot(
  data$Duration_of_CPB_Quart,
  data$Protein_intake__d1_4,
  xlab = "Days of exclusive oral feeding",
  ylab = "Average artificial protein intake",
  main = "Protein intake vs. oral feeding"
)

# Protein_intake__d1_4 per SBP_Quart
plot(
  data$SBP_Quart,
  data$Protein_intake__d1_4,
  xlab = "Days of exclusive oral feeding",
  ylab = "Average artificial protein intake",
  main = "Protein intake vs. oral feeding"
)

# Set up 2x2 plotting layout
par(mfrow = c(2, 2))

# 1. Protein intake per EuroScore2 quartile
plot(
  data$ES2_Quart,
  data$Protein_intake__d1_4,
  xlab = "EuroScore2 quartile",
  ylab = "Average artificial protein intake (g/kg/day)",
  main = "Protein intake vs. EuroScore2",
  xaxt = "n",
  pch = 16,
  col = rgb(0, 0, 0, 0.25)
)

axis(1, at = 1:4, labels = 1:4)

boxplot(
  Protein_intake__d1_4 ~ ES2_Quart,
  data = data,
  add = TRUE,
  boxwex = 0.35,
  outline = FALSE,
  col = rgb(0, 0.4, 0.8, 0.25),
  border = "blue"
)


# 2. Protein intake per BMI quartile
plot(
  data$BMI_Quart,
  data$Protein_intake__d1_4,
  xlab = "BMI quartile",
  ylab = "Average artificial protein intake (g/kg/day)",
  main = "Protein intake vs. BMI",
  xaxt = "n",
  pch = 16,
  col = rgb(0, 0, 0, 0.25)
)

axis(1, at = 1:4, labels = 1:4)

boxplot(
  Protein_intake__d1_4 ~ BMI_Quart,
  data = data,
  add = TRUE,
  boxwex = 0.35,
  outline = FALSE,
  col = rgb(0, 0.4, 0.8, 0.25),
  border = "blue"
)


# 3. Protein intake per CPB duration quartile
plot(
  data$Duration_of_CPB_Quart,
  data$Protein_intake__d1_4,
  xlab = "CPB duration quartile",
  ylab = "Average artificial protein intake (g/kg/day)",
  main = "Protein intake vs. CPB duration",
  xaxt = "n",
  pch = 16,
  col = rgb(0, 0, 0, 0.25)
)

axis(1, at = 1:4, labels = 1:4)

boxplot(
  Protein_intake__d1_4 ~ Duration_of_CPB_Quart,
  data = data,
  add = TRUE,
  boxwex = 0.35,
  outline = FALSE,
  col = rgb(0, 0.4, 0.8, 0.25),
  border = "blue"
)


# 4. Protein intake per SBP quartile
plot(
  data$SBP_Quart,
  data$Protein_intake__d1_4,
  xlab = "SBP quartile",
  ylab = "Average artificial protein intake (g/kg/day)",
  main = "Protein intake vs. SBP",
  xaxt = "n",
  pch = 16,
  col = rgb(0, 0, 0, 0.25)
)

axis(1, at = 1:4, labels = 1:4)

boxplot(
  Protein_intake__d1_4 ~ SBP_Quart,
  data = data,
  add = TRUE,
  boxwex = 0.35,
  outline = FALSE,
  col = rgb(0, 0.4, 0.8, 0.25),
  border = "blue"
)

# Reset plotting layout
par(mfrow = c(1, 1))






