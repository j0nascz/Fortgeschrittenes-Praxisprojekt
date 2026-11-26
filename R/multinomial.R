# =============================================================================
# 04_multinomial.R
# Multinomial logistic regression: protein intake (days 1-4) and AKI after day 4
#
# Why multinomial? Patients who die in the ICU without AKI can no longer
# develop AKI (competing risk). Counting them as "no AKI" would make them look
# like healthy survivors. So we use 3 outcome groups instead of 2:
#   "No AKI, survived" (reference) | "AKI" | "Died without AKI"
# The model gives one OR per group compared with the reference group.
#
# Exposures are modelled per 0.1 g/kg/day and fitted in separate models
# (they are highly correlated, Spearman r = 0.91).
#
# Run from the project root. Tables go to output/, figures to figures/.
# =============================================================================

source("R/helpers.R")
source("R/data_prep.R")


# -----------------------------------------------------------------------------
# 1) Outcome groups and exposures for this analysis
# -----------------------------------------------------------------------------
grp_levels <- c("No AKI, survived", "AKI", "Died without AKI")

# Outcome A: any new AKI after day 4
dat$group_A <- factor(
  ifelse(dat$AKI_new == 1, "AKI",
         ifelse(dat$died == 1, "Died without AKI", "No AKI, survived")),
  levels = grp_levels)

# Outcome B: AKI on day 5 or 6.
# "Died without AKI" = died without ANY AKI, because the death date is unknown.
# Patients with AKI only on day 7+ stay in the reference group.
dat$group_B <- factor(
  ifelse(dat$AKI_on_day_5_or_6 == 1, "AKI",
         ifelse(dat$AKI_new == 0 & dat$died == 1, "Died without AKI", "No AKI, survived")),
  levels = grp_levels)

cat("Outcome A groups:\n"); print(table(dat$group_A, useNA = "ifany"))
cat("Outcome B groups:\n"); print(table(dat$group_B, useNA = "ifany"))

# Protein per 0.1 g/kg/day (OR = change in odds per 0.1 g/kg/day more protein)
dat$protein_avg_d1_4 <- dat$Protein_intake__d1_4 * 10
dat$protein_day4     <- dat$Protein_intake_per_kg_on_day4 * 10

# Confounders from the protocol (all in every adjusted model)
confounders <- paste(cov_full, "+ days_onexclusive_oral_feeding")


# -----------------------------------------------------------------------------
# 2) Function: fit one multinomial model, return the exposure ORs
#    nnet::multinom fits one logistic equation per non-reference group.
#    CIs are Wald CIs: exp(estimate +/- 1.96 * SE).
# -----------------------------------------------------------------------------
fit_multinom_or <- function(outcome, expo, adjusted, data) {
  rhs <- if (adjusted) paste(expo, confounders, sep = " + ") else expo
  m   <- multinom(as.formula(paste(outcome, "~", rhs)), data = data,
                  trace = FALSE, maxit = 500)

  est <- coef(m)[, expo]                         # one estimate per group
  se  <- summary(m)$standard.errors[, expo]
  data.frame(Group = names(est),
             OR    = exp(est),
             lower = exp(est - 1.96 * se),
             upper = exp(est + 1.96 * se),
             p     = 2 * pnorm(-abs(est / se)),
             n     = nrow(fitted(m)),            # patients used (complete cases)
             row.names = NULL)
}


# -----------------------------------------------------------------------------
# 3) All models: 2 outcomes x 2 exposures x crude / adjusted
# -----------------------------------------------------------------------------
analyses <- list(
  list(name = "A: AKI after day 4 | avg protein d1-4", outcome = "group_A", expo = "protein_avg_d1_4"),
  list(name = "A: AKI after day 4 | protein day 4",    outcome = "group_A", expo = "protein_day4"),
  list(name = "B: AKI day 5-6 | avg protein d1-4",     outcome = "group_B", expo = "protein_avg_d1_4"),
  list(name = "B: AKI day 5-6 | protein day 4",        outcome = "group_B", expo = "protein_day4")
)

results <- do.call(rbind, lapply(analyses, function(a) {
  rbind(
    cbind(Analysis = a$name, Model = "Crude",
          fit_multinom_or(a$outcome, a$expo, adjusted = FALSE, data = dat)),
    cbind(Analysis = a$name, Model = "Adjusted",
          fit_multinom_or(a$outcome, a$expo, adjusted = TRUE,  data = dat))
  )
}))

results$Analysis <- factor(results$Analysis, levels = rev(sapply(analyses, `[[`, "name")))
results$Model    <- factor(results$Model, levels = c("Crude", "Adjusted"))
results$Group    <- factor(results$Group, levels = grp_levels[-1])


# -----------------------------------------------------------------------------
# 4) Table: one row per analysis and model, one column per outcome group
# -----------------------------------------------------------------------------
results$Text <- fmt_or(results$OR, results$lower, results$upper)

table_or <- reshape(results[, c("Analysis", "Model", "Group", "Text")],
                    idvar = c("Analysis", "Model"), timevar = "Group", direction = "wide")
names(table_or) <- sub("^Text\\.", "", names(table_or))
table_or <- table_or[order(-as.integer(table_or$Analysis), table_or$Model), ]

cat("\nOR per 0.1 g/kg/day protein (95% CI), reference = No AKI, survived:\n")
print(table_or, row.names = FALSE)
write.csv(table_or, "output/multinomial_protein_or.csv", row.names = FALSE)


# -----------------------------------------------------------------------------
# 5) Full primary model (Outcome A, avg protein d1-4, adjusted):
#    ORs of all covariates, to see what drives AKI and death
# -----------------------------------------------------------------------------
m_primary <- multinom(as.formula(paste("group_A ~ protein_avg_d1_4 +", confounders)),
                      data = dat, trace = FALSE, maxit = 500)

est <- coef(m_primary)
se  <- summary(m_primary)$standard.errors
primary_full <- data.frame(
  Group    = rep(rownames(est), times = ncol(est)),
  Variable = rep(colnames(est), each = nrow(est)),
  `OR (95% CI)` = fmt_or(exp(as.vector(est)),
                         exp(as.vector(est - 1.96 * se)),
                         exp(as.vector(est + 1.96 * se))),
  p = sprintf("%.3f", 2 * pnorm(-abs(as.vector(est / se)))),
  check.names = FALSE)
primary_full <- primary_full[primary_full$Variable != "(Intercept)", ]

cat("\nPrimary model, all covariates:\n")
print(primary_full, row.names = FALSE)
write.csv(primary_full, "output/multinomial_primary_full_model.csv", row.names = FALSE)


# -----------------------------------------------------------------------------
# 6) Figure: forest plot, crude vs adjusted, one panel per outcome group
# -----------------------------------------------------------------------------
p_multinom <- ggplot(results, aes(x = OR, y = Analysis, colour = Model)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.25, orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_point(size = 3.5, position = position_dodge(width = 0.5)) +
  facet_wrap(~ Group) +
  scale_x_log10() +
  scale_colour_manual(values = c("Crude" = col_red, "Adjusted" = col_blue)) +
  labs(x = "Odds ratio per 0.1 g/kg/day protein (95% CI, log scale)", y = NULL, colour = NULL,
       title = "Multinomial regression: protein intake and AKI",
       subtitle = "Reference group: survived without AKI") +
  theme(legend.position = "bottom")

print(p_multinom)
save_slide(p_multinom, "multinomial_protein_or", width = 12, height = 6.5)


message("Done. Figures saved in: ", normalizePath("figures"),
        "\nTables saved in: ", normalizePath("output"))
