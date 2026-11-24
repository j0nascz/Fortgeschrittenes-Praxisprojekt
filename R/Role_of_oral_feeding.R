# =============================================================================
# 03_oral_feeding.R
# Role of oral feeding
#
# The days on exclusive oral feeding (days 1-4) are a marker of clinical
# condition: patients who are able to eat normally early on are more stable,
# do not require artificial nutrition and therefore receive hardly any
# artificial protein. This script shows (1) that oral feeding is associated
# with the patient's state of health, and (2) how strongly it affects the
# results for protein intake.
#
# Run from the project root (open Fortgeschrittenes-Praxisprojekt.Rproj first).
# Figures are saved to figures/, tables to output/.
# Packages, helpers and data come from R/helpers.R and R/data_prep.R.
# =============================================================================

source("R/helpers.R")
source("R/data_prep.R")

# Artificially fed patients only (0 days on exclusive oral feeding)
dat_artificial <- subset(dat, days_onexclusive_oral_feeding == 0)
cat(sprintf("Artificially fed subgroup: %d patients, %d with new AKI\n",
            nrow(dat_artificial), as.integer(sum(dat_artificial$AKI_new, na.rm = TRUE))))


# -----------------------------------------------------------------------------
# 1) Table: patient characteristics by days on exclusive oral feeding
# -----------------------------------------------------------------------------
oral_grp <- do.call(rbind, lapply(split(dat, dat$days_onexclusive_oral_feeding), function(g) {
  data.frame(
    `n`                     = nrow(g),
    `EuroSCORE II`          = sprintf("%.1f", median(g$EuroScore2, na.rm = TRUE)),
    `CPB duration (min)`    = sprintf("%.0f", median(g$Duration_of_cardiopulmonary_bypass_minutes, na.rm = TRUE)),
    `Median protein days 1-4` = sprintf("%.2f", median(g$Protein_intake__d1_4, na.rm = TRUE)),
    `New AKI`               = sprintf("%d (%.0f%%)", as.integer(sum(g$AKI_new, na.rm = TRUE)),
                                      100 * mean(g$AKI_new, na.rm = TRUE)),
    `Died in ICU`           = sprintf("%d (%.0f%%)", as.integer(sum(g$died, na.rm = TRUE)),
                                      100 * mean(g$died, na.rm = TRUE)),
    check.names = FALSE)
}))
oral_grp <- cbind(`Oral feeding days` = rownames(oral_grp), oral_grp)

print(oral_grp, row.names = FALSE)
write.csv(oral_grp, "output/oral_feeding_characteristics.csv", row.names = FALSE)


# -----------------------------------------------------------------------------
# 2) Figure: AKI and death by days on oral feeding
# -----------------------------------------------------------------------------
oral_long <- do.call(rbind, lapply(split(dat, dat$days_onexclusive_oral_feeding), function(g) {
  aki <- binom.test(sum(g$AKI_new, na.rm = TRUE), sum(!is.na(g$AKI_new)))$conf.int
  dth <- binom.test(sum(g$died,    na.rm = TRUE), sum(!is.na(g$died)))$conf.int
  rbind(
    data.frame(Days = g$days_onexclusive_oral_feeding[1], Outcome = "New AKI",
               Proportion = mean(g$AKI_new, na.rm = TRUE), lower = aki[1], upper = aki[2]),
    data.frame(Days = g$days_onexclusive_oral_feeding[1], Outcome = "Death in ICU",
               Proportion = mean(g$died, na.rm = TRUE), lower = dth[1], upper = dth[2]))
}))

p_oral_outcomes <- ggplot(oral_long, aes(x = factor(Days), y = Proportion, fill = Outcome)) +
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.85) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(0.8), width = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("New AKI" = col_blue, "Death in ICU" = col_red)) +
  labs(x = "Days on exclusive oral feeding (days 1–4)", y = "Proportion (95% CI)", fill = NULL,
       title = "AKI and death by days on oral feeding") +
  theme(legend.position = "bottom")

print(p_oral_outcomes)
save_slide(p_oral_outcomes, "aki_death_by_oral_feeding")


# -----------------------------------------------------------------------------
# 3) Table: oral feeding as a predictor of AKI and death (adjusted)
# -----------------------------------------------------------------------------
m_oral_aki <- glm(AKI_new ~ days_onexclusive_oral_feeding + log(EuroScore2) + BMI +
                    Duration_of_cardiopulmonary_bypass_minutes + SBP,
                  family = binomial, data = dat)
m_oral_death <- logistf(died ~ days_onexclusive_oral_feeding + log(EuroScore2) +
                          Duration_of_cardiopulmonary_bypass_minutes,
                        data = dat)

ci_a <- exp(confint.default(m_oral_aki)["days_onexclusive_oral_feeding", ])
i_d  <- which(names(coef(m_oral_death)) == "days_onexclusive_oral_feeding")

oral_predictor <- data.frame(
  Outcome = c("New AKI (logistic)", "Death in ICU (Firth)"),
  `OR per day of oral feeding` = c(
    sprintf("%.2f", exp(coef(m_oral_aki)["days_onexclusive_oral_feeding"])),
    sprintf("%.2f", exp(coef(m_oral_death)[i_d]))),
  `95% CI` = c(sprintf("%.2f-%.2f", ci_a[1], ci_a[2]),
               sprintf("%.2f-%.2f", exp(m_oral_death$ci.lower[i_d]), exp(m_oral_death$ci.upper[i_d]))),
  check.names = FALSE)

print(oral_predictor, row.names = FALSE)
write.csv(oral_predictor, "output/oral_feeding_as_predictor.csv", row.names = FALSE)


# -----------------------------------------------------------------------------
# 4) Comparison A / B / C
#    A: all patients, not adjusted for oral feeding
#    B: all patients, adjusted for oral feeding
#    C: artificially fed patients only (feeding route is the same for everyone)
#    If A differs clearly from B and C, oral feeding (or the clinical condition
#    behind it) explains the association between protein intake and AKI.
# -----------------------------------------------------------------------------
analyses <- list(
  list(name = "Primary: new AKI after day 4",      outcome = "AKI_new",           expo = "Protein_intake__d1_4",          firth = FALSE),
  list(name = "Secondary: AKI on day 5 or 6",      outcome = "AKI_on_day_5_or_6", expo = "Protein_intake__d1_4",          firth = TRUE),
  list(name = "Worst case: AKI or death",          outcome = "AKI_worst",         expo = "Protein_intake__d1_4",          firth = FALSE),
  list(name = "Exposure: protein intake on day 4", outcome = "AKI_new",           expo = "Protein_intake_per_kg_on_day4", firth = FALSE)
)

strategies <- c("A: not adjusted for oral feeding",
                "B: adjusted for oral feeding",
                "C: artificially fed patients only")

comparison <- do.call(rbind, lapply(analyses, function(a) {
  cov_all <- if (a$firth) cov_small else cov_full
  rbind(
    data.frame(Analysis = a$name, Strategy = strategies[1],
               t(fit_or(a$outcome, a$expo, cov_all, FALSE, dat, a$firth)), check.names = FALSE),
    data.frame(Analysis = a$name, Strategy = strategies[2],
               t(fit_or(a$outcome, a$expo, cov_all, TRUE, dat, a$firth)), check.names = FALSE),
    data.frame(Analysis = a$name, Strategy = strategies[3],
               t(fit_or(a$outcome, a$expo, cov_small, FALSE, dat_artificial, a$firth)), check.names = FALSE)
  )
}))
names(comparison)[3:6] <- c("OR", "lower", "upper", "Events")
comparison$Analysis <- factor(comparison$Analysis, levels = rev(sapply(analyses, `[[`, "name")))
comparison$Strategy <- factor(comparison$Strategy, levels = strategies)

# Wide table: one row per analysis, one column per strategy
comparison$Text <- fmt_or(comparison$OR, comparison$lower, comparison$upper)
wide <- reshape(comparison[, c("Analysis", "Strategy", "Text")],
                idvar = "Analysis", timevar = "Strategy", direction = "wide")
names(wide) <- sub("^Text\\.", "", names(wide))
wide <- wide[rev(seq_len(nrow(wide))), ]

print(wide, row.names = FALSE)
write.csv(wide, "output/oral_feeding_comparison.csv", row.names = FALSE)


# -----------------------------------------------------------------------------
# 5) Figure: comparison A / B / C (forest plot)
# -----------------------------------------------------------------------------
p_comparison <- ggplot(comparison, aes(x = OR, y = Analysis, colour = Strategy)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25,
                 position = position_dodge(width = 0.6)) +
  geom_point(size = 3.5, position = position_dodge(width = 0.6)) +
  scale_x_log10() +
  scale_colour_manual(values = c(col_red, col_blue, col_green)) +
  guides(colour = guide_legend(nrow = 3)) +
  labs(x = "Odds ratio of protein intake per 1 g/kg (95% CI, log scale)", y = NULL, colour = NULL,
       title = "Effect of oral feeding on the protein–AKI association") +
  theme(legend.position = "bottom")

print(p_comparison)
save_slide(p_comparison, "oral_feeding_comparison", width = 11, height = 7)


message("Done. Figures saved in: ", normalizePath("figures"),
        "\nTables saved in: ", normalizePath("output"))