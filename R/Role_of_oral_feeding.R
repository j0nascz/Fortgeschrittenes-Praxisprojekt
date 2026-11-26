# =============================================================================
# 03_oral_feeding.R
# Role of oral feeding
#
# The days on exclusive oral feeding (days 1-4) are a marker of clinical
# condition: patients who are able to eat normally early on are more stable,
# do not require artificial nutrition and therefore receive hardly any
# artificial protein. This script shows that oral feeding is associated
# with the patient's state of health, and how strongly it affects the
# results for protein intake.
#
# Run from the project root (open Fortgeschrittenes-Praxisprojekt.Rproj first).
# Figures are saved to figures/.
# Packages, helpers and data come from R/helpers.R and R/data_prep.R.
# =============================================================================

source("R/helpers.R")
source("R/data_prep.R")


# -----------------------------------------------------------------------------
# 1) Figure: AKI and death by days on oral feeding
# -----------------------------------------------------------------------------
oral_long <- do.call(rbind, lapply(split(dat, dat$days_onexclusive_oral_feeding), function(g) {
  data.frame(
    Days       = g$days_onexclusive_oral_feeding[1],
    Outcome    = c("New AKI", "Death in ICU"),
    Proportion = c(mean(g$AKI_new, na.rm = TRUE), mean(g$died, na.rm = TRUE)))
}))

p_oral_outcomes <- ggplot(oral_long, aes(x = factor(Days), y = Proportion, fill = Outcome)) +
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.85) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("New AKI" = col_blue, "Death in ICU" = col_red)) +
  labs(x = "Days on exclusive oral feeding (days 1–4)", y = "Proportion of patients", fill = NULL,
       title = "AKI and death by days on oral feeding") +
  theme(legend.position = "bottom")

print(p_oral_outcomes)
save_slide(p_oral_outcomes, "aki_death_by_oral_feeding")


# -----------------------------------------------------------------------------
# 2) Comparison: not adjusted vs. adjusted for oral feeding
#    If the two estimates differ clearly, oral feeding (or the clinical
#    condition behind it) explains the association between protein intake
#    and AKI.
#    cov_full, cov_small and fit_or() come from R/helpers.R.
# -----------------------------------------------------------------------------
analyses <- list(
  list(name = "Primary: new AKI after day 4",      outcome = "AKI_new",           expo = "Protein_intake__d1_4",          firth = FALSE),
  list(name = "Secondary: AKI on day 5 or 6",      outcome = "AKI_on_day_5_or_6", expo = "Protein_intake__d1_4",          firth = TRUE),
  list(name = "Worst case: AKI or death",          outcome = "AKI_worst",         expo = "Protein_intake__d1_4",          firth = FALSE),
  list(name = "Primary outcome, protein intake on day 4 only", outcome = "AKI_new", expo = "Protein_intake_per_kg_on_day4", firth = FALSE)
)

strategies <- c("Not adjusted for oral feeding",
                "Adjusted for oral feeding")

comparison <- do.call(rbind, lapply(analyses, function(a) {
  cov_all <- if (a$firth) cov_small else cov_full
  rbind(
    data.frame(Analysis = a$name, Strategy = strategies[1],
               t(fit_or(a$outcome, a$expo, cov_all, FALSE, dat, a$firth)), check.names = FALSE),
    data.frame(Analysis = a$name, Strategy = strategies[2],
               t(fit_or(a$outcome, a$expo, cov_all, TRUE, dat, a$firth)), check.names = FALSE)
  )
}))
comparison$Analysis <- factor(comparison$Analysis, levels = rev(sapply(analyses, `[[`, "name")))
comparison$Strategy <- factor(comparison$Strategy, levels = strategies)


# -----------------------------------------------------------------------------
# 3) Figure: comparison (forest plot)
# -----------------------------------------------------------------------------
p_comparison <- ggplot(comparison, aes(x = OR, y = Analysis, colour = Strategy)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25,
                 position = position_dodge(width = 0.5)) +
  geom_point(size = 3.5, position = position_dodge(width = 0.5)) +
  scale_x_log10() +
  scale_colour_manual(values = c(col_red, col_blue)) +
  guides(colour = guide_legend(nrow = 2)) +
  labs(x = "Odds ratio of protein intake per 1 g/kg (95% CI, log scale)", y = NULL, colour = NULL,
       title = "Effect of oral feeding on the protein–AKI association") +
  theme(legend.position = "bottom")

print(p_comparison)
save_slide(p_comparison, "oral_feeding_comparison", width = 11, height = 7)


message("Done. Figures saved in: ", normalizePath("figures"))
