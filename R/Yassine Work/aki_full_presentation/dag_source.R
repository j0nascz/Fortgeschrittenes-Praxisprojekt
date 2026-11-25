# Proposed DAG for the AKI project
# This is a CAUSAL HYPOTHESIS, not a fitted statistical model.
# The dots/lines do not imply statistical significance.
# ClinicalSeverity is deliberately unobserved: it is not in the data.
# The OralFeeding -> Protein direction is uncertain because both summaries
# cover ICU days 1-4. Revisit with your project partner.

library(dagitty)
library(ggdag)
library(ggplot2)

dag <- dagitty('dag {
  Protein [exposure]
  AKI [outcome]
  ClinicalSeverity [unobserved]

  EuroScore2 -> ClinicalSeverity
  BMI -> ClinicalSeverity
  CPB -> ClinicalSeverity
  SBP -> ClinicalSeverity

  EuroScore2 -> AKI
  BMI -> AKI
  CPB -> AKI
  SBP -> AKI

  ClinicalSeverity -> Protein
  ClinicalSeverity -> AKI
  ClinicalSeverity -> OralFeeding
  OralFeeding -> Protein
  Protein -> AKI
}')

coordinates(dag) <- list(
  x = c(EuroScore2=0, BMI=0, CPB=0, SBP=0,
        ClinicalSeverity=1.4, OralFeeding=2.1, Protein=3, AKI=4.2),
  y = c(EuroScore2=4, BMI=3, CPB=2, SBP=1,
        ClinicalSeverity=2.65, OralFeeding=0.8, Protein=1.8, AKI=1.8)
)

labels <- c(
  EuroScore2="EuroSCORE II", BMI="BMI",
  CPB="CPB duration", SBP="Minimum intraoperative SBP",
  ClinicalSeverity="Postoperative severity (unmeasured)",
  OralFeeding="Exclusive oral feeding, days 1-4",
  Protein="Artificial protein intake, days 1-4 / day 4",
  AKI="New AKI after day 4"
)

p <- ggdag(dag, text=FALSE, use_labels="name") +
  theme_dag() + ggtitle("Proposed DAG: artificial protein intake and later AKI")
print(p)

# There is an unmeasured common cause of protein and AKI.
# Observe what dagitty says about identification, and DO NOT treat
# measured covariates as necessarily sufficient to close backdoor paths.
print(adjustmentSets(dag, exposure="Protein", outcome="AKI", type="minimal"))

# Project partner's proposed *pragmatic* adjusted model (not guaranteed
# to remove all confounding): EuroScore2 + BMI + CPB + SBP + OralFeeding.
# OralFeeding may be co-determined with protein over the same interval.
# Avoid claiming its coefficient is causally identified.
