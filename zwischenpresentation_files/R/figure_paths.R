# Figure names used by presentation.qmd.
# PNG/SVG figures are generated locally by your team; no patient-level data here.
figure_files <- c(
  dag            = "assets/dag_artificial_protein.svg",
  flow           = "assets/patient_flow.png",
  density        = "assets/protein_density.png",
  confounders    = "assets/confounders_eda.png",
  forest         = "assets/main_forest.png",
  sensitivity    = "assets/sensitivity_forest.png",
  best_worst     = "assets/best_worst_forest.png",
  oral           = "assets/oral_feeding_forest.png",
  multinomial    = "assets/multinomial_forest.png",
  interpretation = "assets/interpretation.png",
  subgroup       = "assets/subgroup_forest.png",
  logit          = "assets/adjusted model logitr.png"
)

# Use this only to SHOW a graph (reads an existing file or a labelled fallback).
# For ggsave(), use the explicit PNG path above; don't call plot_path() for output.
plot_path <- function(key) {
  if (!key %in% names(figure_files)) stop("Unknown figure key: ", key)
  path <- unname(figure_files[[key]])
  if (file.exists(path)) return(path)

  dir.create("assets", recursive = TRUE, showWarnings = FALSE)
  fallback <- file.path("assets", paste0("missing_", key, ".svg"))
  titles <- c(
    dag = "Causal DAG",
    flow = "Patient selection flowchart",
    density = "Protein intake distribution",
    confounders = "EDA: associations with potential confounders",
    forest = "Logistic regression and odds ratios",
    sensitivity = "Sensitivity analyses",
    best_worst = "Best-case versus worst-case forest plot",
    oral = "Oral feeding forest plot",
    multinomial = "Multinomial regression",
    interpretation = "Interpretation and limitations",
    subgroup = "Exploratory effect modification"
  )
  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x
  }
  svg <- c(
    '<svg xmlns="http://www.w3.org/2000/svg" width="1100" height="430" viewBox="0 0 1100 430">',
    '<rect x="12" y="12" width="1076" height="406" rx="20" fill="#f2f6fa" stroke="#b6c8d7" stroke-width="2"/>',
    paste0('<text x="550" y="132" text-anchor="middle" font-family="Arial" font-weight="bold" font-size="34" fill="#16253e">',esc(unname(titles[[key]])),'</text>'),
    '<text x="550" y="205" text-anchor="middle" font-family="Arial" font-size="28" fill="#9f3939">Figure not yet provided</text>',
    '<text x="550" y="263" text-anchor="middle" font-family="Arial" font-size="19" fill="#52627a">Save your exported figure to the expected path:</text>',
    paste0('<text x="550" y="300" text-anchor="middle" font-family="monospace" font-size="20" fill="#16253e">',esc(path),'</text>'),
    '<text x="550" y="367" text-anchor="middle" font-family="Arial" font-size="17" fill="#52627a">No patient results have been simulated or invented.</text>',
    '</svg>'
  )
  writeLines(svg, fallback, useBytes = TRUE)
  message("Missing figure '", key, "' at ", path, "; showing a labelled notice.")
  fallback
}
