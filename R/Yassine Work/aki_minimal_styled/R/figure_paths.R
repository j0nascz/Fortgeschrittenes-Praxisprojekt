# Update these paths to point to figures produced locally by your team.
# This script never creates or estimates patient-level results.
figure_files <- c(
  dag         = "assets/dag_artificial_protein.svg",
  flow        = "assets/patient_flow.png",
  density     = "assets/protein_density.png",
  forest      = "assets/main_forest.png",
  subgroup    = "assets/subgroup_forest.png",
  sensitivity = "assets/sensitivity_forest.png"
)

# Missing figures should not prevent the slideshow from rendering.
# Create an EXPLICITLY LABELLED message graphic, not a fictional plot.
plot_path <- function(key) {
  if (!key %in% names(figure_files)) stop("Unknown figure key: ", key)
  path <- unname(figure_files[[key]])
  if (file.exists(path)) return(path)
  dir.create("assets", showWarnings = FALSE, recursive = TRUE)
  fallback <- file.path("assets", paste0("missing_", key, ".svg"))
  title <- switch(key,
    dag="Causal DAG", flow="Patient selection flowchart",
    density="Protein intake distribution", forest="Main model estimates",
    subgroup="Subgroup comparison", sensitivity="Sensitivity analysis",
    key)
  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x
  }
  svg <- c(
    '<svg xmlns="http://www.w3.org/2000/svg" width="1100" height="430" viewBox="0 0 1100 430">',
    '<rect x="10" y="10" width="1080" height="410" rx="22" fill="#f2f6fa" stroke="#bacbd7" stroke-width="3"/>',
    paste0('<text x="550" y="132" text-anchor="middle" font-family="Arial" font-size="35" font-weight="bold" fill="#16253e">',esc(title),'</text>'),
    '<text x="550" y="203" text-anchor="middle" font-family="Arial" font-size="27" fill="#a13b37">Real project figure not found</text>',
    '<text x="550" y="263" text-anchor="middle" font-family="Arial" font-size="19" fill="#52627a">Update R/figure_paths.R to use your existing exported plot:</text>',
    paste0('<text x="550" y="300" text-anchor="middle" font-family="monospace" font-size="19" fill="#16253e">',esc(path),'</text>'),
    '<text x="550" y="370" text-anchor="middle" font-family="Arial" font-size="17" fill="#52627a">No patient results are inferred or simulated here.</text>',
    '</svg>'
  )
  writeLines(svg, fallback, useBytes = TRUE)
  message("Missing figure '", key, "' at ", path, "; rendering a clearly labeled notice instead.")
  fallback
}
