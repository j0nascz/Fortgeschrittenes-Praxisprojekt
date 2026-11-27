# ==========================================
# 1. OUTPUT FOLDER
# ==========================================

project_root <- getwd()

while (!length(list.files(project_root, pattern = "\\.Rproj$"))) {
  parent <- dirname(project_root)
  
  if (identical(parent, project_root)) {
    stop("Cannot find project root. Open your .Rproj project first.")
  }
  
  project_root <- parent
}

output_dir <- file.path(project_root, "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ==========================================
# 2. DRAWING FUNCTIONS
# ==========================================

box <- function(x, y, w, h,
                fill = "#F3F4F6",
                border = "#666666") {
  
  grid.roundrect(
    x = unit(x, "native"),
    y = unit(y, "native"),
    width = unit(w, "native"),
    height = unit(h, "native"),
    r = unit(4, "mm"),
    gp = gpar(fill = fill, col = border, lwd = 2)
  )
}

txt <- function(text, x, y, size = 17,
                bold = FALSE, color = "#111111") {
  
  grid.text(
    text,
    x = unit(x, "native"),
    y = unit(y, "native"),
    gp = gpar(
      fontsize = size,
      fontfamily = "sans",
      fontface = if (bold) "bold" else "plain",
      col = color,
      lineheight = 1.3
    )
  )
}

arr <- function(x0, y0, x1, y1) {
  
  grid.lines(
    x = unit(c(x0, x1), "native"),
    y = unit(c(y0, y1), "native"),
    arrow = grid::arrow(
      type = "closed",
      length = unit(3, "mm")
    ),
    gp = gpar(
      col = "#333333",
      fill = "#333333",
      lwd = 2.3
    )
  )
}

# ==========================================
# 3. CENTERED HORIZONTAL FLOWCHART
# ==========================================

draw_flowchart <- function() {
  
  grid.newpage()
  
  pushViewport(viewport(
    xscale = c(0, 250),
    yscale = c(0, 90)
  ))
  
  grid.rect(gp = gpar(fill = "white", col = NA))
  
  # BOX 1 — POOLING
  box(22, 45, 36, 25)
  
  txt("Pooling of", 22, 51, size = 19, bold = TRUE)
  txt("treatment arms", 22, 46, size = 19, bold = TRUE)
  txt("Selenium + placebo", 22, 39, size = 15)
  
  # BOX 2 — ELIGIBILITY
  box(76, 45, 56, 25)
  
  txt("Eligibility criteria", 76, 53, size = 20, bold = TRUE)
  txt("ICU stay > 4 days", 76, 47.5, size = 16)
  txt("Creatinine measurement available", 76, 43, size = 15)
  txt("Protein documented on ≥2", 76, 38.5, size = 14)
  txt("of ICU days 1–4", 76, 35, size = 14)
  
  # BOX 3 — RENAL EXCLUSIONS
  box(133, 45, 40, 25)
  
  txt("Renal exclusions", 133, 52, size = 19, bold = TRUE)
  txt("Preoperative CrCl < 50", 133, 45, size = 15)
  txt("Pre-existing dialysis", 133, 40, size = 15)
  
  # BOX 4 — ANALYSIS POPULATION
  box(
    176, 45, 34, 25,
    fill = "#E8F3EB",
    border = "#00883A"
  )
  
  txt("Final analysis", 176, 48, size = 19, bold = TRUE)
  txt("population", 176, 42, size = 19, bold = TRUE)
  
  # OUTCOME 1 — AKI
  box(
    227, 61, 40, 16,
    fill = "#E8F3EB",
    border = "#00883A"
  )
  
  txt("New AKI", 227, 64, size = 19, bold = TRUE)
  txt("after ICU day 4", 227, 58, size = 15)
  
  # OUTCOME 2 — NO AKI
  box(227, 29, 40, 16)
  
  txt(
    "No observed new AKI",
    227, 32, size = 17, bold = TRUE
  )
  
  txt(
    "Includes ICU deaths without AKI",
    227, 26, size = 13
  )
  
  # STRAIGHT ARROWS — CONNECTED TO BOX EDGES
  arr(40, 45, 48, 45)
  arr(104, 45, 113, 45)
  arr(153, 45, 159, 45)
  
  # BRANCH TO AKI / NO AKI
  arr(193, 50, 207, 61)
  arr(193, 40, 207, 29)
  
  popViewport()
}

# ==========================================
# 4. SAVE PNG AND SVG
# ==========================================

png_path <- file.path(output_dir, "patient_flow.png")
svg_path <- file.path(output_dir, "patient_flow.svg")

png(
  png_path,
  width = 3200,
  height = 1152,
  res = 200,
  bg = "white"
)

draw_flowchart()
dev.off()

svg(
  svg_path,
  width = 16,
  height = 5.76,
  bg = "white"
)

draw_flowchart()
dev.off()

cat(
  "Flowchart saved to:\n",
  png_path, "\n",
  svg_path, "\n"
)
#==================================================================================#
# Use your already-loaded dataset
stopifnot(
  is.data.frame(data),
  "Protein_intake__d1_4" %in% names(data)
)

# Prepare protein intake values
protein <- data$Protein_intake__d1_4
protein <- protein[is.finite(protein)]

if (length(protein) == 0 || any(protein < 0)) {
  stop("Protein intake contains no valid values or negative values.")
}

plot_data <- data.frame(protein = protein)

# Calculate descriptive statistics
n <- length(protein)
n_zero <- sum(protein == 0)
pct_zero <- 100 * n_zero / n

# Create plot
p <- ggplot(plot_data, aes(x = protein)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 25,
    fill = "#B8DDE5",
    color = "white",
    alpha = 0.75
  ) +
  {
    if (length(unique(protein)) > 1) {
      geom_density(
        color = "#007E87",
        linewidth = 1.2,
        adjust = 1.2
      )
    }
  } +
  annotate(
    "label",
    x = Inf,
    y = Inf,
    label = sprintf(
      "N = %d\nZero intake: %d (%.1f%%)",
      n, n_zero, pct_zero
    ),
    hjust = 1.05,
    vjust = 1.2,
    size = 4.5,
    fill = "white"
  ) +
  labs(
    x = "Average artificial protein intake (verify units)",
    y = "Density"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 19,
      color = "#10274B"
    ),
    plot.subtitle = element_text(color = "#5F7799"),
    panel.grid.minor = element_blank()
  )

# Show plot in RStudio
print(p)

# Save figure to your project's figures folder
ggsave(
  filename = file.path(output_dir, "protein_density.png"),
  plot = p,
  width = 11,
  height = 6,
  dpi = 300,
  bg = "white"
)

cat("Figure saved to:",
    normalizePath(file.path(output_dir, "protein_density.png")), "\n")

# ==========================================
# 1. SELECT VARIABLES
# ==========================================

covariates <- c(
  EuroScore2 = "EuroSCORE II",
  BMI = "BMI",
  Duration_of_cardiopulmonary_bypass_minutes = "CPB duration",
  SBP = "Minimum intraoperative SBP",
  days_onexclusive_oral_feeding = "Days on exclusive oral feeding"
)

exposure <- "Protein_intake__d1_4"
outcome <- "AKI_any"

stopifnot(
  is.data.frame(data),
  all(c(names(covariates), exposure, outcome) %in% names(data))
)

# Check binary AKI coding
stopifnot(
  all(is.na(data[[outcome]]) |
        data[[outcome]] %in% c(0, 1))
)

# ==========================================
# 2. CALCULATE SPEARMAN CORRELATIONS
# ==========================================

safe_cor <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]
  y <- y[keep]
  
  if (length(x) < 3 ||
      length(unique(x)) < 2 ||
      length(unique(y)) < 2) {
    return(NA_real_)
  }
  
  cor(x, y, method = "spearman")
}

results <- do.call(rbind, lapply(names(covariates), function(v) {
  
  data.frame(
    Variable = covariates[[v]],
    Target = c("Artificial Protein intake", "AKI"),
    Correlation = c(
      safe_cor(data[[v]], data[[exposure]]),
      safe_cor(data[[v]], data[[outcome]])
    )
  )
}))

results$Variable <- factor(
  results$Variable,
  levels = rev(unname(covariates))
)

results$Target <- factor(
  results$Target,
  levels = c("Artificial Protein intake", "AKI")
)

results$Label <- ifelse(
  is.na(results$Correlation),
  "NA",
  sprintf("%+.2f", results$Correlation)
)

# ==========================================
# 3. CREATE HEATMAP
# ==========================================

p_conf <- ggplot(
  results,
  aes(x = Target, y = Variable, fill = Correlation)
) +
  geom_tile(
    color = "white",
    linewidth = 2,
    width = 0.95,
    height = 0.85
  ) +
  geom_text(
    aes(label = Label),
    size = 6,
    fontface = "bold",
    color = "#10274B"
  ) +
  scale_fill_gradient2(
    low = "#E7A09B",
    mid = "#F5F7FA",
    high = "#6EC6BE",
    midpoint = 0,
    limits = c(-1, 1),
    na.value = "#E5E7EB",
    name = "Spearman rho"
  ) +
  labs(
    x = NULL,
    y = NULL,
    caption = paste(
      "Spearman rank correlations.",
      "Associations alone do not establish confounding."
    )
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 20,
      color = "#10274B"
    ),
    plot.subtitle = element_text(
      size = 13,
      color = "#475569"
    ),
    axis.text = element_text(
      size = 14,
      color = "#10274B"
    ),
    axis.text.x = element_text(face = "bold"),
    panel.grid = element_blank(),
    legend.position = "right",
    plot.caption = element_text(
      size = 10,
      hjust = 0
    ),
    plot.margin = margin(25, 30, 25, 25)
  )

print(p_conf)

# ==========================================
# 4. SAVE IN PROJECT FIGURES FOLDER
# ==========================================#

ggsave(
  filename = file.path(output_dir, "confounders_eda.png"),
  plot = p_conf,
  width = 10,
  height = 5.5,
  dpi = 300,
  bg = "white"
)