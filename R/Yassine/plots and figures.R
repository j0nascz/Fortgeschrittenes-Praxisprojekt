# Patient selection flowchart — SUSTAIN CSX post-hoc protein–AKI analysis
# Reproduces the layout of the reference illustration using base R grid.
# No patient-level data are uploaded or hard-coded.
# Sources: Stoppe et al., JAMA Surgery (2023), doi:10.1001/jamasurg.2022.6855;
#          project partner's written study-design clarification.# ─────────────────────────────────────────────────────────────────────────────
# 1. CALCULATE COUNTS FROM YOUR LOCAL, ALREADY-SELECTED ANALYSIS DATA
# ─────────────────────────────────────────────────────────────────────────────
# Your data frame must already exist in R as `data` before sourcing this script.
# The 19 supplied variable names show this is an analysis dataset, not the
# original unfiltered cohort. Selection/exclusion counts cannot be recovered.
if (!exists("data", envir = .GlobalEnv, inherits = FALSE) ||
    !is.data.frame(get("data", envir = .GlobalEnv))) {
  stop("Load your local analysis dataset into a data frame named `data` first.")
}
local_data <- get("data", envir = .GlobalEnv)
needed <- c("PatientID", "AKI_any", "died_in_ICU")
missing_columns <- setdiff(needed, names(local_data))
if (length(missing_columns)) {
  stop("Missing expected columns: ", paste(missing_columns, collapse = ", "))
}
if (anyNA(local_data$PatientID) || anyDuplicated(local_data$PatientID)) {
  stop("PatientID must be complete and unique (one row per patient).")
}
validate_binary <- function(x, name) {
  if (!is.numeric(x) && !is.integer(x) && !is.logical(x)) {
    stop(name, " must be coded numerically/logically as 0/1 with optional NA.")
  }
  if (any(!is.na(x) & !(x %in% c(0, 1)))) {
    stop(name, " must contain only 0, 1, or NA.")
  }
}
validate_binary(local_data$AKI_any, "AKI_any")
validate_binary(local_data$died_in_ICU, "died_in_ICU")

n_final <- nrow(local_data)
n_aki <- sum(local_data$AKI_any == 1, na.rm = TRUE)
n_no_aki <- sum(local_data$AKI_any == 0, na.rm = TRUE)
n_missing_aki <- sum(is.na(local_data$AKI_any))

# If ICU death is missing for any no-AKI patient, we cannot report a complete
# ICU-death-without-AKI count. Don't silently interpret missing death as alive.
if (any(is.na(local_data$died_in_ICU) & local_data$AKI_any == 0,
        na.rm = TRUE)) {
  n_death_without_aki <- NA_integer_
} else {
  n_death_without_aki <- sum(
    local_data$died_in_ICU == 1 & local_data$AKI_any == 0,
    na.rm = TRUE
  )
}

# These counts are NOT derivable from the already-selected 19-column dataset.
# Published trial figures: 1,416 randomized and 1,394 in modified ITT.
# Neither is automatically equal to the size of your supplied dataset.

stopifnot(n_aki + n_no_aki + n_missing_aki == n_final)
stopifnot(is.na(n_death_without_aki) || n_death_without_aki <= n_no_aki)

cat("\nVerified local patient counts:\n")
print(c(n_final = n_final,
        n_aki = n_aki,
        n_no_aki = n_no_aki,
        n_missing_aki = n_missing_aki,
        n_death_without_aki = n_death_without_aki))

fmt_n <- function(x, prefix = "N") {
  if (length(x) != 1L || is.na(x)) return(paste0(prefix, " = —"))
  paste0(prefix, " = ", format(x, big.mark = ",", scientific = FALSE, trim = TRUE))
}
fmt_pct <- function(part, total) {
  if (is.na(part) || is.na(total) || total == 0) return("")
  paste0(" (", sprintf("%.1f", 100 * part / total), "%)")
}

# ─────────────────────────────────────────────────────────────────────────────
# 2. STYLING AND DRAWING HELPERS
# ─────────────────────────────────────────────────────────────────────────────
navy      <- "#10274B"
slate     <- "#5F7799"
teal      <- "#007E87"
ltblue    <- "#E9F1F9"
ltblue2   <- "#F2F6FA"
ltteal    <- "#DFF5F4"
lineblue  <- "#B8CADC"
white     <- "#FFFFFF"

# All coordinates are in a 0–100 x 0–100 native viewport, origin lower left.
rect <- function(x, y, w, h, fill, stroke = NA, radius = 1.8, lw = 1.1) {
  grid.roundrect(x = unit(x, "native"), y = unit(y, "native"),
                 width = unit(w, "native"), height = unit(h, "native"),
                 r = unit(radius, "mm"),
                 gp = gpar(fill = fill, col = stroke, lwd = lw))
}
label <- function(s, x, y, size = 16, col = navy, bold = FALSE,
                  just = "left", lineheight = 1.15) {
  grid.text(s, x = unit(x, "native"), y = unit(y, "native"),
            just = just, default.units = "native",
            gp = gpar(col = col, fontsize = size,
                      fontface = if (bold) "bold" else "plain", lineheight = lineheight))
}
seg <- function(x0,y0,x1,y1, col = navy, lw = 2) {
  grid.lines(x = unit(c(x0,x1), "native"), y = unit(c(y0,y1), "native"),
             gp = gpar(col = col, lwd = lw, lineend = "round"))
}
draw_arrow <- function(x0,y0,x1,y1, col = navy, lw = 2.1) {
  grid.lines(x = unit(c(x0,x1), "native"), y = unit(c(y0,y1), "native"),
             arrow = grid::arrow(type = "closed", length = grid::unit(3.4, "mm")),
             gp = gpar(col = col, fill = col, lwd = lw, lineend = "round"))
}
bullet <- function(s, x, y, size = 13.7, col = navy) {
  grid.circle(x = unit(x,"native"), y = unit(y,"native"),
              r = unit(0.56, "native"), gp = gpar(col = teal, fill = teal))
  label(s, x + 1.8, y, size = size, col = col)
}
# Decorative pictograms, drawn as vector shapes instead of font-dependent emoji
people_icon <- function(cx, cy, color = "#07568B") {
  for (z in list(c(-2.2,0.2,1.35), c(2.2,0.2,1.35), c(0,1.45,1.7))) {
    grid.circle(x = unit(cx+z[1], "native"), y = unit(cy+z[2]+1.45,"native"),
                r = unit(z[3]/2.2,"native"),gp=gpar(fill=color,col=NA))
  }
  rect(cx-2.3,cy-1.1,3.3,3.0,color,radius=1)
  rect(cx+2.3,cy-1.1,3.3,3.0,color,radius=1)
  rect(cx,cy-1.05,4.5,3.55,color,radius=1)
}
filter_icon <- function(cx,cy) {
  grid.polygon(x=unit(cx+c(-2.8,2.8,0.8,0.8,-0.8,-0.8),"native"),
               y=unit(cy+c(2.4,2.4,-0.2,-3,-3,-0.2),"native"),
               gp=gpar(fill=NA,col="#00628F",lwd=3.2))
}
database_icon <- function(cx,cy) {
  rect(cx,cy,6,5,NA,"#067981",radius=0.7,lw=2.1)
  grid.ellipse <- function(x,y,w,h) {
    grid.circle(x=unit(x,"native"),y=unit(y,"native"),
                r=unit(h/2,"native"),gp=gpar(fill=NA,col="#067981",lwd=2))
  }
  # cylinder bands
  grid.ellipse(cx,cy+2.2,6,2.3)
  seg(cx-3,cy+0.1,cx+3,cy+0.1,"#067981",1.6)
  seg(cx-3,cy-1.8,cx+3,cy-1.8,"#067981",1.6)
}


# Vector icons, drawn entirely in grid (no external image files needed).
merge_icon <- function(cx, cy, color = "#07568B") {
  grid::grid.lines(unit(cx + c(-3,-1,1.7,3.1), "native"),
                   unit(cy + c(2.3,2.3,0,0), "native"),
                   gp=gpar(col=color,lwd=3.4,lineend="round"))
  grid::grid.lines(unit(cx + c(-3,-1,1.7,3.1), "native"),
                   unit(cy + c(-2.3,-2.3,0,0), "native"),
                   gp=gpar(col=color,lwd=3.4,lineend="round"))
  grid::grid.polygon(x=unit(cx+c(3.1,1.6,1.6),"native"),
                     y=unit(cy+c(0,1.25,-1.25),"native"),
                     gp=gpar(fill=color,col=color))
}

# Cylinder / database (three curved horizontal bands).
database_icon2 <- function(cx, cy, color="#007E87") {
  # Visible database-cylinder icon, without fonts or image dependencies.
  rx <- 3.1; ry <- 1.12; top <- cy + 2.1; bottom <- cy - 2.2
  theta <- seq(0, 2*pi, length.out = 100)
  grid::grid.polygon(
    x=unit(cx + rx*cos(theta), "native"),
    y=unit(top + ry*sin(theta), "native"),
    gp=gpar(fill="#F9FFFF", col=color, lwd=2.7))
  seg(cx-rx, top, cx-rx, bottom, col=color, lw=2.7)
  seg(cx+rx, top, cx+rx, bottom, col=color, lw=2.7)
  for (level in c(cy+0.15, bottom)) {
    theta2 <- seq(pi, 2*pi, length.out=60)
    grid::grid.lines(
      x=unit(cx+rx*cos(theta2), "native"),
      y=unit(level+ry*sin(theta2), "native"),
      gp=gpar(col=color, lwd=2.6, lineend="round"))
  }
}

info_icon <- function(cx, cy, color="#007E87", r=2.4) {
  grid::grid.circle(unit(cx,"native"),unit(cy,"native"),
                    r=unit(r,"native"),gp=gpar(fill=color,col=NA))
  label("i",cx,cy-0.05,size=18,col="white",bold=TRUE,just="center")
}

# Simple kidney silhouette represented by paired asymmetric lobes.
kidney_icon <- function(cx, cy, color="#007E87") {
  # Two kidney-shaped lobes plus ureters; larger than the previous symbols.
  xx <- c(-1.1,-2.0,-2.9,-3.5,-3.5,-3.1,-2.2,-1.3,-0.85,-1.0,-1.55,-1.1)
  yy <- c(2.8,3.05,2.25,1.1,-0.65,-2.2,-3.0,-2.55,-1.45,-0.25,0.8,2.8)
  for (side in c(1,-1)) {
    grid::grid.polygon(
      x=unit(cx + side*xx, "native"),
      y=unit(cy + yy, "native"),
      gp=gpar(fill=color, col=color, lwd=1))
  }
  # Renal pelvis and ureters, visible on the light background
  seg(cx-0.65, cy-0.5, cx-0.30, cy-2.9, col=color, lw=2.7)
  seg(cx+0.65, cy-0.5, cx+0.30, cy-2.9, col=color, lw=2.7)
}

# Clear no-AKI icon: circle with diagonal slash.
# ICU deaths are reported in text, not represented by a misleading medical cross.
no_aki_icon <- function(cx, cy, color = "#26547A") {
  grid::grid.circle(
    x = unit(cx, "native"), y = unit(cy, "native"),
    r = unit(2.7, "native"),
    gp = gpar(fill = NA, col = color, lwd = 3.2)
  )
  seg(cx - 1.85, cy + 1.85, cx + 1.85, cy - 1.85,
      col = color, lw = 3.2)
}

make_flowchart <- function() {
  grid.newpage()
  pushViewport(viewport(xscale = c(0,100), yscale = c(0,100)))
  grid.rect(gp = gpar(fill=white,col=NA))
  
  label("Patient Selection Flowchart", 3.5, 96, size=31,bold=TRUE)
  label("From pooled SUSTAIN CSX data to our analysis population",
        3.5,91.8,size=15.3,col=slate)
  
  # Top: primary source and side panel
  rect(27.5,80,48,17,ltblue,stroke=lineblue)
  people_icon(9,81.2)
  label("SUSTAIN CSX trial",16.3,85.5,size=18.3,bold=TRUE)
  label("Prospective, multicentre, randomized,",16.3,81.7,size=11.9)
  label("double-blind, placebo-controlled",16.3,79.2,size=11.9)
  label("selenium vs placebo trial",16.3,76.7,size=11.9)
  label("Available pooled data: 1416 Patients",16.3,73.8,size=15.3,bold=TRUE)
  
  rect(76.7,81.3,40,14.5,ltteal)
  database_icon2(61.0,82.2)
  label("Data source",66.0,86.0,size=18,bold=TRUE,col="#006C78")
  # Published SUSTAIN CSX trial totals, NOT the size of the supplied
  # preselected protein–AKI analysis dataset.
  label("Randomized: N = 1,416",66.0,82.5,size=11.1,bold=TRUE)
  label("Modified ITT: N = 1,394",66.0,80.0,size=11.1,bold=TRUE)
  label("697 selenium + 697 placebo",66.0,77.5,size=10.5,col=slate)
  draw_arrow(27.5,71.5,27.5,69.5)
  
  # Pooling (explicit and independent)
  rect(27.5,64.0,48,11.0,ltblue,stroke=lineblue)
  merge_icon(10.1,64.0)
  label("Pooling of treatment arms",16.0,66.8,size=16.4,bold=TRUE)
  label("Selenium + placebo patients combined",16.0,63.5,size=11.3)
  label("after no significant selenium benefit",16.0,60.8,size=11.0,col=slate)
  draw_arrow(27.5,58.5,27.5,55.2)
  
  # Study-design clarification on the right
  rect(76.7,62.5,40,15.8,ltteal)
  info_icon(61.0,66.8)
  label("Study design: our project",65.0,67.2,size=15.1,bold=TRUE,col="#006C78")
  bullet("Observational post-hoc analysis",59.2,63.4,size=11.1)
  bullet("Protein exposure NOT randomized",59.2,60.5,size=11.1)
  bullet("Clinician-directed supplementary feeding",59.2,57.6,size=10.6)
  
  # Eligibility and exclusion panel
  rect(27.5,44.3,48,20.2,ltblue,stroke=lineblue)
  filter_icon(8.7,45.9)
  label("Eligibility criteria",15.2,51.2,size=17.3,bold=TRUE)
  bullet("ICU stay > 4 days",15.1,48.0,size=11.5)
  bullet("Creatinine measurement available",15.1,45.3,size=11.5)
  bullet("Protein intake ≥ 0 g/day documented",15.1,42.6,size=11.2)
  label("on at least 2 of ICU days 1–4",17.0,40.0,size=11.2)
  label("Exclude severe renal impairment and dialysis",15.1,37.3,size=10.3,col=slate)
  
  rect(77.4,44.5,37.3,17.3,ltblue2,stroke=lineblue)
  label("Excluded / not eligible",60.4,50.1,size=15.4,bold=TRUE)
  bullet("Preoperative creatinine clearance < 50",60.4,46.6,size=10.1)
  bullet("Chronic kidney disease with dialysis",60.4,43.8,size=10.1)
  bullet("Other eligibility criteria not met",60.4,41.0,size=10.1)
  draw_arrow(51.5,44.3,57.8,44.3)
  draw_arrow(27.5,34.2,27.5,30.6)
  
  # Final analysis panel
  rect(50,25.3,84,10.2,ltteal,stroke="#A6DBD9")
  people_icon(13.5,25.3,color=teal)
  label("Final analysis population",21.5,28.1,size=18.0,bold=TRUE)
  label(fmt_n(n_final),21.5,24.3,size=16.8,bold=TRUE)
  label("Eligible patients in the protein–AKI analysis",
        43.0,23.7,size=11.1,col=slate)
  
  # Outcome branches. The death count is a SUBSET of no observed new AKI.
  seg(50,20.2,50,19.4)
  seg(28,19.4,72,19.4)
  draw_arrow(28,19.4,28,16.7)
  draw_arrow(72,19.4,72,16.7)
  
  rect(28,11.5,43,9.5,ltteal,stroke="#A6DBD9")
  rect(72,11.5,43,9.5,ltblue,stroke=lineblue)
  
  kidney_icon(12.3,11.5)
  label("New AKI after day 4",17.4,13.4,size=13.7,bold=TRUE)
  label(paste0(fmt_n(n_aki,"n"),fmt_pct(n_aki,n_final)),
        17.4,10.3,size=13.2,col=teal,bold=TRUE)
  
  # No-AKI box: make ICU deaths without AKI prominent INSIDE the box.
  # The icon means no observed AKI; deaths are a subset, not another outcome.
  no_aki_icon(56.8,11.3)
  label("No observed new AKI", 62.0, 14.3, size=12.8, bold=TRUE)
  label(paste0(fmt_n(n_no_aki, "n"), fmt_pct(n_no_aki, n_final)),
        62.0, 11.7, size=12.6, col=teal, bold=TRUE)
  
  # Two short, clearly readable lines (the previous long label could
  # extend beyond the right edge of the box).
  label("Died in ICU without AKI:",
        62.0, 9.3, size=10.6, col=slate, bold=TRUE)
  death_count_text <- if (is.na(n_death_without_aki)) {
    "n = unavailable (missing death status)"
  } else {
    paste0("n = ", n_death_without_aki)
  }
  label(death_count_text, 62.0, 7.6,
        size=11.7, col="#A34545", bold=TRUE)
}

# ─────────────────────────────────────────────────────────────────────────────
# 3. EXPORT: PNG for slides, SVG for crisp scaling
# ─────────────────────────────────────────────────────────────────────────────
output_dir <- file.path(getwd(), "figures")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
if (!dir.exists(output_dir) || file.access(output_dir,2) != 0) {
  stop("Cannot write to figures folder: ", output_dir)
}
png_path <- file.path(output_dir, "patient_flow.png")
svg_path <- file.path(output_dir, "patient_flow.svg")

grDevices::png(png_path, width = 2000, height = 1500, res = 180,
               bg = "white")
make_flowchart()
dev.off()

grDevices::svg(svg_path, width = 10, height = 7.5, bg = "white")
make_flowchart()
dev.off()

message("Created ", png_path, " and ", svg_path)
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
    title = "Distribution of artificial protein intake",
    subtitle = "Average intake during ICU days 1–4",
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

# Create figures folder if it doesn't exist
dir.create("figures", recursive = TRUE, showWarnings = FALSE)

# Save figure to your project's figures folder
ggsave(
  filename = file.path("figures", "protein_density.png"),
  plot = p,
  width = 11,
  height = 6,
  dpi = 300,
  bg = "white"
)

cat("Figure saved to:",
    normalizePath("figures/protein_density.png"), "\n")

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
    Target = c("Protein intake", "AKI"),
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
  levels = c("Protein intake", "AKI")
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
    title = "Potential confounders: exploratory associations",
    subtitle = "Correlations with protein intake and AKI",
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
# 4. SAVE IN EXISTING ASSETS FOLDER
# ==========================================#

ggsave(
  filename = file.path("figures", "confounders_eda.png"),
  plot = p_conf,
  width = 10,
  height = 5.5,
  dpi = 300,
  bg = "white"
)