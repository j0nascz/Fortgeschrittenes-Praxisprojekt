# Patient selection flowchart — SUSTAIN CSX post-hoc protein–AKI analysis
# Reproduces the layout of the reference illustration using base R grid.
# No patient-level data are uploaded or hard-coded.
# Sources: Stoppe et al., JAMA Surgery (2023), doi:10.1001/jamasurg.2022.6855;
#          project partner's written study-design clarification.

library(grid)   # Ships with R; no extra packages needed.

# ─────────────────────────────────────────────────────────────────────────────
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
  n_death_without_aki <- sum(local_data$died_in_ICU == 1 &
                               local_data$AKI_any == 0, na.rm = TRUE)
}

# These counts are NOT derivable from the already-selected 19-column dataset.
n_source <- NA_integer_
n_excluded <- NA_integer_

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

make_flowchart <- function() {
  grid.newpage()
  pushViewport(viewport(xscale = c(0,100), yscale = c(0,100)))
  grid.rect(gp = gpar(fill=white,col=NA))
  
  label("Patient Selection Flowchart", 3.5, 96, size=31,bold=TRUE)
  label("From pooled SUSTAIN CSX data to our analysis population",
        3.5,92.5,size=16.3,col=slate)
  
  # Top: primary source and side panel
  rect(27.5,80,48,17,ltblue,stroke=lineblue)
  people_icon(9,81.2)
  label("SUSTAIN CSX trial",16.3,85.3,size=19,bold=TRUE)
  label("Prospective, multicentre, randomized,",16.3,82.0,size=13.0)
  label("double-blind, placebo-controlled",16.3,79.7,size=13.0)
  label("selenium vs placebo trial",16.3,77.4,size=13.0)
  if (!is.na(n_source)) label(paste0("Available pooled data: ",fmt_n(n_source)),
                              16.3,74.8,size=12.2,bold=TRUE)
  
  rect(76.7,81.3,40,14.5,ltteal)
  label("Data source",59.1,86.0,size=18,bold=TRUE,col="#006C78")
  label("SUSTAIN CSX patient-level records",59.1,82.7,size=12.6)
  label("from the original trial",59.1,80.4,size=12.6)
  label("(Stoppe et al., 2023)",59.1,78.1,size=11.5,col=slate)
  draw_arrow(27.5,71.5,27.5,68.5)
  
  # Pooling (explicit and independent)
  rect(27.5,63.7,48,9.0,ltblue,stroke=lineblue)
  label("Pooling of treatment arms",6,65.4,size=18.3,bold=TRUE)
  label("Selenium + placebo patients combined",6,62.5,size=13.2)
  label("after no significant selenium benefit",6,60.3,size=12.1,col=slate)
  draw_arrow(27.5,59.2,27.5,55.7)
  
  # Study-design clarification on the right
  rect(76.7,62.5,40,15.8,ltteal)
  label("Study design: our project",59.1,67.1,size=17.2,bold=TRUE,col="#006C78")
  bullet("Observational post-hoc analysis",59.2,63.4,size=12.3)
  bullet("Protein exposure NOT randomized",59.2,60.9,size=12.3)
  bullet("Clinician-directed supplementary feeding",59.2,58.4,size=12.3)
  
  # Eligibility and exclusion panel
  rect(27.5,44.3,48,20.2,ltblue,stroke=lineblue)
  filter_icon(8.7,45.9)
  label("Eligibility criteria",15.2,51.1,size=18.2,bold=TRUE)
  bullet("ICU stay > 4 days",15.1,47.8,size=12.4)
  bullet("Creatinine measurement available",15.1,45.0,size=12.4)
  bullet("Protein intake ≥ 0 g/day documented",15.1,42.2,size=12.0)
  label("on at least 2 of ICU days 1–4",17.0,39.8,size=12.0)
  label("Exclude severe renal impairment and dialysis",15.1,36.9,size=11.0,col=slate)
  
  rect(77.4,44.5,37.3,17.3,ltblue2,stroke=lineblue)
  label("Excluded / not eligible",60.4,49.9,size=16.2,bold=TRUE)
  bullet("Preoperative creatinine clearance < 50",60.4,46.7,size=11.0)
  bullet("Chronic kidney disease with dialysis",60.4,43.9,size=11.0)
  bullet("Other eligibility criteria not met",60.4,41.1,size=11.0)
  label("Counts unavailable from selected dataset",60.4,37.9,size=10.6,bold=TRUE,col=teal)
  draw_arrow(51.5,44.3,57.8,44.3)
  draw_arrow(27.5,34.2,27.5,30.6)
  
  # Final analysis panel
  rect(50,25.3,84,10.2,ltteal,stroke="#A6DBD9")
  people_icon(13.5,25.3,color=teal)
  label("Final analysis population",21.5,27.6,size=20.0,bold=TRUE)
  label(fmt_n(n_final),21.5,23.7,size=18.5,bold=TRUE)
  label("Eligible patients in our observational protein–AKI analysis",
        40.0,23.5,size=12.3,col=slate)
  
  # Outcome branches
  seg(50,20.2,50,17.9)
  seg(28,17.9,72,17.9)
  draw_arrow(28,17.9,28,15.0)
  draw_arrow(72,17.9,72,15.0)
  rect(28,10.8,43,8.1,ltteal,stroke="#A6DBD9")
  rect(72,10.8,43,8.1,ltblue,stroke=lineblue)
  label("New AKI after day 4",10.8,12.9,size=16.5,bold=TRUE)
  label(paste0(fmt_n(n_aki,"n"),fmt_pct(n_aki,n_final)),10.8,9.6,size=15.2,col=teal,bold=TRUE)
  label("No observed new AKI",54.5,12.9,size=16.5,bold=TRUE)
  label(paste0(fmt_n(n_no_aki,"n"),fmt_pct(n_no_aki,n_final)),54.5,9.6,size=15.2,col=teal,bold=TRUE)
  
  # Note: primary outcome vs sensitivity handling
  rect(50,3.8,93,4.8,ltblue2,stroke=lineblue)
  note <- if (is.na(n_death_without_aki)) {
    "ICU deaths without observed AKI: incomplete mortality data"
  } else {
    paste0("ICU deaths without observed AKI: ",fmt_n(n_death_without_aki,"n"))
  }
  if (n_missing_aki > 0) {
    note <- paste0(note, "   |   Missing AKI outcomes: ", n_missing_aki)
  }
  label(note,5.1,3.8,size=11.2,col=navy)
  popViewport()
}

# ─────────────────────────────────────────────────────────────────────────────
# 3. EXPORT: PNG for slides, SVG for crisp scaling
# ─────────────────────────────────────────────────────────────────────────────
dir.create("assets", showWarnings = FALSE, recursive = TRUE)

png("assets/patient_flow.png", width = 1600, height = 1200, res = 160,
    bg = "white")
make_flowchart()
dev.off()

svg("assets/patient_flow.svg", width = 10, height = 7.5, bg = "white")
make_flowchart()
dev.off()

message("Created assets/patient_flow.png and assets/patient_flow.svg")
