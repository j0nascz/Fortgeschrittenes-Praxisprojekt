# Presentation styling update

Open `presentation.qmd` and render with `quarto render presentation.qmd --to revealjs`.

The theme uses a navy cover, white content slides, teal accents, 33px body type and 45px headings. All slides use level-two (`##`) titles with no embedded `<section>` tags, and forward navigation is linear, non-looping.

Real figures are loaded from `R/figure_paths.R`. The included conceptual DAG is the only real graphic supplied; missing result figures receive clearly labelled notices rather than fabricated results. To add existing plots, place your exported image files in `assets/` with the expected names or change `figure_files` in `R/figure_paths.R`.

No patient-level data is included.
