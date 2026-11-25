# AKI intermediate presentation — Quarto Reveal.js

Open `presentation.qmd` in RStudio and run:

```bash
quarto preview presentation.qmd --to revealjs
```

Or export the HTML file:

```bash
quarto render presentation.qmd --to revealjs
```

## Files
- `presentation.qmd`: complete 12-slide presentation (including title and final discussion slide).
- `slides.css`: slide styling.
- `assets/dag_artificial_protein.svg`: embedded revised DAG.
- `dag_source.R`: editable R code for the diagram.
- `references.bib`: bibliography (same folder as the Quarto file).

## Before presenting
Replace all `[ ... ]` placeholders with locally computed results. The density plot, eight-model forest plot, and stratified forest plot are deliberately marked as placeholders, not fabricated data.

Check the precise definition and units of `Protein_intake__d1_4`, `AKI_any`, `AKI_any_early`, and the oral-feeding sensitivity analysis. The project partner says `days_onexclusive_oral_feeding` covers ICU days 1–4. SBP is the minimum systolic pressure measured during surgery. Current DAG is a working causal hypothesis, not a proven adjustment identification strategy.

Keep the final discussion slide last. No confidential patient data are included in this package; insert figures generated locally only.

Remember to document AI assistance under LMU course requirements and review any generated scientific text or code before submission.
