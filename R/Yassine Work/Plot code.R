# Patient selection flowchart — SUSTAIN CSX post-hoc AKI project
# 1. Replace 'your_data_frame' with your loaded dataset object.
dat <- your_data_frame

# 2. Set TRUE if your data are ALREADY the selected project cohort.
#    If TRUE, only the final cohort size is calculated; exclusion counts are not inferred.
already_selected <- TRUE

# 3. Map your actual dataset's column names. Only needed when already_selected=FALSE.
cols <- list(
  id = 'patient_id',
  icu_days = 'icu_days',
  creatinine = 'creatinine', # value or measurement showing availability (see note below)
  protein_days = c('protein_day1', 'protein_day2', 'protein_day3', 'protein_day4'),
  crcl = 'preop_creatinine_clearance', # mL/min; confirm units
  dialysis = 'chronic_dialysis' # logical TRUE/FALSE or numeric 1/0
)

if (!is.data.frame(dat)) stop('dat must be a data frame.')
if (already_selected) {
  # If one row = one patient, nrow(dat) is sufficient.
  # Otherwise set id to your actual patient-ID column and count unique IDs.
  if (cols$id %in% names(dat)) {
    n_final <- length(unique(dat[[cols$id]][!is.na(dat[[cols$id]])]))
  } else {
    n_final <- nrow(dat)
    message('No ID column found: counting rows. Verify one row per patient.')
  }
  counts <- NULL
} else {
  needed <- unlist(cols, use.names = FALSE)
  missing_columns <- setdiff(needed, names(dat))
  if (length(missing_columns)) stop('Missing columns: ', paste(missing_columns, collapse=', '))
  if (anyNA(dat[[cols$id]]) || anyDuplicated(dat[[cols$id]])) {
    stop('Provide one record per patient with a unique, nonmissing ID.')
  }
  n_source <- nrow(dat)
  s1 <- dat[!is.na(dat[[cols$icu_days]]) & dat[[cols$icu_days]] > 4, , drop=FALSE]
  s2 <- s1[!is.na(s1[[cols$creatinine]]), , drop=FALSE]
  protein <- as.matrix(s2[, cols$protein_days, drop=FALSE])
  if (!is.numeric(protein)) stop('Daily protein columns must be numeric (g/day).')
  documented <- rowSums(!is.na(protein) & protein >= 0)
  s3 <- s2[documented >= 2, , drop=FALSE]
  # Missing renal eligibility information is excluded, not assumed normal.
  dialysis <- s3[[cols$dialysis]]
  if (!is.logical(dialysis) && !all(is.na(dialysis) | dialysis %in% c(0,1))) {
    stop('Dialysis must be logical or encoded 0/1.')
  }
  eligible_renal <- !is.na(s3[[cols$crcl]]) & s3[[cols$crcl]] >= 50 &
    !is.na(dialysis) & !as.logical(dialysis)
  s4 <- s3[eligible_renal, , drop=FALSE]
  counts <- c(n_source, nrow(s1), nrow(s2), nrow(s3), nrow(s4))
  n_final <- nrow(s4)
  message('Sequential counts: ', paste(counts, collapse=' -> '))
  message('Sequential exclusions: ', paste(-diff(counts), collapse=', '))
  # NOTE: confirm that your creatinine column reflects the relevant measurement
  # and that the eligibility rule matches the actual preprocessing code.
}

# 4. Build a clean, presentation-ready diagram.
source_note <- 'Prospective, randomized, double-blind selenium vs placebo trial'
labels <- if (already_selected) {
  c(
    paste0('DATA SOURCE  |  SUSTAIN CSX trial\n', source_note),
    'POOLED TREATMENT ARMS\nSelenium + placebo (no statistically significant primary benefit)',
    paste0('PROJECT SUBGROUP SELECTION\nICU stay > 4 days  |  creatinine measurement available\n',
           'Protein intake recorded >= 0 g/day on at least 2 of ICU days 1-4\n',
           'Exclude preoperative CrCl < 50 mL/min or chronic dialysis'),
    paste0('FINAL ANALYSIS COHORT  |  N = ', format(n_final, big.mark=','), '\n',
           'Observational, nonrandomized post-hoc protein-AKI analysis')
  )
} else {
  c(
    paste0('DATA SOURCE  |  Pooled trial arms\nN in local input = ', counts[1]),
    paste0('ICU STAY > 4 DAYS\nRemaining N = ', counts[2],
           '  |  Excluded = ', counts[1] - counts[2]),
    paste0('CREATININE MEASUREMENT AVAILABLE\nRemaining N = ', counts[3],
           '  |  Excluded = ', counts[2] - counts[3]),
    paste0('DOCUMENTED PROTEIN INTAKE\n>= 0 g/day on >= 2 of ICU days 1-4\n',
           'Remaining N = ', counts[4], '  |  Excluded = ', counts[3] - counts[4]),
    paste0('RENAL EXCLUSIONS\nExclude CrCl < 50 mL/min, chronic dialysis, unknown eligibility\n',
           'Remaining N = ', counts[5], '  |  Excluded = ', counts[4] - counts[5]),
    paste0('FINAL ANALYSIS COHORT  |  N = ', counts[5], '\n',
           'Observational, nonrandomized post-hoc protein-AKI analysis')
  )
}

k <- length(labels)
y <- rev(seq_len(k))
rects <- data.frame(y=y, xmin=0.5, xmax=9.5,
                    ymin=y-0.39, ymax=y+0.39,
                    label=labels, is_final=seq_len(k)==k)
arrow <- if (k > 1) data.frame(x=5, xend=5,
                               y=y[-k]-0.41, yend=y[-1]+0.41) else NULL
p <- ggplot() +
  geom_rect(data=rects, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=is_final),
            color='#183451', linewidth=0.8) +
  geom_text(data=rects, aes(x=5, y=y, label=label),
            color='#142A45', size=4.4, lineheight=1.15, fontface='plain') +
  scale_fill_manual(values=c('FALSE'='#EAF3F8','TRUE'='#BCE8E7'), guide='none') +
  coord_cartesian(xlim=c(0,10), ylim=c(0.25,k+0.85), clip='off') +
  labs(title='Data source and selection of our study population',
       subtitle='SUSTAIN CSX  →  pooled arms  →  eligibility  →  protein–AKI analysis',
       caption='Selenium was randomized in the original trial; protein intake was clinician-directed, not randomized.\nSource: Stoppe et al. (2023), JAMA Surgery; project partner’s written clarification.') +
  theme_void(base_size=15) +
  theme(plot.title=element_text(face='bold',size=21,color='#142A45',margin=margin(b=8)),
        plot.subtitle=element_text(size=13,color='#41647C',margin=margin(b=18)),
        plot.caption=element_text(size=9.5,color='#50667A',hjust=0,margin=margin(t=16)),
        plot.margin=margin(24,25,24,25))
if (!is.null(arrow)) p <- p +
  geom_segment(data=arrow, aes(x=x,y=y,xend=xend,yend=yend),
               color='#087C88',linewidth=0.85,
               arrow=grid::arrow(length=grid::unit(0.18,'cm'),type='closed'))

if (!dir.exists('assets')) dir.create('assets', recursive=TRUE)
ggsave('assets/patient_flow.png', p, width=14, height=if(already_selected) 8 else 11,
       dpi=200, bg='white')
print(p)
cat('Saved assets/patient_flow.png; final N =', n_final, '\n')
