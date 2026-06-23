# Environment

- **Language/runtime**: R (≥ 4.0). Analysis authored in Quarto (`data_ana.qmd`).
- **Framework**: tidyverse / ggplot2 stack; statistics via `afex` (`aov_ez`, Type-III mixed ANOVA), `emmeans` (post-hoc), base `lm` (trait models), `effectsize` (partial η²).
- **Hardware**: n/a (analytical). Data collection: Dell UltraSharp U2412M 24" LCD (1920×1200, 60 Hz), EyeLink 1000 Desktop eye tracker, PsychoPy 2022.2.2.
- **Data sources**:
  - `data/behav.csv` — trial-level behavioural data (23,600 rows): RT, accuracy, condition factors, outlier flag.
  - `data/fix_with_behav.csv` — fixation-level eye data merged with behaviour (98,635 rows).
  - `data/participants.tsv` — 70-row participant table (group, gender, age, session prevalence order, outlier/exclusion).
  - `data/questionnaires_for_correlation.csv` — per-subject BDI/IU/STAI-trait scores for appendix correlations (59 rows).
- **Key dependencies** (from README): tidyverse, ggplot2, readr, dplyr, patchwork, cowplot, scales, ggsci, ggsignif, lme4, lmerTest, afex, emmeans, effectsize, broom.mixed, knitr, kableExtra, grid.
- **Protocols**: ICD-10 diagnostic confirmation; PANGEA power analysis; counterbalanced session order; 5-point eye-tracker calibration + validation; outlier rule RT < 0.1 s or > mean ± 3 SD; correct trials only; 1.5° ROIs.
- **Random seeds**: not specified in paper/code.
- **Reproduce**: `quarto::quarto_render("data_ana.qmd")` from the project root (renders `data_ana.html` with all tests and figures).
