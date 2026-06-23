# Analysis Plan

Full executable analysis: `src/execution/data_ana.qmd` (helpers in `helper_functions.R`).

## Preprocessing
- Outlier trials excluded when RT < 0.1 s, RT > participant mean ± 3 SD, or response incorrect.
- This removed **1.33%** of trials; all subsequent analyses use correct trials only.
- Eye movements analysed with **1.5° visual-angle ROIs** around each stimulus location; the first fixation is the fixation immediately following central fixation.

## Behavioural models
- **RT**: Type-III mixed ANOVA via `aov_ez` — DV = RT, within = {Prevalence, Singleton}, between = Group (E01). Group × Singleton decomposed with `emmeans` (Bonferroni).
- **RT interference** (present − absent, per participant × prevalence): Group × Prevalence mixed ANOVA + one-sample t-test of TD interference vs 0 (E02).

## Eye-movement models
- **First fixation**: separate Group × Prevalence × Singleton ANOVAs on the percentage of first fixations landing on the distractor and on the target (E03).
- **Number of fixations** (max fixationNo − 1, per trial): Group × Prevalence × Singleton mixed ANOVA (E04); fixation-count interference ANOVA + per-condition one-sample t-tests (E05).
- **Refixations**: separate Group × Prevalence × Singleton ANOVAs on target-location fixations (E06) and distractor-location fixations (E07, three-way interaction of interest).

## Trait correlation models (appendix)
- RT interference regressed on z-scored trait (BDI / STAI-Trait / IU) across three nested `lm` models — Model 1 (trait), Model 2 (+ sum-coded Group + Prevalence), Model 3 (+ Trait × Group) — compared by AIC/BIC (E08).
- Contrast coding: Group (TD = −1, ASD = +1), Prevalence (Low = −1, High = +1); trait z-scored.

## Reporting conventions
- Effect sizes: partial η² for ANOVAs, Cohen's d (or rank-biserial r) for t-tests / group comparisons.
- α = .05. Note: the README/Quarto header states "mixed-effects models with participants as random effects," but the implemented behavioural analyses are repeated-measures/mixed ANOVAs (`aov_ez`) on per-participant aggregates, and the trait models are ordinary `lm` (an earlier `lmer` trait model was replaced by `lm`; see CHANGELOG 2026-01-27). Treat the ANOVA/`lm` implementation as authoritative.
