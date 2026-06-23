# Captured Source Code — Grounding

These files are **transcribed verbatim** from the project root (copied unchanged on 2026-06-23 by
`code/bootstrap_ara_20260623.py`). They are the real, runnable analysis artifacts, captured in
native form per ARA Rule 14(c). No edits were made — line references in claims/evidence that point
at `data_ana.qmd` remain valid against the project-root copy.

| File | Origin | Grounding | What it contains |
|------|--------|-----------|------------------|
| `data_ana.qmd` | `/data_ana.qmd` | transcribed | Full Quarto analysis: data loading, RT mixed ANOVA + post-hocs, interference ANOVA + one-sample tests, first-fixation ANOVAs, number-of-fixations ANOVA + interference t-tests, target/distractor refixation ANOVAs, trait `lm` model comparison, all figures. |
| `helper_functions.R` | `/helper_functions.R` | transcribed | Reusable plotting and statistical-reporting helpers (`create_anova_table`, `create_posthoc_table`, figure styling). |

Chunk-to-experiment map (chunk labels in `data_ana.qmd`):
- `rt-mixed-model`, `rt_post-hoc-tests` → E01
- `interference-calculation`, `interference-analysis`, line ~313 one-sample t-test → E02
- `first-fixation-data`, `first-fixation-analysis1/2`, `first-fixation-posthoc_2` → E03
- `nfixations-analysis`, `nfixations-plot` → E04
- `nfixations-interference-calculation/analysis/ttest` → E05
- `target-refixations` → E06
- `distractor-refixations` → E07
- `lm-setup`, `lmm-fit-models`, `lm-model-comparison`, `lm-summary-table` → E08
