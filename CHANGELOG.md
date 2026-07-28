# Changelog

Project-local running ledger. Stable project identity, design, and durable methodological notes belong in `prj-memory.md`.

## 2026-07-28 — Biological Psychiatry: Global Open Science submission
- Completed/updated: reworked the cover letter and recorded the new submission milestone.
- Key files: `docs/cover-letter-BPGOS.md`, `prj-memory.md`, `CHANGELOG.md`, `ara/PAPER.md`.
- Result/state: *Biological Psychiatry: Cognitive Neuroscience and Neuroimaging* desk rejection received on 2026-07-24; manuscript submitted to *Biological Psychiatry: Global Open Science* on 2026-07-28.
- Next: await editorial decision.

## 2026-07-17 — Biological Psychiatry: CNNI submission
- Completed/updated: project status milestone recorded.
- Key files: `prj-memory.md`, `CHANGELOG.md`, `ara/PAPER.md`.
- Result/state: *Nature Mental Health* desk rejection received on 2026-07-14; manuscript submitted to *Biological Psychiatry: Cognitive Neuroscience and Neuroimaging* on 2026-07-17.
- Next: await editorial decision.

## 2026-06-23 — ARA bootstrap
- Completed/updated: bootstrapped project-local Agent-Native Research Artifact under `ara/` via the project-ara-workflow + ara-compiler.
- Key files: `ara/` (PAPER.md, logic/, src/, data/, trace/, evidence/, validation/), `code/bootstrap_ara_20260623.py`, `prj-memory.md`, research-index `projects.yaml`.
- Result/state: ARA Seal Level 1 validated — 8 claims (C01–C08), 8 experiments, 10 concepts, 16 trace nodes, 6 figures + 2 tables. All claim statistics grounded with verbatim quotes to `docs/manuscript-r3.md` / `docs/appendix.md`; real R analysis (`data_ana.qmd`, `helper_functions.R`) captured in `ara/src/execution/`. Figures use the high-res `figures/fig_*.png` renders. Known limitation: Figure 1 (trial-procedure schematic) is not a committed asset — described in `ara/logic/solution/study_design.md` and accounted for in `ara/evidence/README.md`; table PNGs are rendered transcription snapshots, not DOCX crops.
- Next: re-run `code/bootstrap_ara_20260623.py` after manuscript edits (re-verify claim source line numbers); run ara-rigor-reviewer (Level 2) before resubmission.

## 2026-06-18 — Nature Mental Health transfer
- Completed/updated: project status milestone recorded.
- Key files: `prj-memory.md`, `CHANGELOG.md`.
- Result/state: Psychological Science rejection received on 2026-06-17; manuscript transferred from Nature Human Behaviour to Nature Mental Health on 2026-06-18.
- Next: await Nature Mental Health editorial decision.

## 2026-05-29 — Psychological Science submission
- Completed/updated: project status milestone recorded.
- Key files: `prj-memory.md`, `CHANGELOG.md`.
- Result/state: PNAS Nexus rejection received on 2026-05-28; manuscript submitted to Psychological Science on 2026-05-29.
- Next: await Psychological Science editorial decision.

## 2026-05-19 — changelog initialized
- Completed/updated: created project-local changelog from existing project memory, git commit history, and recent filesystem activity.
- Key files: `CHANGELOG.md`.
- Result/state: recent task history now has a dedicated ledger separate from stable project memory.
- Next: add brief entries here after substantial analyses, manuscript changes, or unfinished multi-day work.

## Git commit history

### 2025-12-04
- init commit (`50728e2`). Files: `.gitignore`, `README.md`, `data/behav.csv`, `data/fix_with_behav.csv`.
- minor readme typo (`054e6c7`). Files: `README.md`.

### 2025-12-05
- reint RT plots (`a40e99b`). Files: `data_ana.qmd`, `figures/fig_combined_RT_interference.png`.

### 2025-12-11
- revise first fixation analysis and visualization. (`8f2955f`). Files: `README.md`, `data_ana.qmd`, `figures/fig_first_fixation_combined.png`, `figures/fig_first_fixation_perc.png`.

### 2025-12-12
- update bar plots (`0265544`). Files: `data_ana.html`, `data_ana.qmd`, `figures/fig_combined_RT_interference.png`, `figures/fig_combined_nfixations_interference.png`.

### 2025-12-14
- include partial eta squares (`8560836`). Files: `.gitignore`, `README.md`, `data_ana.html`, `data_ana.qmd`.

### 2025-12-17
- add trait scores for correlation (`121a28b`). Files: `data/questionnaires_for_correlation.csv`.
- quarto to hugo (`88141f8`). Files: `data_ana.html`, `data_ana.markdown_strict_files/figure-markdown_strict/combined-nfixation-1.png`, `data_ana.markdown_strict_files/figure-markdown_strict/combined-refixation-plots-1.png`, `data_ana.markdown_strict_files/figure-markdown_strict/combined-rt-interference-1.png`.

### 2025-12-18
- add correlation analysis (`ab5d23a`). Files: `data_ana.qmd`, `figures/fig_trait_correlations_lmm.png`.

### 2026-01-13
- update trait-interference correlation plots (`1375cc5`). Files: `data_ana.qmd`, `figures/fig_trait_correlations_lmm.png`.

### 2026-01-15
- html render trait-interference plots (`7ef3274`). Files: `data_ana.qmd`.

### 2026-01-27
- refactor: Replace lmer with lm for trait correlation models (`37a45c9`). Files: `data_ana.qmd`.
- fix: Replace fixef() with coef() for lm objects (`560fad2`). Files: `data_ana.qmd`.
- docs: Update appendix.md with linear model results (`b772e14`). Files: `docs/appendix.md`.

### 2026-03-12
- Update analysis code and figures (`54138c9`). Files: `data_ana.qmd`, `figures/fig_combined_RT_interference.png`, `figures/fig_combined_nfixations_interference.png`, `figures/fig_distractor_refixations.png`.

### 2026-03-13
- Update analysis and figures (`a1cd42b`). Files: `README.md`, `data_ana.qmd`, `figures/fig_combined_nfixations_interference.png`, `figures/fig_distractor_refixations.png`.
- Finalize p-value corrections in analysis figures (`3401e51`). Files: `data_ana.qmd`.

### 2026-04-17
- edit subtitle (`d08a827`). Files: `data_ana.qmd`.

## Recent file activity snapshot

### 2026-03-12
- `README.md`
- `data_ana.tex`
- `data_ana_files/figure-pdf/combined-refixation-plots-1.pdf`
- `data_ana_files/figure-pdf/distractor-refixation-plot-1.pdf`
- `data_ana_files/figure-pdf/lmm-diagnostics-1.pdf`
- `data_ana_files/figure-pdf/lmm-visualizations-1.pdf`
- `data_ana_files/figure-pdf/lmm-visualizations-2.pdf`
- `data_ana_files/figure-pdf/lmm-visualizations-3.pdf`
- `data_ana_files/figure-pdf/target-refixation-plot-1.pdf`

### 2026-03-13
- `figures/fig_trait_correlations_lmm.png`
- `osf_submission/data_ana.qmd`

### 2026-04-17
- `data_ana.html`
- `data_ana.qmd`
- `figures/fig_combined_RT_interference.png`
- `figures/fig_combined_nfixations_interference.png`
- `figures/fig_distractor_refixations.png`
- `figures/fig_first_fixation_perc.png`
- `figures/fig_first_fixation_stacked.png`
- `figures/fig_nfixations_interference.png`
- `figures/fig_refixations_combined.png`
- `figures/fig_target_refixations.png`
- `figures/fig_trait_correlations_combined.png`
- `figures/fixations_example.png`

### 2026-05-11
- `prj-memory.md`
