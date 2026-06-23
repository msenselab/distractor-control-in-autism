---
title: "Intact Priors but Prolonged Updating: Distractor Suppression in Autism under Perfect Predictability"
authors: ["Yun Wai Foo", "Sonja Coenen", "Christine M. Falter-Wagner", "Zhuanghua Shi"]
year: 2026
venue: "Manuscript under review (transferred to Nature Mental Health, 2026-06-18; previously submitted to PNAS Nexus, Psychological Science)"
doi: "Not assigned (unpublished); repository: https://github.com/msenselab/distractor-control-in-autism"
ara_version: "1.0"
domain: "Cognitive neuroscience / experimental psychology — visual attention, predictive processing in autism"
keywords: ["attentional control", "distractor suppression", "predictive coding", "autism", "ASD", "additional-singleton search", "eye movements", "atypical iterative prior updating", "distractor prevalence", "spatial cueing"]
claims_summary:
  - "Autistic adults show larger RT distractor interference than TD even with a 100% valid spatial cue (Group x Singleton)."
  - "TD participants show essentially no reliable RT distractor interference (not different from zero)."
  - "Initial oculomotor capture by the singleton distractor is comparable across groups (intact early selection)."
  - "Autistic adults make more total fixations overall, with a stronger singleton-driven increase."
  - "Fixation-count interference is present in BOTH prevalence conditions in ASD but only low-prevalence in TD (prevalence-insensitive search depth in ASD)."
  - "Autistic adults make more target refixations (compensatory re-checking)."
  - "A three-way Group x Prevalence x Singleton effect on distractor fixations shows ASD learned the frequency prior (avoided the cued location under high prevalence when no singleton appeared)."
  - "Group differences in interference are robust to trait BDI/STAI/IU and not explained by affective comorbidity."
abstract: "Predictive coding accounts of autism spectrum disorder (ASD) propose that perception and attention depend on how priors and prediction errors are regulated. The atypical iterative prior updating (AIPU) account holds that autistic individuals form priors typically but weight sensory evidence more heavily when updating beliefs. We examined distractor suppression in autistic and typically developing (TD) adults using an additional-singleton search task with perfect spatial predictability (a 100% valid cue indicating the potential distractor location), while varying distractor prevalence (low 50% vs. high 90%) across two sessions. Response times and eye movements tracked attentional selection over time. Both groups showed equivalent first-fixation capture by the singleton distractor, indicating intact initial orienting. However, autistic participants showed larger distractor interference, more total fixations (particularly in distractor-present trials), and more frequent target refixations, reflecting prolonged post-selective processing. Spatial priors and early capture are preserved in ASD, whereas disengagement and belief-updating are delayed — a pattern more consistent with the AIPU account than with strictly surprise-driven precision-regulation accounts."
---

# Intact Priors but Prolonged Updating: Distractor Suppression in Autism under Perfect Predictability

## Overview

This study tests whether the *atypical iterative prior updating* (AIPU) account of autism — typical
prior formation but heavier weighting of sensory evidence during belief updating — extends from
perception to attentional control. 28 autistic and 31 TD adults performed an additional-singleton
visual search task in which a 100% valid line cue signalled the potential distractor location
(perfect spatial predictability), while distractor prevalence (50% low vs. 90% high) was
manipulated across two counterbalanced sessions. Manual response times and eye movements (first
fixations, number of fixations, target/distractor refixations) tracked the time course of selection.

The dissociation is the contribution: with spatial uncertainty removed, **initial capture is
comparable across groups** (intact spatial prior + early selection), yet autistic participants show
**larger distractor interference, deeper search, and more target re-checking** — differences that
emerge *after* selection. Critically, interference in ASD persists even under high prevalence, where
the distractor is fully expected, favouring AIPU (updating-driven) over strictly phasic
precision-regulation (surprise-driven) accounts.

## Layer Index

### Cognitive Layer (`/logic`)
| File | Description |
|------|-------------|
| [problem.md](logic/problem.md) | Observations → gaps → key insight → assumptions |
| [claims.md](logic/claims.md) | 8 falsifiable claims (C01–C08) |
| [concepts.md](logic/concepts.md) | 10 technical constructs |
| [experiments.md](logic/experiments.md) | 8 analysis plans (E01–E08), directional only |
| [related_work.md](logic/related_work.md) | Typed dependency graph (RW01–RW09 + citation footprint) |
| [solution/study_design.md](logic/solution/study_design.md) | Task, design, procedure, measures |
| [solution/analysis_plan.md](logic/solution/analysis_plan.md) | ANOVA / linear-model analysis design |
| [solution/constraints.md](logic/solution/constraints.md) | Limitations, assumptions, scope guards |

### Physical Layer (`/src`)
| File | Description | Claims |
|------|-------------|--------|
| [environment.md](src/environment.md) | R toolchain, data sources, protocols | — |
| [execution/data_ana.qmd](src/execution/data_ana.qmd) | Full Quarto analysis (transcribed, verbatim) | C01–C08 |
| [execution/helper_functions.R](src/execution/helper_functions.R) | Plotting/reporting helpers (transcribed) | C01–C08 |
| [execution/README.md](src/execution/README.md) | Grounding notes for captured source | — |

### Data Layer (`/data`)
| File | Description |
|------|-------------|
| [dataset.md](data/dataset.md) | Cohort, sample, ethics, variables |
| [preprocessing.md](data/preprocessing.md) | Trial exclusion, ROI definition, derived measures |

### Exploration Graph (`/trace`)
| File | Description |
|------|-------------|
| [exploration_tree.yaml](trace/exploration_tree.yaml) | Research DAG (questions, analyses, decisions, dead ends) |

### Evidence (`/evidence`)
| File | Description |
|------|-------------|
| [README.md](evidence/README.md) | Index of 7 figures + 2 tables |
| tables/table1, tableA1 | Descriptives; trait model comparison |
| figures/figure1–6, figureA1 | Trial procedure, RT, scan paths, first fixation, n-fixations, refixations, trait correlations |

### Validation (`/validation`)
| File | Description |
|------|-------------|
| [level1_report.json](validation/level1_report.json) | Seal Level 1 structural check (generated by `code/bootstrap_ara_20260623.py`) |
