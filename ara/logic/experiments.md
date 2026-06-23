# Experiments (Analysis Plans)

Declarative analysis plans only. Exact statistics live in `evidence/`. All analyses use correct,
non-outlier trials; ANOVAs are Type-III mixed ANOVAs via `afex::aov_ez` (see
`src/execution/data_ana.qmd`).

## E01: Mixed ANOVA on mean reaction time
- **Verifies**: C01, C02
- **Setup**:
  - Sample: 28 ASD, 31 TD adults
  - Design: Group (ASD/TD, between) × Distractor Prevalence (low/high, within) × Singleton (absent/present, within)
  - DV: mean RT (correct trials)
  - Tool: `aov_ez(id="Subj", dv="RT", within=c("Prevalence","Singleton"), between="Group", type=3)`
- **Procedure**:
  1. Filter correct, non-outlier trials; aggregate mean RT per participant × condition.
  2. Fit Type-III mixed ANOVA.
  3. Decompose Group × Singleton with `emmeans` post-hoc contrasts (Bonferroni).
- **Metrics**: F, p, partial η² for each effect; effect of singleton within each group.
- **Expected outcome**: Slower RT in ASD overall; slower RT on present vs absent; the singleton cost larger in ASD than TD (Group × Singleton); no Prevalence effect.
- **Baselines**: TD group as comparison.
- **Dependencies**: none

## E02: Interference ANOVA + one-sample test against zero (RT)
- **Verifies**: C01, C02
- **Setup**: Derived interference = RT_present − RT_absent per participant × prevalence; Group × Prevalence mixed ANOVA; one-sample t-test of TD interference vs 0.
- **Procedure**:
  1. Compute interference per participant × prevalence.
  2. Mixed ANOVA (Group between, Prevalence within).
  3. One-sample t-test on TD interference against zero.
- **Metrics**: F, p, partial η² for Group; one-sample t, df, p for TD.
- **Expected outcome**: Higher interference in ASD than TD; TD interference indistinguishable from zero; no Prevalence modulation.
- **Baselines**: zero (no-interference null).
- **Dependencies**: E01

## E03: First-fixation proportion ANOVAs (distractor-directed and target-directed)
- **Verifies**: C03
- **Setup**: Proportion of first fixations landing on the distractor (and separately the target), classified via 1.5° ROIs; Group × Prevalence × Singleton mixed ANOVA on each.
- **Procedure**:
  1. Identify the first post-central fixation per trial; classify by ROI (`close_to`).
  2. Compute per-participant condition percentages.
  3. Separate ANOVAs for distractor-directed and target-directed first fixations.
- **Metrics**: F, p, partial η² for each effect.
- **Expected outcome**: Distractor-directed first fixations rise with singleton presence; no Group main effect or Group-involving interaction (capture comparable across groups).
- **Baselines**: target-directed first fixations as a comparison measure.
- **Dependencies**: none

## E04: Number-of-fixations mixed ANOVA
- **Verifies**: C04
- **Setup**: Mean number of fixations per trial (max fixationNo − 1); Group × Prevalence × Singleton mixed ANOVA.
- **Procedure**:
  1. Compute number of fixations per trial, aggregate per participant × condition.
  2. Fit Type-III mixed ANOVA.
  3. Post-hoc decomposition of Group × Singleton.
- **Metrics**: F, p, partial η² for each effect.
- **Expected outcome**: More fixations in ASD overall; more with singleton present; larger singleton-driven increase in ASD (Group × Singleton).
- **Baselines**: TD group.
- **Dependencies**: none

## E05: Fixation-count interference ANOVA + per-condition one-sample tests
- **Verifies**: C05
- **Setup**: Interference in number of fixations (present − absent) per participant × prevalence; Group × Prevalence ANOVA; one-sample t-tests per Group × Prevalence cell against zero.
- **Procedure**:
  1. Compute fixation-count interference.
  2. ANOVA across Group × Prevalence.
  3. One-sample t-tests for each Group × Prevalence condition.
- **Metrics**: per-condition t/p against zero.
- **Expected outcome**: ASD interference significant in both prevalence conditions; TD interference significant only under low prevalence.
- **Baselines**: zero.
- **Dependencies**: E04

## E06: Target-refixation ANOVA
- **Verifies**: C06
- **Setup**: Number of fixations directed to the target location; Group × Prevalence × Singleton mixed ANOVA.
- **Procedure**:
  1. Count target-location fixations per trial via ROI; aggregate per participant × condition.
  2. Fit Type-III mixed ANOVA.
- **Metrics**: F, p, partial η² for Group and other effects.
- **Expected outcome**: More target fixations in ASD overall; other effects non-significant.
- **Baselines**: TD group.
- **Dependencies**: none

## E07: Distractor-refixation ANOVA (three-way)
- **Verifies**: C07
- **Setup**: Number of fixations directed to the distractor/cued location; Group × Prevalence × Singleton mixed ANOVA.
- **Procedure**:
  1. Count distractor-location fixations per trial via ROI; aggregate per participant × condition.
  2. Fit Type-III mixed ANOVA; examine the three-way interaction.
- **Metrics**: F, p, partial η² for main effects and interactions, especially Group × Prevalence × Singleton.
- **Expected outcome**: Singleton presence and low prevalence increase distractor fixations; a three-way interaction shows ASD avoid the cued location under high prevalence when no singleton appears (frequency-prior learning).
- **Baselines**: TD group.
- **Dependencies**: E03, E05

## E08: Trait linear-model comparison (appendix)
- **Verifies**: C08
- **Setup**: Per-participant × prevalence RT interference regressed on z-scored trait (BDI / STAI-Trait / IU) across three nested linear models (Model 1: trait only; Model 2: + sum-coded Group + Prevalence; Model 3: + Trait × Group); compared by AIC/BIC.
- **Procedure**:
  1. Merge trait questionnaire scores with interference scores.
  2. Fit and compare the three models per trait via `lm`.
  3. Inspect trait, Group, Prevalence, and interaction coefficients.
- **Metrics**: AIC, BIC, coefficient estimates and p-values per model.
- **Expected outcome**: Trait predictors non-significant once Group is included; Group remains a reliable predictor (BDI, STAI); trait-only models show an IU association that is absent after Group.
- **Baselines**: nested model comparison (Model 1 vs 2 vs 3).
- **Dependencies**: E02
