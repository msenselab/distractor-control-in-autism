# Preprocessing

## Trial exclusion (behavioural)
- Drop trials with RT < 0.1 s, RT beyond participant mean ± 3 SD, or incorrect response.
- Net removal: **1.33%** of trials. All analyses use correct, non-outlier trials.

## Participant exclusion
- 8 TD excluded (elevated RADS-R indicating autism-like traits, or incomplete eye-tracking).
- 4 ASD excluded (incomplete/poor-quality eye-tracking).
- Tracked in `data/participants.tsv` (`outlier`, `exclusion_reason`).

## Eye-movement feature construction
- ROIs: 1.5° visual angle around each of the six stimulus locations.
- First fixation: the fixation immediately following central fixation; classified by ROI into Target / Distractor / Other (`close_to`).
- Number of fixations per trial: `max(fixationNo) − 1` (subtracting the initial central fixation).
- Target / distractor refixations: counts of fixations whose ROI is the target / distractor (cued) location.

## Derived measures
- **Distractor interference (RT)**: mean RT(present) − mean RT(absent) per participant × prevalence.
- **Distractor interference (fixations)**: mean n-fixations(present) − mean n-fixations(absent) per participant × prevalence.
- **First-fixation percentages**: per participant × condition proportion of first fixations to each ROI.

## Trait-model preparation
- Merge `questionnaires_for_correlation.csv` with per-participant interference; z-score each trait; sum-contrast code Group and Prevalence (see `analysis_plan.md`).
