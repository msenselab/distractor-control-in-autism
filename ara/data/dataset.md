# Dataset

## Provenance
- Collected at the Department of Psychiatry and Psychotherapy, LMU Munich (MSense Lab & NEVIA Lab); behavioural + EyeLink 1000 eye-tracking from an additional-singleton visual-search task.
- Licensed CC BY 4.0 (repository: https://github.com/msenselab/distractor-control-in-autism).

## Sample
- Final analysed: 28 ASD + 31 TD adults (59 total) after exclusions (8 TD, 4 ASD). 400 trials/participant (2 sessions × 200).
- Ethics: ICD-10 (F84.x) diagnoses confirmed; ethics application prepared with assistance acknowledged in the Author Note. (Exact protocol number not stated in the analysis repo.)

## Files & key variables
### `data/behav.csv` (trial level, 23,600 rows)
- `subj`, `group` (ASD/TD), `session`, `distractor_prevalence` (0.5 / 0.9), `singleton` (1 = present), `trialNo`
- `pos1`,`pos2`,`shape1`,`shape2`,`target_color`,`target_orientation`,`cue_validity` (1.0)
- `rt` (s), `corr` (1 = correct), `outlier` (True/False)

### `data/fix_with_behav.csv` (fixation level, 98,635 rows)
- `event_id`,`duration`,`average_gaze_x`,`average_gaze_y`,`average_pupil_measure1`
- `subj`,`group`,`session`,`trialNo`,`fixationNo`
- `tar_pos`,`dis_pos` (target/distractor location indices), `singleton` (Present/Absent)
- `close_to` (ROI classification: Target / Distractor / Other), `distractor_prevalence` (note: a duplicate misspelled `distractor_prevanlence` column also exists)
- behavioural fields merged in (`rt`,`corr`,`target_orientation`, etc.)

### `data/participants.tsv` (70 rows)
- `participant_id`,`group`,`gender`,`age`,`session1`,`session2` (prevalence order, 0.9/0.5),`embrace_id`,`outlier`,`exclusion_reason`

### `data/questionnaires_for_correlation.csv` (59 rows)
- `Subj`,`Diagnostic_group`,`BDI_final`,`IU_final`,`STAI_trait_final`

## Notes / gotchas
- `singleton` is numeric (0/1) in `behav.csv` but a factor (Absent/Present) in `fix_with_behav.csv`.
- `distractor_prevalence` is the session base rate (0.5 = low, 0.9 = high), not a per-trial flag.
- Raw subject-level data is **not** copied into the ARA; it lives in the project `data/` directory.
