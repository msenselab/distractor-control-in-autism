# Figure 5: Number of fixations per trial and fixation-count interference

- **Source**: Figure 5, Results §3.2 (`docs/manuscript-r3.md:173–175`); rendered from `figures/fig_combined_nfixations_interference.png` (built by `data_ana.qmd` chunk `combined-nfixation`).
- **Caption**: "(A) Mean number of fixations per trial, separated by Group (ASD vs. TD), Distractor Prevalence (Low vs. High), and Singleton (Absent vs. Present) conditions. ... (B) Interference effect, in terms of the difference in number of fixations between singleton distractor-present and singleton distractor-absent conditions, separated by Group and Distractor Prevalence."
- **Screenshot**: figure5.png
- **Figure type**: quantitative_plot
- **Extraction method**: exact_from_labels (significance brackets) + digitized_estimate (bar heights)
- **Reading confidence**: high

## Panel A — Mean number of fixations
- **Plot kind**: grouped bar; **Axes**: X = Distractor Prevalence (Low, High); Y = Mean Number of Fixations (~2.5–5.0, linear)

| Condition | TD Absent | TD Present | ASD Absent | ASD Present |
|---|---|---|---|---|
| Low | ≈2.70 | ≈2.82 | ≈3.50 | ≈3.74 |
| High | ≈3.02 | ≈3.06 | ≈3.33 | ≈3.63 |

- Brackets: group difference p = .023; within-group singleton effect p < .001 (both prevalences).

## Panel B — Interference (present − absent, number of fixations)
- **Axes**: X = Group (TD, ASD); Y = Interference Effect (number of fixations, ~0–0.45)

| Group | Low | High |
|---|---|---|
| TD | ≈0.13 | ≈0.03 (CI crosses 0) |
| ASD | ≈0.24 | ≈0.30 |

- Bracket: ASD > TD, p = .006.

## Trend summary
ASD make more fixations than TD across conditions; the singleton-driven increase (present − absent) is larger in ASD. In Panel B, ASD interference is positive in **both** prevalence conditions whereas TD interference is positive only under low prevalence and near-zero (CI overlapping 0) under high. Supports **C04** (Group F = 5.49; Group × Singleton F = 8.32) and **C05** (ASD interference both prevalences, ps < .01; TD only low, p = .0357).
