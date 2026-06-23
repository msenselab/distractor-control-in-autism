# Figure 6: Fixations to target and distractor locations

- **Source**: Figure 6, Results §3.2 (`docs/manuscript-r3.md:183–185`); rendered from `figures/fig_refixations_combined.png` (built by `data_ana.qmd` chunk `combined-refixation-plots`).
- **Caption**: "Mean fixations to target locations (A) and distractor locations (B), separated for Group (ASD vs. TD), Distractor Prevalence (Low vs. High), and Singleton (Absent vs. Present). The ASD group showed increased target fixations, suggesting compensatory search strategies. Error bars represent one standard error across participants."
- **Screenshot**: figure6.png
- **Figure type**: quantitative_plot
- **Extraction method**: exact_from_labels (significance brackets) + digitized_estimate (bar heights)
- **Reading confidence**: high

## Panel A — Mean fixations to target
- **Axes**: X = Distractor Prevalence (Low, High); Y = Mean Fixations to Target (~0.8–1.4, linear)

| Condition | TD Absent | TD Present | ASD Absent | ASD Present |
|---|---|---|---|---|
| Low | ≈1.16 | ≈1.17 | ≈1.29 | ≈1.31 |
| High | ≈1.19 | ≈1.22 | ≈1.29 | ≈1.29 |

- Bracket: group difference p = .04 (ASD > TD), both prevalences.

## Panel B — Mean fixations to distractor
- **Axes**: X = Distractor Prevalence (Low, High); Y = Mean Fixations to Distractor (~0.8–1.4, linear)

| Condition | TD Absent | TD Present | ASD Absent | ASD Present |
|---|---|---|---|---|
| Low | ≈1.07 | ≈1.09 | ≈1.10 | ≈1.11 |
| High | ≈1.02 | ≈1.08 | ≈0.88 | ≈1.10 |

- Brackets: overall p < .001 (singleton presence increases distractor fixations); high-prevalence contrasts p = .038 and p < .001 — note the ASD **Absent–High** bar is the lowest (≈0.88), i.e. strong avoidance of the cued location when no singleton appears under high prevalence.

## Trend summary
Panel A: ASD consistently make more target fixations than TD (compensatory re-checking) → **C06** (Group F = 4.42, p = .040). Panel B: distractor fixations rise with singleton presence and fall under high prevalence; the ASD absent-high bar drops sharply, the visual signature of the three-way Group × Prevalence × Singleton interaction (F = 6.12, p = .016) → **C07** (ASD learned the frequency prior, avoiding the cued location under high prevalence when no singleton is present).
