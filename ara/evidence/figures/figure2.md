# Figure 2: Reaction times and distractor interference

- **Source**: Figure 2, Results §3.1 (`docs/manuscript-r3.md:143–145`); rendered from `figures/fig_combined_RT_interference.png` (built by `data_ana.qmd` chunk `combined-rt-interference`).
- **Caption**: "(A) Mean reaction times (RTs) by Group (ASD vs. TD), Distractor Prevalence (Low vs. High), and Singleton (Absent vs. Present) conditions. Error bars represent the standard error of the mean. ... (B) Distractor interference effects by Group and Distractor Prevalence. ... Distractor interference was calculated as the difference between the singleton distractor-present and singleton distractor-absent conditions."
- **Screenshot**: figure2.png
- **Figure type**: quantitative_plot
- **Extraction method**: exact_from_labels (significance brackets) + digitized_estimate (bar heights)
- **Reading confidence**: high

## Panel A — Mean RT (bar chart)
- **Plot kind**: bar (grouped by Prevalence Low/High × Group TD/ASD × Distractor Absent/Present)
- **Axes**: X = Session (Distractor Prevalence: Low, High); Y = Reaction Time (ms), linear, ~700–1600 shown

| Condition | TD Absent | TD Present | ASD Absent | ASD Present |
|---|---|---|---|---|
| Low prevalence | ≈1050 | ≈1075 | ≈1320 | ≈1370 |
| High prevalence | ≈1110 | ≈1110 | ≈1300 | ≈1340 |

- Annotated brackets: ASD vs TD group difference p = .003; within-ASD singleton effect p < .001 (both prevalences).

## Panel B — Interference effect (present − absent, ms)
- **Axes**: X = Group (TD, ASD); Y = Interference Effect (ms), linear, ~ −15 to 100

| Group | Low prevalence | High prevalence |
|---|---|---|
| TD | ≈21 (CI crosses/near 0) | ≈ −2 (CI crosses 0) |
| ASD | ≈53 | ≈73 |

- Annotated bracket: ASD > TD, p < .001.

## Trend summary
ASD bars are markedly higher than TD throughout. The singleton (present−absent) gap is small for TD and large for ASD; TD interference straddles zero (≈21 ms low, ≈ −2 ms high) while ASD interference is robustly positive (≈53 ms low, ≈73 ms high). Supports **C01** (larger ASD interference, Group × Singleton) and **C02** (TD ≈ zero). Matches exact text values: ASD 62.88 ms vs TD 9.64 ms post-hoc; descriptives TD Low 21.4 / High −2.2, ASD Low 52.9 / High 72.9.
