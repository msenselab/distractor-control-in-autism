# Figure A1: Trait measures vs. distractor interference

- **Source**: Figure A1, Appendix 1 (`docs/appendix.md:27–29`); rendered from `figures/fig_trait_correlations_combined.png` (built by `data_ana.qmd` chunk `lm-visualizations`).
- **Caption**: "Linear models examining the relationship between trait-level measures (BDI depression scores, STAI-Trait anxiety, and Intolerance of Uncertainty) and distractor interference effects in autistic (ASD) and typically developing (TD) individuals in low and high distractor prevalences. Separate models were fitted for each trait. Points represent individual participant data with semi-transparent overlays; lines indicate model-predicted relationships with 95% confidence intervals."
- **Screenshot**: figureA1.png
- **Figure type**: quantitative_plot
- **Extraction method**: visual_description (individual points not individually digitized)
- **Reading confidence**: medium

## Plot kind / axes
- **Plot kind**: scatter + fitted regression lines with 95% CI ribbons; 3 stacked panels (BDI, STAI-Trait, IU)
- **Axes**: X = trait score (BDI / STAI-Trait / IU); Y = Interference (ms). Series = Group × Prevalence (ASD/Low, ASD/High, TD/Low, TD/High).

## Trend summary
- ASD points (teal/orange) cluster at higher trait scores; TD points (squares/magenta) at lower trait scores — the group separation in trait scores is the dominant structure.
- Within each group the trait–interference slopes are shallow with wide CIs (no reliable within-group trait effect once Group is in the model).
- Demonstrates **C08**: the apparent trait–interference relationship is carried by group membership; trait predictors are non-significant after controlling for Group (BDI p = .738, STAI p = .720, IU p = .657), while Group remains significant (BDI p = .005, STAI p = .027). Numbers transcribed in `evidence/tables/tableA1.md`.
