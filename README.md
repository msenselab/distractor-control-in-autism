# Predictability Isn't Enough: Disengagement Limits Distractor Control in Autism

[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

This repository contains data and analysis code for the manuscript:

**"Predictability Isn't Enough: Disengagement Limits Distractor Control in Autism"**

MSense Lab and NEVIA Lab, LMU Munich

## Abstract

Predictive coding accounts propose that attention in autism spectrum disorder (ASD) depends on how priors and prediction errors are balanced. Distractor-suppression paradigms offer a way to operationalize the core predictive coding constructs within the domain of visual perception. Yet findings on distractor suppression remain mixed. Few studies test distractor suppression when spatial predictability is perfect while global predictability varies, and fewer still track the time course of attention to distinguish capture from disengagement.

We combined an additional singleton search task with a perfectly valid cue to the potential distractor location and manipulated distractor prevalence across blocks. This allowed us to probe the reliance on priors and the regulation of prediction errors in conditions of differing predictability/volatility. We compared participants with ASD and typically developing (TD) participants, measuring reaction times and eye movements.

Predictability in space did not eliminate group differences in performance. Participants with ASD showed larger distractor interference than TD participants. Initial capture was comparable across groups: singletons drew first fixations toward the distractor in both groups. Differences emerged afterward: Participants with ASD dwelled longer on the first fixation under low prevalence, made more fixations overall—especially when distractors were present—and refixated targets more often. Refixations to distractors increased with singleton presence and high prevalence similarly for both groups.

The pattern suggests intact spatial priors and initial selection in ASD, with differences emerging during disengagement and post-selective decision-making processes. This profile aligns with predictive coding accounts that emphasize altered regulation (high precision) of prediction errors rather than weak priors.

## Repository Contents

- `data/` - Raw behavioral data and preprocessed eye movement data
  - `behav.csv` - Behavioral data (reaction times, accuracy)
  - `fix_with_behav.csv` - Eye movement data with behavioral information
- `data_ana.qmd` - Main Quarto document with all analyses and visualizations
- `helper_functions.R` - Reusable R functions for data visualization and statistical reporting
- `figures/` - Generated figures and plots (created after rendering)

## Requirements

The analysis requires R (≥ 4.0) with the following packages:

```r
# Data manipulation and visualization
tidyverse, ggplot2, readr, dplyr, patchwork, cowplot, scales, ggsci, ggsignif

# Statistical analysis
lme4, lmerTest, afex, emmeans, effectsize, broom.mixed

# Reporting
knitr, kableExtra
```

Install all dependencies with:

```r
install.packages(c("tidyverse", "ggplot2", "readr", "dplyr", "lme4",
                   "lmerTest", "afex", "emmeans", "effectsize",
                   "broom.mixed", "knitr", "kableExtra", "patchwork",
                   "cowplot", "scales", "ggsci", "ggsignif", "grid"))
```

## Reproducing the Analysis

1. Clone this repository
2. Install required R packages (see above)
3. Open `data_ana.qmd` in RStudio or your preferred editor
4. Render the document:

   ```r
   quarto::quarto_render("data_ana.qmd")
   ```

   Or use the "Render" button in RStudio

The rendered HTML output will contain all analyses, statistical tests, and visualizations.

## Data Description

### Experimental Design

- **Groups**: ASD (autism spectrum disorder) vs. TD (typically developing)
- **Distractor Prevalence**: Low (50%) vs. High (90%) across blocks
- **Singleton Presence**: Distractor present vs. absent trials
- **Precueing**: 100% valid spatial cues to potential distractor locations

### Measures

- **Behavioral**: Reaction times (RT), accuracy, distractor interference effects
- **Eye movements**: First fixation patterns, dwell times, number of fixations, refixations to target/distractor locations

## Key Findings

1. **Increased distractor interference** in ASD group despite perfect spatial cues
2. **Comparable initial capture** by salient distractors across groups
3. **Prolonged disengagement** in ASD, especially under low prevalence
4. **More fixations overall** in ASD, particularly when distractors present
5. **Increased target refixations** in ASD, suggesting compensatory strategies

## Citation

If you use this data or code, please cite:

```bibtex
@misc{msenselab2025distractor,
  title = {Predictability Isn't Enough: Disengagement Limits Distractor Control in Autism},
  author = {{Yun Wai Foo}, {Sonja Coenen1}, {Christine M. Falter-Wagner}, and {Zhuanghua Shi}},
  year = {2025},
  howpublished = {\url{https://github.com/msenselab/distractor-control-in-autism}},
  note = {Data and analysis code for distractor control study in autism spectrum disorder}
}
```

**Repository:** https://github.com/msenselab/distractor-control-in-autism

## License

This work is licensed under a [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/).

**You are free to:**

- Share — copy and redistribute the material in any medium or format
- Adapt — remix, transform, and build upon the material for any purpose, even commercially

**Under the following terms:**

- Attribution — You must give appropriate credit, provide a link to the license, and indicate if changes were made

## Acknowledgments

This research was conducted by MSense Lab and NEVIA Lab.
