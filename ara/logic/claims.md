# Claims

Source references cite the latest manuscript draft `docs/manuscript-r3.md` (Results §3 and Appendix),
with statistics also reproducible from `data_ana.qmd`. Line numbers are as of ARA bootstrap
(2026-06-23); re-verify if the manuscript is edited.

## C01: Larger RT distractor interference in ASD despite a 100% valid spatial cue
- **Statement**: Autistic participants showed a larger reaction-time distractor-interference effect (singleton present − absent) than TD participants, i.e. a reliable Group × Singleton interaction, even though the distractor location was perfectly cued.
- **Status**: supported
- **Falsification criteria**: A non-significant Group × Singleton interaction on RT, or equal/smaller interference in ASD than TD.
- **Proof**: [E01, E02]
- **Evidence basis**: Group × Singleton on RT, F(1,57) = 13.86, p < .001, ηp² = .196; ASD post-hoc interference 62.88 ms (p < .001) vs TD 9.64 ms (p = .332).
- **Interpretation**: Perfect spatial predictability does not abolish the group difference, implying the difference is not in spatial-prior availability.
- **Dependencies**: none
- **Tags**: RT, interference, distractor suppression, group difference
- **Sources**:
  - "13.86" ← `docs/manuscript-r3.md:137` «this distractor interference was larger for autistic participants than TD participants (Group × Singleton), *F*(1, 57) = 13.86, *p* \< .001, [η~p~² = .196,]{.mark}» [result]
  - "62.88 ms" ← `docs/manuscript-r3.md:141` «whereas autistic participants showed robust slowing (62.88 ms, *p* \< .001)» [result]
  - "9.64 ms" ← `docs/manuscript-r3.md:141` «Post‑hoc tests revealed virtually no interference for TD participants (9.64 ms, *p* = .332)» [result]

## C02: TD participants show essentially no reliable RT distractor interference
- **Statement**: TD participants' RT distractor-interference effect did not differ reliably from zero, indicating effective suppression under perfect cue validity.
- **Status**: supported
- **Falsification criteria**: A TD interference effect significantly greater than zero, or a significant singleton effect within the TD group.
- **Proof**: [E01, E02]
- **Evidence basis**: One-sample test of TD interference vs zero, t(61) = 1.07, p = .287; TD descriptive interference near zero (Low = 21.4 ms; High = −2.2 ms).
- **Interpretation**: With a valid spatial cue, TD observers proactively suppress the singleton; contrasts with persistent ASD costs (C01).
- **Dependencies**: C01
- **Tags**: RT, interference, TD, suppression
- **Sources**:
  - "t(61) = 1.07" / "p = .287" ← `docs/manuscript-r3.md:141` «a one‑sample test confirmed TD interference did not differ from zero, *t*(61) = 1.07, *p* = .287» [result]
  - "21.4 ms" / "−2.2 ms" ← `docs/manuscript-r3.md:141` «TD participants hovered near zero (Low = 21.4 ms; High = −2.2 ms)» [result]

## C03: Comparable initial oculomotor capture across groups
- **Statement**: The proportion of first fixations landing on the singleton distractor increased with singleton presence equally in both groups; there was no Group effect or Group × Prevalence interaction on first-fixation capture.
- **Status**: supported
- **Falsification criteria**: A significant Group main effect or Group-involving interaction on the proportion of first fixations landing on the distractor.
- **Proof**: [E03]
- **Evidence basis**: First-fixation-to-distractor increased with singleton presence, F(1,57) = 37.18, p < .001, ηp² = .395; Group and Group × Prevalence non-significant (Fs < 2.15, ps > .148); target-directed first fixations comparable (Fs < 3.05, ps > .086).
- **Interpretation**: Early selection and use of the spatial prior ("where not to look") are intact in ASD.
- **Dependencies**: none
- **Tags**: first fixation, capture, eye movements, intact early selection
- **Sources**:
  - "37.18" ← `docs/manuscript-r3.md:163` «repeated-measures ANOVA revealed a significant increase with the distractor presence, *F*(1, 57) = 37.18, *p* \< .001, [η~p~² = .395,]{.mark}» [result]
  - "Fs < 2.15" / "ps > .148" ← `docs/manuscript-r3.md:163` «Neither Distractor Prevalence, Group, nor Group × Distractor Prevalence interaction reached significance, *F*s \< 2.15, *p*s \> .148» [result]

## C04: More total fixations in ASD, with a stronger singleton-driven increase
- **Statement**: Autistic participants made more fixations per trial overall, and the increase in fixations caused by singleton presence was larger in ASD than TD (Group × Singleton).
- **Status**: supported
- **Falsification criteria**: No Group main effect and no Group × Singleton interaction on number of fixations per trial.
- **Proof**: [E04]
- **Evidence basis**: Group main effect F(1,57) = 5.49, p = .023, ηp² = .088; Singleton F(1,57) = 28.06, p < .001, ηp² = .33; Group × Singleton F(1,57) = 8.32, p = .006, ηp² = .127.
- **Interpretation**: Deeper search and sustained sensory engagement in ASD, mirroring the RT interference pattern.
- **Dependencies**: none
- **Tags**: number of fixations, search depth, eye movements
- **Sources**:
  - "5.49" ← `docs/manuscript-r3.md:171` «Autistic participants made more fixations overall, *F*(1, 57) = 5.49, *p* = .023, [η~p~² = .088]{.mark}» [result]
  - "8.32" ← `docs/manuscript-r3.md:171` «this increase was stronger for autistic participants than TD participants (Group × Singleton), *F*(1, 57) = 8.32, *p* = .006, [η~p~² = .127]{.mark}» [result]

## C05: Prevalence-insensitive fixation-count interference in ASD
- **Statement**: Distractor interference measured in number of fixations was significant in ASD in both low- and high-prevalence sessions, whereas in TD it reached significance only in the low-prevalence session.
- **Status**: supported
- **Falsification criteria**: TD showing fixation interference under high prevalence, or ASD failing to show it under high prevalence.
- **Proof**: [E05]
- **Evidence basis**: Post-hoc one-sample t-tests — ASD interference significant in both prevalence sessions (ps < .01); TD significant only in low prevalence (p = .0357).
- **Interpretation**: TD adapt search depth to distractor frequency; ASD do not, despite high prevalence and perfect cue validity.
- **Dependencies**: C04
- **Tags**: prevalence, interference, search depth, frequency prior
- **Sources**:
  - "ps < .01" ← `docs/manuscript-r3.md:171` «distractor interference effects were significant in both high and low prevalence sessions with autistic participants (ps \< .01)» [result]
  - "p = .0357" ← `docs/manuscript-r3.md:171` «the interference was only significant in the low-prevalence session with TD group (p = .0357)» [result]

## C06: More target refixations in ASD (compensatory re-checking)
- **Statement**: Autistic participants returned to fixate the target location more often than TD participants overall.
- **Status**: supported
- **Falsification criteria**: No Group main effect on number of target-directed fixations/refixations.
- **Proof**: [E06]
- **Evidence basis**: Group main effect on target fixations, F(1,57) = 4.42, p = .040, ηp² = .072; remaining factors/interactions non-significant (Fs < 1.25, ps > .268).
- **Interpretation**: Compensatory re-checking — accumulating confirmatory evidence before committing to a response.
- **Dependencies**: none
- **Tags**: target refixation, re-checking, eye movements
- **Sources**:
  - "4.42" ← `docs/manuscript-r3.md:179` «autistic participants made more returns to targets than did TD participants overall, *F*(1, 57) = 4.42, *p* = .040, [η~p~² = .072,]{.mark}» [result]

## C07: Distractor-fixation pattern shows ASD learned the frequency prior
- **Statement**: A three-way Group × Distractor Prevalence × Singleton Presence interaction on distractor-location fixations indicates that under high prevalence, autistic participants avoided the cued location more strongly when no singleton appeared there — evidence they acquired the prevalence (frequency) prior.
- **Status**: supported
- **Falsification criteria**: Absence of the three-way interaction, or ASD failing to reduce cued-location fixations under high prevalence on singleton-absent trials.
- **Proof**: [E07]
- **Evidence basis**: Three-way Group × Prevalence × Singleton F(1,57) = 6.12, p = .016, ηp² = .097; Singleton main effect F(1,57) = 17.71, p < .001; Prevalence main effect F(1,57) = 14.28, p < .001; Prevalence × Singleton F(1,57) = 15.59, p < .001; Group × Singleton F(1,57) = 4.51, p = .038.
- **Interpretation**: ASD acquire both the explicit spatial prior (C03) and the statistical frequency prior, yet still over-engage when a singleton is present — locating the deficit at updating/disengagement, not prior acquisition.
- **Dependencies**: C03, C05
- **Tags**: distractor refixation, prevalence, frequency prior, three-way interaction
- **Sources**:
  - "6.12" ← `docs/manuscript-r3.md:181` «a three-way interaction among Group, Distractor Prevalence and Singleton Presence (*F*(1 ,57) = 6.12, *p* = .016, [η~p~² = .097]{.mark})» [result]
  - "4.51" ← `docs/manuscript-r3.md:181` «a clear interaction emerged between Group and Singleton Presence, *F*(1, 57) = 4.51, *p* = .038, [η~p~² = .073]{.mark}» [result]

## C08: Group difference in interference is robust to trait scores
- **Statement**: Once Group is included, trait BDI, STAI-Trait, and IU do not significantly predict distractor interference, while the Group effect remains significant — group differences in attentional control are not explained by affective comorbidity.
- **Status**: supported
- **Falsification criteria**: A significant trait predictor of interference after controlling for Group, or a non-significant Group effect once traits are included.
- **Proof**: [E08]
- **Evidence basis**: In the +Group+Prevalence models, trait p-values non-significant (BDI p = .738, STAI p = .720, IU p = .657) while Group remains significant for BDI (p = .005) and STAI (p = .027); a trait-only model showed IU significantly correlated with interference (p = .002).
- **Interpretation**: The authors note IU's trait-only association (and its disappearance after Group) is consistent with AIPU producing residual uncertainty rather than IU driving attentional control; directionality is not testable cross-sectionally.
- **Dependencies**: C01
- **Tags**: traits, BDI, STAI, intolerance of uncertainty, linear model, appendix
- **Sources**:
  - "p = .738" / "p = .005" ← `docs/appendix.md:16` «| BDI | 2: +Group+Prev | 1378.6 | 1392.5 | 36.184 | <.0001 | 2.927 | .738 | 25.124 | .005 | −1.455 | .846 | — | — | 6603.08 | 81.26 |» [result]
  - "p = .720" / "p = .027" ← `docs/appendix.md:19` «| STAI-Trait | 2: +Group+Prev | 1378.6 | 1392.5 | 36.121 | <.0001 | 3.851 | .720 | 23.881 | .027 | −1.455 | .846 | — | — | 6602.08 | 81.25 |» [result]
  - "IU ... p = .002" ← `docs/appendix.md:21` «| IU | 1: Trait | 1376.8 | 1385.1 | 34.907 | <.0001 | 24.515 | .002 | — | — | — | — | — | — | 6610.47 | 81.30 |» [result]
