# Three-survey reanalysis: main findings (revised 2026-11-23)

Source: raw `../Data/{ih,nk,sudan}.csv`, filtered to Finished + consent ("Yes, I am").
N = 3,294 (IH, fielded Dec 9–10, 2024), 1,398 (NK) and 1,404 (Sudan), both fielded Oct 26 – Nov 7, 2026.
855 people took both the NK and the Sudan survey.

## Coding (primary, since 2026-11-23)

Condition X = suffering *of* side X's community. Side A = Israel / Azerbaijan / SAF (the actor Q6/Q7 judge); side B = Palestinians / Armenians / RSF.

Every respondent is kept, and non-committal answers sit at the scale midpoint:

| Outcome | Item | Coding |
|---|---|---|
| `symp_A` | Q5 | 1 entirely B … 5 entirely A; equally / neither / not sure = 3 |
| `justified` | Q6 | 1 gone too far, 2 don't know, 3 justified |
| `us_lenient` | Q7 | 1 reduce aid, 2 diplomatic pressure **or** don't know, 3 no pressure |
| `prior_A` | Pr5/6 (pre-treatment) | +1 A, −1 B, 0 both / neither / don't know |

The old substantive-only codings are kept as `*_subst`. Three-way shares are in `12_coding_robustness.R`.

**Why the change:** the posts changed who gave a substantive answer. For example, the Sudan SAF statistics post raised the share answering Q6 by 23 points. Dropping the non-committal answers therefore mixed persuasion with selection, and it inflated the large NK/Sudan effects: Sudan B posts on Q5 were −0.67 substantive-only vs −0.31 at the midpoint.

**Q7 as four outcomes (since 2026-11-24).** In the effect figures (`04`, `05`, `07`), `16_q7_binary.R` and the dashboard, Q7 is analysed as four 0/1 shares on the full sample, which sum to 100%: `q7_reduce` (reduce military aid to A), `q7_pressure` (diplomatic pressure, keep aid), `q7_nopress` (let A act without pressure) and `dk_us` (don't know). The 1–3 `us_lenient` scale remains only inside the Q5–Q7 victim-direction index (`13`).

Pooled across surveys:
- **Palestine / Armenia / RSF posts** raise "reduce aid" by 5.1 points and lower "no pressure" by 6.6 (both p < .01).
- **Israel / Azerbaijan / SAF posts** mostly raise "don't know" (+3.8, p < .05).
- **Sudan:** RSF posts move "reduce aid" +15 and "no pressure" −14.
- **NK:** posts about either side raise "diplomatic pressure" by about 7 points.

**Q6 as three outcomes (since 2026-11-24).** Q6 is likewise analysed as three 0/1 shares on the full sample, which sum to 100%: `q6_toofar` (A has gone too far), `q6_defend` (A is defending its interests) and `dk_justified` (don't know). This applies to `04`, `05`, `07`, `17_q6_binary.R` and the dashboard; the 1–3 `justified` scale remains only inside the index (`13`).

Pooled across surveys:
- **Palestine / Armenia / RSF posts** raise "gone too far" by 7.6 points and lower "defending" by 5.4 (both p < .01).
- **Israel / Azerbaijan / SAF posts** move neither.
- **Sudan:** RSF posts move "gone too far" +16.8 and "don't know" −9.5.
- **NK:** Azerbaijan posts lower "gone too far" by 8.6 and raise "don't know" by 10.3.

## Hypotheses (`13_victim_direction.R`; primary = Lin adjustment for prior sympathy)

Victim-direction effect = average shift toward the side whose suffering was shown, over posts about both sides. Index = mean of Q5–Q7, each in control-group SDs.

| Index, control SDs | IH | NK | Sudan | Pooled |
|---|---|---|---|---|
| Witness | +0.07\*\* | +0.09\*\* | +0.11\*\* | +0.08\*\* |
| News story | +0.07\*\* | +0.10\*\* | +0.16\*\* | +0.10\*\* |
| Statistics | +0.06\*\* | +0.34\*\* | +0.34\*\* | +0.19\*\* |
| **H3a** personal − statistics | +0.01 | **−0.24\*\*** | **−0.21\*\*** | **−0.10\*\*** |
| **H3b** witness − news story | 0.00 | −0.02 | −0.05 | −0.01 |

- **H1 and H2 are supported in all three surveys, including Israel-Hamas.** Adjusting for prior sympathy roughly halves the IH standard error, because prior and post sympathy correlate at r = .85 in control.
- **H3a is reversed:** statistics move attitudes more than personal narratives in NK and Sudan; there's no difference in IH.
- **H3b is not supported anywhere.**
- Results hold among first-time takers (people who hadn't already taken the other survey).

## Behaviour and emotion (`14_treated_only_outcomes.R`, treated arms only)

- **Personal narratives raise willingness to donate** relative to statistics: IH +6.9 points, NK +8.9 points (both p < .01). The NK Armenian witness post reached 69%, versus 51% after the Armenian statistics post.
- **Narratives evoke more sadness** (IH, NK) and anger (NK).
- **So statistics move attitudes while narratives move emotion and giving.**
- Donation amounts above $10,000 (12 answers, up to 6×10¹⁷) are dropped; the rest are winsorized at the 99th percentile.

## Other results

- **Cross-survey pooled effects** (`07`) use SEs clustered by participant. In control-group SDs, sympathy shifts about ±0.2 SD; conclusions are unchanged by clustering.
- **Spillover** (`08`, `11`): no reliable effect on attention to, or sympathy about, other conflicts. Same-conflict sympathy partly persists into the next survey.
- **Survey order** (`10`): no order test survives correction. NK sympathy effects differ for people who took Sudan first (unadjusted p = .015).
- **Heterogeneity** (`06`, dashboard): under the midpoint coding, only two interactions pass BH q < .10, both in Sudan and on Q5: views on military force, and prior Israel-Palestine sympathy.
- **IH-only outcomes** (`15`): the hostages item, thermometers and social distance show nothing after correction.

## Data and design cautions

- **Questionnaire PDFs:** the 6-26 PDFs differ from the fielded surveys (different manipulation-check options). Treat them as drafts.
- **Manipulation check:** 84–98% pass in 16 of 18 arms. The NK statistics arms reach only 51–60%, because respondents split between two numbers (155 vs 19,000; 190 vs 90,000). Check what those posts showed, since this is the arm with the largest effects.
- **Sudan Pr2_4** ("followed the Sudan war") looks miscoded and is excluded from controls and moderators for Sudan.
- **Sample size:** the co-authors' preliminary script implies N ≈ 1,170 (NK) and 1,209 (Sudan) — still to reconcile.
- **Comparing effect sizes across conflicts** is confounded by fielding date (2024 vs 2026), sample (only NK/Sudan have panel IDs; dropout 1–2% vs 7–13%), vignette content and opinion rates. Present it as a cross-study comparison in common units, not as a salience test.
- **No survey weights:** avoid population claims in the descriptives.
- **Fine:** dropout is unrelated to condition, there's no speeding, and randomization is balanced. The "Select England" check passes at ~100% and is uninformative.
