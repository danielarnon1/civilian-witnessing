# Three-survey re-estimation: main findings (2026-11-20)

Source: raw `../Data/{ih,nk,sudan}.csv`, filtered to Finished + consent ("Yes, I am").
N = 3,294 (IH), 1,398 (NK), 1,404 (Sudan). Each arm vs Control, `lm_robust` (HC2), no multiple-comparison correction.

**Coding.** Condition X = suffering *of* side X's community (checked against the questionnaires).
Side A = Israel / Azerbaijan / SAF (the actor that Q6/Q7 judge). Side B = Palestinians / Armenians / RSF.
Higher = more favorable to side A.

| Outcome | Item | Scale |
|---|---|---|
| `sympathy` | Q5 | 1–5; "Not sure" / "Neither" dropped |
| `justified` | Q6 | 1 = A's actions justified; "Don't know" dropped |
| `us_lenient` | Q7 | 1 reduce aid · 2 pressure · 3 let A act; "Don't know" dropped |
| `milaid` | Q12 | importance of military aid to A, 1–5 |
| `ceasefire`, `humanit` | Q10, Q11 | 1–5 |

## Pooled side effects vs control (`effects_pooled_by_side.csv`)

| Sympathy | IH | NK | Sudan |
|---|---|---|---|
| Any side-A post | +0.13 (p=.06) | **+0.52** | **+0.49** |
| Any side-B post | **−0.16** | **−0.25** | **−0.67** |
| % "Not sure" / "Neither" on sympathy | 15% | 36% | 55% |
| % "Don't know" on Q6 | 24% | 52% | 61% |

## Takeaways

1. **Salience.** IH effects are small and mostly null; they replicate the draft (Palestine News −0.19, Statistics −0.17). In NK and Sudan, sympathy shifts are 3–5× larger, and the foreign-policy items move as well.
2. **Mobilization.** In NK and Sudan, posts about *either* side raise the importance of a ceasefire and humanitarian relief. This doesn't happen in IH.
   - **Q7 (U.S. approach, 1 reduce aid – 3 no pressure):** posts about Armenian or RSF-side suffering lower support for leaving A unpressured (pooled NK −0.18, Sudan −0.55). In Sudan, SAF-side posts do too (−0.21); in NK, Azerbaijan-side posts don't (+0.04).
   - *Correction (2026-11-20):* the first version of `01_harmonize_and_effects.R` miscoded Q7. The "diplomatic pressure, but **not reduce** aid" answer matched "reduce" first and was coded as "reduce aid", which overstated the Q7 effects. Fixed and rerun; all Q7 numbers here and in the CSVs are corrected.
3. **Format.** Statistics move sympathies most in NK and Sudan (significant side × format interactions). Witness and news-outlet versions don't differ consistently, so H3a and H3b are not supported.
4. **Asymmetries.**
   - NK: the Armenia witness post moves sympathy ≈0, while Armenia statistics moves it −0.48.
   - Sudan: posts about *both* sides lower the share saying SAF's actions are justified.

## Arm-by-arm treatment effects (`04_treatment_effects.R`)

Each arm vs Control, by survey, for nine outcomes. Results are in `treatment_effects.csv` and figures in `plots/TE_*.png`. Variable definitions are in `00_prepare.R`, which every script from 03 onward uses.

## Caveats / open items

- **Original scripts don't run as shipped.** `Cleaning_NK.R` and `Cleaning_Sudan.R` never build `attentioncheck`, `donate` or `donate_amt` (they fail at line 100). Under R 4.x, all three cleaning scripts also fail at the first `relevel()` on a character variable. The scripts are left untouched.
- **Sample sizes differ.** Comments in `Preliminary Analysis.R` imply N ≈ 1,170 (NK) and 1,209 (Sudan); here N = 1,398 and 1,404. Some additional filter isn't reproduced yet.
- **Feelings items (Q4).** These weren't asked in Control, so they can only be compared across treated arms.
