# PRE-SUBMISSION REVIEW — 2026-03-04
## Paper: Competition or Chaos? Donor Fragmentation and the Politics of Aid Effectiveness
## Target journal: Journal of Development Economics / AEJ: Applied Economics

---

## TRIAGE — CRITICAL ISSUES (must fix before submission)

These issues would likely cause desk rejection or a fatal referee objection.

### C1. Heterogeneity direction is reversed in the text (Agent 2 #3)
**Location:** Line ~679. Text says: *"the fragmentation coefficient is more negative for low-capacity regions."*
**Fact:** `het_nl_admin1.tex` shows High Cap CFA = −4.808** (SE 1.707), Low Cap CFA = −2.419 (not significant). High-capacity regions have the larger, significant negative effect — the opposite of what the text states. This is the paper's central heterogeneity finding (H1) and it is described backwards. Either the saved table is from a superseded run and the live code produces a different result (in which case regenerate and recheck), or the narrative must be completely rewritten to explain why high-capacity regions suffer *more* from fragmentation — which requires a different theoretical interpretation.

### C2. OLS described as "significant" when it has no significance star (Agent 2 #2)
**Location:** Line ~658. *"The fragmentation coefficient is negative and significant across all three specifications."*
**Fact:** OLS column in `main_nl_admin1.tex` shows −3.577 (SE 2.525), no stars. Only FE and 2SLS are significant. Fix: "negative across all three specifications, and statistically significant in the fixed-effects and IV specifications."

### C3. Reduced-form coefficient described as "significant" when it is not (Agent 2 #5)
**Location:** Line ~813 in Appendix A. *"The reduced-form coefficient is negative and significant, consistent with the IV sign."*
**Fact:** `iv_diagnostics.tex` Column 2 shows −0.3172 (SE 0.3047), no stars. Fix: "negative, consistent in sign with the IV estimate, though imprecisely estimated."

### C4. CFA overclaims "recovers the causal effect of fragmentation" (Agents 3 #2, 4 #14)
**Location:** Line ~650. *"This recovers the causal effect of fragmentation conditional on instrumented aid volume."*
**Fact:** The CFA purges endogeneity of total aid volume. Fragmentation itself is not instrumented and remains potentially endogenous to unobservable regional characteristics (donor selection on crisis intensity, governance deterioration, etc.). Fix: "This partial control function approach addresses endogeneity of total aid volume, allowing fragmentation to be included alongside instrumented aid. The fragmentation coefficient is treated as conditionally exogenous after absorbing country fixed effects and aid-volume endogeneity; it should be interpreted as a conditional association, not a fully causal estimate."

### C5. CFA residual coefficient is ~10^{-10} — unit mismatch makes it uninterpretable (Agents 4 #6, 6)
**Location:** All CFA tables (`main_nl_admin1.tex`, `het_nl_admin1.tex`). CFA Residual = −2.52×10⁻¹⁰ (SE 1.69×10⁻⁹).
**Fact:** Stage 1 estimates `total_aid_raw` in raw USD (tens of millions); Stage 2 outcome is percent nightlights growth. The coefficient is vanishingly small purely due to unit mismatch. This also means the CFA endogeneity test shows no correction is occurring — which either means (a) aid is not endogenous conditional on country FE (undermining the motivation for the CFA entirely) or (b) the specification has a scaling error. The paper must address this directly. Options: (a) estimate Stage 1 in log-USD so residuals are on the same scale; (b) add a table note explaining the scaling and show that the F-test for the CFA residual is the relevant inference (not the coefficient magnitude); (c) reconsider whether the CFA is adding anything given that FE and CFA estimates are nearly identical.

### C6. "Three specifications" but four are estimated (Agent 1 #15)
**Location:** Line ~634. *"We estimate three specifications for comparison: (1) OLS … (2) Country FE … (3) 2SLS … (4) CFA."*
Fix: "four specifications."

### C7. Killick et al. cited and directly quoted but absent from references.bib (Agent 2 #8)
**Location:** Lines ~538, ~542. A block quotation is attributed to Killick et al. (2005) but this reference has no cite key and is not in `references.bib`. The paper will not compile cleanly and the quotation lacks a verifiable source. Fix: Add the reference to the bib file and convert to `[@killick_...]`.

### C8. No engagement with Goldsmith-Pinkham / Borusyak-Hull-Jaravel shift-share conditions (Agents 3 #6, 4 #9, 6)
**Location:** Empirical strategy / Appendix A. The paper uses a Bartik/shift-share IV but never discusses whether identification rests on exogenous shifts (donor fractionalization) or exogenous shares (historical aid targeting). These two conditions have distinct empirical implications. At a minimum: (i) state which condition is being relied upon; (ii) defend why donor government fractionalization is plausibly orthogonal to recipient-region growth trends; (iii) address whether historical shares (`p̄_{ji}`) are pre-period or within-sample (currently undefined — see C9). Any submission to JDE or AEJ:Applied will receive this comment from referees.

### C9. `p̄_{ji}` (share component of IV) does not define the baseline period or what `aid_{jit}` measures (Agent 4 #8)
**Location:** IV formula, line ~648. The formula `p̄_{ji} = (1/T)∑_t 𝟏[aid_{jit} > 0]` uses `aid_{jit}` which is never defined (disbursements? commitments? project counts?), and does not state whether T is the full panel or a pre-treatment window. For BHJ/GPS validity, shares should be from a pre-treatment baseline. Fix: Define `aid_{jit}` explicitly, state the baseline window, and note whether shares are fixed at pre-period.

---

## MAJOR ISSUES (fix before submission)

### M1. SUTVA / spatial spillovers never mentioned (Agent 3 #5)
The paper does not mention SUTVA violations or spatial spillovers anywhere. Donor fragmentation in one province could affect neighboring provinces through shared labor markets, donor programming decisions, or government coordination committees. Standard errors clustered at country level do not address within-country spatial autocorrelation. Add a paragraph acknowledging this limitation.

### M2. Fragmentation endogeneity is unresolved and not caveated (Agent 3 #7)
The paper's key causal concern (from CLAUDE.md) is donor selection into more/less fragmented regions. The CFA addresses aid *volume* endogeneity but not fragmentation endogeneity. High fragmentation could reflect crisis response (reverse causation), selection on poor governance, or institutional change. This threat is raised in the Admin2 discussion as an alternative to be dismissed, but is never directly caveated as an unresolved identification threat. Add a dedicated sentence in Section 3.

### M3. HHI equation shows the raw index, not the transformed variable used in regressions (Agent 4 #1)
**Location:** Section 3.1, equation. The displayed equation is `HHI_{it} = Σ π²_{jit}`, but the text says "we subtract from 1." The equation should be either `Frag_{it} = 1 − Σ π²_{jit}` or the raw HHI followed by a separate displayed transformation. As written, the regression variable and the displayed equation disagree.

### M4. Admin2 null needs a formal equivalence test, not just an assertion (Agent 6 Required Revision #5)
The paper claims the Admin2 near-zero coefficient is a "positive mechanistic result" ruling out alternatives. This requires formal support: report the Admin2 confidence intervals explicitly in text, show they exclude effects comparable in magnitude to Admin1, and ideally conduct an equivalence test. Currently the claim is asserted ("the intervals are informative enough to reject effects comparable to those at Admin1") without tabling the intervals.

### M5. No displayed Stage 2 CFA equation (Agent 4 #5)
The CFA is the primary heterogeneity estimator but no displayed equation shows its Stage 2 form. Add:
$$\Delta\text{NL}_{it} = \beta_1 \text{Frag}_{it-1} + \beta_2 \ln(\text{Aid}_{it-1}) + \beta_3 \ln(\text{Pop}_{it-1}) + \beta_4 \ln(\text{NL}_{it-1}) + \delta \hat{v}_{it} + \alpha_c + \varepsilon_{it}$$
where $\hat{v}_{it}$ are first-stage residuals from Stage 1.

### M6. CFA Stage 1 description omits `lag_frag` and `log_lag_nl` (Agent 4 #4)
The text describes Stage 1 as "total aid regressed on the Bartik IV plus controls" but does not enumerate the controls. Both `lag_frag` and `log_lag_nl` appear in Stage 1 (confirmed in code and `iv_diagnostics.tex`). Because fragmentation appears in Stage 1, this is important for the CFA interpretation — it means the IV variation in aid is already orthogonalized to fragmentation before entering Stage 2. State explicitly: "we regress total aid (raw USD) on the Bartik IV, lagged fragmentation, log population, and lagged log nightlights, with country fixed effects."

### M7. Afrobarometer rounds inconsistency: "rounds 3–6" vs. "rounds 1–6" (Agent 2 #7)
**Location:** Lines ~599, ~616 say "rounds 3–6"; line ~622 says "rounds 1–6." These cannot both be correct. Decide which rounds supply the SGQ items and which supply EA coordinates for geographic matching, and state them consistently.

### M8. Long-difference vs. full-panel FE discrepancy not explained (Agent 6 Required Revision #7)
`robust_estimator.tex` (on disk but not in the paper) shows the panel FE estimate of fragmentation is −1.05 (insignificant) vs. the long-difference estimate of −3.06 (significant at 10%). This is a non-trivial discrepancy. The paper must either include this comparison and explain why the long-difference is preferred (attenuation from within-unit differencing? composition effects?), or provide a footnote acknowledging it. Leaving it silently off-disk is not appropriate.

### M9. Admin1/Admin2 contrast used to "rule out" alternatives — argument is partially circular (Agent 3 #4)
**Location:** Lines ~706–712. The paper uses the Admin2 null to rule out factor-market competition and donor selection as alternative explanations, but the paper itself argues that Admin1 and Admin2 fragmentation measure "qualitatively different phenomena." If they are qualitatively different variables, the Admin2 null is consistent with the coordination mechanism but does not formally rule out alternatives that might operate differently at the two scales. Reframe as "consistent with" rather than "inconsistent with."

### M10. Exclusion restriction defense is insufficient; trade/FDI/diaspora channel not addressed (Agent 3 #6, Agent 6)
The paper identifies one potential exclusion restriction violation ("direct political channels") and dismisses it in one sentence. Additional threats: (a) politically fragmented donor governments may correlate with donor country economic health or institutional quality, affecting aid program quality independently of volume; (b) historical shares may encode persistent selection by region; (c) the pre-period placebo only rules out trend-based violations, not contemporaneous ones. Expand the exclusion restriction discussion.

### M11. Mechanisms stated as established facts rather than hypotheses (Agent 3 #8)
**Location:** Lines ~561–567. Three mechanism paragraphs describe how high-capacity governments "diminish coordination failures," "minimize duplicative paperwork," and produce "greater development outcomes" as facts. These are untested mechanisms — the paper has only a reduced-form correlation. Rewrite in the conditional voice: "We hypothesize that… If this mechanism operates, we would expect…"

### M12. All figures in `03_output/figs/` are orphaned — none referenced in `paper.qmd` (Agent 5 #30–39)
Twelve figure files exist on disk, including `fig2_binscatter_nl.pdf`, `fig3_first_stage.pdf`, `fig4_coef_plot.pdf`, `fig6_admin_comparison.pdf`, `interflex_binning_a1.pdf`, `interflex_binning_a2.pdf`. None are included in the paper via `knitr::include_graphics()` or any code chunk (the paper generates only `fig-sector` inline). Each must be explicitly included in the paper or deleted. The interflex plots are particularly important as non-parametric evidence for the heterogeneity finding.

### M13. `het_interaction_continuous.tex` exists but continuous interaction never appears in paper (Agent 5 #11)
This table tests the fragmentation × governance quality continuous interaction — a natural specification check on the binary split result — and is entirely absent from the paper. Include in Appendix B as a robustness check (or explain why it is omitted). Reviewers will ask for it.

### M14. `robust_estimator.tex` exists on disk but is never referenced (Agent 5 #7)
The long-difference vs. panel FE comparison (see M8) is on disk but not integrated. Given the discrepancy, this must be addressed.

### M15. Placebo test description overstates what non-significance demonstrates (Agent 3 #15, Agent 4 #7)
**Location:** Line ~813. The placebo IV coefficient (−0.289) is about 91% the magnitude of the reduced-form coefficient (−0.317). Calling this "close to zero" is contestable. More precise: "The instrument's reduced-form coefficient on pre-period growth (−0.289, SE 0.413) is statistically indistinguishable from zero and smaller than the contemporaneous reduced-form coefficient (−0.317), consistent with the exclusion restriction, though the point estimate is not negligible in magnitude. This test rules out trend-based violations but not contemporaneous exclusion restriction failures."

### M16. First-stage F-statistic not reported in Table A1 (Agents 2 #10, 5 #25)
The text at lines ~652 and ~813 promises that the first-stage F-statistic can be found in Appendix A, but `tbl-iv-diag` uses `fitstat = c("n", "r2")` and omits the F-statistic. Fix: add `"ivf"` to the fitstat argument.

### M17. High-capacity subsample has only 107 observations with ~40 country clusters (Agent 6 Required Revision #6)
Cluster-robust inference with 107 observations and ~40 clusters (some countries may contribute only 1–2 units to the high-capacity subsample) may be unreliable. Report wild cluster bootstrap p-values (or equivalent small-sample correction) for the high-capacity estimates. Also report how many distinct countries are in the high-capacity subsample — if the result is driven by 5–6 countries, it needs a jackknife robustness check.

### M18. `p̄_{ji}` shares undefined: within-sample vs. pre-period (Agent 4 #8, Agent 6 Q4)
The formula uses (1/T)∑_t 𝟏[aid_{jit} > 0] where the time window T is not defined. For shift-share validity (BHJ/GPS), shares should be pre-period. Clarify whether shares are computed over the full 1995–2015 window or a baseline sub-period, and what `aid_{jit}` refers to.

---

## MINOR ISSUES (fix if time permits)

### PROSE & STYLE (Agent 1)
- **mn1.** "three specifications" → "four specifications" (also a Critical issue C6, repeat here for completeness)
- **mn2.** "programme" → "program"; "artefact" → "artifact"; "towards" → "toward"; "co-ordination" → "coordination"; "sub-nationally" → "subnationally"; "sub-national" (one instance) → "subnational" (Agents 1 #6, 7, 8, 14, 33, 34)
- **mn3.** "Extant research highlights" → "Research shows..." (throat-clearing); "Indeed, a number of" → "Several" (Agent 1 #1, 2)
- **mn4.** "across 7 items" → "across seven items" (numbers one–nine spelled out) (Agent 1 #10, 35)
- **mn5.** "There is no shortage of research focusing on the problems..." → "Research on aid fragmentation has focused primarily on its costs." (Agent 1 #20)
- **mn6.** "In this paper, we attempt to reconcile" → "This paper reconciles" (Agent 1 #18)
- **mn7.** "Capitalizing on the GODAD dataset" → "Using the GODAD dataset" (Agent 1 #19)
- **mn8.** Driving innovation: "which can offer … and minimizes" → "that offers … and minimizes" (relative clause agreement) (Agent 1 #3)
- **mn9.** "political willingness" not operationalized anywhere; replace with "political incentives" (Agent 1 #27)
- **mn10.** Drop "Importantly," and "Crucially," as sentence adverbs (Agent 1 #25, 26)
- **mn11.** "This capacity helps governments to: (1)…" — colon after "to" is non-standard; restructure (Agent 1 #41)
- **mn12.** Duplicate government-capacity sentence across consecutive paragraphs (lines ~556–558) — merge or cut one (Agent 1 #28)
- **mn13.** "transform fragmentation from a liability into a more manageable constraint for economic growth" — a constraint is negative by definition; say "a manageable challenge" (Agent 1 #31)
- **mn14.** "The mono-project signature of project-level rather than portfolio-level engagement" — undefined neologism; replace with "the hallmark of project-level engagement" (Agent 1 #42)
- **mn15.** "the newly divergent empirical findings" — remove "newly" (Agent 1 #30)
- **mn16.** Road-map paragraph omits Section 5 (Conclusion) and the Appendix (Agent 1 #43)

### NOTATION & MATH (Agent 4)
- **mn17.** $\alpha_c$ introduced in specification (2) but subscript $c$ never defined as country index. Define when first introduced and carry through IV/CFA specifications (Agent 4 #11)
- **mn18.** SGQ formula: define $\bar{S}_i$ explicitly (mean of 7 items across what unit?) and clarify whether $S_{\min}/S_{\max}$ are sample extremes or theoretical bounds (Agent 4 #12)
- **mn19.** The 0.01 smoothing constant in the nightlights growth formula is not explained. Add a footnote noting it handles zero-luminosity observations and discuss potential bias in dark rural areas (Agent 4 #15)
- **mn20.** Table A1 note: "total aid (USD, lagged)" should read "total aid in raw USD levels (not log-transformed), lagged one 5-year period" to avoid confusion with `lag_total_aid` (log) in Stage 2 (Agent 4 #16)
- **mn21.** "well above conventional thresholds" — name the threshold: "well above the Stock-Yogo conventional threshold of 10 for weak instruments" (Agent 1 #44)
- **mn22.** `fractionalization` used in one instance as a synonym for `fragmentation`; reserve `fractionalization` exclusively for the DPI political science variable (Agent 1 #4)
- **mn23.** "We subtract from 1, so that higher values indicate more dispersed donor portfolios" is clearer as "We define the fragmentation index as $1-\text{HHI}_{it}$, so that…" — makes the transformation explicit immediately below the HHI equation (Agent 1 #45)

### TABLES & FIGURES (Agent 5)
- **mn24.** All inline tables: dependent variable header rows show raw R variable names (`nl_growth`, `u5m`, `pre_nl_growth`). Add `depvar.labels=` argument to each `etable()` call with decoded labels: "$\Delta$ NL Growth (pp/yr)", "Under-5 Mortality Rate", "$\Delta$ NL Growth (Pre-period)" (Agent 5 #28, 29)
- **mn25.** Table 1 notes: add sample/period/coverage sentence: "Sample: [n_countries] Sub-Saharan African countries, 2005–2015 (5-year windows, admin1 level)" (Agent 5 #18)
- **mn26.** Table 2 column headers: "Hi Admin1" → "(1) A1 High Cap (CFA)"; add note "Admin1 cols use CFA; Admin2 cols use country FE only (IV not available at Admin2 level)" (Agent 5 #20, 21)
- **mn27.** Table 3 notes: add "Stacked panels include 2010 and 2015 windows with unit and country-by-year fixed effects." (Agent 5 #22)
- **mn28.** Table 4 notes: add "U5M measured as the probability of death before age 5 (IHME GBD), level (not growth rate)." (Agent 5 #23)
- **mn29.** Table A1 notes: clarify that total aid coefficient is in raw USD (not log); add unit line: "Total aid in 2014 constant USD." (Agent 5 #24)
- **mn30.** Table B2 notes: define all alternative fragmentation measures — "Frag. Index = 1−ΣπΩ²; Tail Share = aid share outside top-3 donors; Small Donors = count of donors with <10% share." (Agent 5 #26)
- **mn31.** Table B3 notes: define facilities and services indices (Afrobarometer enumerator-observed counts, scales, and aggregation level) (Agent 5 #27)
- **mn32.** Figure 1 (fig-sector chunk): remove `title=` from `ggplot2::labs()` — the `fig-cap` already provides the caption, producing duplicate titles in PDF output (Agents 1 #11, 5 #40)
- **mn33.** Figure 1 caption: add data source — "Source: GODAD geocoded aid data; Sub-Saharan Africa, 2005–2015." (Agent 5 #41)
- **mn34.** Standardize appendix table LaTeX labels to match prose numbering: `label="tab:a1"`, `label="tab:b1"`, etc. Currently `tab:iv_diag`, `tab:wins`, etc. do not match (Agent 5 #42)
- **mn35.** Orphaned `.tex` files audit: `main_nl_admin1.tex`, `main_u5m_admin1.tex`, `het_nl_admin1.tex`, `het_u5m_admin1.tex`, `iv_diagnostics.tex`, `robust_winsorization.tex`, `ols_nl_growth_5year.tex`, `fe_nl_growth_5year.tex`, `sector_u5m_admin1.tex` — delete or reconcile with rendered paper content (Agent 5 #1–16)
- **mn36.** `sector_nl_admin1.tex` on disk (tabular form of sector analysis); decide whether to add to Appendix B or delete — readers wanting precise sector coefficients cannot read them from Figure 1 alone (Agent 5 #10)

### IDENTIFICATION FRAMING (Agent 3 — lower severity)
- **mn37.** Headline magnitude claim (0.4 pp/yr from 25th–75th percentile) has no confidence interval in text. Report: "…(95% CI: [X, Y])" and state the 25th/75th percentile values of fragmentation used for the calculation (Agent 3 #9)
- **mn38.** Governance capacity moderator (SGQ) treated as exogenous without argument. Add note: "SGQ is treated as predetermined, but governance quality may itself be shaped by historical aid coordination, limiting interpretation of the interaction as purely exogenous moderation." (Agent 3 #12)
- **mn39.** Sector analysis lacks IV/CFA; state explicitly that sector estimates are OLS with country FE only and are "suggestive evidence for the mechanism, not causal decompositions" (Agent 3 #10)
- **mn40.** "Consistent across estimators" in conclusion is imprecise — OLS and IV are designed to have different properties. Be specific: "The negative fragmentation coefficient is stable in sign and approximate magnitude across specifications; the IV estimate is larger than OLS, consistent with attenuation bias from measurement error." (Agent 3 #13)
- **mn41.** SSA generalization may be too broad given data-availability selection (Agent 3 #14). Add: "Our findings are conditional on data availability and may not generalize to the most fragile or conflict-affected contexts where geocoded aid, nightlights, and Afrobarometer coverage are most sparse."
- **mn42.** "points specifically to" in conclusion → "is most consistent with"; add caveat that Admin2 CFA is unavailable, so the scale comparison has different identification strength at each level (Agent 3 #16)

---

## AGENT REPORTS (full detail)

### Agent 1 — Copy Editor
45 issues covering: British spellings (programme, artefact, co-ordination, towards, sub-national); "three specifications" listing four; hedging openers (Extant, Indeed, "In this paper we attempt"); passive constructions; tautological repetition of the government-capacity sentence; dangling modifier ("Following closely @citation"); colon after infinitive "to:"; numeral vs. word inconsistency ("7 items"); inline `$r = `r scale_corr`$` fragility; inconsistent use of "fractionalization" vs. "fragmentation"; Figure 1 title duplication; road-map omits Conclusion section; missing comma after introductory phrase.

Key issues: #3 (pronoun agreement), #9 (awkward modifier), #15 (count error), #28 (duplicate sentences), #38 (inline r expression inside math block is fragile), #44 (threshold should be named), #45 (HHI transformation should be displayed as equation).

### Agent 2 — Consistency Checker
12 issues. Critical: OLS significance overstated (C2); heterogeneity direction reversed (C1); reduced-form significance overstated (C3); Killick et al. in text but not in bib (C7). Major: Afrobarometer rounds mismatch (M7); F-stat not in Table A1 (M16); Table 1 and Table 2 saved .tex files do not match live code structure. The saved files appear to be from an earlier version of the analysis pipeline; the rendered paper (via live code chunks) will differ in column structure, observation counts, and possibly in heterogeneity direction from the pre-saved tables.

### Agent 3 — Claims & Identification Auditor
16 issues across: CFA overclaim (C4); abstract causal language for unidentified coefficient (C4 companion); SUTVA entirely absent (M1); exclusion restriction defense insufficient (M10); fragmentation endogeneity unresolved (M2); mechanisms stated as facts (M11); Admin2 comparison argument partially circular (M9); magnitude claim lacks CI (mn37); sector analysis overstates identification (mn39); moderator endogeneity not noted (mn38); placebo overstated (M15); conclusion language too strong (mn42); estimator consistency claim imprecise (mn40); SSA generalization too broad (mn41).

### Agent 4 — Mathematics & Econometrics Reviewer
18 issues. Critical/High: HHI equation shows raw index not transformation (M3); no displayed Stage 2 CFA equation (M5); CFA residual ~10⁻¹⁰ from level-vs.-log unit mismatch (C5); no GPS/BHJ discussion (C8); shares baseline period undefined (C9); fragmentation exogeneity assumption never stated (C4 companion). Medium: CFA Stage 1 controls not enumerated (M6); het table saved file shows different columns than live code (M structure); SGQ normalization undefined (mn18); `p̄_{ji}` baseline period undefined (M18). Low: α_c subscript never defined (mn17); 0.01 smoothing constant unexplained (mn19); F-stat not in Appendix A table (M16); table note "total aid (USD, lagged)" ambiguous re: log vs. level (mn20). No broken LaTeX math syntax detected.

### Agent 5 — Tables & Figures Auditor
43 issues. All 12 figures in `03_output/figs/` are orphaned (M12); 16 `.tex` files are orphaned or version-mismatched (mn35); raw variable names in dep var rows across all tables (mn24); F-stat missing from Table A1 (M16); `het_interaction_continuous.tex` not integrated (M13); `robust_estimator.tex` not integrated (M14); Admin2 U5M results (`admin2_u5m.tex`) not in paper; Table 2 column headers ambiguous (mn26); Table 4 does not define U5M scale (mn28); Table B2 does not define alternative fragmentation measures (mn30); Figure 1 caption lacks data source (mn33); figure title duplicated in ggplot labs() and fig-cap (mn32).

### Agent 6 — Referee Report (Journal of Development Economics / AEJ: Applied)

**Summary:** This paper asks whether donor fragmentation improves or worsens local economic development and whether local government capacity moderates the relationship. Using geocoded GODAD aid data matched to ~40 Sub-Saharan African countries at admin1 and admin2 levels, 2005–2015, the paper constructs an HHI-based fragmentation index and estimates effects on nightlights growth and under-5 mortality. The headline findings are a negative fragmentation effect on provincial growth, attenuated in high-governance-capacity regions, and a near-zero effect at district level. The Admin1/Admin2 contrast is presented as direct evidence for a portfolio-coordination bandwidth mechanism.

**Contribution:** The subnational scale and two-level administrative design are genuinely creative contributions to a literature that has operated primarily at the national level. The mechanistic use of the Admin1/Admin2 contrast (not merely as a robustness check but as a within-sample test of a spatial prediction) is distinctive. Whether this clears the non-incremental bar depends heavily on whether the Admin2 null survives a formal equivalence test (see Required Revision 5). The paper does not engage with Djankov et al. (2009), Bourguignon & Sundberg (2007), or the Goldsmith-Pinkham/BHJ shift-share IV literature — conspicuous omissions.

**Identification assessment:**
- *First stage:* F = 104.8 clears Stock-Yogo thresholds, but the paper should report the Kleibergen-Paap rk Wald F or Olea-Pflueger effective F robust to cluster-robust variance; with ~40 clusters, asymptotic approximations may not be reliable.
- *Exclusion restriction:* Defense is one sentence and addresses only the "direct political channels" threat. Trade/FDI/diaspora and historical-shares endogeneity are unaddressed. No GPS/BHJ discussion.
- *CFA vs. 2SLS:* In the linear case with one endogenous regressor, CFA and 2SLS are numerically equivalent — the distinction is only SE correction for the generated regressor. Standard errors must be bootstrapped or analytically corrected; the paper does not state whether this was done.
- *Negligible CFA residuals:* The CFA endogeneity control term is always statistically indistinguishable from zero by multiple orders of magnitude. This undermines the motivation for the CFA approach entirely. Either aid is not endogenous conditional on country FE (requiring revision of the identification narrative) or there is a unit scaling error (requiring correction).

**Required revisions:**
1. Report Kleibergen-Paap or Olea-Pflueger F-statistic robust to clustering.
2. Expand exclusion restriction defense: address shares exogeneity, GPS/BHJ conditions, trade/FDI/diaspora threats.
3. Bootstrap or analytically correct CFA standard errors for the generated-regressor problem.
4. Address the negligible CFA residuals directly: either explain the unit-scaling issue, or reframe whether the CFA is providing any correction.
5. Provide formal equivalence test or power analysis to sustain the Admin2 null as a positive mechanistic result.
6. Report wild cluster bootstrap p-values for high-capacity subsample estimates (107 obs, ~40 clusters).
7. Include and explain the long-difference vs. panel FE comparison (`robust_estimator.tex`).

**Suggested analyses:**
1. Hausman-type test for fragmentation endogeneity.
2. Falsification using near-zero-aid regions (fragmentation should have no effect where there is no aid portfolio to coordinate).
3. Continuous interaction of fragmentation × SGQ alongside the binary split.
4. Pre-period share orthogonality table (correlate historical shares with pre-period outcome levels and trends).
5. Sensitivity excluding conflict-affected regions (ACLED).

**Pointed questions (selected):**
1. The CFA endogeneity control term is negligible across every specification. If aid is not endogenous conditional on country FE, what identification problem is the CFA solving?
2. The Admin2 sample has more observations (2,160) than Admin1 (529) — so power is not obviously the issue. Can you show the Admin2 confidence intervals explicitly and demonstrate they rule out Admin1-magnitude effects?
3. The fragmentation index is mechanically near zero when there is only one donor (HHI = 1). How are single-donor observations distributed, and do results change if restricted to units with ≥2 active donors?
4. The share component `p̄_{ji}` is computed from the within-sample window. Is this a pre-period baseline? Can you show that share-weighted fractionalization changes are uncorrelated with pre-period growth trends?
5. How many distinct countries contribute to the high-capacity subsample? Does the capacity heterogeneity result survive a jackknife by country?

**Verdict: Major Revision.** The research question is well-motivated, the two-level design is creative, and the main findings are plausible. But the identification narrative contains unresolved issues — the negligible CFA residuals, the absence of GPS/BHJ conditions, the need for formal support for the Admin2 null, and the unaddressed long-diff vs. panel FE discrepancy — that must be corrected before external review.
