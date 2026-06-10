# PRE-SUBMISSION REVIEW — 2026-03-05

## Paper: Competition or Chaos? Donor Fragmentation and Aid Effectiveness at the Sub-National Level

## Target journal: Journal of Development Economics

---

## TRIAGE — Critical issues (must fix before submission)

**C1 — Table 1 column mismatch: code renders 3 columns, saved `.tex` has 4**
The `tbl-main` chunk passes `m_ols, m_fe, m_2sls` (three models), but `main_nl_admin1.tex` on disk shows four columns (OLS / Country FE / CFA / 2SLS). The saved file is stale. When the paper is compiled, Table 1 will have three columns; if the saved `.tex` is used directly, readers see four. Decision needed: add `m_cfa` back to the chunk and update headers/prose, or confirm the three-column structure is canonical and regenerate the saved file.
_(Agents 2, 5)_

**C2 — Country-FE coefficient has a significance star but prose says it "spans zero"**
`main_nl_admin1.tex` shows Country FE fragmentation = −3.062\* (significant at 10%). The current prose says "The OLS and country-FE estimates are imprecisely estimated with confidence intervals that include zero." Only the OLS (−3.577, no star) is actually imprecise. Fix: "The OLS estimate is imprecise (confidence interval includes zero); the country-FE estimate is marginally significant at the 10% level (−3.062\*)."
_(Agent 2)_

**C3 — Missing bib entry for `@borusyak_quasi-experimental_2022`**
This citation (line 662) will produce an undefined-reference error at compile time. The entry is absent from `references.bib`. Add it immediately.
_(Agent 2)_

**C4 — Heterogeneity sign anomaly inadequately addressed**
H1 predicts fragmentation harms low-capacity regions more. Table 2 (het_nl_admin1.tex) shows the opposite: High-capacity CFA = −4.81\*\*, Low-capacity CFA = −2.42 (n.s.). The prose now acknowledges the subgroup contrast is "not statistically distinguishable" but frames this as directional support for H1. A referee will flag this as a major problem. The paper needs a frank theoretical explanation for why high-capacity regions show the larger significant coefficient — or it must reframe H1 and refocus the contribution. See Referee Question Q1.
_(Agents 2, 6)_

**C5 — CFA residual ~10⁻¹⁰: unit mismatch requires formal resolution**
The CFA residual coefficient is of order 10⁻¹⁰ because Stage 1 is in USD levels while Stage 2 uses log-percent growth. The paper acknowledges this and notes γ is "not interpreted substantively," but the referee correctly flags this as insufficient for a JDE submission. Required: either (a) rescale the first stage to millions of USD or log-aid, so the CFA residual is interpretable as a Wu-Hausman test of endogeneity; or (b) formally report a Wu-Hausman statistic and discuss what the near-zero test implies (aid volume is conditionally exogenous, weakening the IV motivation).
_(Agents 4, 6)_

**C6 — Reduced-form causal chain description is incorrect**
Appendix A (line 834) describes the reduced-form pathway as "donor fractionalization → aid volume → fragmentation → growth." This is wrong: the IV instruments aid volume, not fragmentation. Fragmentation is a conditioning covariate, not a mediator. The correct chain is: "donor fractionalization → total aid volume → nightlights growth (with fragmentation conditioning the second-stage regression)." Fix this sentence immediately — it misrepresents the design.
_(Agents 3, 4)_

**C7 — All table dependent-variable headers will render as raw R variable names**
`etable()` does not rename the "Dependent Variable:" header row via `dict`; only regressor row labels are renamed. All tables will show `nl_growth`, `u5m`, and `total_aid_raw` in the dependent variable row. Fix: add `depvar.labels = c(...)` to every `etable()` call:

- Tables 1–3, B1–B2: `depvar.labels = "$\\Delta$ NL Growth"`
- Table 4: `depvar.labels = c("Under-5 Mortality", ...)`
- Table A1: `depvar.labels = c("Total Aid (USD)", "$\\Delta$ NL Growth", "$\\Delta$ NL Growth (Pre-Period)")`
  _(Agent 5)_

**C8 — IV diagnostics insufficient relative to shift-share standards**
Appendix A contains only first-stage, reduced-form, and pre-trend placebo. Missing: (a) a test for correlation between baseline exposure shares and pre-period growth trends (Goldsmith-Pinkham overidentification); (b) a Rotemberg-weight concentration diagnostic showing which donors drive the identifying variation; (c) a check that main results survive controlling for aid composition (bilateral/multilateral share, budget support share). Without these, the exclusion restriction defense is not credible at a top field journal.
_(Agent 6)_

**C9 — Abstract overstates heterogeneity finding**
Abstract says "This negative association is attenuated in regions with stronger subnational governance capacity." The body (correctly) shows the high-capacity coefficient is larger and significant, the low-capacity coefficient is smaller and insignificant, and the subgroup difference is statistically indistinguishable. The abstract should read: "The negative association is no larger — and may be smaller — in regions with stronger governance capacity, consistent with a capacity-moderation hypothesis, though the subgroup contrast is not statistically significant at conventional levels."
_(Agents 1, 2)_

---

## MAJOR issues (fix before submission)

**M1 — Table 2 column mismatch: code renders Admin2 FE, saved `.tex` has Admin1 2SLS**
`het_nl_admin1.tex` shows columns labeled "High Cap (2SLS) / Low Cap (2SLS)" for columns 3–4. The current chunk passes `hi_a2_fe, lo_a2_fe` (Admin2 country FE). The saved file is stale. Regenerate.
_(Agent 5)_

**M2 — Table 3 "similar magnitude" claim is factually incorrect**
Prose (line 724): "negative coefficients on fragmentation of similar sign and magnitude." `robust_stacked_window.tex`: Long-Diff = −3.058\*, Stacked = −1.050 (n.s.). The stacked estimate is one-third the size. Remove "similar magnitude"; note the attenuation and discuss what drives it (see Referee Q6).
_(Agent 2)_

**M3 — Table 4 column count mismatch: code has 5 columns, saved `.tex` has 4**
`main_u5m_admin1.tex` shows OLS / Country FE / CFA / 2SLS (four columns). The `tbl-u5m` chunk passes five models including the capacity split. The saved file is stale. Regenerate.
_(Agent 5)_

**M4 — Stacked FE vs. long-difference magnitude discrepancy (~3×) unaddressed**
The primary (long-diff) estimate is −3.058\*; the stacked panel-FE is −1.050 (n.s.). This three-fold gap is large and goes unexplained. The paper presents the stacked estimate as robustness, but a referee will ask whether the stacked estimate is the more credible one (more FE, more variation). Either discuss the decomposition of this gap or promote the stacked estimate to co-primary.
_(Agent 6)_

**M5 — SGQ endogeneity caveat confined to Data section**
The caveat that "governance quality may itself be partly shaped by historical aid patterns" appears only at line 632. It is absent from the Table 2 discussion and the Conclusion where it is most needed. Repeat it in both locations. Consider restricting the SGQ split to pre-period (pre-2005) SGQ values as a robustness check.
_(Agent 3)_

**M6 — Aid-quality exclusion restriction threat not addressed**
The paper defends against trade-policy and diplomatic channels, but not against the concern that fractionalized donor governments change _aid type_ (project vs. budget support, tied vs. untied, sectoral composition) independently of total volume. This is an exclusion restriction violation that the pre-trend placebo does not test. Acknowledge explicitly and ideally provide a robustness check controlling for aid composition.
_(Agents 3, 6)_

**M7 — SUTVA dismissal without evidence**
Line 662 dismisses spillover concerns by noting "administrative units are large enough that within-country spillovers are unlikely to dominate." This is an assertion without evidence. Add either Conley standard errors or a spatial-lag test to Appendix B, or honestly acknowledge this as a limitation.
_(Agent 3)_

**M8 — IV quantitative magnitude stated without confidence interval**
Line 680: "reduces annualized nightlights growth by approximately 0.4 percentage points per year." Report the 95% CI around this estimate. Given ~40 clusters, the effective degrees of freedom are limited.
_(Agent 3)_

**M9 — Multiple comparisons not acknowledged**
The paper tests ~20–25 fragmentation coefficients across tables and Figure 1. No multiple-comparisons caveat exists. Add one sentence in the empirical strategy: "We do not apply multiple-testing corrections; estimates in the sector analysis and robustness tables should be interpreted as exploratory."
_(Agent 3)_

**M10 — Stage 1 CFA equation: `Aid_{it}` vs `Aid_{i,t-1}` subscript inconsistency**
The Stage 1 equation uses `Aid_{it}` (current period) on the left, but Stage 2 uses `ln(Aid_{i,t-1})` (lagged). In the long-difference setup, `total_aid_raw` is already the lagged value; both equations should use consistent `t-1` subscripts. Fix Stage 1 LHS to `Aid_{i,t-1}` and note the subscript convention.
_(Agent 4)_

**M11 — IV formula defines `IV_{it}` but uses the lag `IV_{i,t-1}`**
Line 658 defines the instrument as `IV_{it}`, then line 660 says "We use the lag of this instrument, IV*{i,t-1}." Better to define the formula with the t−1 subscript directly: `IV*{i,t-1} = Σ*j p̄*{ji} · Frac\_{j,t-1}`.
_(Agent 4)_

**M12 — CFA Stage 2 residual subscript: `η̂_{it}` should be `η̂_{i,t-1}`**
The residual is generated from a Stage 1 regression of `t-1` variables; write it as `η̂_{i,t-1}` in Stage 2 for notational consistency.
_(Agent 4)_

**M13 — `N` in HHI summation never defined symbolically**
Line 615: `HHI_{it} = Σ_{j=1}^{N} π_{jit}²`. The upper bound `N` is never defined. Add "where `N_{it}` is the number of active donors in unit `i` period `t`." Also note the symbol `π_{jit}` should be defined before or simultaneously with the equation.
_(Agent 4)_

**M14 — Table A1: `fitstat = "ivf"` silently produces blank rows**
`ivf` is a 2SLS statistic; it does not apply to the standalone OLS `first_stage` feols object. The F-stat row will be blank. Fix: use `fitstat = c("n", "r2", "f")` for the OLS first-stage, or add `extralines` to manually insert the F-stat from `fs_fstat_val`. Also note in the table caption that the F-statistic cited in prose (104.8) comes from the 2SLS model in Table 1.
_(Agents 4, 5)_

**M15 — All tables missing sample definition, time period, unit of observation in notes**
No table note specifies: Sub-Saharan Africa, 2005–2015, Admin1 provinces (or Admin2 districts), or number of countries. JDE requires this. Add "Sample: Sub-Saharan African provinces (Admin1), 2005–2015. Unit of observation: administrative region." to every table note (adapt for Admin2 tables).
_(Agent 5)_

**M16 — Table B2: alternative fragmentation measures not defined in table notes**
"Tail Share" (fraction outside top three donors) and "Small Donors" (count with <10% share) are not defined in the table. Add definitions to the notes.
_(Agent 5)_

**M17 — Figure 1 missing `#| label: fig-sector` chunk option**
The chunk is named `{r fig-sector}` but has no `#| label: fig-sector` option inside the chunk. For Quarto cross-reference machinery, add `#| label: fig-sector` as the first option line. Also add `#| fig-alt:` for accessibility.
_(Agent 5)_

**M18 — Sample composition of SGQ split not documented**
107 high-capacity vs. 422 low-capacity observations is a ~1:4 ratio, far from the expected 1:3 for a 75th-percentile split. This implies uneven Afrobarometer coverage or interpolation. Add a data appendix or inline note documenting: (a) fraction of SGQ values observed vs. imputed; (b) distribution across countries; (c) whether attrition is systematic.
_(Agent 6)_

**M19 — U5M 2SLS sign is negative (wrong sign vs. narrative); needs acknowledgment**
Table `main_u5m_admin1.tex` shows the 2SLS U5M coefficient is negative (opposite to the stated positive direction). The prose does not acknowledge this. Either report the 2SLS U5M results explicitly and discuss the sign reversal, or remove the 2SLS U5M specification from the paper.
_(Agent 6)_

---

## MINOR issues (fix if time permits)

**mn1** — Abstract opener ("National policies and institutional quality have long been recognized...") is a throat-clearing truism. Replace with a direct statement of the paper's question and finding. _(Agent 1 #1–2)_

**mn2** — Abstract: "underscore," "critically" — hedging intensifiers. Remove. _(Agent 1 #3)_

**mn3** — Line 526: "This pattern — where fragmentation sometimes hinders, yet occasionally boosts development — poses a clear puzzle: why does fragmentation stifle economic growth in some contexts, while boosting it in others?" — Tautological. Delete the parenthetical and start with the question. _(Agent 1 #6)_

**mn4** — Line 526: "transform fragmentation from a liability into a more manageable challenge" — vague. Replace with "attenuate fragmentation's negative effects on growth." _(Agent 1 #7)_

**mn5** — Line 536: "administrative burden can overburden" — "burden/overburden" tautology. Change to "This load can overwhelm public officials." _(Agent 1 #12)_

**mn6** — Line 536: "siphon off the most skilled local staff" and "creating a 'brain drain' that diverts skilled officials" — same idea stated twice in consecutive sentences. Remove the first occurrence. _(Agent 1 #13)_

**mn7** — Line 538: "Other work demonstrates the impact of a lack of coordination on recipients." — Throat-clearing opener. Delete; begin with "When donors fail to coordinate..." _(Agent 1 #14)_

**mn8** — Line 540: "Weaker programs...resulting in weaker governance structures" — "weaker" used twice for different referents. Revise: "skew donor behavior toward quick-disbursement projects rather than institution building, eroding governance quality." _(Agent 1 #18)_

**mn9** — Line 546: "The idea is that..." — throat-clearing. Delete; start with "Under certain conditions..." _(Agent 1 #19)_

**mn10** — Line 548: "despite the best of intentions" — cliché. Replace with "despite repeated commitments." _(Agent 1 #22)_

**mn11** — Line 548: "interests and priorities...are precisely what hampers" — subject-verb agreement error; should be "hamper." _(Agent 1 #23)_

**mn12** — Line 550: "underscore how" — style violation. Replace "underscore how" with "show how" or "reveal how." _(Agent 1 #24)_

**mn13** — Line 552: "(fail to)" — informal parenthetical. Replace with "whether donors coordinate." _(Agent 1 #25)_

**mn14** — Line 556: "potential drawbacks...potential benefits" — "potential" appears twice. Drop the first: "mitigating the drawbacks of fragmentation while harnessing its potential benefits." _(Agent 1 #27)_

**mn15** — Line 556: "parlay" — colloquialism. Replace with "convert" or "translate." _(Agent 1 #28)_

**mn16** — Line 558: "leverage donor competition" — business jargon. Replace with "exploit donor competition." _(Agent 1 #29)_

**mn17** — Line 558: "local government capacity is pivotal" — vague intensifier; sentence restates topic sentence. Delete it. _(Agent 1 #30)_

**mn18** — Line 574: "The capacity argument above says _who_" — informal. Replace with "The argument above identifies _who_ bears fragmentation costs most heavily." _(Agent 1 #35)_

**mn19** — Line 577: "budget cycles" / "budgeting cycles" (line 536) — inconsistent. Standardize to "budget cycles." _(Agent 1 #36)_

**mn20** — Line 579: "SSA" used without prior definition. Spell out "Sub-Saharan Africa" or define the abbreviation on first use. _(Agent 1 #37)_

**mn21** — Line 583 (H2): "portfolio-level coordination costs accumulate at the administrative tier responsible for integrating the full donor portfolio" — "portfolio" twice. Revise: "coordination costs accumulate at the administrative tier responsible for integrating all active donors simultaneously." _(Agent 1 #39)_

**mn22** — Line 589: "most legible when examined against" — unusual phrasing. Replace with "clearest when illustrated with." _(Agent 1 #40)_

**mn23** — Line 589: "cleanly separates" — informal. Use "formally separates." _(Agent 1 #41)_

**mn24** — Line 592: "attenuates below it" — intransitive use of "attenuate" is jargon. Revise: "peaks at the district and diminishes below it." _(Agent 1 #44)_

**mn25** — Line 597: "impacted by" — bureaucratic jargon. Replace with "associated with" or "shaped by." _(Agent 1 #45)_

**mn26** — Line 597: "sub-Saharan Africa" (lowercase 's') — inconsistent. Standardize to "Sub-Saharan Africa." _(Agents 1, 2)_

**mn27** — Line 602: "We believe that" — weak epistemic hedge. Replace with "Aggregation to regional units mitigates rural measurement error; we treat mean nightlights as a reasonable proxy for regional economic trends." _(Agent 1 #47)_

**mn28** — Line 602: "the seminal paper by @henderson_measuring_2012" — "seminal" is a promotional intensifier. Remove: "following @henderson_measuring_2012." _(Agent 1 #48)_

**mn29** — Line 604: "NTL" abbreviation introduced and never used again. Drop the abbreviation; just say "nightlights." _(Agent 1 #49)_

**mn30** — Line 621: "less than 10% share" — countable noun: "fewer than" preferred, or rephrase to "shares below 10%." _(Agent 1 #50)_

**mn31** — Line 621: missing comma after "For each year–admin unit pair". Fix: "For each year–admin-unit pair, we calculate..." _(Agent 1 #51)_

**mn32** — Line 624: "Following @iddawela_quality_2021 closely" — "closely" weakens attribution. Change to "Following the procedure of @iddawela_quality_2021." _(Agent 1 #52)_

**mn33** — Line 630: "a better indicator of actual government functioning" — "indicator" appears twice in the same clause. Revise: "we hypothesize that the perception-based SGQ better captures actual government functioning." _(Agent 1 #54)_

**mn34** — Line 632: "carried forward/backward" — slash is informal. Use "extrapolated at the panel boundaries." _(Agent 1 #55)_

**mn35** — Line 635: "Following the recent literature, we rely on fixed effects to absorb additional unobserved heterogeneity." — "recent literature" is vague appeal to authority. Specify: "We include country fixed effects to absorb time-invariant national-level confounders." _(Agent 1 #56)_

**mn36** — Line 638: Semicolon joins a definitional clause to a sample statistic — use a period instead. _(Agent 1 #57)_

**mn37** — Line 646: "establishes the raw conditional correlation" — OLS doesn't "establish" anything causal. Replace with "reports the raw conditional correlation." _(Agent 1 #58)_

**mn38** — Line 650: "which controls" — subject-verb agreement error with plural subject "Country fixed effects." Fix to "which control." Also "country dummies" → "country fixed effects." _(Agent 1 #59)_

**mn39** — Line 660: "fixed across time" — redundant (the share is time-invariant by construction). Delete. _(Agent 1 #60)_

**mn40** — Line 662: "readers should interpret IV results with this in mind" — throat-clearing. Replace with "the Borusyak et al. critique therefore applies in principle." _(Agent 1 #61)_

**mn41** — Line 672: "One technical note:" — throat-clearing opener. Integrate the sentence directly into the paragraph. _(Agent 1 #62)_

**mn42** — Lines 674, 680, 701: "admin1"/"admin2" lowercase — inconsistent with "Admin1"/"Admin2" used elsewhere. Standardize to capitalized forms throughout all prose and table notes. _(Agent 1 #63–64, #67)_

**mn43** — Line 680: "imprecisely estimated with confidence intervals that include zero" — previous version said "standard errors that span zero" which was incorrect. Current version is fixed but verify. _(Agent 1 #65)_

**mn44** — Line 680: "consistent with attenuation bias from measurement error in aid volumes" — present this as one possible interpretation, not settled: add "though other explanations for the IV-OLS gap cannot be ruled out." _(Agent 1 #66)_

**mn45** — Line 701: "the contrast between subgroups is not as sharp as the theory might predict; the difference in coefficients is not statistically distinguishable across subgroups" — redundant; the two clauses say the same thing. Keep one. _(Agent 1 #68)_

**mn46** — Line 722: "The results are sharp" — informal intensifier. Change to "The results are unambiguous" or "The pattern is clear." _(Agent 1 #69)_

**mn47** — Line 755: "measuring a direct welfare dimension of aid effectiveness" — vague. Replace with "capturing a direct welfare consequence of aid delivery." _(Agent 1 #71)_

**mn48** — Line 755: "though readers should note" — throat-clearing. Delete; say "though several estimates are not statistically significant." _(Agent 1 #72)_

**mn49** — Line 776: "bandwidth-intensive" — computer-science jargon. Replace with "coordination-intensive" (used elsewhere in the paper) or "administratively demanding." _(Agent 1 #73)_

**mn50** — Line 776: "less ministerial coordination required" — dangling participial phrase. Rewrite: "lower coordination demands on line ministries." _(Agent 1 #74)_

**mn51** — Line 776: "donors respond to crises in fragmented ways" — ambiguous. Replace with "crisis regions attract many donors, confounding the fragmentation-growth relationship." _(Agent 1 #75)_

**mn52** — Line 816 (Conclusion opener): "This paper examines why..." — weak opener. Lead with the finding. _(Agent 1 #76)_

**mn53** — Line 816: "more frequently succumbing to duplication" — informal. Revise: "suffer the familiar costs: duplication, gaps in coverage, and a bias toward short-term disbursements." _(Agent 1 #77)_

**mn54** — Line 820: "arguably the paper's sharpest empirical contribution" — if true, drop "arguably." _(Agent 1 #79)_

**mn55** — Line 834 (Appendix A): "which would be required for a violation" — inverted logic. Rephrase: "which would indicate a violation of the exclusion restriction through pre-existing growth trends." _(Agent 1 #80)_

**mn56** — Line 878: "top 3" — numeral inconsistency; the Data section uses "top three." Fix to "top three." _(Agent 1 #81)_

**mn57** — Line 878: "specific aggregation of the donor distribution" — imprecise. Replace with "how fragmentation is operationalized." _(Agent 1 #82)_

**mn58** — Line 899: "broadly consistent" — weak hedge if pattern truly holds. If it holds, say "consistent." _(Agent 1 #83)_

**mn59** — Terminology: "Bartik IV" vs. "shift-share IV" vs. "Bartik shift-share IV" used interchangeably throughout. Choose one canonical term (recommend "shift-share IV") and use it consistently. DICT uses "Bartik IV" — update if switching. _(Agent 2)_

**mn60** — Table B2, Column (1) header: `m_fe` is used as the "Full" reference, but all other robustness tables use `m_cfa` for comparison. Label column (1) as "(1) Full (FE)" to prevent confusion. _(Agent 5)_

**mn61** — All tables: add unit of observation and number of countries to notes (JDE standard). _(Agent 5)_

**mn62** — DMSP-OLS and VIIRS acronyms introduced (line 604) without expansion. Define on first use: "Defense Meteorological Satellite Program Operational Linescan System (DMSP-OLS)" and "Visible Infrared Imaging Radiometer Suite (VIIRS)." _(Agent 1 #90)_

**mn63** — "SWAp" in block quotation (line 544) not defined for the reader. Add a parenthetical before the quote: "(SWAp = Sector-Wide Approach)." _(Agent 1 #91)_

**mn64** — "DAC" (line 548) not expanded on first use. Write "OECD Development Assistance Committee (DAC)" on first occurrence. _(Agent 1 #92)_

**mn65** — Line 3 (Malawi theory section): Agent 3 notes the Malawi case study risks circular reasoning — it documents institutional preconditions for H2 but is framed as if it confirms H2. Add: "Malawi's governance architecture illustrates the institutional preconditions under which H2's logic applies; whether the predicted empirical pattern holds in Malawi specifically is not separately tested." _(Agent 3)_

**mn66** — Line 701 (CFA bias direction): "unmeasured selection...may attenuate the low-capacity estimate" — asymmetrically applied. Selection could bias either estimate in either direction. Replace with: "Selection into high-fragmentation environments may bias either estimate in either direction; we cannot sign the bias with confidence." _(Agent 3)_

**mn67** — Conclusion line 822: "further triangulate the welfare interpretation" — "triangulate" implies independent corroboration. Most U5M estimates are insignificant. Change to "are directionally consistent with." _(Agent 3)_

**mn68** — Line 548: "Starting with the DAC's 'Shaping the 21st Century' in 1996...policy documents continually emphasize...continuing with subsequent" — "continually" and "continuing" in the same sentence. Revise: "Since the DAC's 1996 _Shaping the 21st Century_ and continuing through subsequent high-level forums, policy documents have repeatedly called for..." _(Agent 1 #21)_

---

## AGENT REPORTS (full detail)

### Agent 1 — Copy Editor

See numbered list above (97 issues), organized by paper section. Key highest-priority copy issues:

- Abstract opener (#1–3): throat-clearing, redundancy, hedging intensifiers
- Line 523–24 (#4–10): intro phrasing, "robust," "establishes"
- Theory (#11–44): "overburden," "brain drain" duplication, "underscore," "(fail to)", "parlay," "leverage," "pivotal," "SSA" undefined
- Data/Methods (#45–63): "impacted by," "sub-Saharan," "we believe," "seminal," "NTL," "less than" vs. "fewer than"
- Results (#64–75): Admin1/admin1 inconsistency, "sharp," "bandwidth-intensive," dangling phrase
- Conclusion (#76–79): weak opener, informal phrasing, "arguably"
- Appendix (#80–83): inverted logic on placebo, "top 3" vs. "top three," "aggregation"
- Typographic (#84–97): Admin1 capitalization, Sub-Saharan capitalization, British spellings in quotes, DMSP/VIIRS not expanded, SWAp undefined, DAC not expanded

### Agent 2 — Consistency Checker

13 issues found (see above). Critical: Table 1 column mismatch (C1), Country-FE significance (C2), missing bib entry (C3), heterogeneity direction (C4), U5M direction (M19). Major: Tables 2–4 stale `.tex` artifacts, stacked magnitude discrepancy (M2), abstract overstatement (C9). Minor: "Bartik IV" vs. "shift-share IV" terminology drift (mn59), figure caption "OLS estimates with country FE" is self-contradictory.

### Agent 3 — Claims & Identification Auditor

19 issues found (see above). Critical: reduced-form causal chain (C6), conclusion asserts mechanism causally (must hedge). High: SGQ endogeneity absent from Results/Conclusion (M5), aid-quality exclusion restriction threat (M6), SUTVA dismissal without evidence (M7), IV magnitude without CI (M8). Medium: H1/H2 use "effect" for unidentified parameters, multiple comparisons (M9), asymmetric bias direction in CFA caveat (mn66), sector conclusions use weakest identification, Malawi framing risks circularity (mn65). Lower: placebo scope overstated, "triangulate" for insignificant U5M (mn67).

### Agent 4 — Mathematics & Econometrics Reviewer

14 issues found (see above). Critical: Aid subscript inconsistency Stage 1 vs. Stage 2 (M10), IV lag subscript (M11), CFA Stage 2 residual subscript (M12). High: undefined N in HHI (M13), long-difference subscript convention (footnote needed). Medium: HHI π\_{jit} defined after equation, fragmentation range claim slightly imprecise. Lower: IV share baseline overlap with panel (prose clarification needed), NL growth convergence-control interpretation (add remark).

### Agent 5 — Tables & Figures Auditor

33 deficiencies found (see above). Critical: dependent variable headers render as raw variable names in all tables (C7). Major: Table 1 CFA column omission (C1), Tables 2–4 stale saved `.tex` artifacts (M1, M3), `fitstat = "ivf"` silently blank for OLS first-stage (M14), Figure 1 missing `#| label:` (M17), all tables missing sample definition (M15), Table B2 measures undefined (M16). Minor: Table B3 Column 1 label missing "FE" (mn60), `etable()` labels use underscores (hyphens preferred), Figure 1 missing `#| fig-alt:`, empty-figure fallback missing.

### Agent 6 — Referee Report (JDE)

**Verdict: Major Revision**

**Summary:** The paper examines whether donor fragmentation in Sub-Saharan Africa harms subnational development outcomes, using a Bartik shift-share IV for total aid volume and the Admin1/Admin2 spatial contrast as a mechanistic test. The central finding is a negative association between Admin1 fragmentation and nightlights growth, concentrated in high-capacity regions (sign anomaly vs. H1) and absent at Admin2.

**Contribution assessment:** The Admin1/Admin2 mechanistic test is the paper's most distinctive move and is intellectually coherent. The GODAD-based two-level fragmentation panel is a genuine data contribution. However, the sign anomaly in the heterogeneity results (high-capacity regions show the larger, more significant negative coefficient, the opposite of H1) is a major presentational and interpretive problem that must be addressed head-on.

**Required revisions (6):**

1. The H1 sign anomaly: frank theoretical explanation required or reframing of the contribution.
2. IV diagnostics expansion: Rotemberg/Goldsmith-Pinkham concentration test, shares pre-trend test, aid-composition robustness.
3. CFA residual: rescale or provide formal Wu-Hausman test; current ~10⁻¹⁰ coefficient is uninformative.
4. SGQ sample composition: document imputation, coverage, attrition.
5. U5M results: acknowledge 2SLS sign reversal and insignificance; do not present as confirmatory.
6. Long-difference vs. stacked FE gap (~3×): explain or promote stacked to co-primary.

**Suggested analyses (4):**

1. Rotemberg donor-weight decomposition (standard in shift-share literature post-GPS 2020).
2. Continuous `Fragmentation × SGQ` interaction replacing the capacity split.
3. Spatial lag of neighbor fragmentation as SUTVA test.
4. Event-study around donor-country political transitions.

**Pointed questions (8):** See full agent report above (Q1–Q8).
