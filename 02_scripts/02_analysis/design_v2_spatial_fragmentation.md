# Design v2 — Donor Fragmentation and the Local Return to Aid (SSA)

**Status:** proposed (supersedes the admin-region panel analysis).
**Decisions locked:** (i) two co-equal identification strategies — a *composition shift-share*
(exclusion-restriction design) and an *admin-border discontinuity* (smoothness design) —
that must triangulate; (ii) scope = Sub-Saharan Africa (~39 Afrobarometer countries) so the
governance moderator `mean_sgq` is retained; (iii) harmonized DMSP–VIIRS nightlights.

## 1. Research question

> **Does donor fragmentation reduce the local development return to aid, and does state
> capacity moderate this?**

Estimand of interest = the **aid × fragmentation interaction** (conditional on aid volume),
not the fragmentation level effect. Rationale: the level effect is selection-ridden ("more
donors go to needier/more-accessible places") and we showed it is underpowered/confounded.
The interaction asks whether the *coordination structure* of aid matters holding volume fixed
— the gap the aid-volume literature (Dreher et al.) leaves open.

## 2. Unit of analysis

**Grid cell × year**, 0.1° (~11 km) baseline; robustness at 0.05°. Each cell assigned to its
GID_1 / GID_2 / GID_0 and to terrain, population, distance-to-capital, distance-to-border.
Moving off admin regions is what unlocks (a) the geocoded GODAD points and (b) a spatial RD.

## 3. Treatment & key variables (built from GODAD points, not admin averages)

Use **only precisely geocoded projects** (`precision_code` 1–3; drop 4–8 snapped to centroids —
they would manufacture spurious within-region uniformity).

For cell `c`, year `t`, over a rolling window (e.g. trailing 5 yrs), within radius `r` (25 km
baseline; 10/50 km robustness):
- `aid_ct`  = total disbursements (the endogenous *volume*).
- `ndonor_ct` = # distinct active donors.
- `frag_ct` = 1 − HHI of donor disbursement shares (continuous fragmentation).
- `share_dc` = **pre-period** (e.g. 1995–2000) share of cell `c`'s aid from donor `d`
  (the time-invariant exposure weights for the shift-shares; computed leave-out).

Outcome: cell-year nightlights (harmonized DMSP–VIIRS), modeled by **PPML** (the log(y+c)
lesson stands — no additive constant). Drop `u5m` (IHME-modeled, smooth, empty in our tests).

## 4. Design A — composition shift-share (exclusion restriction)

Two endogenous objects (volume and fragmentation) → control-function (CFA) handles the
interaction cleanly (Wooldridge): instrument each, include both first-stage residuals, then
interact the fitted treatments.

- **Volume instrument (existing):** `Bv_ct = Σ_d share_dc · FRAC_dt`
  (FRAC = donor-government fractionalization, DPI2020 — the Ahmed 2016 / Dreher–Langlotz 2020
  shift). Already built in `04_create_iv.r`.
- **Fragmentation instrument (new):** predict each donor's flow to the cell from its own
  supply shock, `flow_dct = share_dc · Supply_dt`, then `Bf_ct` = (1 − HHI of predicted
  `flow_dct` across donors). Identifies fragmentation from *differential* donor supply shocks
  hitting cells with different historical donor mixes — not from the place being "frag-prone."
- **First stages:** `aid_ct ~ Bv + Bf + X | cell + country-year`;
  `frag_ct ~ Bv + Bf + X | cell + country-year`. Report KP-F / effective-F per endogenous var.
- **Second stage (PPML CFA):**
  `NL_ct ~ aid_ct · frag_ct + X + r̂_aid + r̂_frag | cell + country-year`.
  Coefficient on `aid_ct:frag_ct` is the target.
- **SEs:** cluster country **and** report Adão–Kolesár–Morales shift-share-robust SEs
  (fixes the 33–39-cluster weakness better than country clustering).
- **Validity tests (pre-committed):** placebo on pre-period NL growth; Goldsmith-Pinkham
  share-exogeneity + Rotemberg weights (which donors drive identification); leave-one-donor-out;
  bandwidth `r` sensitivity.

## 5. Design B — admin-border discontinuity (smoothness)

Donors/governments allocate by admin unit, so `frag_ct` can jump at an internal GID_2 border
while geography varies smoothly. Compare cells in a band around shared borders.

- **Sample:** cells ≤25 km (robustness 10/15/50) from an internal border where the two sides'
  fragmentation differs by ≥ a threshold. Running var = signed distance to border.
- **Spec:** `NL ~ frag + f(dist)·side + X | border-segment FE` (each shared border = its own FE;
  identification is within-border-pair, cross-side). Local-linear in distance; PPML for levels.
- **The known threat (state openly): compound treatment.** Governance, public goods, and ethnic
  lines also jump at admin borders — and `mean_sgq` is admin-level, so it jumps too. Tests:
  (1) covariate balance at the border (pop, terrain, dist-capital, **pre-period** NL & NL trend);
  (2) does `mean_sgq` jump? If yes, condition on it / interact (turns the confound into the
  moderation test); (3) project-density (McCrary-style) at the border = manipulation check;
  (4) **donut** dropping cells within the NL bloom radius (~5–10 km), and a VIIRS-only (2013+)
  cut where blooming is minimal; (5) placebo (fake) borders.

## 6. Governance moderation (retained — the reason we stayed in SSA)

- Design A: add `aid · frag · sgq` (CFA) and a capacity split (top-25% `mean_sgq`).
- Design B: compare the border discontinuity across high- vs low-capacity border pairs.
Hypothesis to test, not assume: capacity *attenuates* the fragmentation penalty. Our earlier
(credible) runs found a tight null on moderation — so this is genuinely open and worth a clean test.

## 7. Build sequence (shared spine first)

1. Grid construction: cells + admin/terrain/pop/dist-capital/dist-border covariates.
2. GODAD point processing → cell-year donor-level disbursements (precision filter) →
   `aid_ct`, `frag_ct`, `ndonor_ct`, pre-period `share_dc`.
3. Nightlights zonal stats → cell-year `NL_ct` (harmonized rasters; also tag VIIRS-era years).
4. Shift-shares: `Bv` (reuse `04_create_iv.r` logic at cell level), `Bf` (new).
5. Border dataset: adjacent GID_2 pairs, shared segments, signed distance-to-border per cell.
6. Estimate A (CFA-PPML) and B (RD-PPML); moderation; full robustness battery.

Steps 1–3 are the shared data spine and gate everything; build and validate those first.

## 8. What we are NOT doing / risks acknowledged

- Not claiming fragmentation level effects — only the interaction (return to aid).
- Border RDD is confounded by compound treatment; it is co-equal *evidence*, not a magic bullet.
  The argument rests on A and B agreeing despite *different* failure modes.
- SSA scope caps clusters at ~39 → shift-share-robust SEs are mandatory, not optional.
- u5m dropped.
