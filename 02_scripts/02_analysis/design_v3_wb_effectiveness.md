# Design v3 — Does Donor Fragmentation Undermine World Bank Aid Effectiveness?

**Status:** proposed. Feasibility CONFIRMED (see audit below). Supersedes v2 (spatial
fragmentation, killed because only the WB is point-geocoded — [[godad-geocoding-feasibility]]).

## 1. The idea (and why it survives the data constraints)

Only the World Bank is precisely geocoded (99.3% of precise points). So we use the WB as a
**spatially-precise treatment** and ask whether its local effectiveness is degraded where
**other donors are also present** (district fragmentation, a coarse-but-adequate moderator).

> **Q: Does a World Bank project raise local economic activity less in districts crowded with
> other donors — and does state capacity change that?**

This respects the data: precise running variable (WB site location + timing) where we need
precision; coarse measure (district fragmentation, governance) only where it is a contextual
moderator and centroid-snapping is tolerable.

## 2. Feasibility (confirmed)

- WB precise treatment: 7,603 distinct coords, 1,260 projects, 32 SSA countries, startyears
  well spread 1995–2018 (heavy 2008–2018). Median 208 sites/country.
- Fragmentation moderator (non-WB, district): mean 0.28, sd 0.27; WB sites balanced across
  low/mid/high terciles (~530 districts each, 1 / 3 / 6 other donors). Interaction is powered.
- 68% of WB-containing districts have a defined non-WB fragmentation moderator.

## 3. Unit & data spine

**Grid cell × year**, 0.05–0.1° over SSA. Per cell-year:
- `wb_active_ct` = WB project active within `d` km (baseline 10 km; robustness 5/25) in year `t`,
  using project `startyear`–`closingyear`. Also `wb_disb_ct` (intensity) and `wb_arrival` (event time).
- `NL_ct` = nightlights (harmonized DMSP–VIIRS), modeled by **PPML**.
- `frag_d` = non-WB donor fragmentation of the cell's district (pre-period and/or time-varying).
- `sgq_d` = Afrobarometer governance of the district (SSA moderator).
- Covariates: population, terrain, distance-to-capital/road/border (selection & balance checks).

## 4. Primary identification — staggered WB-arrival event study

Within-cell timing of WB project arrival is the identifying variation.

- **Event study:** `NL_ct ~ Σ_k β_k · 1{event_time = k} | cell + country-year` — leads test for
  pre-trends (does WB target cells already trending?), lags trace the dynamic effect.
- **Pooled DiD (PPML):** `NL_ct ~ wb_active_ct + ... | cell + country-year`, cluster by district/country.
- **Staggered-robust:** Callaway–Sant'Anna / Sun–Abraham (heterogeneous timing across cohorts);
  not-yet-treated cells as controls.
- **The moderation (the paper):** `wb_active_ct · frag_d` (does the WB effect shrink with
  fragmentation?) and the triple `wb_active · frag · sgq` (does capacity rescue it?).

## 5. Threats & robustness

- **WB site selection on trends** → event-study pre-trends must be flat; as IV robustness,
  instrument WB intensity with the **WB-specific shift-share** (IBRD equity-to-loans / IDA
  funding position × district WB-exposure) — already specified in the Dreher et al. appendix.
- **Fragmentation moderator endogeneity** → use *pre-period* (1995–2004) non-WB fragmentation
  so the moderator predates the outcome window; report time-varying as robustness.
- **Spatial design robustness:** distance-band DiD (rings 0–10 / 10–25 / 25–50 km, near vs far ×
  post) to confirm the timing result is spatial, not a district-wide shock.
- **NL blooming/SUTVA:** VIIRS-only (2013+) cut; drop cells with overlapping multi-project rings
  for a clean dose; donut around site.
- **Multiple WB projects / dosage:** control WB count & disbursement; saturation checks.

## 6. What we are NOT claiming
- Not a fragmentation *level* effect on growth (dead — v1/v2).
- WB selection is handled by timing + (robustness) the WB instrument, not assumed random.
- Fragmentation is a district moderator, never a point-level treatment.

## 7. Build sequence (spine gates everything)
1. SSA grid (0.05–0.1°) + admin/terrain/pop/distance covariates.
2. WB precise sites → cell-year `wb_active`/`wb_disb`/event-time (precision 1-2, start–closing years).
3. Non-WB district fragmentation moderator (pre-period + time-varying) + Afrobarometer sgq.
4. Nightlights zonal stats → cell-year.
5. Event study + PPML DiD + moderation; then IV / distance-band / VIIRS robustness.
