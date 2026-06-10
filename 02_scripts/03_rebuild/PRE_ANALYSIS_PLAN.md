# Pre-Analysis Plan (Rebuild)

## Scope

This plan applies to the 5-year panel window (`1995-2015`) using the
Admin1/Admin2 financial panel files listed in `config.yml`.

## Estimands

1. `E1` (Confirmatory): Effect of lagged donor fragmentation (`lag_frag`) on
   Admin1 annualized nightlights growth (`nl_growth`).
2. `E2` (Confirmatory): Capacity heterogeneity in `E1` using a pre-declared
   75th-percentile split in governance quality.
3. `E3` (Exploratory): Sector-specific Admin1 fragmentation effects with
   Benjamini-Hochberg adjustment.

## Confirmatory model hierarchy

1. OLS (Admin1)
2. Country FE (Admin1)
3. 2SLS with shift-share instrument for aid volume (Admin1)
4. CFA high/low capacity split (Admin1)
5. Country FE split (Admin2)

All confirmatory models report the `lag_frag` coefficient, clustered at country
level.

## Identification diagnostics

- 2SLS first-stage F statistic (`ivf`) from the main 2SLS model.
- Standalone first-stage OLS F statistic for Appendix-style diagnostics.
- Reduced-form and placebo sample sizes.

## Exploratory analysis

Sector-specific Admin1 country-FE regressions (health, education,
infrastructure, governance support, humanitarian). P-values are adjusted with
Benjamini-Hochberg and are explicitly labeled exploratory.

## Reporting rules

- Confirmatory and exploratory outputs are separated in output files.
- No reclassification of exploratory models as confirmatory post hoc.
- Any model failures are preserved in logs/manifest, not silently dropped.
- Confirmatory effects include 90% confidence intervals and IQR-scaled effect
  translations for direct interpretability.
