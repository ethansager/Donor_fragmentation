# Rebuild Analysis Module

This module implements a redesign-first workflow with explicit estimands,
confirmatory vs exploratory separation, and reproducible outputs.

## Run

Sequential pipeline:

```bash
Rscript 02_scripts/03_rebuild/run_rebuild_pipeline.R
```

`targets` pipeline (optional, requires `targets` package):

```bash
Rscript -e "targets::tar_make(script = '02_scripts/03_rebuild/_targets.R')"
```

## Outputs

All outputs are written to `03_output/rebuild/`:

- `fragmentation_coefficients.csv`
- `diagnostics.csv`
- `sample_audit.csv`
- `missingness_audit.csv`
- `models.rds`
- `run_config.yml`
- `run_manifest.txt`

## Design choices

- Confirmatory models are estimated first and reported separately.
- Exploratory sector models receive Benjamini-Hochberg adjusted p-values.
- Aid-level first stage uses millions of USD for interpretable CFA residual units.
