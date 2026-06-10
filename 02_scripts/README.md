# Scripts — current pipeline

Full data/variable documentation: [`docs/PR_MEMO.md`](../docs/PR_MEMO.md).

## Current design (v3): WB aid effectiveness × fragmentation × capacity + Afrobarometer

### Cleaning (`01_cleaning/`)
| Script | Purpose |
|---|---|
| `00_build_full_shp.r` | GADM admin1/2 shapefiles |
| `01_build_afro_full.r` | Afrobarometer panel build |
| `02_build_dependent_vars.r` | nightlights / U5M zonal stats → `processed_dep_vars_admin*.csv` |
| `03_build_pop.r` | population rasters → admin panels |
| `03_geo_clean.r` | annual aid panel `panel_aid_admin{1,2}_fin.csv` (GODAD → admin) |
| `04_create_iv.r` | shift-share IV (`IV` = Σ pbar_ji · FRAC_jt; DPI2020 fractionalization) |
| **`06_build_wb_grid_spine.r`** | **builds `01_panel_data/wb_grid_spine.csv`** — 0.1° SSA grid × 2000–2020 with NL (Regan `bltcfix`), WB precise treatment, predetermined district fragmentation |

### Analysis (`02_analysis/`) — run after the spine
| Script | Produces |
|---|---|
| `23_wb_dynamic_moderation.r` | dynamic WB effect + fragmentation moderation; `03_output/tabs/wb_fragmentation_moderation.tex` |
| `24_firm_up_governance.r` | admin1 governance moderation (continuous) |
| `25_robust_clean_controls.r` | event study (never-treated controls) + robust triple → `03_output/wb_robust_clean.rds` |
| `26_afrobarometer_outcomes.r` | citizen outcomes vs fragmentation/WB → `03_output/afrobarometer_outcomes.csv` |
| `27_afro_capacity_moderation.r` | capacity (state-reach) moderation → `03_output/afro_capacity_moderation.csv` |
| `28_figures.r` | `03_output/figs/fig{1,2,3}_*.png` + service×capacity moderation |

Manuscript: [`paper.qmd`](../paper.qmd) (reads the saved figures/results above; does not re-run the spine).

## Archived (`*/archive/`)
Superseded v1/v2 work — the admin-region fragmentation panel, 5-year build, hybrid panel, annual
PPML, shift-share CFA, sector/health, Sun–Abraham attempt. Kept for provenance; not part of the
current pipeline. See `memory/` notes (`variation-collapse-finding`, `iv-cfa-annual-dreher`,
`godad-geocoding-feasibility`, `design-v3-wb-effectiveness`, `afrobarometer-outcomes`).

## Nightlights caveat
Use `00_rawdata/nightlights/africa/DMSP*_bltcfix.tif` (Tanner Regan `world_nightlights`,
continuous DMSP-scale). **Not** `Harmonized_DN_NTL_*` (raw Li et al.) — 2.3× jump at the 2014
DMSP→VIIRS splice contaminates within-cell event studies.
