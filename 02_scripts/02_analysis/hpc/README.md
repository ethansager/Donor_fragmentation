# Running the heavy robustness on HPC

The robustness checks in this folder OOM on the 5.8 GB workstation. They are single-node,
**memory-bound** (not parallel) R jobs → request one high-memory node, not many nodes.

## What runs (`30_hpc_robustness.r`)
1. Native-0.1° cell-specific **linear-trend** de-trended triple (the key pre-trend robustness).
2. **Sun–Abraham** heterogeneity-robust event study + ATT at native resolution.
3. **Callaway–Sant'Anna** group-time ATT (if `did` is installed).
4. **Wild-cluster bootstrap** of the triple (linear asinh-NL analog; 33 clusters).

Outputs → `03_output/hpc/`.

## Resource sizing
- `--mem=128G` is the starting point; the cell[year] de-trend and Sun–Abraham interaction matrix
  are the memory hogs. Raise to `256G` if you see OOM / `cannot allocate vector`.
- `--cpus-per-task=16` helps `data.table`/`fixest`/the bootstrap; memory matters more than cores.
- Wall time: a few hours; `08:00:00` is generous.

## Setup
1. Transfer the repo (data included): `rsync -av --exclude '.git' Donor_fragmentation/ <user>@<cluster>:~/Donor_fragmentation/`
   (the spine `01_panel_data/wb_grid_spine.csv` is ~130 MB; the nightlight rasters are large but
   are **not** needed for the robustness suite — only the prebuilt spine + panels are.)
2. R packages: `data.table, fixest, here, fwildclusterboot, did` (+ `terra, sf` only if rebuilding
   the spine). Install once into a personal library or a conda/Singularity env.
3. Edit the four `<PLACEHOLDER>` lines in `run_robustness.slurm` (partition, account, R module).
4. `sbatch 02_scripts/02_analysis/hpc/run_robustness.slurm`

## To rebuild the spine on HPC (only if needed)
`06_build_wb_grid_spine.r` needs `terra`/`sf` and the raster set; it is also memory-friendly
(~few GB). Run it the same way before the robustness suite if the spine isn't transferred.
