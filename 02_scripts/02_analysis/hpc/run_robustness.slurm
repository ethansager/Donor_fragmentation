#!/bin/bash
# SLURM batch script for the heavy robustness suite (high-memory, single node).
#
# >>> Run these ONCE on the login node to fill the 3 placeholders below:
#       sinfo -s                         # partition names + limits  -> --partition
#       sacctmgr show assoc user=$USER format=account%30   # your account -> --account
#       module avail 2>&1 | grep -i '\bR\b\|^R/'           # R module    -> module load line
# Then:  mkdir -p logs && sbatch 02_scripts/02_analysis/hpc/run_robustness.slurm
#SBATCH --job-name=frag_robust
#SBATCH --partition=base1
#SBATCH --account=az332
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16             # data.table + fixest multithread
#SBATCH --mem=256G                     # the binding constraint (raise to 256G if it OOMs)
#SBATCH --time=08:00:00
#SBATCH --output=logs/frag_robust_%j.out
#SBATCH --error=logs/frag_robust_%j.err

set -euo pipefail
mkdir -p logs

# --- R via environment modules ---
module load R/4.3.3

# personal package library (module R is usually read-only); the R script installs here on first run
export R_LIBS_USER="$HOME/R/frag-lib"
mkdir -p "$R_LIBS_USER"
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
cd "$SLURM_SUBMIT_DIR"

echo "host=$(hostname) cpus=$SLURM_CPUS_PER_TASK mem=$SLURM_MEM_PER_NODE start=$(date)"
Rscript 02_scripts/02_analysis/hpc/30_hpc_robustness.r
echo "end=$(date)"
