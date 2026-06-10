#!/usr/bin/env Rscript

suppressPackageStartupMessages({
    library(here)
    library(yaml)
    library(readr)
    library(dplyr)
    library(purrr)
    library(fixest)
    library(tibble)
})

source(here::here("02_scripts", "03_rebuild", "rebuild_utils.R"))

result <- run_pipeline(
    config_path = here::here("02_scripts", "03_rebuild", "config.yml")
)

cat("Rebuild pipeline complete.\n")
cat("Output directory:", result$output_dir, "\n")
