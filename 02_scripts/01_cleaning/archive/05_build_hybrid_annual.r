# Build hybrid annual panel:
#   - Outcomes (nightlights, U5M) kept ANNUAL (2005-2015)
#   - Aid / fragmentation / IV / governance taken from the 5-year buckets and
#     HELD CONSTANT within each window (the design documented in CLAUDE.md).
#
# This recovers the within-window annual outcome variation that the pure
# 5-year build averaged away, and that the long-difference analysis layer
# collapsed to a single 2015 cross-section.
#
# Output: 01_panel_data/panel_aid_admin{1,2}_annual_hybrid.csv

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here, readr)

source(here("02_scripts", "utils", "panel_time_windows.r"))

START_YEAR <- 2005L
END_YEAR <- 2015L

# ---- columns carried (held constant) from the 5-year panel -------------------
held_constant_cols <- function(admin) {
  c(
    paste0("frag_index_admin", admin),
    paste0("frag_1_admin", admin),
    paste0("frag_3_admin", admin),
    paste0("frag_below10_admin", admin),
    paste0("donor_count_admin", admin),
    paste0("mean_sgq_admin", admin),
    paste0("total_aid_admin", admin),
    paste0("ln_pop_admin", admin),
    "IV", "IV_lag"
  )
}

build_hybrid <- function(admin) {
  uid <- paste0("GID_", admin)

  # annual outcomes: GID_0, GID_1[, GID_2], year, sum, mean, u5m
  annual <- read_csv(
    here("00_rawdata", paste0("processed_dep_vars_admin", admin, ".csv")),
    show_col_types = FALSE
  ) |>
    rename(sum_nl = sum, mean_nl = mean) |>
    filter(year >= START_YEAR, year <= END_YEAR) |>
    mutate(bucket = map_to_5year_bucket_1995_2015(year))

  # 5-year held-constant covariates, keyed by the window (its `year` == bucket)
  five <- read_csv(
    here("01_panel_data", paste0("panel_aid_admin", admin, "_fin_5year_1995_2015.csv")),
    show_col_types = FALSE
  )

  keep <- intersect(held_constant_cols(admin), names(five))
  five_keyed <- five |>
    select(all_of(c(uid, "year", keep))) |>
    rename(bucket = year)

  hybrid <- annual |>
    inner_join(five_keyed, by = c(uid, "bucket"))

  out <- here("01_panel_data", paste0("panel_aid_admin", admin, "_annual_hybrid.csv"))
  write_csv(hybrid, out)
  message(sprintf(
    "admin%s hybrid: %d rows, %d units, %d countries, years %d-%d -> %s",
    admin, nrow(hybrid), dplyr::n_distinct(hybrid[[uid]]),
    dplyr::n_distinct(hybrid$GID_0),
    min(hybrid$year), max(hybrid$year), basename(out)
  ))
  invisible(hybrid)
}

build_hybrid(1)
build_hybrid(2)
