# Annual hybrid panel analysis vs. the old 2015 cross-section.
#
# Demonstrates how much identifying variation the annual outcome panel recovers
# relative to the long-difference collapse, and folds in the log(y+c) robustness
# (PPML + c-sensitivity) from Chen & Roth (2024).

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here, readr, fixest)

source(here("02_scripts", "03_rebuild", "rebuild_utils.R"))

C <- 0.01  # additive constant currently used throughout

# =============================================================================
# (A) OLD design: 5-year build -> long-difference to single 2015 cross-section
# =============================================================================
raw5 <- read_csv(here("01_panel_data", "panel_aid_admin1_fin_5year_1995_2015.csv"),
                 show_col_types = FALSE) |> norm_nl()
old_panel <- build_vars(raw5, "GID_1", "total_aid_admin1", "frag_index_admin1",
                        "ln_pop_admin1") |>
  to_long_diff("GID_1", 2005L, 2015L)

old_fe <- est_fe(old_panel, "nl_growth")   # country FE on the 2015 cross-section

# =============================================================================
# (B) NEW design: annual hybrid panel (annual NL, held-constant aid/frag)
# =============================================================================
ann <- read_csv(here("01_panel_data", "panel_aid_admin1_annual_hybrid.csv"),
                show_col_types = FALSE) |>
  arrange(GID_1, year) |>
  group_by(GID_1) |>
  mutate(
    lag_nl       = lag(mean_nl, 1L),
    log_lag_nl   = log(lag_nl + C),
    nl_growth    = (log(mean_nl + C) - log(lag_nl + C)) * 100,   # ANNUAL growth
    frag         = frag_index_admin1,
    ln_aid       = log(total_aid_admin1 + C),
    lag_log_pop  = ln_pop_admin1
  ) |>
  ungroup() |>
  filter(is.finite(nl_growth), is.finite(log_lag_nl), is.finite(frag))

# Country FE (comparable to old spec, but on the annual panel)
ann_cfe <- feols(nl_growth ~ frag + ln_aid + lag_log_pop + log_lag_nl | GID_0,
                 cluster = ~GID_0, data = ann)

# Unit + Year FE: the genuine within-region panel
ann_twfe <- feols(nl_growth ~ frag + ln_aid + lag_log_pop + log_lag_nl | GID_1 + year,
                  cluster = ~GID_0, data = ann)

# 2SLS unit + year FE, instrumenting aid with the lagged shift-share
ann_2sls <- feols(nl_growth ~ frag + lag_log_pop + log_lag_nl | GID_1 + year |
                    ln_aid ~ IV_lag,
                  cluster = ~GID_0, data = filter(ann, is.finite(IV_lag)))

cat("\n================ OLD cross-section vs NEW annual panel ================\n")
etable(old_fe, ann_cfe, ann_twfe, ann_2sls,
       headers = c("OLD 2015 x-sec\n(country FE)", "Annual\n(country FE)",
                   "Annual\n(unit+year FE)", "Annual 2SLS\n(unit+year FE)"),
       fitstat = ~ n + r2 + war2)

# =============================================================================
# (C) log(y+c) robustness on the annual panel (Chen & Roth)
# =============================================================================
# c-sensitivity: re-build the growth outcome under different constants
csens <- map_dfr(c(0.01, 0.1, 1), function(cc) {
  d <- ann |>
    mutate(y = (log(mean_nl + cc) - log(lag_nl + cc)) * 100)
  m <- feols(y ~ frag + ln_aid + lag_log_pop + log_lag_nl | GID_1 + year,
             cluster = ~GID_0, data = filter(d, is.finite(y)))
  ct <- coeftable(m)["frag", ]
  tibble(c = cc, frag_coef = ct[1], se = ct[2], p = ct[4], n = nobs(m))
})

# PPML: proportional effect on NL levels, no constant, handles zeros natively
ann_ppml <- fepois(mean_nl ~ frag + ln_aid + lag_log_pop | GID_1 + year,
                   cluster = ~GID_0, data = ann)

cat("\n================ c-sensitivity of frag coef (unit+year FE) ============\n")
print(csens)
cat("\n================ PPML on NL levels (frag = semi-elasticity) ===========\n")
print(coeftable(ann_ppml)["frag", ])

cat("\nDONE.\n")
