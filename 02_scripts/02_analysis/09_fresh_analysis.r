# =============================================================================
# 09_fresh_analysis.r
#
# Donor Fragmentation and Aid Effectiveness — Full Analysis (Fresh Build)
#
# Approach:
#   1.  Balance diagnostics — understand panel structure before modelling
#   2.  Descriptive statistics — Table 1 (means/SDs by capacity group)
#   3.  Sample construction — lags, winsorization, capacity split
#   4.  IV diagnostics — first-stage relevance, plausibility of exclusion
#   5.  Main results: OLS → country FE → CFA, escalating identification
#   6.  Heterogeneity: high vs. low governance capacity
#   7.  Robustness: balanced panel, winsorization sensitivity, admin2
#   8.  Figures: variable distributions, binscatters, first-stage scatter
#
# Estimator choice rationale:
#   Long-difference is the primary estimator. Each unit contributes its terminal
#   observation (2015) with all RHS variables measured as of 2010.  This limits
#   the sample to one obs per unit but eliminates attenuation bias from
#   year-to-year measurement error in nightlights.  Panel FE (all 5-year
#   transitions) is reported as robustness.
#
# Identification:
#   Shift-share (Bartik) IV instruments total aid volume with
#   sum_j(share_ij * donor_frac_j), where share_ij = historical probability
#   region i received aid from donor j, and donor_frac_j = government
#   fractionalization of donor j's parliament.  IV_lag is the lagged IV.
#   CFA: include first-stage residuals in stage 2 to purge endogenous variation
#   in aid volume while retaining fragmentation as main regressor.
#
# Standard errors: clustered at GID_0 (country) — level at which the
#   "shift" variation (donor fractionalization) is common.
# =============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here, readr, fixest, janitor, modelsummary, gt)

# =============================================================================
# SECTION 0: Global configuration — change these; leave the rest unchanged
# =============================================================================

# Admin level to use as primary (1 = admin1, 2 = admin2)
PRIMARY_ADMIN <- 1L

# Balanced panel? FALSE = unbalanced (preferred; more power, check attrition)
USE_BALANCED_PANEL <- FALSE

# Estimator: "long_diff" or "panel_fe"
ESTIMATOR <- "long_diff"

# Winsorization bounds for nl_growth (primary outcome)
WINS_LO <- 0.05
WINS_HI <- 0.95

# Governance-capacity split (units above this percentile = "High")
CAPACITY_PERCENTILE <- 0.75

# IV variable name
IV_VAR <- "IV_lag"

# Panel suffix
PANEL_TAG <- "_5year_1995_2015"

# Output directories
TAB_DIR <- here("03_output", "tabs")
FIG_DIR <- here("03_output", "figs")
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

# Significance codes
SIG_CODE <- c("***" = 0.01, "**" = 0.05, "*" = 0.1)

# =============================================================================
# SECTION 1: Load data and diagnose panel structure
# =============================================================================
# Before modelling, understand coverage: how many periods per unit?
# Does attrition correlate with treatment intensity?
# Are singletons (units with only one period) contaminating the panel?

a1_path <- here("01_panel_data", paste0("panel_aid_admin1_fin", PANEL_TAG, ".csv"))
a2_path <- here("01_panel_data", paste0("panel_aid_admin2_fin", PANEL_TAG, ".csv"))

if (!file.exists(a1_path)) stop("Missing admin1 panel: ", a1_path)
if (!file.exists(a2_path)) stop("Missing admin2 panel: ", a2_path)

raw_admin1 <- read_csv(a1_path, show_col_types = FALSE)
raw_admin2 <- read_csv(a2_path, show_col_types = FALSE)

# ---- Panel balance diagnostic ------------------------------------------------
diagnose_balance <- function(data, uid, label) {
  n_periods_total <- n_distinct(data$year)
  periods         <- sort(unique(data$year))

  by_unit <- data |>
    group_by(.data[[uid]]) |>
    summarise(
      n_periods    = n_distinct(year),
      has_singleton = any(is_singleton, na.rm = TRUE),
      mean_frag    = mean(.data[[paste0("frag_index_admin", substr(uid, 5, 5))]],
                          na.rm = TRUE),
      mean_aid     = mean(.data[[paste0("total_aid_admin",  substr(uid, 5, 5))]],
                          na.rm = TRUE),
      .groups = "drop"
    )

  n_total    <- nrow(by_unit)
  n_balanced <- sum(by_unit$n_periods == n_periods_total)

  cat("\n----------------------------------------------------------------------\n")
  cat(sprintf("BALANCE DIAGNOSTIC: %s\n", label))
  cat(sprintf("  Periods in data      : %s\n",  paste(periods, collapse = ", ")))
  cat(sprintf("  Total unique units   : %d\n",  n_total))
  cat(sprintf("  Balanced units       : %d  (%.1f%%)\n",
              n_balanced, 100 * n_balanced / n_total))
  cat("  Distribution of period count per unit:\n")
  print(table(by_unit$n_periods))

  # Attrition check: do units with fewer periods have systematically different
  # average fragmentation?  A strong correlation would suggest endogenous attrition.
  if (n_distinct(by_unit$n_periods) > 1) {
    corr <- cor(by_unit$n_periods, by_unit$mean_frag, use = "complete.obs")
    cat(sprintf(
      "\n  Correlation(n_periods, mean_frag): %.3f\n",
      corr
    ))
    cat("  (Near-zero → attrition is not strongly related to fragmentation)\n")
  }
  cat("----------------------------------------------------------------------\n\n")

  invisible(by_unit)
}

diag_admin1 <- diagnose_balance(raw_admin1, "GID_1", "Admin1 (primary)")
diag_admin2 <- diagnose_balance(raw_admin2, "GID_2", "Admin2 (robustness)")

# =============================================================================
# SECTION 2: Sample construction
# =============================================================================

# ---- Helper: rename nightlights columns if inconsistent ---------------------
norm_nl_cols <- function(d) {
  if (!"mean_nl" %in% names(d) && "mean" %in% names(d)) d <- rename(d, mean_nl = mean)
  if (!"sum_nl"  %in% names(d) && "sum"  %in% names(d)) d <- rename(d, sum_nl  = sum)
  d
}

# ---- Helper: apply balanced-panel filter ------------------------------------
apply_balance_filter <- function(data, uid, diag_tbl, n_required) {
  if (!USE_BALANCED_PANEL) return(data)
  keep <- diag_tbl |> filter(n_periods == n_required) |> pull(.data[[uid]])
  filter(data, .data[[uid]] %in% keep)
}

n_periods_a1 <- n_distinct(raw_admin1$year)
n_periods_a2 <- n_distinct(raw_admin2$year)

panel_admin1 <- raw_admin1 |> norm_nl_cols() |>
  apply_balance_filter("GID_1", diag_admin1, n_periods_a1)
panel_admin2 <- raw_admin2 |> norm_nl_cols() |>
  apply_balance_filter("GID_2", diag_admin2, n_periods_a2)

# ---- Compute lags and growth ------------------------------------------------
# NL growth: annualized log change over the 5-year bucket.
# All RHS variables are lagged one bucket to avoid simultaneity bias.
# Winsorize nl_growth to remove extreme outliers driven by raster artifacts.

build_panel_vars <- function(data, uid, aid_var, frag_var, pop_var) {
  data |>
    arrange(.data[[uid]], year) |>
    group_by(.data[[uid]]) |>
    mutate(
      lag_mean_nl   = dplyr::lag(mean_nl, 1L),
      lag2_mean_nl  = dplyr::lag(mean_nl, 2L),            # two-period lag for pre-trend
      lag_frag      = dplyr::lag(.data[[frag_var]], 1L),
      lag_log_pop   = dplyr::lag(.data[[pop_var]],  1L),
      lag_total_aid = log(dplyr::lag(.data[[aid_var]], 1L) + 0.01),
      total_aid_raw = dplyr::lag(.data[[aid_var]], 1L),   # raw aid for CFA stage 1
      # Log pre-period NL level: controls for pre-existing economic activity.
      # Including this in all specs makes the exclusion restriction conditional
      # on initial conditions — a weaker and more plausible assumption.
      log_lag_nl    = log(dplyr::lag(mean_nl, 1L) + 0.01),
      nl_growth     = case_when(
        is.na(lag_mean_nl) ~ NA_real_,
        TRUE ~ ((log(mean_nl + 0.01) - log(lag_mean_nl + 0.01)) / 5) * 100
      ),
      # Pre-period NL growth (t-2 → t-1): used as placebo outcome.
      # If the IV predicts this, the exclusion restriction fails even
      # conditional on initial levels.
      pre_nl_growth = case_when(
        is.na(lag2_mean_nl) ~ NA_real_,
        TRUE ~ ((log(lag_mean_nl + 0.01) - log(lag2_mean_nl + 0.01)) / 5) * 100
      )
    ) |>
    ungroup()
}

panel_admin1 <- build_panel_vars(
  panel_admin1, "GID_1",
  aid_var  = "total_aid_admin1",
  frag_var = "frag_index_admin1",
  pop_var  = "ln_pop_admin1"
)
panel_admin2 <- build_panel_vars(
  panel_admin2, "GID_2",
  aid_var  = "total_aid_admin2",
  frag_var = "frag_index_admin2",
  pop_var  = "ln_pop_admin2"
)

# ---- Winsorize nl_growth ----------------------------------------------------
winsorise_outcome <- function(data, var, lo = WINS_LO, hi = WINS_HI) {
  vals <- data[[var]]
  if (sum(is.finite(vals)) < 20) return(data)
  q <- quantile(vals, c(lo, hi), na.rm = TRUE)
  data[[var]] <- pmax(pmin(vals, q[2]), q[1])
  data
}

panel_admin1 <- winsorise_outcome(panel_admin1, "nl_growth")
panel_admin2 <- winsorise_outcome(panel_admin2, "nl_growth")

# ---- Governance-capacity split ----------------------------------------------
# Classify each unit as High or Low capacity based on mean state governance
# quality (Afrobarometer SGQ) at the CAPACITY_PERCENTILE threshold.
# Threshold is computed on the full sample so it is not affected by the
# estimator-specific sample restriction applied below.

add_capacity <- function(data, sgq_var) {
  cutoff <- quantile(data[[sgq_var]], CAPACITY_PERCENTILE, na.rm = TRUE)
  if (!is.finite(cutoff)) cutoff <- median(data[[sgq_var]], na.rm = TRUE)
  mutate(data, high_cap = as.integer(.data[[sgq_var]] > cutoff))
}

panel_admin1 <- add_capacity(panel_admin1, "mean_sgq_admin1")
panel_admin2 <- add_capacity(panel_admin2, "mean_sgq_admin2")

# ---- Apply estimator-specific restriction -----------------------------------
# long_diff: keep only the terminal observation per unit (year == max year).
#   All variation comes from the 2010 → 2015 bucket transition.
# panel_fe:  keep all periods with a valid lag (drop first period per unit).

apply_estimator <- function(data, uid) {
  if (ESTIMATOR == "long_diff") {
    data |>
      group_by(.data[[uid]]) |>
      filter(year == max(year, na.rm = TRUE)) |>
      ungroup()
  } else {
    data |>
      group_by(.data[[uid]]) |>
      filter(!is.na(lag_frag)) |>
      ungroup()
  }
}

panel_admin1 <- apply_estimator(panel_admin1, "GID_1")
panel_admin2 <- apply_estimator(panel_admin2, "GID_2")

# ---- Sub-samples by capacity ------------------------------------------------
hi_a1 <- filter(panel_admin1, high_cap == 1L)
lo_a1 <- filter(panel_admin1, high_cap == 0L)
hi_a2 <- filter(panel_admin2, high_cap == 1L)
lo_a2 <- filter(panel_admin2, high_cap == 0L)

cat(sprintf(
  "Estimation sample (%s, %s panel):\n",
  ESTIMATOR,
  ifelse(USE_BALANCED_PANEL, "balanced", "unbalanced")
))
cat(sprintf("  Admin1 full sample : %d obs (%d countries)\n",
            nrow(panel_admin1), n_distinct(panel_admin1$GID_0)))
cat(sprintf("  Admin1 High cap    : %d obs\n", nrow(hi_a1)))
cat(sprintf("  Admin1 Low  cap    : %d obs\n", nrow(lo_a1)))
cat(sprintf("  Admin2 full sample : %d obs (%d countries)\n",
            nrow(panel_admin2), n_distinct(panel_admin2$GID_0)))
cat(sprintf("  Admin2 High cap    : %d obs\n", nrow(hi_a2)))
cat(sprintf("  Admin2 Low  cap    : %d obs\n\n", nrow(lo_a2)))

# =============================================================================
# SECTION 3: Descriptive statistics (Table 1)
# =============================================================================
# Report means and SDs for key variables by capacity group.
# This is the first table in the paper — check balance on observables and
# give the reader a sense of the data.

make_desc_row <- function(data, var, label) {
  vals <- data[[var]]
  tibble(
    variable = label,
    mean     = round(mean(vals,  na.rm = TRUE), 3),
    sd       = round(sd(vals,    na.rm = TRUE), 3),
    p25      = round(quantile(vals, 0.25, na.rm = TRUE), 3),
    p75      = round(quantile(vals, 0.75, na.rm = TRUE), 3),
    n        = sum(!is.na(vals))
  )
}

desc_table <- function(data, sgq_var, frag_var, aid_var, pop_var,
                       capacity_label = "Full") {
  bind_rows(
    make_desc_row(data, "nl_growth",   "NL Growth (% / yr)"),
    make_desc_row(data, "u5m",         "U5 Mortality"),
    make_desc_row(data, frag_var,      "Frag. Index (HHI-based)"),
    make_desc_row(data, aid_var,       "Total Aid (USD)"),
    make_desc_row(data, pop_var,       "LN Population"),
    make_desc_row(data, sgq_var,       "State Gov. Quality"),
    make_desc_row(data, IV_VAR,        "Shift-Share IV (lag)")
  ) |>
    mutate(group = capacity_label)
}

desc_admin1 <- bind_rows(
  desc_table(panel_admin1, "mean_sgq_admin1", "frag_index_admin1",
             "total_aid_admin1", "ln_pop_admin1", "Full"),
  desc_table(hi_a1, "mean_sgq_admin1", "frag_index_admin1",
             "total_aid_admin1", "ln_pop_admin1", "High Capacity"),
  desc_table(lo_a1, "mean_sgq_admin1", "frag_index_admin1",
             "total_aid_admin1", "ln_pop_admin1", "Low Capacity")
)

cat("=== Table 1: Descriptive Statistics (Admin1) ===\n")
print(desc_admin1)

# TODO: format desc_admin1 as a LaTeX table with gt or knitr::kable
# For now, write to CSV for manual inspection
write_csv(desc_admin1, here(TAB_DIR, "table1_desc_stats.csv"))

# =============================================================================
# SECTION 4: IV diagnostics
# =============================================================================
# Before reporting structural estimates, verify the instrument is relevant
# and passed a basic plausibility check for the exclusion restriction.
#
# (a) First-stage: total_aid_raw ~ IV_lag + controls + country FE
#     → report coefficient on IV_lag, F-stat, and partial R²
# (b) Reduced form: nl_growth ~ IV_lag + controls + country FE
#     → coefficient should have the sign consistent with structure
# (c) Pre-period placebo: regress LAGGED nl_growth on IV_lag
#     → if IV predicts pre-period outcomes, the exclusion restriction is suspect

fe_spec <- if (ESTIMATOR == "long_diff") "GID_0" else "GID_0^year + GID_1"

cat("\n=== IV Diagnostics (Admin1, full sample) ===\n")

# ---- (a) First stage --------------------------------------------------------
s1_data <- panel_admin1 |>
  filter(
    is.finite(total_aid_raw),
    is.finite(.data[[IV_VAR]]),
    is.finite(lag_log_pop),
    is.finite(lag_frag),
    is.finite(log_lag_nl)
  )

first_stage <- feols(
  as.formula(paste0(
    "total_aid_raw ~ ", IV_VAR, " + lag_log_pop + lag_frag + log_lag_nl | ", fe_spec
  )),
  cluster = ~GID_0,
  data    = s1_data
)
cat("\n--- First Stage: total_aid_raw ~ IV_lag + controls + country FE ---\n")
print(summary(first_stage))
# fitstat("ivf") only works on feols IV models; compute manually as t-stat squared
fs_fstat <- (coef(first_stage)[IV_VAR] / se(first_stage)[IV_VAR])^2
cat(sprintf("  First-stage F-stat on IV_lag (t^2): %.2f\n", fs_fstat))

# ---- (b) Reduced form -------------------------------------------------------
reduced_form_nl <- feols(
  as.formula(paste0(
    "nl_growth ~ ", IV_VAR, " + lag_log_pop + lag_frag + log_lag_nl | ", fe_spec
  )),
  cluster = ~GID_0,
  data    = filter(s1_data, is.finite(nl_growth))
)
cat("\n--- Reduced Form: nl_growth ~ IV_lag + controls + country FE ---\n")
print(summary(reduced_form_nl))

# ---- (c) Placebo: does IV predict PRE-PERIOD NL GROWTH? --------------------
# The prior run used pre-period NL *levels* as the placebo — that failed
# because the IV (historical shares × donor fractionalization) is mechanically
# correlated with regions that historically attracted more donors, which tend
# to be more economically active.
#
# Fix: two-step.
#   Step 1 — add log_lag_nl (pre-period NL level) as a control in all main
#             specs. This makes the exclusion restriction conditional on initial
#             conditions: the IV only needs to be uncorrelated with *changes*
#             in economic activity, not with levels. This is a standard and
#             more plausible assumption (Goldsmith-Pinkham et al. 2020).
#   Step 2 — use PRE-PERIOD NL GROWTH (t-2 → t-1) as the placebo outcome,
#             controlling for log_lag_nl. If the IV predicts past growth
#             conditional on the initial level, the exclusion restriction still
#             fails; otherwise, we pass the harder test.
placebo_nl <- feols(
  as.formula(paste0(
    "pre_nl_growth ~ ", IV_VAR, " + lag_log_pop + lag_frag + log_lag_nl | ", fe_spec
  )),
  cluster = ~GID_0,
  data    = filter(s1_data, is.finite(pre_nl_growth))
)
cat("\n--- Placebo: pre_nl_growth (t-2 -> t-1) ~ IV_lag + controls ---\n")
cat("  (Controlling for log_lag_nl; should be ~0 if exclusion holds conditional on levels)\n")
print(summary(placebo_nl))

# Write IV diagnostics table
IV_DICT <- c(
  IV_lag        = "Shift-Share IV (lag)",
  lag_frag      = "Lag Frag. Index",
  lag_log_pop   = "Lag LN(Pop)",
  log_lag_nl    = "LN NL Level (t-1)",
  total_aid_raw = "Total Aid (USD)"
)

etable(
  first_stage, reduced_form_nl, placebo_nl,
  headers  = c("First Stage", "Reduced Form (NL)", "Placebo (Pre-Period Growth)"),
  dict     = IV_DICT,
  se.below = TRUE,
  signif.code = SIG_CODE,
  fitstat  = c("n", "r2"),
  tex      = TRUE,
  replace  = TRUE,
  file     = here(TAB_DIR, "iv_diagnostics.tex")
)
cat("  IV diagnostics table written.\n\n")

# =============================================================================
# SECTION 5: Estimation functions
# =============================================================================

# ---- Fixed effects by estimator ---------------------------------------------
fe_a1 <- if (ESTIMATOR == "long_diff") "GID_0" else "GID_0^year + GID_1"
fe_a2 <- if (ESTIMATOR == "long_diff") "GID_0" else "GID_0^year + GID_2"

# ---- OLS (benchmark; no fixed effects) -------------------------------------
est_ols <- function(data, outcome, cluster = ~GID_0) {
  fml <- as.formula(paste0(
    outcome, " ~ lag_frag + lag_total_aid + lag_log_pop + log_lag_nl"
  ))
  feols(fml, cluster = cluster, data = filter(data, is.finite(.data[[outcome]]),
                                               is.finite(log_lag_nl)))
}

# ---- Country FE (or unit + country-year FE in panel mode) ------------------
est_fe <- function(data, outcome, fe_str, cluster = ~GID_0) {
  fml <- as.formula(paste0(
    outcome, " ~ lag_frag + lag_total_aid + lag_log_pop + log_lag_nl | ", fe_str
  ))
  feols(fml, cluster = cluster, data = filter(data, is.finite(.data[[outcome]]),
                                               is.finite(log_lag_nl)))
}

# ---- Control function approach (CFA) ----------------------------------------
# Stage 1: regress total_aid_raw on IV + controls (incl. log_lag_nl) + FE
# Stage 2: regress outcome on frag + log_aid + pop + log_lag_nl + CFA resid + FE
# log_lag_nl (pre-period NL level) controls for pre-existing economic activity
# so the exclusion restriction only requires IV is uncorrelated with NL *changes*.
est_cfa <- function(data, outcome, fe_str, iv = IV_VAR, cluster = ~GID_0) {
  s1_d <- data |>
    filter(
      is.finite(total_aid_raw),
      is.finite(.data[[iv]]),
      is.finite(lag_log_pop),
      is.finite(log_lag_nl)
    )

  s1 <- feols(
    as.formula(paste0(
      "total_aid_raw ~ ", iv, " + lag_log_pop + lag_frag + log_lag_nl | ", fe_str
    )),
    cluster = cluster,
    data    = s1_d
  )

  s2_d <- s1_d |>
    mutate(cfa_resid = resid(s1)) |>
    filter(
      is.finite(.data[[outcome]]),
      is.finite(lag_frag),
      is.finite(lag_total_aid),
      is.finite(cfa_resid)
    )

  feols(
    as.formula(paste0(
      outcome,
      " ~ lag_frag + lag_total_aid + lag_log_pop + log_lag_nl + cfa_resid | ", fe_str
    )),
    cluster = cluster,
    data    = s2_d
  )
}

# ---- 2SLS (alternative to CFA; same structural params, different SE calc) ---
# Instrument total_aid_raw with IV_lag.  Fragmentation is treated as exogenous
# conditional on instrumented aid volume, pre-period NL level, and country FE.
est_2sls <- function(data, outcome, fe_str, iv = IV_VAR, cluster = ~GID_0) {
  fml <- as.formula(paste0(
    outcome,
    " ~ lag_frag + lag_log_pop + log_lag_nl | ", fe_str,
    " | lag_total_aid ~ ", iv
  ))
  feols(fml, cluster = cluster,
        data = filter(
          data,
          is.finite(.data[[outcome]]),
          is.finite(lag_frag),
          is.finite(log_lag_nl),
          is.finite(lag_log_pop),
          is.finite(lag_total_aid),
          is.finite(.data[[iv]])
        ))
}

# =============================================================================
# SECTION 6: Main regression tables
# =============================================================================
# Structure follows a standard escalation in identification:
#   Col 1: OLS (biased but useful benchmark)
#   Col 2: OLS + country FE (removes time-invariant country confounders)
#   Col 3: CFA — instrumented aid + country FE (headline estimate)
#   Col 4: 2SLS — same structural model, different SE approach
#
# Each table pools all units; Section 7 splits by capacity group.

VAR_DICT <- c(
  lag_frag            = "Fragmentation Index (t-1)",
  lag_total_aid       = "LN Total Aid (t-1)",
  lag_log_pop         = "LN Population (t-1)",
  log_lag_nl          = "LN NL Level (t-1)",
  cfa_resid           = "CFA Residual (endogeneity control)",
  `fit_lag_total_aid` = "LN Total Aid (t-1, instrumented)"
)

run_main_table <- function(data, outcome, fe_str, label, out_stub) {
  cat(sprintf("\n=== Main Table: %s | outcome = %s ===\n", label, outcome))

  m_ols    <- est_ols (data, outcome)
  m_fe     <- est_fe  (data, outcome, fe_str)
  m_cfa    <- est_cfa (data, outcome, fe_str)
  m_2sls   <- est_2sls(data, outcome, fe_str)

  etable(
    m_ols, m_fe, m_cfa, m_2sls,
    headers     = c("OLS", "Country FE", "CFA", "2SLS"),
    dict        = VAR_DICT,
    se.below    = TRUE,
    signif.code = SIG_CODE,
    fitstat     = c("n", "r2", "ivf"),
    tex         = TRUE,
    replace     = TRUE,
    file        = here(TAB_DIR, paste0(out_stub, ".tex"))
  )
  cat(sprintf("  Table written: %s.tex\n", out_stub))

  invisible(list(ols = m_ols, fe = m_fe, cfa = m_cfa, sls = m_2sls))
}

# --- NL growth: admin1 full sample ---
res_nl_a1 <- run_main_table(
  panel_admin1, "nl_growth", fe_a1,
  "Admin1 full sample", "main_nl_admin1"
)

# --- U5 mortality: admin1 full sample ---
res_u5_a1 <- run_main_table(
  filter(panel_admin1, !is.na(u5m)), "u5m", fe_a1,
  "Admin1 full sample", "main_u5m_admin1"
)

# =============================================================================
# SECTION 7: Heterogeneity by governance capacity
# =============================================================================
# The core economic question: does governance capacity determine whether
# donor fragmentation helps or hurts?
# High capacity → can coordinate; more donors may bring useful competition.
# Low capacity → coordination problems; more donors → chaos.
# We expect the effect of fragmentation on NL growth to be more positive
# (or less negative) in high-capacity regions.

run_capacity_table <- function(hi_data, lo_data, outcome, fe_str,
                               label, out_stub) {
  cat(sprintf("\n=== Heterogeneity Table: %s | outcome = %s ===\n",
              label, outcome))

  hi_cfa <- est_cfa(hi_data, outcome, fe_str)
  lo_cfa <- est_cfa(lo_data, outcome, fe_str)
  hi_2sls <- est_2sls(hi_data, outcome, fe_str)
  lo_2sls <- est_2sls(lo_data, outcome, fe_str)

  etable(
    hi_cfa, lo_cfa, hi_2sls, lo_2sls,
    headers     = c("High Cap (CFA)", "Low Cap (CFA)",
                    "High Cap (2SLS)", "Low Cap (2SLS)"),
    dict        = VAR_DICT,
    se.below    = TRUE,
    signif.code = SIG_CODE,
    fitstat     = c("n", "r2"),
    tex         = TRUE,
    replace     = TRUE,
    file        = here(TAB_DIR, paste0(out_stub, ".tex"))
  )
  cat(sprintf("  Table written: %s.tex\n", out_stub))

  invisible(list(hi_cfa = hi_cfa, lo_cfa = lo_cfa,
                 hi_2sls = hi_2sls, lo_2sls = lo_2sls))
}

het_nl_a1 <- run_capacity_table(
  hi_a1, lo_a1, "nl_growth", fe_a1,
  "Admin1: High vs. Low Capacity", "het_nl_admin1"
)

het_u5_a1 <- run_capacity_table(
  filter(hi_a1, !is.na(u5m)),
  filter(lo_a1, !is.na(u5m)),
  "u5m", fe_a1,
  "Admin1: High vs. Low Capacity", "het_u5m_admin1"
)

# =============================================================================
# SECTION 8: Robustness checks
# =============================================================================

# ---- 8a: Balanced panel -----------------------------------------------------
# Repeat main CFA on balanced panel (all 5 periods required).
# If results change substantially → composition bias is a concern.
cat("\n=== Robustness 8a: Balanced panel (Admin1) ===\n")

balanced_ids <- diag_admin1 |>
  filter(n_periods == n_periods_a1) |>
  pull(GID_1)

balanced_a1 <- panel_admin1 |> filter(GID_1 %in% balanced_ids)
bal_cfa_nl  <- est_cfa(balanced_a1, "nl_growth", fe_a1)

cat(sprintf(
  "  Balanced sample: %d units. CFA coef on lag_frag: %.4f (SE: %.4f)\n",
  n_distinct(balanced_a1$GID_1),
  coef(bal_cfa_nl)["lag_frag"],
  se(bal_cfa_nl)["lag_frag"]
))

# ---- 8b: Winsorization sensitivity ------------------------------------------
# Does the main result depend on the winsorization cutoff?
cat("\n=== Robustness 8b: Winsorization sensitivity (Admin1, NL growth) ===\n")

wins_specs <- list(
  "1/99"  = c(0.01, 0.99),
  "5/95"  = c(0.05, 0.95),
  "10/90" = c(0.10, 0.90)
)

wins_results <- lapply(names(wins_specs), function(label) {
  bounds <- wins_specs[[label]]
  d <- raw_admin1 |>
    norm_nl_cols() |>
    apply_balance_filter("GID_1", diag_admin1, n_periods_a1) |>
    build_panel_vars("GID_1", "total_aid_admin1",
                     "frag_index_admin1", "ln_pop_admin1") |>
    winsorise_outcome("nl_growth", bounds[1], bounds[2]) |>
    add_capacity("mean_sgq_admin1") |>
    apply_estimator("GID_1")
  m <- est_cfa(d, "nl_growth", fe_a1)
  cat(sprintf(
    "  Wins %s → lag_frag coef: %.4f (SE: %.4f)\n",
    label, coef(m)["lag_frag"], se(m)["lag_frag"]
  ))
  m
})
names(wins_results) <- names(wins_specs)

etable(
  wins_results[["1/99"]], wins_results[["5/95"]], wins_results[["10/90"]],
  headers     = c("Wins 1/99", "Wins 5/95", "Wins 10/90"),
  dict        = VAR_DICT,
  se.below    = TRUE,
  signif.code = SIG_CODE,
  fitstat     = c("n", "r2"),
  tex         = TRUE,
  replace     = TRUE,
  file        = here(TAB_DIR, "robust_winsorization.tex")
)
cat("  Winsorization robustness table written.\n")

# ---- 8c: Admin2 replication -------------------------------------------------
cat("\n=== Robustness 8c: Admin2 replication ===\n")

rob_nl_a2 <- run_main_table(
  panel_admin2, "nl_growth", fe_a2,
  "Admin2 (robustness)", "robust_nl_admin2"
)

# ---- 8d: Panel FE estimator (all 5-year transitions) -----------------------
cat("\n=== Robustness 8d: Panel FE estimator (Admin1) ===\n")

# Re-build with ESTIMATOR = "panel_fe" using local scope
panel_a1_pfe <- raw_admin1 |>
  norm_nl_cols() |>
  build_panel_vars("GID_1", "total_aid_admin1",
                   "frag_index_admin1", "ln_pop_admin1") |>
  winsorise_outcome("nl_growth") |>
  add_capacity("mean_sgq_admin1") |>
  group_by(GID_1) |>
  filter(!is.na(lag_frag)) |>   # drop first period, keep all others
  ungroup()

fe_pfe <- "GID_0^year + GID_1"   # unit + country-year FE for panel estimator
pfe_cfa_nl <- est_cfa(panel_a1_pfe, "nl_growth", fe_pfe)

cat(sprintf(
  "  Panel FE CFA coef on lag_frag: %.4f (SE: %.4f, N = %d)\n",
  coef(pfe_cfa_nl)["lag_frag"],
  se(pfe_cfa_nl)["lag_frag"],
  nobs(pfe_cfa_nl)
))

etable(
  res_nl_a1$cfa, pfe_cfa_nl,
  headers     = c("Long-Difference (primary)", "Panel FE (robustness)"),
  dict        = VAR_DICT,
  se.below    = TRUE,
  signif.code = SIG_CODE,
  fitstat     = c("n", "r2"),
  tex         = TRUE,
  replace     = TRUE,
  file        = here(TAB_DIR, "robust_estimator.tex")
)
cat("  Estimator robustness table written.\n")

# =============================================================================
# SECTION 9: Figures
# =============================================================================

# ---- Fig 1: Distribution of fragmentation index by capacity group -----------
fig1 <- ggplot(panel_admin1, aes(x = lag_frag, fill = factor(high_cap))) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  scale_fill_manual(
    values = c("0" = "#d95f02", "1" = "#1b9e77"),
    labels = c("0" = "Low Capacity", "1" = "High Capacity")
  ) +
  labs(
    x    = "Fragmentation Index (lagged)",
    y    = "Count",
    fill = NULL,
    title = "Distribution of Donor Fragmentation by Governance Capacity"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(here(FIG_DIR, "fig1_frag_dist.pdf"), fig1,
       width = 7, height = 4.5)
cat("\nFig 1 written: fig1_frag_dist.pdf\n")

# ---- Fig 2: Binscatter — fragmentation vs. NL growth -----------------------
# Residualise both axes on country FE (partial out GID_0) to isolate
# within-country variation before plotting.

partial_on_country_fe <- function(data, y_var, x_var) {
  d <- data |> filter(is.finite(.data[[y_var]]), is.finite(.data[[x_var]]),
                       !is.na(GID_0))
  # Residuals from country FE demeaning
  dy <- feols(as.formula(paste0(y_var, " ~ 1 | GID_0")),
              data = d)$residuals
  dx <- feols(as.formula(paste0(x_var, " ~ 1 | GID_0")),
              data = d)$residuals
  tibble(x_resid = dx, y_resid = dy,
         high_cap = d$high_cap)
}

binscat_data <- partial_on_country_fe(panel_admin1, "nl_growth", "lag_frag")

# Compute binned means (20 quantile bins by x)
binscat_binned <- binscat_data |>
  mutate(bin = ntile(x_resid, 20)) |>
  group_by(bin, high_cap) |>
  summarise(x = mean(x_resid), y = mean(y_resid), .groups = "drop")

fig2 <- ggplot(binscat_binned,
               aes(x = x, y = y,
                   colour = factor(high_cap),
                   shape  = factor(high_cap))) +
  geom_point(size = 2.5, alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  scale_colour_manual(
    values = c("0" = "#d95f02", "1" = "#1b9e77"),
    labels = c("0" = "Low Capacity", "1" = "High Capacity")
  ) +
  scale_shape_manual(
    values = c("0" = 17, "1" = 16),
    labels = c("0" = "Low Capacity", "1" = "High Capacity")
  ) +
  labs(
    x      = "Fragmentation Index — residualised on country FE",
    y      = "NL Growth (% / yr) — residualised on country FE",
    colour = NULL,
    shape  = NULL,
    title  = "Fragmentation vs. Nightlights Growth (within-country variation)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(here(FIG_DIR, "fig2_binscatter_nl.pdf"), fig2,
       width = 7, height = 4.5)
cat("Fig 2 written: fig2_binscatter_nl.pdf\n")

# ---- Fig 3: First-stage scatter — IV vs. total aid -------------------------
fig3_data <- panel_admin1 |>
  filter(is.finite(.data[[IV_VAR]]), is.finite(total_aid_raw))

fig3 <- ggplot(fig3_data, aes(x = .data[[IV_VAR]], y = total_aid_raw / 1e6)) +
  geom_point(alpha = 0.3, size = 1.2, colour = "grey40") +
  geom_smooth(method = "lm", se = TRUE, colour = "#2166ac", linewidth = 1) +
  labs(
    x     = "Shift-Share IV (lagged)",
    y     = "Total Aid (USD millions, lagged)",
    title = "First-Stage: Shift-Share IV vs. Total Aid"
  ) +
  theme_minimal(base_size = 12)

ggsave(here(FIG_DIR, "fig3_first_stage.pdf"), fig3,
       width = 6, height = 4.5)
cat("Fig 3 written: fig3_first_stage.pdf\n")

# ---- Fig 4: Coefficient plot — fragmentation across all specs ---------------
# Extract lag_frag coefficient and CI from each main specification for admin1

coef_plot_data <- bind_rows(
  tibble(
    spec   = "OLS",
    est    = coef(res_nl_a1$ols)["lag_frag"],
    lo     = confint(res_nl_a1$ols)["lag_frag", 1],
    hi     = confint(res_nl_a1$ols)["lag_frag", 2],
    sample = "Full"
  ),
  tibble(
    spec   = "Country FE",
    est    = coef(res_nl_a1$fe)["lag_frag"],
    lo     = confint(res_nl_a1$fe)["lag_frag", 1],
    hi     = confint(res_nl_a1$fe)["lag_frag", 2],
    sample = "Full"
  ),
  tibble(
    spec   = "CFA",
    est    = coef(res_nl_a1$cfa)["lag_frag"],
    lo     = confint(res_nl_a1$cfa)["lag_frag", 1],
    hi     = confint(res_nl_a1$cfa)["lag_frag", 2],
    sample = "Full"
  ),
  tibble(
    spec   = "CFA",
    est    = coef(het_nl_a1$hi_cfa)["lag_frag"],
    lo     = confint(het_nl_a1$hi_cfa)["lag_frag", 1],
    hi     = confint(het_nl_a1$hi_cfa)["lag_frag", 2],
    sample = "High Cap"
  ),
  tibble(
    spec   = "CFA",
    est    = coef(het_nl_a1$lo_cfa)["lag_frag"],
    lo     = confint(het_nl_a1$lo_cfa)["lag_frag", 1],
    hi     = confint(het_nl_a1$lo_cfa)["lag_frag", 2],
    sample = "Low Cap"
  )
) |>
  mutate(
    label = paste0(spec, "\n(", sample, ")"),
    label = factor(label, levels = unique(label))
  )

fig4 <- ggplot(coef_plot_data, aes(x = label, y = est,
                                    colour = sample, shape = spec)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2, linewidth = 0.8) +
  geom_point(size = 3) +
  scale_colour_manual(
    values = c("Full" = "#555555", "High Cap" = "#1b9e77", "Low Cap" = "#d95f02")
  ) +
  labs(
    x      = NULL,
    y      = "Coefficient on Fragmentation Index",
    colour = "Sample",
    shape  = "Estimator",
    title  = "Effect of Donor Fragmentation on NL Growth — Across Specifications"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(size = 9))

ggsave(here(FIG_DIR, "fig4_coef_plot.pdf"), fig4,
       width = 8, height = 5)
cat("Fig 4 written: fig4_coef_plot.pdf\n")

# =============================================================================
# DONE
# =============================================================================

message(
  "\n======================================================\n",
  "Analysis complete.\n",
  "Tables → ", TAB_DIR, "\n",
  "Figures → ", FIG_DIR, "\n",
  "Settings: ESTIMATOR = ", ESTIMATOR,
  ", BALANCED = ", USE_BALANCED_PANEL,
  ", CAPACITY_PCT = ", CAPACITY_PERCENTILE,
  "\n======================================================"
)
