# =============================================================================
# 07_panel_analysis_5year.r
#
# Five-year panel analysis: donor fragmentation and aid effectiveness
#
# Decision flow:
#   1. Diagnose panel balance → decide balanced vs. unbalanced sample
#   2. Choose estimator: long-difference or full panel FE
#   3. Run OLS, country FE, and CFA (control function) regressions
#   4. Outcomes: night-lights growth (nl_growth) and U5M mortality (u5m)
# =============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here, readr, fixest, janitor)

# =============================================================================
# SECTION 0: Configuration — edit these before running
# =============================================================================

# --- Panel sample choice ---
# TRUE  → keep only units observed in ALL five 5-year periods (1995–2015)
# FALSE → unbalanced panel; all units regardless of period coverage
# Run Section 1 diagnostics first, then set this.
USE_BALANCED_PANEL <- FALSE

# --- Estimator choice ---
# "long_diff" → for each unit, retain only the terminal observation (year == 2015)
#               after lagging is applied; one observation per unit; pure cross-section
#               long-difference. Preferred: reduces measurement-error attenuation.
# "panel_fe"  → retain all annual observations; absorb unit + country-year FE.
#               More observations but may amplify noise in annual nightlights.
ESTIMATOR <- "long_diff"

# --- Outcome variables ---
OUTCOMES <- list(
  nl_growth = list(label = "NL Growth (\\%/yr)",  cluster = "GID_0"),
  u5m       = list(label = "U5 Mortality",         cluster = "GID_0")
)

# --- IV variable ---
IV_VAR <- "IV_lag"

# --- Capacity-split percentile (default: 75th) ---
CAPACITY_PERCENTILE <- 0.75

# =============================================================================
# SECTION 1: Load data and diagnose panel balance
# =============================================================================

panel_suffix <- "_5year_1995_2015"

admin1_path <- here("01_panel_data", paste0("panel_aid_admin1_fin", panel_suffix, ".csv"))
admin2_path <- here("01_panel_data", paste0("panel_aid_admin2_fin", panel_suffix, ".csv"))

if (!file.exists(admin1_path)) stop("Missing admin1 panel: ", admin1_path)
if (!file.exists(admin2_path)) stop("Missing admin2 panel: ", admin2_path)

raw_admin1 <- read_csv(admin1_path, show_col_types = FALSE)
raw_admin2 <- read_csv(admin2_path, show_col_types = FALSE)

diagnose_balance <- function(data, unit_id, label) {
  periods <- sort(unique(data$year))
  n_periods <- length(periods)

  obs_per_unit <- data %>%
    group_by(.data[[unit_id]]) %>%
    summarise(n_obs = n(), n_periods_obs = n_distinct(year), .groups = "drop")

  n_total    <- n_distinct(data[[unit_id]])
  n_balanced <- sum(obs_per_unit$n_periods_obs == n_periods)
  pct_bal    <- round(100 * n_balanced / n_total, 1)

  cat("--------------------------------------------------------------\n")
  cat(sprintf("Panel balance diagnostic: %s\n", label))
  cat(sprintf("  Years in panel         : %s\n", paste(periods, collapse = ", ")))
  cat(sprintf("  Total unique units     : %d\n", n_total))
  cat(sprintf("  Fully balanced units   : %d (%.1f%%)\n", n_balanced, pct_bal))
  cat(sprintf("  Units with 1 period    : %d\n", sum(obs_per_unit$n_periods_obs == 1)))
  cat(sprintf("  Units with 2-4 periods : %d\n",
              sum(obs_per_unit$n_periods_obs >= 2 & obs_per_unit$n_periods_obs < n_periods)))
  cat("  Observations per unit (distribution):\n")
  print(table(obs_per_unit$n_periods_obs))
  cat("\n")

  invisible(obs_per_unit)
}

obs_admin1 <- diagnose_balance(raw_admin1, "GID_1", "Admin1 panel")
obs_admin2 <- diagnose_balance(raw_admin2, "GID_2", "Admin2 panel")

# =============================================================================
# SECTION 2: Sample construction
# =============================================================================

# Rename nightlights columns for consistency (may be "mean" or "mean_nl")
normalize_nl_cols <- function(data) {
  if (!("mean_nl" %in% names(data)) && ("mean" %in% names(data))) {
    data <- rename(data, mean_nl = mean)
  }
  if (!("sum_nl" %in% names(data)) && ("sum" %in% names(data))) {
    data <- rename(data, sum_nl = sum)
  }
  data
}

apply_balance_filter <- function(data, unit_id, obs_summary, n_all_periods) {
  if (!USE_BALANCED_PANEL) return(data)
  keep_ids <- obs_summary %>%
    filter(n_periods_obs == n_all_periods) %>%
    pull(.data[[unit_id]])
  filter(data, .data[[unit_id]] %in% keep_ids)
}

n_periods_admin1 <- n_distinct(raw_admin1$year)
n_periods_admin2 <- n_distinct(raw_admin2$year)

panel_admin1 <- raw_admin1 %>%
  normalize_nl_cols() %>%
  apply_balance_filter("GID_1", obs_admin1, n_periods_admin1)

panel_admin2 <- raw_admin2 %>%
  normalize_nl_cols() %>%
  apply_balance_filter("GID_2", obs_admin2, n_periods_admin2)

cat(sprintf(
  "Sample after balance filter (%s):\n  Admin1: %d units, %d obs\n  Admin2: %d units, %d obs\n\n",
  ifelse(USE_BALANCED_PANEL, "balanced", "unbalanced"),
  n_distinct(panel_admin1$GID_1), nrow(panel_admin1),
  n_distinct(panel_admin2$GID_2), nrow(panel_admin2)
))

# -----------------------------------------------------------------------------
# Compute lags and growth within each unit
# -----------------------------------------------------------------------------
#
# lag_t = 1 means: for year t, the lagged value is from year t-1.
# For the 5-year panel the data represents 5-year bucket averages; each row is
# one annual observation. Growth is scaled by 5 (bucket length) to be
# annualized over the 5-year window.
#
# Note: growth_denom = 5 ensures nl_growth is interpretable as average annual
# percentage growth across the 5-year period.

build_panel_vars <- function(data, unit_id, aid_var, frag_var, pop_var) {
  data %>%
    arrange(.data[[unit_id]], year) %>%
    group_by(.data[[unit_id]]) %>%
    mutate(
      lag_mean_nl    = dplyr::lag(mean_nl, n = 1L),
      nl_growth      = case_when(
        is.na(lag_mean_nl) ~ NA_real_,
        TRUE ~ ((log(mean_nl + 0.01) - log(lag_mean_nl + 0.01)) / 5) * 100
      ),
      lag_frag       = dplyr::lag(.data[[frag_var]], n = 1L),
      lag_log_pop    = dplyr::lag(.data[[pop_var]],  n = 1L),
      lag_total_aid  = log(dplyr::lag(.data[[aid_var]], n = 1L) + 0.01)
    ) %>%
    ungroup() %>%
    # Remove extreme outliers in nl_growth (winsorize at 5th/95th)
    (function(d) {
      if (sum(is.finite(d$nl_growth)) < 20) return(d)
      q <- quantile(d$nl_growth, c(0.05, 0.95), na.rm = TRUE)
      filter(d, !is.finite(nl_growth) | (nl_growth > q[1] & nl_growth < q[2]))
    })()
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

# -----------------------------------------------------------------------------
# Capacity split (High / Low governance quality at CAPACITY_PERCENTILE)
# -----------------------------------------------------------------------------
split_by_capacity <- function(data, sgq_var) {
  cutoff <- quantile(data[[sgq_var]], CAPACITY_PERCENTILE, na.rm = TRUE)
  if (!is.finite(cutoff)) cutoff <- median(data[[sgq_var]], na.rm = TRUE)
  mutate(data, high_cap = as.integer(.data[[sgq_var]] > cutoff))
}

panel_admin1 <- split_by_capacity(panel_admin1, "mean_sgq_admin1")
panel_admin2 <- split_by_capacity(panel_admin2, "mean_sgq_admin2")

# -----------------------------------------------------------------------------
# Apply estimator-specific sample restriction
# -----------------------------------------------------------------------------
apply_estimator_filter <- function(data, unit_id) {
  if (ESTIMATOR == "long_diff") {
    # Keep only the last year per unit (post-period of the longest difference).
    # All outcome/covariate variation is captured via the lagged variables.
    data %>%
      group_by(.data[[unit_id]]) %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      ungroup()
  } else {
    # Full panel: drop first year of each unit (no lag available)
    data %>%
      group_by(.data[[unit_id]]) %>%
      filter(!is.na(lag_frag)) %>%
      ungroup()
  }
}

panel_admin1 <- apply_estimator_filter(panel_admin1, "GID_1")
panel_admin2 <- apply_estimator_filter(panel_admin2, "GID_2")

# Sub-samples by capacity
high_admin1 <- filter(panel_admin1, high_cap == 1L)
low_admin1  <- filter(panel_admin1, high_cap == 0L)
high_admin2 <- filter(panel_admin2, high_cap == 1L)
low_admin2  <- filter(panel_admin2, high_cap == 0L)

cat(sprintf(
  "Estimation sample (%s estimator):\n",
  ESTIMATOR
))
cat(sprintf("  Admin1 High capacity: %d obs\n", nrow(high_admin1)))
cat(sprintf("  Admin1 Low  capacity: %d obs\n", nrow(low_admin1)))
cat(sprintf("  Admin2 High capacity: %d obs\n", nrow(high_admin2)))
cat(sprintf("  Admin2 Low  capacity: %d obs\n\n", nrow(low_admin2)))

# =============================================================================
# SECTION 3: Estimation functions
# =============================================================================

# Fixed effects term depends on estimator
fe_term <- function(admin_level) {
  if (ESTIMATOR == "long_diff") {
    "GID_0"           # country FE only (cross-section; no within-unit time variation)
  } else {
    paste0("GID_0^year + ", admin_level)  # country-year + unit FE
  }
}

# --- OLS (no FE) ---
est_ols <- function(data, outcome, cluster) {
  fml <- as.formula(paste0(outcome, " ~ lag_frag + lag_total_aid + lag_log_pop"))
  feols(fml, cluster = cluster, data = data)
}

# --- Country (or country-year + unit) FE ---
est_fe <- function(data, outcome, admin_level, cluster) {
  fe  <- fe_term(admin_level)
  fml <- as.formula(paste0(outcome, " ~ lag_frag + lag_total_aid + lag_log_pop | ", fe))
  feols(fml, cluster = cluster, data = data)
}

# --- Control function approach (CFA) ---
# Stage 1: total_aid ~ IV + controls + FE
# Stage 2: outcome ~ lag_frag + lag_total_aid + lag_log_pop + cfa_resid + FE
#
# Note: We instrument *total_aid* (the current-period endogenous variable) to
# obtain a clean measure of aid volume. Fragmentation (lag_frag) is treated as
# exogenous conditional on instrumented aid, the CFA residual, and country FE.
est_cfa <- function(data, outcome, admin_level, iv, cluster) {
  fe <- fe_term(admin_level)

  s1_data <- data %>%
    filter(
      is.finite(total_aid_raw),
      is.finite(.data[[iv]]),
      is.finite(lag_log_pop)
    )

  # Stage 1
  s1_fml <- as.formula(paste0("total_aid_raw ~ ", iv, " + lag_log_pop | ", fe))
  s1     <- feols(s1_fml, cluster = cluster, data = s1_data)

  # Attach residuals and filter to complete cases for stage 2
  s2_data <- s1_data %>%
    mutate(cfa_resid = resid(s1)) %>%
    filter(
      is.finite(.data[[outcome]]),
      is.finite(lag_frag),
      is.finite(lag_total_aid),
      is.finite(cfa_resid)
    )

  s2_fml <- as.formula(
    paste0(outcome, " ~ lag_frag + lag_total_aid + lag_log_pop + cfa_resid | ", fe)
  )
  feols(s2_fml, cluster = cluster, data = s2_data)
}

# Attach raw (un-logged) total aid for CFA stage 1 endogenous variable
panel_admin1 <- mutate(panel_admin1,
  total_aid_raw = dplyr::lag(total_aid_admin1, n = 1L, order_by = NULL)
) %>%
  group_by(GID_1) %>%
  mutate(total_aid_raw = dplyr::lag(total_aid_admin1, n = 1L)) %>%
  ungroup()

panel_admin2 <- panel_admin2 %>%
  group_by(GID_2) %>%
  mutate(total_aid_raw = dplyr::lag(total_aid_admin2, n = 1L)) %>%
  ungroup()

# Rebuild sub-samples after adding total_aid_raw
high_admin1 <- filter(panel_admin1, high_cap == 1L)
low_admin1  <- filter(panel_admin1, high_cap == 0L)
high_admin2 <- filter(panel_admin2, high_cap == 1L)
low_admin2  <- filter(panel_admin2, high_cap == 0L)

# =============================================================================
# SECTION 4: Run regressions for each outcome
# =============================================================================

tab_path <- function(stub) {
  here("03_output", "tabs", paste0(stub, "_5year.tex"))
}

VAR_DICT <- c(
  lag_frag       = "Lag Frag. Index",
  lag_total_aid  = "Lag LN(Total Aid)",
  lag_log_pop    = "Lag LN(Population)",
  cfa_resid      = "CFA Residual"
)

run_outcome <- function(outcome, label, cluster) {
  cat(sprintf("=== Outcome: %s ===\n", outcome))

  # ---- OLS ----
  h1_ols <- est_ols(high_admin1, outcome, cluster)
  l1_ols <- est_ols(low_admin1,  outcome, cluster)
  h2_ols <- est_ols(high_admin2, outcome, cluster)
  l2_ols <- est_ols(low_admin2,  outcome, cluster)

  etable(
    h1_ols, l1_ols, h2_ols, l2_ols,
    headers = c("High-Admin1", "Low-Admin1", "High-Admin2", "Low-Admin2"),
    dict    = VAR_DICT,
    se.below = TRUE,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    fitstat = c("n", "r2"),
    tex     = TRUE,
    replace = TRUE,
    file    = tab_path(paste0("ols_", outcome))
  )

  # ---- Country FE (or unit+country-year FE) ----
  h1_fe <- est_fe(high_admin1, outcome, "GID_1", cluster)
  l1_fe <- est_fe(low_admin1,  outcome, "GID_1", cluster)
  h2_fe <- est_fe(high_admin2, outcome, "GID_2", cluster)
  l2_fe <- est_fe(low_admin2,  outcome, "GID_2", cluster)

  etable(
    h1_fe, l1_fe, h2_fe, l2_fe,
    headers = c("High-Admin1", "Low-Admin1", "High-Admin2", "Low-Admin2"),
    dict    = VAR_DICT,
    se.below = TRUE,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    fitstat = c("n", "r2"),
    tex     = TRUE,
    replace = TRUE,
    file    = tab_path(paste0("fe_", outcome))
  )

  # ---- Control function (CFA) ----
  h1_cfa <- est_cfa(high_admin1, outcome, "GID_1", IV_VAR, cluster)
  l1_cfa <- est_cfa(low_admin1,  outcome, "GID_1", IV_VAR, cluster)
  h2_cfa <- est_cfa(high_admin2, outcome, "GID_2", IV_VAR, cluster)
  l2_cfa <- est_cfa(low_admin2,  outcome, "GID_2", IV_VAR, cluster)

  etable(
    h1_cfa, l1_cfa, h2_cfa, l2_cfa,
    headers = c("High-Admin1", "Low-Admin1", "High-Admin2", "Low-Admin2"),
    dict    = VAR_DICT,
    se.below = TRUE,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    fitstat = c("n", "r2"),
    tex     = TRUE,
    replace = TRUE,
    file    = tab_path(paste0("cfa_", outcome))
  )

  cat(sprintf("  Tables written to 03_output/tabs/ for outcome: %s\n\n", outcome))

  invisible(list(
    ols = list(high_admin1 = h1_ols, low_admin1 = l1_ols,
               high_admin2 = h2_ols, low_admin2 = l2_ols),
    fe  = list(high_admin1 = h1_fe,  low_admin1 = l1_fe,
               high_admin2 = h2_fe,  low_admin2 = l2_fe),
    cfa = list(high_admin1 = h1_cfa, low_admin1 = l1_cfa,
               high_admin2 = h2_cfa, low_admin2 = l2_cfa)
  ))
}

results <- lapply(names(OUTCOMES), function(oc) {
  info <- OUTCOMES[[oc]]
  run_outcome(outcome = oc, label = info$label, cluster = info$cluster)
})
names(results) <- names(OUTCOMES)

message(
  "Done. Tables written to ", here("03_output", "tabs"), "\n",
  "Settings: USE_BALANCED_PANEL=", USE_BALANCED_PANEL,
  ", ESTIMATOR=", ESTIMATOR
)
