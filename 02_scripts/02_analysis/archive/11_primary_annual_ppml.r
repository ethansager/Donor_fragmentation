# =============================================================================
# PRIMARY ANALYSIS: annual hybrid panel, PPML headline + c-sensitivity robustness
#
# Design:
#   - Outcome = annual nightlight LEVEL (mean_nl); aid/frag held constant within
#     5-year window. Region (GID_1/2) + year fixed effects -> within-region,
#     across-window identification, common shocks absorbed.
#   - Headline estimator = PPML (fepois): handles the 100s of zeros / near-zeros
#     natively, no log(y+c) constant, coef = proportional (semi-elasticity) effect.
#   - Robustness = c-sensitivity: shows the log(y+c) growth coef is unstable in c
#     (Chen & Roth 2024), motivating PPML.
#
# Inputs : 01_panel_data/panel_aid_admin{1,2}_annual_hybrid.csv
# Outputs: 03_output/tabs/annual_ppml_*.tex  (+ console summary)
# =============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here, readr, fixest)

C_MAIN          <- 0.01            # legacy additive constant (for comparison only)
CAP_QUANTILE    <- 0.75            # high-capacity = above 75th pct of mean_sgq
TAB_DIR         <- here("03_output", "tabs")
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)

DICT <- c(
  frag        = "Donor Fragmentation",
  ln_aid      = "Log Total Aid",
  lag_log_pop = "Log Population",
  log_lag_nl  = "Log Lagged NL",
  mean_nl     = "Nightlights (level)",
  u5m         = "Under-5 Mortality",
  GID_1 = "Admin1", GID_2 = "Admin2", GID_0 = "Country", year = "Year"
)

# ---- prepare an annual hybrid panel for one admin level ---------------------
prep_annual <- function(admin) {
  uid  <- paste0("GID_", admin)
  frag_v <- paste0("frag_index_admin", admin)
  aid_v  <- paste0("total_aid_admin", admin)
  pop_v  <- paste0("ln_pop_admin", admin)
  sgq_v  <- paste0("mean_sgq_admin", admin)

  read_csv(here("01_panel_data", paste0("panel_aid_admin", admin, "_annual_hybrid.csv")),
           show_col_types = FALSE) |>
    arrange(.data[[uid]], year) |>
    group_by(.data[[uid]]) |>
    mutate(
      lag_nl      = lag(mean_nl, 1L),
      log_lag_nl  = log(lag_nl + C_MAIN),
      frag        = .data[[frag_v]],
      ln_aid      = log(.data[[aid_v]] + C_MAIN),
      lag_log_pop = .data[[pop_v]],
      sgq         = .data[[sgq_v]]
    ) |>
    ungroup() |>
    filter(is.finite(lag_nl), is.finite(frag), is.finite(ln_aid),
           is.finite(lag_log_pop), is.finite(mean_nl))
}

split_high <- function(d) {
  cut <- quantile(d$sgq, CAP_QUANTILE, na.rm = TRUE)
  list(high = filter(d, sgq > cut), low = filter(d, sgq <= cut), cut = cut)
}

# PPML conditional-mean level model (the headline estimator)
ppml <- function(d, fe) {
  fixest::fepois(
    as.formula(paste0("mean_nl ~ frag + ln_aid + lag_log_pop + log_lag_nl | ", fe)),
    cluster = ~GID_0, data = d
  )
}

a1 <- prep_annual(1)
a2 <- prep_annual(2)
cap1 <- split_high(a1); cap2 <- split_high(a2)

# =============================================================================
# TABLE 1 — primary PPML results
# =============================================================================
m_a1_cfe  <- ppml(a1, "GID_0")              # country FE (between-region)
m_a1_twfe <- ppml(a1, "GID_1 + year")       # region + year FE (headline)
m_a2_twfe <- ppml(a2, "GID_2 + year")       # admin2 region + year FE

etable(m_a1_cfe, m_a1_twfe, m_a2_twfe,
       dict = DICT,
       headers = list("Sample" = c("Admin1", "Admin1", "Admin2"),
                      "FE" = c("Country", "Region+Year", "Region+Year")),
       fitstat = ~ n + pr2,
       tex = TRUE, replace = TRUE, style.tex = style.tex("aer"),
       title = "Donor fragmentation and nightlights: PPML (annual panel).",
       label = "tab:annual_ppml_main",
       file = file.path(TAB_DIR, "annual_ppml_main.tex"))

# =============================================================================
# TABLE 2 — capacity heterogeneity (PPML, region + year FE)
# =============================================================================
m_a1_hi <- ppml(cap1$high, "GID_1 + year")
m_a1_lo <- ppml(cap1$low,  "GID_1 + year")
m_a2_hi <- ppml(cap2$high, "GID_2 + year")
m_a2_lo <- ppml(cap2$low,  "GID_2 + year")

etable(m_a1_hi, m_a1_lo, m_a2_hi, m_a2_lo,
       dict = DICT,
       headers = list("Sample" = c("Admin1", "Admin1", "Admin2", "Admin2"),
                      "Capacity" = c("High", "Low", "High", "Low")),
       fitstat = ~ n + pr2,
       tex = TRUE, replace = TRUE, style.tex = style.tex("aer"),
       title = "Heterogeneity by state capacity: PPML (annual panel, region + year FE).",
       label = "tab:annual_ppml_cap",
       file = file.path(TAB_DIR, "annual_ppml_capacity.tex"))

# =============================================================================
# TABLE 3 — c-sensitivity robustness (Admin1, region + year FE)
#   log(y+c) growth coef is unstable in c; IHS likewise; PPML is the clean target
# =============================================================================
make_logc <- function(d, cc) {
  d |>
    group_by(GID_1) |>
    mutate(y = (log(mean_nl + cc) - log(lag(mean_nl, 1L) + cc)) * 100) |>
    ungroup()
}
fit_logc <- function(cc) {
  d <- make_logc(a1, cc) |> filter(is.finite(y))
  feols(y ~ frag + ln_aid + lag_log_pop + log_lag_nl | GID_1 + year,
        cluster = ~GID_0, data = d)
}
m_c001 <- fit_logc(0.01)
m_c01  <- fit_logc(0.1)
m_c1   <- fit_logc(1)
# IHS (asinh) level
m_ihs  <- feols(asinh(mean_nl) ~ frag + ln_aid + lag_log_pop + log_lag_nl | GID_1 + year,
                cluster = ~GID_0, data = a1)

etable(m_c001, m_c01, m_c1, m_ihs, m_a1_twfe,
       dict = c(DICT, y = "$\\Delta\\log(\\mathrm{NL}+c)$"),
       keep = "Fragmentation",
       headers = c("log, c=.01", "log, c=.1", "log, c=1", "IHS level", "PPML level"),
       fitstat = ~ n,
       tex = TRUE, replace = TRUE, style.tex = style.tex("aer"),
       title = "Sensitivity to the log additive constant $c$ vs. PPML (Admin1, region + year FE).",
       label = "tab:annual_csens",
       file = file.path(TAB_DIR, "annual_csens.tex"))

# =============================================================================
# Console summary
# =============================================================================
cat("\n========================= PRIMARY (PPML) =============================\n")
etable(m_a1_cfe, m_a1_twfe, m_a2_twfe, dict = DICT, fitstat = ~ n + pr2,
       headers = c("A1 country FE", "A1 region+year", "A2 region+year"))
cat("\n===================== CAPACITY HETEROGENEITY =========================\n")
etable(m_a1_hi, m_a1_lo, m_a2_hi, m_a2_lo, dict = DICT, fitstat = ~ n + pr2,
       headers = c("A1 high", "A1 low", "A2 high", "A2 low"))
cat("\n===================== c-SENSITIVITY (frag) ===========================\n")
csens <- tibble(
  spec = c("log c=0.01", "log c=0.1", "log c=1", "IHS", "PPML"),
  frag = c(coef(m_c001)["frag"], coef(m_c01)["frag"], coef(m_c1)["frag"],
           coef(m_ihs)["frag"], coef(m_a1_twfe)["frag"]),
  se   = c(se(m_c001)["frag"], se(m_c01)["frag"], se(m_c1)["frag"],
           se(m_ihs)["frag"], se(m_a1_twfe)["frag"]),
  p    = c(pvalue(m_c001)["frag"], pvalue(m_c01)["frag"], pvalue(m_c1)["frag"],
           pvalue(m_ihs)["frag"], pvalue(m_a1_twfe)["frag"])
)
print(csens)
cat("\nTables written to 03_output/tabs/: annual_ppml_main.tex, annual_ppml_capacity.tex, annual_csens.tex\n")
cat("DONE.\n")
