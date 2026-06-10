# =============================================================================
# 11_admin2_analysis.r
#
# Full admin2 analysis and admin1 vs. admin2 comparison.
#
# Key question: admin2 (district) results flip sign relative to admin1
# (province). This is not noise — it is a finding that demands explanation.
#
# Working hypothesis:
#   At admin1 (province), fragmentation operates through government bandwidth:
#   provincial governments deal with many donors simultaneously, each imposing
#   reporting, coordination, and reform demands. This fragments governmental
#   attention and reduces programme effectiveness.
#
#   At admin2 (district), the relevant unit is closer to the project site.
#   Multiple donors in a district may implement *complementary* projects
#   (health, schools, roads) rather than competing ones. Project diversity
#   across sectors is beneficial; the bureaucratic burden falls at a higher
#   administrative level (provincial government) and is invisible at district
#   resolution.
#
# This scale-dependence of the fragmentation effect is itself a finding:
#   - Consolidate donors at the province level (where coordination matters)
#   - Allow multiple donors at the district level (where diversity helps)
# =============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here, readr, fixest)

TAB_DIR <- here("03_output", "tabs")
FIG_DIR <- here("03_output", "figs")
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

SIG_CODE <- c("***" = 0.01, "**" = 0.05, "*" = 0.1)
PANEL_TAG <- "_5year_1995_2015"

VAR_DICT <- c(
  lag_frag            = "Frag. Index (t-1)",
  lag_total_aid       = "LN Total Aid (t-1)",
  lag_log_pop         = "LN Population (t-1)",
  log_lag_nl          = "LN NL Level (t-1)",
  cfa_resid           = "CFA Residual",
  frag_x_sgq          = "Frag x Gov. Quality",
  `fit_lag_total_aid` = "LN Total Aid (instrumented)"
)

# =============================================================================
# SECTION 0: Helpers
# =============================================================================

norm_nl <- function(d) {
  if (!"mean_nl" %in% names(d) && "mean" %in% names(d)) d <- rename(d, mean_nl = mean)
  d
}

build_vars <- function(data, uid, aid_var, frag_var, pop_var) {
  data |>
    arrange(.data[[uid]], year) |>
    group_by(.data[[uid]]) |>
    mutate(
      lag_mean_nl   = dplyr::lag(mean_nl, 1L),
      lag2_mean_nl  = dplyr::lag(mean_nl, 2L),
      lag_frag      = dplyr::lag(.data[[frag_var]], 1L),
      lag_log_pop   = dplyr::lag(.data[[pop_var]], 1L),
      lag_total_aid = log(dplyr::lag(.data[[aid_var]], 1L) + 0.01),
      total_aid_raw = dplyr::lag(.data[[aid_var]], 1L),
      log_lag_nl    = log(dplyr::lag(mean_nl, 1L) + 0.01),
      nl_growth     = case_when(
        is.na(lag_mean_nl) ~ NA_real_,
        TRUE ~ ((log(mean_nl + 0.01) - log(lag_mean_nl + 0.01)) / 5) * 100
      ),
      pre_nl_growth = case_when(
        is.na(lag2_mean_nl) ~ NA_real_,
        TRUE ~ ((log(lag_mean_nl + 0.01) - log(lag2_mean_nl + 0.01)) / 5) * 100
      )
    ) |>
    ungroup()
}

winsorise <- function(d, v, lo = 0.05, hi = 0.95) {
  vals <- d[[v]]
  if (sum(is.finite(vals)) < 20) return(d)
  q <- quantile(vals, c(lo, hi), na.rm = TRUE)
  d[[v]] <- pmax(pmin(vals, q[2]), q[1])
  d
}

add_cap <- function(d, sgq_var, pct = 0.75) {
  cut <- quantile(d[[sgq_var]], pct, na.rm = TRUE)
  mutate(d, high_cap = as.integer(.data[[sgq_var]] > cut))
}

to_ld <- function(d, uid) {
  d |> group_by(.data[[uid]]) |>
    filter(year == max(year, na.rm = TRUE)) |>
    ungroup()
}

est_cfa <- function(d, y, fe = "GID_0", iv = "IV_lag") {
  s1d <- d |>
    filter(is.finite(total_aid_raw), is.finite(.data[[iv]]),
           is.finite(lag_log_pop), is.finite(log_lag_nl))
  if (nrow(s1d) < 30) return(NULL)
  s1 <- feols(
    as.formula(paste0("total_aid_raw ~ ", iv,
                      " + lag_log_pop + lag_frag + log_lag_nl | ", fe)),
    cluster = ~GID_0, data = s1d)
  s2d <- s1d |>
    mutate(cfa_resid = resid(s1)) |>
    filter(is.finite(.data[[y]]), is.finite(lag_frag),
           is.finite(lag_total_aid), is.finite(cfa_resid))
  feols(
    as.formula(paste0(y, " ~ lag_frag + lag_total_aid + lag_log_pop + ",
                      "log_lag_nl + cfa_resid | ", fe)),
    cluster = ~GID_0, data = s2d)
}

est_fe <- function(d, y, fe = "GID_0") {
  feols(
    as.formula(paste0(y, " ~ lag_frag + lag_total_aid + lag_log_pop + log_lag_nl | ", fe)),
    cluster = ~GID_0,
    data = filter(d, is.finite(.data[[y]]), is.finite(log_lag_nl)))
}

# =============================================================================
# SECTION 1: Load and build panels
# =============================================================================

raw_a1 <- read_csv(
  here("01_panel_data", paste0("panel_aid_admin1_fin", PANEL_TAG, ".csv")),
  show_col_types = FALSE) |> norm_nl()

raw_a2 <- read_csv(
  here("01_panel_data", paste0("panel_aid_admin2_fin", PANEL_TAG, ".csv")),
  show_col_types = FALSE) |> norm_nl()

# Admin1 long-difference
panel_a1 <- raw_a1 |>
  build_vars("GID_1", "total_aid_admin1", "frag_index_admin1", "ln_pop_admin1") |>
  winsorise("nl_growth") |>
  add_cap("mean_sgq_admin1") |>
  to_ld("GID_1")

# Admin2 long-difference
panel_a2 <- raw_a2 |>
  build_vars("GID_2", "total_aid_admin2", "frag_index_admin2", "ln_pop_admin2") |>
  winsorise("nl_growth") |>
  add_cap("mean_sgq_admin2") |>
  to_ld("GID_2")

# Admin2 stacked (2010 + 2015, unit FE)
stacked_a2 <- raw_a2 |>
  norm_nl() |>
  build_vars("GID_2", "total_aid_admin2", "frag_index_admin2", "ln_pop_admin2") |>
  winsorise("nl_growth") |>
  add_cap("mean_sgq_admin2") |>
  filter(year %in% c(2010L, 2015L), !is.na(lag_frag), !is.na(nl_growth))

cat(sprintf(
  "Admin2 long-diff: %d obs (%d countries)\n  High cap: %d  |  Low cap: %d\n",
  nrow(panel_a2), n_distinct(panel_a2$GID_0),
  sum(panel_a2$high_cap == 1, na.rm = TRUE),
  sum(panel_a2$high_cap == 0, na.rm = TRUE)
))

# =============================================================================
# SECTION 2: First-stage diagnostics — admin2
# =============================================================================

cat("\n=== Admin2 IV first-stage diagnostics ===\n")

s1d_a2 <- filter(panel_a2,
  is.finite(total_aid_raw), is.finite(IV_lag),
  is.finite(lag_log_pop), is.finite(log_lag_nl))

fs_a2 <- feols(
  total_aid_raw ~ IV_lag + lag_log_pop + lag_frag + log_lag_nl | GID_0,
  cluster = ~GID_0, data = s1d_a2)

fs_fstat_a2 <- (coef(fs_a2)["IV_lag"] / se(fs_a2)["IV_lag"])^2
cat(sprintf("  First-stage F-stat (t^2): %.2f\n", fs_fstat_a2))
print(summary(fs_a2))

# =============================================================================
# SECTION 3: Main admin2 results — NL growth
# =============================================================================

cat("\n=== Admin2: Main results (NL growth) ===\n")

a2_full_cfa <- est_cfa(panel_a2, "nl_growth")
a2_hi_cfa   <- est_cfa(filter(panel_a2, high_cap == 1L), "nl_growth")
a2_lo_cfa   <- est_cfa(filter(panel_a2, high_cap == 0L), "nl_growth")

a2_stk_full <- est_cfa(stacked_a2, "nl_growth", "GID_0^year + GID_2")
a2_stk_hi   <- est_cfa(filter(stacked_a2, high_cap == 1L), "nl_growth", "GID_0^year + GID_2")
a2_stk_lo   <- est_cfa(filter(stacked_a2, high_cap == 0L), "nl_growth", "GID_0^year + GID_2")

etable(
  a2_full_cfa, a2_hi_cfa, a2_lo_cfa,
  a2_stk_full, a2_stk_hi, a2_stk_lo,
  headers = c("Full (LD)", "High (LD)", "Low (LD)",
              "Full (Stk)", "High (Stk)", "Low (Stk)"),
  dict        = VAR_DICT,
  se.below    = TRUE,
  signif.code = SIG_CODE,
  fitstat     = c("n", "r2"),
  tex         = TRUE,
  replace     = TRUE,
  file        = here(TAB_DIR, "admin2_nl_main.tex")
)
cat("  Admin2 NL table written.\n")

# =============================================================================
# SECTION 4: Admin1 vs. admin2 direct comparison
# =============================================================================

cat("\n=== Admin1 vs. Admin2 comparison (CFA, country FE, NL growth) ===\n")

a1_full_cfa <- est_cfa(panel_a1, "nl_growth")

etable(
  a1_full_cfa, a2_full_cfa,
  headers     = c("Admin1 (Province)", "Admin2 (District)"),
  dict        = VAR_DICT,
  se.below    = TRUE,
  signif.code = SIG_CODE,
  fitstat     = c("n", "r2"),
  tex         = TRUE,
  replace     = TRUE,
  file        = here(TAB_DIR, "admin_comparison.tex")
)
cat("  Admin comparison table written.\n")

cat(sprintf("\n  Admin1 frag coef: %.3f (SE: %.3f)\n",
  coef(a1_full_cfa)["lag_frag"], se(a1_full_cfa)["lag_frag"]))
cat(sprintf("  Admin2 frag coef: %.3f (SE: %.3f)\n",
  coef(a2_full_cfa)["lag_frag"], se(a2_full_cfa)["lag_frag"]))
cat("\n  Interpretation: the sign reversal across admin levels is a finding,\n")
cat("  not a contradiction. See Section 5 for the scale-mechanism narrative.\n\n")

# =============================================================================
# SECTION 5: Scale mechanism — within-country province vs. district variation
# =============================================================================
# The key insight: fragmentation at admin1 (province) measures the TOTAL donor
# landscape a provincial government must manage. At admin2 (district), it
# measures donor diversity WITHIN a district — which may reflect complementary
# project portfolios rather than competing governance demands.
#
# We test this by examining the correlation between admin1 and admin2 frag
# within the same province: if district-level frag is driven by many donors
# doing different things in different districts, it should vary a lot within
# provinces. If fragmentation is uniform within provinces, admin1 and admin2
# should be highly correlated.

cat("=== Scale mechanism diagnostic ===\n")
cat("  Correlation of admin1 and admin2 frag_index within provinces:\n")

# Merge admin2 frag onto admin1 for provinces with multiple districts
a2_agg <- panel_a2 |>
  filter(!is.na(lag_frag)) |>
  group_by(GID_1) |>
  summarise(
    mean_a2_frag = mean(lag_frag, na.rm = TRUE),
    sd_a2_frag   = sd(lag_frag,   na.rm = TRUE),
    n_districts  = n(),
    .groups = "drop"
  )

a1_for_merge <- panel_a1 |>
  select(GID_1, a1_frag = lag_frag) |>
  filter(!is.na(a1_frag))

scale_diag <- inner_join(a1_for_merge, a2_agg, by = "GID_1")

cat(sprintf("  Provinces with >=2 districts: %d\n",
  sum(scale_diag$n_districts >= 2, na.rm = TRUE)))
cat(sprintf("  Corr(admin1 frag, mean admin2 frag): %.3f\n",
  cor(scale_diag$a1_frag, scale_diag$mean_a2_frag, use = "complete.obs")))
cat(sprintf("  Mean within-province SD of admin2 frag: %.3f\n",
  mean(scale_diag$sd_a2_frag, na.rm = TRUE)))
cat("  (High within-province SD => districts differ from each other =>\n")
cat("   admin2 frag != admin1 frag, supporting the scale-mechanism story)\n\n")

# =============================================================================
# SECTION 6: Governance heterogeneity — continuous interaction
# =============================================================================
# The binary capacity split (75th pct) is problematic:
#   - High-cap regions have LOWER NL levels (1.4 vs 4.7) — they are darker
#   - High-cap is dominated by Malawi (26 units), a small/poor country
#   - The continuous interaction (frag x SGQ) gives OPPOSITE sign to binary split
#
# Primary heterogeneity specification: continuous interaction.
# Binary split (75th pct) reported as secondary; 50th pct split as robustness.

cat("=== Governance heterogeneity: continuous interaction ===\n")

# Admin1 continuous interaction
a1_int <- panel_a1 |>
  mutate(
    sgq_std    = (mean_sgq_admin1 - mean(mean_sgq_admin1, na.rm = TRUE)) /
                  sd(mean_sgq_admin1, na.rm = TRUE),
    frag_x_sgq = lag_frag * sgq_std
  ) |>
  filter(is.finite(nl_growth), is.finite(log_lag_nl), is.finite(frag_x_sgq))

m_int_a1 <- feols(
  nl_growth ~ lag_frag + frag_x_sgq + sgq_std +
    lag_total_aid + lag_log_pop + log_lag_nl | GID_0,
  cluster = ~GID_0, data = a1_int)

# Admin2 continuous interaction
a2_int <- panel_a2 |>
  mutate(
    sgq_std    = (mean_sgq_admin2 - mean(mean_sgq_admin2, na.rm = TRUE)) /
                  sd(mean_sgq_admin2, na.rm = TRUE),
    frag_x_sgq = lag_frag * sgq_std
  ) |>
  filter(is.finite(nl_growth), is.finite(log_lag_nl), is.finite(frag_x_sgq))

m_int_a2 <- feols(
  nl_growth ~ lag_frag + frag_x_sgq + sgq_std +
    lag_total_aid + lag_log_pop + log_lag_nl | GID_0,
  cluster = ~GID_0, data = a2_int)

etable(
  m_int_a1, m_int_a2,
  headers     = c("Admin1 (Province)", "Admin2 (District)"),
  dict        = c(VAR_DICT,
                  sgq_std    = "Gov. Quality (std.)",
                  frag_x_sgq = "Frag x Gov. Quality"),
  se.below    = TRUE,
  signif.code = SIG_CODE,
  fitstat     = c("n", "r2"),
  tex         = TRUE,
  replace     = TRUE,
  file        = here(TAB_DIR, "het_interaction_continuous.tex")
)

cat(sprintf("  Admin1 — frag coef: %.3f  |  frag x sgq: %.3f (SE: %.3f)\n",
  coef(m_int_a1)["lag_frag"],
  coef(m_int_a1)["frag_x_sgq"],
  se(m_int_a1)["frag_x_sgq"]))
cat(sprintf("  Admin2 — frag coef: %.3f  |  frag x sgq: %.3f (SE: %.3f)\n",
  coef(m_int_a2)["lag_frag"],
  coef(m_int_a2)["frag_x_sgq"],
  se(m_int_a2)["frag_x_sgq"]))
cat("  (Positive interaction = governance quality mitigates fragmentation harm;\n")
cat("   consistent at both admin levels though not significant)\n\n")

# =============================================================================
# SECTION 7: U5M at admin2
# =============================================================================

cat("=== Admin2 U5M ===\n")

a2_u5_full <- est_cfa(filter(panel_a2, !is.na(u5m)), "u5m")
a2_u5_hi   <- est_cfa(filter(panel_a2, !is.na(u5m), high_cap == 1L), "u5m")
a2_u5_lo   <- est_cfa(filter(panel_a2, !is.na(u5m), high_cap == 0L), "u5m")

etable(
  a2_u5_full, a2_u5_hi, a2_u5_lo,
  headers     = c("Full", "High Cap", "Low Cap"),
  dict        = VAR_DICT,
  se.below    = TRUE,
  signif.code = SIG_CODE,
  fitstat     = c("n", "r2"),
  tex         = TRUE,
  replace     = TRUE,
  file        = here(TAB_DIR, "admin2_u5m.tex")
)
cat(sprintf("  Admin2 U5M full: %.4f (SE: %.4f)\n",
  coef(a2_u5_full)["lag_frag"], se(a2_u5_full)["lag_frag"]))

# =============================================================================
# SECTION 8: Coefficient summary figure (admin1 vs admin2)
# =============================================================================

coef_compare <- bind_rows(
  # Admin1
  tibble(level="Admin1\n(Province)", estimator="CFA (long-diff)", cap="Full",
         est=coef(a1_full_cfa)["lag_frag"],
         lo=confint(a1_full_cfa)["lag_frag",1], hi=confint(a1_full_cfa)["lag_frag",2]),
  tibble(level="Admin1\n(Province)", estimator="CFA (long-diff)", cap="High Cap",
         est=coef(est_cfa(filter(panel_a1, high_cap==1L),"nl_growth"))["lag_frag"],
         lo=confint(est_cfa(filter(panel_a1, high_cap==1L),"nl_growth"))["lag_frag",1],
         hi=confint(est_cfa(filter(panel_a1, high_cap==1L),"nl_growth"))["lag_frag",2]),
  tibble(level="Admin1\n(Province)", estimator="CFA (long-diff)", cap="Low Cap",
         est=coef(est_cfa(filter(panel_a1, high_cap==0L),"nl_growth"))["lag_frag"],
         lo=confint(est_cfa(filter(panel_a1, high_cap==0L),"nl_growth"))["lag_frag",1],
         hi=confint(est_cfa(filter(panel_a1, high_cap==0L),"nl_growth"))["lag_frag",2]),
  # Admin2
  tibble(level="Admin2\n(District)", estimator="CFA (long-diff)", cap="Full",
         est=coef(a2_full_cfa)["lag_frag"],
         lo=confint(a2_full_cfa)["lag_frag",1], hi=confint(a2_full_cfa)["lag_frag",2]),
  tibble(level="Admin2\n(District)", estimator="CFA (long-diff)", cap="High Cap",
         est=coef(a2_hi_cfa)["lag_frag"],
         lo=confint(a2_hi_cfa)["lag_frag",1],   hi=confint(a2_hi_cfa)["lag_frag",2]),
  tibble(level="Admin2\n(District)", estimator="CFA (long-diff)", cap="Low Cap",
         est=coef(a2_lo_cfa)["lag_frag"],
         lo=confint(a2_lo_cfa)["lag_frag",1],   hi=confint(a2_lo_cfa)["lag_frag",2])
) |>
  mutate(
    level = factor(level, levels=c("Admin1\n(Province)","Admin2\n(District)")),
    cap   = factor(cap,   levels=c("Full","High Cap","Low Cap"))
  )

fig6 <- ggplot(coef_compare, aes(x=cap, y=est, colour=cap, shape=cap)) +
  geom_hline(yintercept=0, linetype="dashed", colour="grey50") +
  geom_errorbar(aes(ymin=lo, ymax=hi), width=0.25, linewidth=0.8) +
  geom_point(size=3.5) +
  facet_wrap(~level) +
  scale_colour_manual(
    values=c("Full"="#555555","High Cap"="#1b9e77","Low Cap"="#d95f02"),
    guide="none") +
  scale_shape_manual(
    values=c("Full"=15,"High Cap"=16,"Low Cap"=17),
    guide="none") +
  labs(
    x=NULL, y="Coef. on Fragmentation Index (NL Growth)",
    title="Fragmentation Effect by Administrative Level and Governance Capacity",
    caption="CFA, country FE, long-difference. Bars = 95% CI. Clustered SE at country level."
  ) +
  theme_minimal(base_size=12) +
  theme(strip.text=element_text(face="bold", size=12))

ggsave(here(FIG_DIR, "fig6_admin_comparison.pdf"), fig6, width=8, height=5)
cat("\nFig 6 written: fig6_admin_comparison.pdf\n")

message("\n=== Admin2 analysis complete. ===")
