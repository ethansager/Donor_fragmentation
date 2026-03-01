# =============================================================================
# 10_sector_analysis.r
#
# Sector-specific fragmentation analysis
#
# Question: does the effect of donor fragmentation on economic growth
# differ by aid sector (health, education, infrastructure, governance,
# humanitarian)?
#
# Each sector file contains frag_index_admin1 and total_aid_admin1
# computed WITHIN that sector — so frag measures how concentrated donors
# are for that sector specifically, not total aid.
#
# Identification: OLS + country FE only. The shift-share IV (IV_lag) was
# constructed for total aid and is not a valid instrument for sector-
# specific aid flows. Treat sector results as descriptive: conditional on
# country and initial NL level, in which sectors does within-sector
# fragmentation predict worse growth outcomes?
#
# The main CFA result (script 09) establishes causality for total aid;
# sector breakdown illuminates mechanism.
# =============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here, readr, fixest, ggplot2)

# =============================================================================
# SECTION 0: Config
# =============================================================================

PANEL_TAG         <- "_5year_1995_2015"
WINS_LO           <- 0.05; WINS_HI <- 0.95
CAPACITY_PCT      <- 0.75

TAB_DIR <- here("03_output", "tabs")
FIG_DIR <- here("03_output", "figs")
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

SIG_CODE <- c("***" = 0.01, "**" = 0.05, "*" = 0.1)

SECTOR_FILES <- list(
  health      = here("01_panel_data", paste0("panel_aid_admin1_health",     PANEL_TAG, ".csv")),
  education   = here("01_panel_data", paste0("panel_aid_admin1_educ",       PANEL_TAG, ".csv")),
  infra       = here("01_panel_data", paste0("panel_aid_admin1_infra",      PANEL_TAG, ".csv")),
  gov_support = here("01_panel_data", paste0("panel_aid_admin1_gov_support",PANEL_TAG, ".csv")),
  humanitarian= here("01_panel_data", paste0("panel_aid_admin1_humanitarian",PANEL_TAG, ".csv"))
)

# Sector labels for tables and plots
SECTOR_LABELS <- c(
  health       = "Health",
  education    = "Education",
  infra        = "Infrastructure",
  gov_support  = "Governance Support",
  humanitarian = "Humanitarian"
)

# =============================================================================
# SECTION 1: Data prep helpers (same as 09_fresh_analysis.r)
# =============================================================================

norm_nl_cols <- function(d) {
  if (!"mean_nl" %in% names(d) && "mean" %in% names(d)) d <- rename(d, mean_nl = mean)
  d
}

build_sector_vars <- function(data) {
  # Uses sector-specific frag_index and total_aid from the sector file
  data |>
    arrange(GID_1, year) |>
    group_by(GID_1) |>
    mutate(
      lag_mean_nl   = dplyr::lag(mean_nl, 1L),
      lag_frag      = dplyr::lag(frag_index_admin1, 1L),
      lag_log_pop   = dplyr::lag(ln_pop_admin1, 1L),
      lag_total_aid = log(dplyr::lag(total_aid_admin1, 1L) + 0.01),
      log_lag_nl    = log(dplyr::lag(mean_nl, 1L) + 0.01),
      nl_growth     = case_when(
        is.na(lag_mean_nl) ~ NA_real_,
        TRUE ~ ((log(mean_nl + 0.01) - log(lag_mean_nl + 0.01)) / 5) * 100
      )
    ) |>
    ungroup()
}

winsorise <- function(d, v, lo = WINS_LO, hi = WINS_HI) {
  vals <- d[[v]]
  if (sum(is.finite(vals)) < 20) return(d)
  q <- quantile(vals, c(lo, hi), na.rm = TRUE)
  d[[v]] <- pmax(pmin(vals, q[2]), q[1])
  d
}

add_capacity <- function(d) {
  cut <- quantile(d$mean_sgq_admin1, CAPACITY_PCT, na.rm = TRUE)
  mutate(d, high_cap = as.integer(mean_sgq_admin1 > cut))
}

# Long-difference: keep terminal year per unit
to_long_diff <- function(d) {
  d |>
    group_by(GID_1) |>
    filter(year == max(year, na.rm = TRUE), !is.na(lag_frag)) |>
    ungroup()
}

# =============================================================================
# SECTION 2: Estimation function (OLS + country FE; no IV for sector files)
# =============================================================================

est_sector_fe <- function(data, outcome, cluster = ~GID_0) {
  d <- filter(data, is.finite(.data[[outcome]]), is.finite(log_lag_nl),
              is.finite(lag_frag), is.finite(lag_total_aid), is.finite(lag_log_pop))
  if (nrow(d) < 30 || n_distinct(d$GID_0) < 5) return(NULL)
  fml <- as.formula(paste0(
    outcome, " ~ lag_frag + lag_total_aid + lag_log_pop + log_lag_nl | GID_0"
  ))
  tryCatch(
    feols(fml, cluster = cluster, data = d),
    error = function(e) { warning(e$message); NULL }
  )
}

# =============================================================================
# SECTION 3: Run all sectors
# =============================================================================

cat("\n=== SECTOR ANALYSIS: NL growth by aid sector ===\n")
cat("(OLS + country FE; no IV — interpret as descriptive)\n\n")

sector_results <- lapply(names(SECTOR_FILES), function(s) {
  path <- SECTOR_FILES[[s]]
  if (!file.exists(path)) {
    cat(sprintf("  [SKIP] %s — file not found\n", s))
    return(NULL)
  }

  raw <- read_csv(path, show_col_types = FALSE) |> norm_nl_cols()

  panel <- raw |>
    build_sector_vars() |>
    winsorise("nl_growth") |>
    add_capacity() |>
    to_long_diff()

  hi <- filter(panel, high_cap == 1L)
  lo <- filter(panel, high_cap == 0L)

  m_full <- est_sector_fe(panel, "nl_growth")
  m_hi   <- est_sector_fe(hi,    "nl_growth")
  m_lo   <- est_sector_fe(lo,    "nl_growth")
  m_u5m  <- est_sector_fe(filter(panel, !is.na(u5m)), "u5m")

  n_full <- if (!is.null(m_full)) nobs(m_full) else NA_integer_
  n_hi   <- nrow(hi)
  n_lo   <- nrow(lo)

  cat(sprintf("Sector: %-20s  N=%d (Hi=%d, Lo=%d)\n",
              SECTOR_LABELS[[s]], n_full, n_hi, n_lo))

  if (!is.null(m_full)) {
    b <- coef(m_full)["lag_frag"]
    s_val <- se(m_full)["lag_frag"]
    p <- 2 * pt(-abs(b / s_val), df = n_full - 1)
    stars <- ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))
    cat(sprintf("  Full:  coef = %+.3f  SE = %.3f%s\n", b, s_val, stars))
  }
  if (!is.null(m_hi)) {
    b <- coef(m_hi)["lag_frag"]
    s_val <- se(m_hi)["lag_frag"]
    cat(sprintf("  Hi:    coef = %+.3f  SE = %.3f\n", b, s_val))
  }
  if (!is.null(m_lo)) {
    b <- coef(m_lo)["lag_frag"]
    s_val <- se(m_lo)["lag_frag"]
    cat(sprintf("  Lo:    coef = %+.3f  SE = %.3f\n", b, s_val))
  }
  cat("\n")

  list(
    sector    = s,
    label     = SECTOR_LABELS[[s]],
    full      = m_full,
    hi        = m_hi,
    lo        = m_lo,
    u5m       = m_u5m,
    n_full    = n_full
  )
})
names(sector_results) <- names(SECTOR_FILES)

# Remove sectors that failed
sector_results <- Filter(Negate(is.null), sector_results)

# =============================================================================
# SECTION 4: Combined sector table
# =============================================================================

VAR_DICT <- c(
  lag_frag      = "Frag. Index (t-1)",
  lag_total_aid = "LN Sector Aid (t-1)",
  lag_log_pop   = "LN Pop (t-1)",
  log_lag_nl    = "LN NL Level (t-1)"
)

full_models <- lapply(sector_results, `[[`, "full")
full_models <- Filter(Negate(is.null), full_models)

if (length(full_models) >= 2) {
  etable(
    .list       = full_models,
    headers     = SECTOR_LABELS[names(full_models)],
    dict        = VAR_DICT,
    se.below    = TRUE,
    signif.code = SIG_CODE,
    fitstat     = c("n", "r2"),
    tex         = TRUE,
    replace     = TRUE,
    file        = here(TAB_DIR, "sector_nl_admin1.tex")
  )
  cat("Sector NL growth table written: sector_nl_admin1.tex\n")
}

# U5M sector table
u5m_models <- lapply(sector_results, `[[`, "u5m")
u5m_models <- Filter(Negate(is.null), u5m_models)

if (length(u5m_models) >= 2) {
  etable(
    .list       = u5m_models,
    headers     = SECTOR_LABELS[names(u5m_models)],
    dict        = VAR_DICT,
    se.below    = TRUE,
    signif.code = SIG_CODE,
    fitstat     = c("n", "r2"),
    tex         = TRUE,
    replace     = TRUE,
    file        = here(TAB_DIR, "sector_u5m_admin1.tex")
  )
  cat("Sector U5M table written: sector_u5m_admin1.tex\n")
}

# =============================================================================
# SECTION 5: Heterogeneity table — High vs. Low capacity by sector
# =============================================================================

cat("\n=== Sector heterogeneity: High vs. Low capacity ===\n")

hi_models <- lapply(sector_results, `[[`, "hi")
lo_models <- lapply(sector_results, `[[`, "lo")
hi_models <- Filter(Negate(is.null), hi_models)
lo_models <- Filter(Negate(is.null), lo_models)

common_sectors <- intersect(names(hi_models), names(lo_models))

if (length(common_sectors) >= 2) {
  # Interleave high/low for readability: Health-Hi, Health-Lo, Educ-Hi, Educ-Lo ...
  interleaved <- unlist(
    lapply(common_sectors, function(s) list(hi = hi_models[[s]], lo = lo_models[[s]])),
    recursive = FALSE
  )
  interleaved_headers <- unlist(
    lapply(common_sectors, function(s)
      c(paste0(SECTOR_LABELS[[s]], " (Hi)"), paste0(SECTOR_LABELS[[s]], " (Lo)")))
  )

  etable(
    .list       = interleaved,
    headers     = interleaved_headers,
    dict        = VAR_DICT,
    se.below    = TRUE,
    signif.code = SIG_CODE,
    fitstat     = c("n", "r2"),
    tex         = TRUE,
    replace     = TRUE,
    file        = here(TAB_DIR, "sector_het_admin1.tex")
  )
  cat("Sector heterogeneity table written: sector_het_admin1.tex\n")
}

# =============================================================================
# SECTION 6: Coefficient plot across sectors
# =============================================================================

coef_rows <- lapply(names(sector_results), function(s) {
  res <- sector_results[[s]]
  extract_row <- function(m, cap_label) {
    if (is.null(m)) return(NULL)
    b <- coef(m)["lag_frag"]
    s_se <- se(m)["lag_frag"]
    if (is.na(b) || is.na(s_se)) return(NULL)
    tibble(
      sector    = res$label,
      capacity  = cap_label,
      est       = b,
      lo95      = b - 1.96 * s_se,
      hi95      = b + 1.96 * s_se,
      lo90      = b - 1.645 * s_se,
      hi90      = b + 1.645 * s_se,
      n         = nobs(m)
    )
  }
  bind_rows(
    extract_row(res$full, "Full"),
    extract_row(res$hi,   "High Cap"),
    extract_row(res$lo,   "Low Cap")
  )
})

coef_df <- bind_rows(coef_rows) |>
  mutate(
    sector   = factor(sector, levels = rev(unname(SECTOR_LABELS))),
    capacity = factor(capacity, levels = c("Full", "High Cap", "Low Cap"))
  )

fig_sector <- ggplot(coef_df, aes(x = est, y = sector,
                                   colour = capacity, shape = capacity)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_linerange(aes(xmin = lo95, xmax = hi95), linewidth = 0.6,
                 position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = lo90, xmax = hi90), linewidth = 1.2,
                 position = position_dodge(width = 0.5)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  scale_colour_manual(
    values = c("Full" = "#555555", "High Cap" = "#1b9e77", "Low Cap" = "#d95f02")
  ) +
  scale_shape_manual(
    values = c("Full" = 15, "High Cap" = 16, "Low Cap" = 17)
  ) +
  labs(
    x      = "Coef. on Frag. Index — NL Growth (country FE, long-difference)",
    y      = NULL,
    colour = NULL, shape  = NULL,
    title  = "Effect of Within-Sector Donor Fragmentation on NL Growth",
    caption = "Thick bars = 90% CI; thin bars = 95% CI. Clustered SE at country level."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(here(FIG_DIR, "fig5_sector_coefs.pdf"), fig_sector,
       width = 7.5, height = 5)
cat("\nFig 5 written: fig5_sector_coefs.pdf\n")

# =============================================================================
# SECTION 7: Summary table — fragmentation variation by sector
# =============================================================================
# How much within-sample variation does each sector's frag_index have?
# Sectors with little variation can't identify an effect even if one exists.

cat("\n=== Fragmentation variation by sector ===\n")
cat(sprintf("  %-20s  %5s  %5s  %5s  %5s  %5s\n",
            "Sector", "N", "Mean", "SD", "p25", "p75"))

for (s in names(SECTOR_FILES)) {
  path <- SECTOR_FILES[[s]]
  if (!file.exists(path)) next
  raw <- read_csv(path, show_col_types = FALSE) |>
    norm_nl_cols() |>
    build_sector_vars() |>
    to_long_diff()
  v <- raw$lag_frag
  cat(sprintf("  %-20s  %5d  %5.3f  %5.3f  %5.3f  %5.3f\n",
              SECTOR_LABELS[[s]],
              sum(!is.na(v)), mean(v, na.rm=TRUE), sd(v, na.rm=TRUE),
              quantile(v, .25, na.rm=TRUE), quantile(v, .75, na.rm=TRUE)))
}

message("\n=== Sector analysis complete. ===")
