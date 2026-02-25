# Governance stability diagnostics (standalone)

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  here,
  readr
)

panel_suffix <- Sys.getenv("PANEL_SUFFIX", unset = "_5year")
output_suffix <- if_else(panel_suffix == "", "_annual", panel_suffix)

read_panel_csv <- function(file_stub) {
  file_path <- here::here("01_panel_data", paste0(file_stub, panel_suffix, ".csv"))
  if (!file.exists(file_path)) {
    stop("Missing panel data file: ", file_path)
  }
  readr::read_csv(file_path, show_col_types = FALSE)
}

safe_num <- function(x, stat = c("mean", "median", "min", "max", "q25", "q75")) {
  stat <- match.arg(stat)
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }

  if (stat == "mean") {
    return(mean(x))
  }
  if (stat == "median") {
    return(median(x))
  }
  if (stat == "min") {
    return(min(x))
  }
  if (stat == "max") {
    return(max(x))
  }
  if (stat == "q25") {
    return(as.numeric(quantile(x, 0.25, na.rm = TRUE)))
  }

  as.numeric(quantile(x, 0.75, na.rm = TRUE))
}

safe_cor <- function(x, y) {
  keep <- is.finite(x) & is.finite(y)
  if (sum(keep) < 2) {
    return(NA_real_)
  }
  cor(x[keep], y[keep])
}

compute_stability_metrics <- function(data, id_var, sgq_var, source_label, small_delta = 2) {
  d <- data %>%
    select(id = all_of(id_var), year, sgq = all_of(sgq_var)) %>%
    filter(!is.na(id), !is.na(year), is.finite(sgq)) %>%
    arrange(id, year)

  if (nrow(d) == 0) {
    return(tibble(
      source = source_label,
      units = NA_integer_,
      rows = NA_integer_,
      year_min = NA_real_,
      year_max = NA_real_,
      lag_corr = NA_real_,
      transition_persistence = NA_real_,
      median_abs_change = NA_real_,
      share_small_change = NA_real_,
      slope_p25 = NA_real_,
      slope_median = NA_real_,
      slope_p75 = NA_real_,
      median_gap_years = NA_real_
    ))
  }

  cutoff <- quantile(d$sgq, probs = 0.75, na.rm = TRUE)

  lag_data <- d %>%
    group_by(id) %>%
    mutate(
      lag_sgq = dplyr::lag(sgq),
      delta = sgq - lag_sgq,
      gap = year - dplyr::lag(year),
      high = sgq > cutoff,
      high_lag = dplyr::lag(high)
    ) %>%
    ungroup()

  lag_pairs <- lag_data %>%
    filter(is.finite(lag_sgq), is.finite(delta), is.finite(gap))

  slope_data <- d %>%
    group_by(id) %>%
    filter(n() >= 3) %>%
    summarise(
      slope = coef(lm(sgq ~ year))[2],
      .groups = "drop"
    )

  tibble(
    source = source_label,
    units = n_distinct(d$id),
    rows = nrow(d),
    year_min = safe_num(d$year, "min"),
    year_max = safe_num(d$year, "max"),
    lag_corr = safe_cor(lag_pairs$sgq, lag_pairs$lag_sgq),
    transition_persistence = safe_num(as.numeric(lag_pairs$high == lag_pairs$high_lag), "mean"),
    median_abs_change = safe_num(abs(lag_pairs$delta), "median"),
    share_small_change = safe_num(as.numeric(abs(lag_pairs$delta) <= small_delta), "mean"),
    slope_p25 = safe_num(slope_data$slope, "q25"),
    slope_median = safe_num(slope_data$slope, "median"),
    slope_p75 = safe_num(slope_data$slope, "q75"),
    median_gap_years = safe_num(lag_pairs$gap, "median")
  )
}

raw_afro <- readr::read_csv(
  here::here("00_rawdata", "ab_raw", "processed", "afrobarometer_w3_w6_geomerged_new.csv"),
  show_col_types = FALSE
)

raw_obs_admin1 <- raw_afro %>%
  filter(!is.na(GID_1), !is.na(year), is.finite(sgqi)) %>%
  group_by(GID_1, year) %>%
  summarise(sgq_observed = mean(sgqi, na.rm = TRUE), .groups = "drop")

raw_obs_admin2 <- raw_afro %>%
  filter(!is.na(GID_2), !is.na(year), is.finite(sgqi)) %>%
  group_by(GID_2, year) %>%
  summarise(sgq_observed = mean(sgqi, na.rm = TRUE), .groups = "drop")

panel_aid_admin1_fin <- read_panel_csv("panel_aid_admin1_fin")
panel_aid_admin2_fin <- read_panel_csv("panel_aid_admin2_fin")

interp_admin1 <- panel_aid_admin1_fin %>%
  select(GID_1, year, mean_sgq_admin1) %>%
  distinct()

interp_admin2 <- panel_aid_admin2_fin %>%
  select(GID_2, year, mean_sgq_admin2) %>%
  distinct()

governance_stability <- bind_rows(
  compute_stability_metrics(raw_obs_admin1, "GID_1", "sgq_observed", "Observed Afro years - admin1"),
  compute_stability_metrics(raw_obs_admin2, "GID_2", "sgq_observed", "Observed Afro years - admin2"),
  compute_stability_metrics(interp_admin1, "GID_1", "mean_sgq_admin1", "Interpolated panel years - admin1"),
  compute_stability_metrics(interp_admin2, "GID_2", "mean_sgq_admin2", "Interpolated panel years - admin2")
)

out_path <- here::here("03_output", "tabs", paste0("governance_stability_metrics", output_suffix, ".csv"))
readr::write_csv(governance_stability, out_path)

print(governance_stability)
cat("\nSaved:", out_path, "\n")
