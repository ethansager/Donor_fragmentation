# Shared setup for regressions and QMD analyses

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  here,
  readr,
  janitor,
  sjmisc,
  plm,
  marginaleffects,
  gtsummary,
  car,
  stargazer,
  broom,
  fixest,
  did,
  interflex
)

panel_suffix <- Sys.getenv("PANEL_SUFFIX", unset = "_5year_1995_2015")
default_lag_t <- as.integer(Sys.getenv("LAG_T", unset = "1"))
default_growth_requires_full_windows <- tolower(
  Sys.getenv("GROWTH_REQUIRES_FULL_WINDOWS", unset = "true")
) %in% c("1", "true", "yes", "y")
default_window_years <- ifelse(grepl("_5year", panel_suffix, fixed = TRUE), 5L, 1L)

panel_admin1_path <- here(
  "01_panel_data",
  paste0("panel_aid_admin1_fin", panel_suffix, ".csv")
)
panel_admin2_path <- here(
  "01_panel_data",
  paste0("panel_aid_admin2_fin", panel_suffix, ".csv")
)

if (!file.exists(panel_admin1_path)) {
  stop("Missing panel data file: ", panel_admin1_path)
}
if (!file.exists(panel_admin2_path)) {
  stop("Missing panel data file: ", panel_admin2_path)
}

normalize_panel_columns <- function(data) {
  if (!("mean_nl" %in% names(data)) && ("mean" %in% names(data))) {
    data <- data %>% rename(mean_nl = mean)
  }
  if (!("sum_nl" %in% names(data)) && ("sum" %in% names(data))) {
    data <- data %>% rename(sum_nl = sum)
  }
  data
}

winsorize_growth <- function(data) {
  data <- data %>% filter(is.finite(nl_growth))
  if (nrow(data) < 20) {
    return(data)
  }

  q <- quantile(data$nl_growth, probs = c(0.05, 0.95), na.rm = TRUE)
  if (!all(is.finite(q)) || q[[1]] >= q[[2]]) {
    return(data)
  }

  data %>%
    filter(
      nl_growth > q[[1]],
      nl_growth < q[[2]]
    )
}

prepare_panel <- function(
    data,
    admin_id,
    frag_var,
    pop_var,
    donor_var,
    proj_var,
    aid_var,
    lag_t = default_lag_t,
    growth_requires_full_windows = default_growth_requires_full_windows,
    window_years = default_window_years
) {
  if (!("period_years" %in% names(data))) {
    data <- data %>% mutate(period_years = window_years)
  }

  growth_denom <- if (window_years == 5L) {
    as.numeric(window_years * lag_t)
  } else {
    as.numeric(lag_t)
  }

  panel <- data %>%
    normalize_panel_columns() %>%
    arrange(.data[[admin_id]], year) %>%
    group_by(.data[[admin_id]]) %>%
    mutate(
      lag_mean_nl = dplyr::lag(mean_nl, n = lag_t),
      lag_period_years = dplyr::lag(period_years, n = lag_t),
      nl_growth = case_when(
        is.na(lag_mean_nl) ~ NA_real_,
        TRUE ~
          ((log(mean_nl + 0.01) - log(lag_mean_nl + 0.01)) /
            growth_denom) * 100
      ),
      lag_frag = dplyr::lag(.data[[frag_var]], n = lag_t),
      lag_log_pop = dplyr::lag(.data[[pop_var]], n = lag_t),
      lag_donor_count = dplyr::lag(.data[[donor_var]], n = lag_t),
      lag_total_proj = dplyr::lag(.data[[proj_var]], n = lag_t),
      lag_total_aid = log(dplyr::lag(.data[[aid_var]], n = lag_t) + 0.01)
    ) %>%
    ungroup()

  if (isTRUE(growth_requires_full_windows)) {
    panel <- panel %>%
      filter(period_years == window_years, lag_period_years == window_years)
  }

  panel %>% winsorize_growth()
}

add_capacity_split <- function(data, sgq_var, out_var) {
  cutoff <- quantile(data[[sgq_var]], 0.75, na.rm = TRUE)
  if (!is.finite(cutoff)) {
    cutoff <- median(data[[sgq_var]], na.rm = TRUE)
  }
  if (!is.finite(cutoff)) {
    cutoff <- 0
  }

  data %>%
    mutate(
      !!out_var := if_else(.data[[sgq_var]] > cutoff, 1L, 0L)
    )
}

has_within_time_variation <- function(data, admin_level, required_vars) {
  if (length(required_vars) == 0) {
    required_vars <- character(0)
  }

  filtered <- data
  if (length(required_vars) > 0) {
    filtered <- filtered %>%
      filter(if_all(all_of(required_vars), ~ !is.na(.x) & is.finite(.x)))
  }

  counts <- filtered %>%
    distinct(.data[[admin_level]], year) %>%
    count(.data[[admin_level]], name = "n_years")

  if (nrow(counts) == 0) {
    return(FALSE)
  }

  any(counts$n_years >= 2)
}

get_fe_term <- function(data, admin_level, required_vars = character(0)) {
  if (has_within_time_variation(data, admin_level, required_vars)) {
    return(paste0("GID_0^year + ", admin_level))
  }
  "GID_0"
}

panel_aid_admin1 <- read_csv(panel_admin1_path, show_col_types = FALSE)
panel_aid_admin2 <- read_csv(panel_admin2_path, show_col_types = FALSE)

panel_aid_admin1 <- prepare_panel(
  data = panel_aid_admin1,
  admin_id = "GID_1",
  frag_var = "frag_index_admin1",
  pop_var = "ln_pop_admin1",
  donor_var = "donor_count_admin1",
  proj_var = "total_proj_admin1",
  aid_var = "total_aid_admin1"
)

panel_aid_admin2 <- prepare_panel(
  data = panel_aid_admin2,
  admin_id = "GID_2",
  frag_var = "frag_index_admin2",
  pop_var = "ln_pop_admin2",
  donor_var = "donor_count_admin2",
  proj_var = "total_proj_admin2",
  aid_var = "total_aid_admin2"
)

panel_aid_admin1 <- add_capacity_split(panel_aid_admin1, "mean_sgq_admin1", "med_sgq_admin1")
panel_aid_admin2 <- add_capacity_split(panel_aid_admin2, "mean_sgq_admin2", "med_sgq_admin2")

high_admin1 <- panel_aid_admin1 %>% filter(med_sgq_admin1 == 1)
low_admin1 <- panel_aid_admin1 %>% filter(med_sgq_admin1 == 0)
high_admin2 <- panel_aid_admin2 %>% filter(med_sgq_admin2 == 1)
low_admin2 <- panel_aid_admin2 %>% filter(med_sgq_admin2 == 0)
