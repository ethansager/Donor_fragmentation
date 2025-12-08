# Regression Utility Functions
# Functions for panel regression analysis

#' Add capacity grouping and nl_growth to panel data
#'
#' @param data Panel data frame
#' @param admin_id Column name for administrative ID (e.g., "GID_1", "GID_2")
#' @param capacity_var Column name for capacity variable (e.g., "mean_sgq_admin1")
#' @param log_offset Small positive constant to add before taking log to avoid log(0) (default: 0.01)
#' @return Data frame with added threshold, high_capacity, and nl_growth columns
add_capacity_and_growth <- function(data, admin_id, capacity_var, log_offset = 0.01) {
  require(tidyverse)
  
  data %>%
    mutate(
      threshold = mean(.data[[capacity_var]], na.rm = TRUE),
      high_capacity = ifelse(.data[[capacity_var]] > threshold, 1, 0)
    ) %>%
    arrange(.data[[admin_id]], paymentyear) %>%
    group_by(.data[[admin_id]]) %>%
    mutate(
      lag_mean_nl = dplyr::lag(mean_nl),
      nl_growth = as.numeric(case_when(
        is.na(lag_mean_nl) ~ NA_real_,
        lag_mean_nl == 0 & mean_nl == 0 ~ log(mean_nl + log_offset),
        lag_mean_nl == 0 ~ log(mean_nl + log_offset),
        TRUE ~ log((mean_nl + log_offset) / (lag_mean_nl + log_offset))
      ))
    ) %>%
    ungroup()
}

#' Run capacity-stratified panel regressions
#'
#' @param data Panel data with high_capacity indicator
#' @param admin_id Column name for administrative ID
#' @param aid_var Column name for aid variable
#' @param frag_var Column name for fragmentation variable
#' @param log_offset Small positive constant to add before taking log to avoid log(0) (default: 0.01)
#' @return List with model results for high and low capacity groups
run_capacity_regressions <- function(data, admin_id, aid_var, frag_var, log_offset = 0.01) {
  require(plm)
  
  # Split data by capacity
  data_high <- data %>% filter(high_capacity == 1)
  data_low <- data %>% filter(high_capacity == 0)
  
  # Create formula with log offset to avoid log(0)
  formula_str <- paste0("log(lag(mean_nl,1)+", log_offset, ") ~ log(", aid_var, ") * ", frag_var)
  
  # Run models
  model_high <- plm(
    as.formula(formula_str),
    data = data_high,
    index = c(admin_id, "paymentyear"),
    effect = "twoways",
    model = "within"
  )
  
  model_low <- plm(
    as.formula(formula_str),
    data = data_low,
    index = c(admin_id, "paymentyear"),
    effect = "twoways",
    model = "within"
  )
  
  return(list(high = model_high, low = model_low))
}
