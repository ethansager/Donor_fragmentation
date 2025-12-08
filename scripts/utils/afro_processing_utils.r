# Afrobarometer Processing Utility Functions
# Functions for creating and processing Afrobarometer panel data

#' Recode a 4-point scale variable
#'
#' @param x Variable to recode
#' @param reverse Whether to reverse code (default: FALSE)
#' @return Recoded variable
recode_four_point <- function(x, reverse = FALSE) {
  if (reverse) {
    case_match(
      x,
      0 ~ 4,
      1 ~ 3,
      2 ~ 2,
      3 ~ 1,
      .default = NA_real_
    )
  } else {
    case_match(
      x,
      0 ~ 1,
      1 ~ 2,
      2 ~ 3,
      3 ~ 4,
      .default = NA_real_
    )
  }
}

#' Create complete panel data for admin level
#'
#' @param data Data frame with survey data
#' @param admin_id Column name for administrative ID (e.g., "GID_1", "GID_2")
#' @param year_range Numeric vector with start and end years (e.g., c(2005, 2015))
#' @return Data frame with complete panel
create_admin_panel <- function(data, admin_id, year_range = c(2005, 2015)) {
  require(tidyverse)
  
  data %>%
    group_by(.data[[admin_id]]) %>%
    complete(year = year_range[1]:year_range[2]) %>%
    arrange(.data[[admin_id]], year) %>%
    fill(wave, GID_0, afro_count, .direction = "down") %>%
    mutate(group_yr = case_when(
      year %in% 2005:2008 ~ 1,
      year %in% 2009:2011 ~ 2,
      year %in% 2012:2015 ~ 3,
    )) %>%
    ungroup()
}

#' Interpolate panel values for mean columns
#'
#' @param data Data frame with panel data
#' @param admin_id Column name for administrative ID
#' @param mean_cols Vector of column names to interpolate
#' @return Data frame with interpolated values
interpolate_panel_values <- function(data, admin_id, mean_cols) {
  require(tidyverse)
  require(zoo)
  
  data %>%
    group_by(.data[[admin_id]]) %>%
    mutate(across(
      all_of(mean_cols),
      ~ {
        # First interpolate using na.approx
        interpolated <- na.approx(., x = year, na.rm = FALSE, rule = 2)
        # Then fill remaining NAs with the single available value
        ifelse(is.na(interpolated),
               first(.[!is.na(.)]),
               interpolated
        )
      }
    )) %>%
    ungroup()
}

#' Process Afrobarometer data to create panel for a specific admin level
#'
#' @param afro_data Merged Afrobarometer data
#' @param admin_level Character string: "admin1" or "admin2"
#' @param year_range Numeric vector with start and end years
#' @return Data frame with processed panel data
process_afro_panel <- function(afro_data, admin_level = "admin1", year_range = c(2005, 2015)) {
  require(tidyverse)
  require(zoo)
  
  # Determine admin ID column based on level
  admin_id <- if (admin_level == "admin1") "GID_1" else "GID_2"
  
  # Determine mean column names based on level
  mean_sgq <- paste0("mean_sgq_", admin_level)
  mean_svc <- paste0("mean_svc_", admin_level)
  mean_fac <- paste0("mean_fac_", admin_level)
  
  # Create the panel
  panel_data <- afro_data %>%
    group_by(.data[[admin_id]], wave) %>%
    filter(!is.na(.data[[admin_id]]) & !is.na(wave)) %>%
    sf::st_drop_geometry() %>%
    mutate(
      afro_count = n(),
      !!mean_sgq := mean(sgqi, na.rm = TRUE),
      !!mean_svc := mean(ea_svc_index, na.rm = TRUE),
      !!mean_fac := mean(ea_fac_index, na.rm = TRUE)
    ) %>%
    select(
      afro_count,
      year,
      wave,
      GID_0,
      all_of(admin_id),
      starts_with("mean")
    ) %>%
    mutate(
      !!mean_svc := if_else(wave == 3, NA, .data[[mean_svc]]),
      !!mean_fac := if_else(wave == 3, NA, .data[[mean_fac]])
    ) %>%
    distinct()
  
  # Convert year to numeric
  panel_data <- panel_data %>%
    mutate(year = as.numeric(year))
  
  # Create complete panel
  panel_data <- create_admin_panel(panel_data, admin_id, year_range)
  
  # Interpolate values
  mean_cols <- c(mean_sgq, mean_svc, mean_fac)
  panel_data <- interpolate_panel_values(panel_data, admin_id, mean_cols)
  
  return(panel_data)
}
