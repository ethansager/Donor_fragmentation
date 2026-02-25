# Shared time-window helpers for five-year panel designs.

get_5year_panel_config_2000_2020 <- function() {
  list(
    start_year = 2000L,
    end_year = 2020L,
    panel_tag = "_5year_2000_2020",
    period_keys = c(2000L, 2005L, 2010L, 2015L, 2020L)
  )
}

required_periods_5year_2000_2020 <- function() {
  get_5year_panel_config_2000_2020()$period_keys
}

map_to_5year_bucket_2000_2020 <- function(year) {
  dplyr::case_when(
    year %in% 2000:2004 ~ 2000L,
    year %in% 2005:2009 ~ 2005L,
    year %in% 2010:2014 ~ 2010L,
    year %in% 2015:2019 ~ 2015L,
    year == 2020 ~ 2020L,
    TRUE ~ NA_integer_
  )
}

bucket_metadata <- function(period) {
  period <- as.integer(period)

  meta <- tibble::tibble(
    year = period,
    period_start = dplyr::case_when(
      period == 1995L ~ 1995L,
      period == 2000L ~ 2000L,
      period == 2005L ~ 2005L,
      period == 2010L ~ 2010L,
      period == 2015L ~ 2015L,
      period == 2020L ~ 2020L,
      TRUE ~ NA_integer_
    ),
    period_end = dplyr::case_when(
      period == 1995L ~ 1999L,
      period == 2000L ~ 2004L,
      period == 2005L ~ 2009L,
      period == 2010L ~ 2014L,
      period == 2015L ~ 2019L,
      period == 2020L ~ 2020L,
      TRUE ~ NA_integer_
    )
  )

  meta$period_years <- meta$period_end - meta$period_start + 1L
  meta$is_singleton <- meta$period_years == 1L

  meta
}

print_year_support <- function(label, years) {
  years <- sort(unique(as.integer(years)))
  years <- years[is.finite(years)]

  if (length(years) == 0) {
    message(label, ": no years found.")
    return(invisible(years))
  }

  message(
    label,
    ": min=",
    min(years),
    ", max=",
    max(years),
    ", n=",
    length(years),
    ", years={",
    paste(years, collapse = ","),
    "}"
  )

  invisible(years)
}

assert_year_coverage <- function(label, years, required_years) {
  years <- sort(unique(as.integer(years)))
  years <- years[is.finite(years)]
  required_years <- sort(unique(as.integer(required_years)))
  required_years <- required_years[is.finite(required_years)]

  missing_years <- setdiff(required_years, years)
  if (length(missing_years) > 0) {
    stop(
      label,
      " is missing required years: ",
      paste(missing_years, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# 1995-2015 five-year panel helpers
# -----------------------------------------------------------------------------

get_5year_panel_config_1995_2015 <- function() {
  list(
    start_year  = 1995L,
    end_year    = 2019L,
    panel_tag   = "_5year_1995_2015",
    period_keys = c(1995L, 2000L, 2005L, 2010L, 2015L)
  )
}

required_periods_5year_1995_2015 <- function() {
  get_5year_panel_config_1995_2015()$period_keys
}

map_to_5year_bucket_1995_2015 <- function(year) {
  dplyr::case_when(
    year %in% 1995:1999 ~ 1995L,
    year %in% 2000:2004 ~ 2000L,
    year %in% 2005:2009 ~ 2005L,
    year %in% 2010:2014 ~ 2010L,
    year %in% 2015:2019 ~ 2015L,
    TRUE ~ NA_integer_
  )
}
