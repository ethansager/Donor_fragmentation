# 5-year bucket regression analysis (control function expansion)

Sys.setenv(PANEL_SUFFIX = "_5year")
source(here::here("02_scripts", "02_analysis", "_setup_new_regressions.R"))

output_suffix <- "_5year"

tab_path <- function(stub) {
  here("03_output", "tabs", paste0(stub, output_suffix, ".tex"))
}

harmonize_admin_vars <- function(data, admin_level) {
  if (admin_level == "GID_1") {
    return(data %>% mutate(
      total_aid = total_aid_admin1,
      frag_index = lag_frag,
      lag_log_pop = lag_log_pop,
      lag_donor_count = lag_donor_count,
      lag_total_proj = lag_total_proj,
      lag_total_aid = lag_total_aid,
      lag_hhi = lag_frag
    ))
  }

  data %>% mutate(
    total_aid = total_aid_admin2,
    frag_index = lag_frag,
    lag_log_pop = lag_log_pop,
    lag_donor_count = lag_donor_count,
    lag_total_proj = lag_total_proj,
    lag_total_aid = lag_total_aid,
    lag_hhi = lag_frag
  )
}

perform_ols_analysis <- function(data, admin_level, outcome_var, cluster_var) {
  data <- harmonize_admin_vars(data, admin_level)

  stage_2_formula <- as.formula(
    paste0(outcome_var, " ~ frag_index + lag_total_aid + lag_log_pop")
  )

  fixest::feols(stage_2_formula, cluster = cluster_var, data = data)
}

perform_fe_analysis <- function(data, admin_level, outcome_var, cluster_var) {
  data <- harmonize_admin_vars(data, admin_level)

  fe_term <- get_fe_term(
    data,
    admin_level,
    required_vars = c(outcome_var, "frag_index", "lag_total_aid", "lag_log_pop")
  )

  stage_2_formula <- as.formula(
    paste0(
      outcome_var,
      " ~ frag_index + lag_total_aid + lag_log_pop | ",
      fe_term
    )
  )

  fixest::feols(stage_2_formula, cluster = cluster_var, data = data)
}

perform_cfa_analysis <- function(
    data,
    admin_level,
    outcome_var,
    iv_var,
    cluster_var
) {
  data <- harmonize_admin_vars(data, admin_level)

  stage_1_data <- data %>%
    filter(
      !is.na(total_aid),
      !is.na(.data[[iv_var]]),
      !is.na(lag_log_pop),
      is.finite(total_aid),
      is.finite(.data[[iv_var]]),
      is.finite(lag_log_pop)
    )

  if (nrow(stage_1_data) == 0) {
    stop("No non-missing observations for CFA stage 1.")
  }

  fe_term <- get_fe_term(
    stage_1_data,
    admin_level,
    required_vars = c("total_aid", iv_var, "lag_log_pop")
  )

  stage_1_formula <- as.formula(
    paste0("total_aid ~ ", iv_var, " + lag_log_pop | ", fe_term)
  )

  stage_1 <- fixest::feols(
    stage_1_formula,
    cluster = cluster_var,
    data = stage_1_data
  )

  stage_2_data <- stage_1_data %>%
    mutate(cfa = resid(stage_1)) %>%
    filter(
      !is.na(.data[[outcome_var]]),
      !is.na(frag_index),
      !is.na(lag_total_aid),
      !is.na(lag_log_pop),
      !is.na(cfa),
      is.finite(.data[[outcome_var]]),
      is.finite(frag_index),
      is.finite(lag_total_aid),
      is.finite(lag_log_pop),
      is.finite(cfa)
    )

  if (nrow(stage_2_data) == 0) {
    stop("No non-missing observations for CFA stage 2.")
  }

  fe_term_stage_2 <- get_fe_term(
    stage_2_data,
    admin_level,
    required_vars = c(outcome_var, "frag_index", "lag_total_aid", "lag_log_pop", "cfa")
  )

  stage_2_formula <- as.formula(
    paste0(
      outcome_var,
      " ~ frag_index + lag_total_aid + lag_log_pop + cfa | ",
      fe_term_stage_2
    )
  )

  fixest::feols(stage_2_formula, cluster = cluster_var, data = stage_2_data)
}

perform_cfa_analysis_frag <- function(
    data,
    admin_level,
    outcome_var,
    iv_var,
    cluster_var
) {
  data <- harmonize_admin_vars(data, admin_level)

  stage_1_data <- data %>%
    filter(
      !is.na(total_aid),
      !is.na(.data[[iv_var]]),
      !is.na(lag_log_pop),
      is.finite(total_aid),
      is.finite(.data[[iv_var]]),
      is.finite(lag_log_pop)
    )

  if (nrow(stage_1_data) == 0) {
    stop("No non-missing observations for CFA frag stage 1.")
  }

  fe_term <- get_fe_term(
    stage_1_data,
    admin_level,
    required_vars = c("total_aid", iv_var, "lag_log_pop")
  )

  stage_1_formula <- as.formula(
    paste0("total_aid ~ ", iv_var, " + lag_log_pop | ", fe_term)
  )

  stage_1 <- fixest::feols(
    stage_1_formula,
    cluster = cluster_var,
    data = stage_1_data
  )

  stage_2_data <- stage_1_data %>%
    mutate(cfa = resid(stage_1)) %>%
    filter(
      !is.na(.data[[outcome_var]]),
      !is.na(lag_hhi),
      !is.na(lag_total_aid),
      !is.na(lag_log_pop),
      !is.na(cfa),
      is.finite(.data[[outcome_var]]),
      is.finite(lag_hhi),
      is.finite(lag_total_aid),
      is.finite(lag_log_pop),
      is.finite(cfa)
    )

  if (nrow(stage_2_data) == 0) {
    stop("No non-missing observations for CFA frag stage 2.")
  }

  fe_term_stage_2 <- get_fe_term(
    stage_2_data,
    admin_level,
    required_vars = c(outcome_var, "lag_hhi", "lag_total_aid", "lag_log_pop", "cfa")
  )

  stage_2_formula <- as.formula(
    paste0(
      outcome_var,
      " ~ lag_hhi + lag_total_aid + lag_log_pop + cfa | ",
      fe_term_stage_2
    )
  )

  fixest::feols(stage_2_formula, cluster = cluster_var, data = stage_2_data)
}

perform_cfa_analysis_interaction <- function(
    data,
    admin_level,
    outcome_var,
    iv_var,
    cluster_var
) {
  data <- harmonize_admin_vars(data, admin_level)

  stage_1_data <- data %>%
    filter(
      !is.na(total_aid),
      !is.na(.data[[iv_var]]),
      !is.na(lag_log_pop),
      is.finite(total_aid),
      is.finite(.data[[iv_var]]),
      is.finite(lag_log_pop)
    )

  if (nrow(stage_1_data) == 0) {
    stop("No non-missing observations for CFA interaction stage 1.")
  }

  fe_term <- get_fe_term(
    stage_1_data,
    admin_level,
    required_vars = c("total_aid", iv_var, "lag_log_pop")
  )

  stage_1_formula <- as.formula(
    paste0("total_aid ~ ", iv_var, " + lag_log_pop | ", fe_term)
  )

  stage_1 <- fixest::feols(
    stage_1_formula,
    cluster = cluster_var,
    data = stage_1_data
  )

  stage_2_data <- stage_1_data %>%
    mutate(cfa = resid(stage_1)) %>%
    filter(
      !is.na(.data[[outcome_var]]),
      !is.na(frag_index),
      !is.na(lag_total_aid),
      !is.na(lag_log_pop),
      !is.na(cfa),
      is.finite(.data[[outcome_var]]),
      is.finite(frag_index),
      is.finite(lag_total_aid),
      is.finite(lag_log_pop),
      is.finite(cfa)
    )

  if (nrow(stage_2_data) == 0) {
    stop("No non-missing observations for CFA interaction stage 2.")
  }

  fe_term_stage_2 <- get_fe_term(
    stage_2_data,
    admin_level,
    required_vars = c(outcome_var, "frag_index", "lag_total_aid", "lag_log_pop", "cfa")
  )

  stage_2_formula <- as.formula(
    paste0(
      outcome_var,
      " ~ frag_index * lag_total_aid + lag_log_pop + cfa | ",
      fe_term_stage_2
    )
  )

  fixest::feols(stage_2_formula, cluster = cluster_var, data = stage_2_data)
}

#------------------------------------------------------------------------------#
# OLS table
#------------------------------------------------------------------------------#

high_1_ols <- perform_ols_analysis(high_admin1, "GID_1", "nl_growth", "GID_0")
low_1_ols <- perform_ols_analysis(low_admin1, "GID_1", "nl_growth", "GID_0")
high_2_ols <- perform_ols_analysis(high_admin2, "GID_2", "nl_growth", "GID_0")
low_2_ols <- perform_ols_analysis(low_admin2, "GID_2", "nl_growth", "GID_0")

etable(
  high_1_ols,
  low_1_ols,
  high_2_ols,
  low_2_ols,
  headers = c("High Admin1", "Low Admin1", "High Admin2", "Low Admin2"),
  dict = c(
    lag_log_pop = "Lag LN(Population)",
    lag_total_aid = "Lag LN(Total Aid)"
  ),
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  fitstat = c("n", "r2"),
  replace = TRUE,
  tex = TRUE,
  file = tab_path("table1_ols")
)

#------------------------------------------------------------------------------#
# FE-style table (falls back to country FE when unit FE are not identified)
#------------------------------------------------------------------------------#

high_1 <- perform_fe_analysis(high_admin1, "GID_1", "nl_growth", "GID_0")
low_1 <- perform_fe_analysis(low_admin1, "GID_1", "nl_growth", "GID_0")
high_2 <- perform_fe_analysis(high_admin2, "GID_2", "nl_growth", "GID_0")
low_2 <- perform_fe_analysis(low_admin2, "GID_2", "nl_growth", "GID_0")

etable(
  high_1,
  low_1,
  high_2,
  low_2,
  headers = c("High Admin1", "Low Admin1", "High Admin2", "Low Admin2"),
  dict = c(
    lag_log_pop = "Lag LN(Population)",
    lag_total_aid = "Lag LN(Total Aid)"
  ),
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  fitstat = c("n", "r2"),
  replace = TRUE,
  tex = TRUE,
  file = tab_path("table1_fe")
)

#------------------------------------------------------------------------------#
# Control function (night lights)
#------------------------------------------------------------------------------#

stage_2_high_admin1 <- perform_cfa_analysis(
  high_admin1,
  "GID_1",
  "nl_growth",
  "IV_lag",
  "GID_0"
)
stage_2_low_admin1 <- perform_cfa_analysis(
  low_admin1,
  "GID_1",
  "nl_growth",
  "IV_lag",
  "GID_0"
)
stage_2_high_admin2 <- perform_cfa_analysis(
  high_admin2,
  "GID_2",
  "nl_growth",
  "IV_lag",
  "GID_0"
)
stage_2_low_admin2 <- perform_cfa_analysis(
  low_admin2,
  "GID_2",
  "nl_growth",
  "IV_lag",
  "GID_0"
)

etable(
  stage_2_high_admin1,
  stage_2_low_admin1,
  stage_2_high_admin2,
  stage_2_low_admin2,
  headers = c("High Admin1", "Low Admin1", "High Admin2", "Low Admin2"),
  dict = c(
    lag_log_pop = "Lag LN(Population)",
    lag_total_aid = "Lag LN(Total Aid)",
    cfa = "CFA Residuals"
  ),
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  fitstat = c("n", "r2"),
  replace = TRUE,
  tex = TRUE,
  file = tab_path("table2_cfe")
)

#------------------------------------------------------------------------------#
# Control function (health)
#------------------------------------------------------------------------------#

stage_2_high_admin1_u5m <- perform_cfa_analysis(
  high_admin1,
  "GID_1",
  "u5m",
  "IV_lag",
  "GID_1"
)
stage_2_low_admin1_u5m <- perform_cfa_analysis(
  low_admin1,
  "GID_1",
  "u5m",
  "IV_lag",
  "GID_1"
)
stage_2_high_admin2_u5m <- perform_cfa_analysis(
  high_admin2,
  "GID_2",
  "u5m",
  "IV_lag",
  "GID_2"
)
stage_2_low_admin2_u5m <- perform_cfa_analysis(
  low_admin2,
  "GID_2",
  "u5m",
  "IV_lag",
  "GID_2"
)

etable(
  stage_2_high_admin1_u5m,
  stage_2_low_admin1_u5m,
  stage_2_high_admin2_u5m,
  stage_2_low_admin2_u5m,
  headers = c("High Admin1", "Low Admin1", "High Admin2", "Low Admin2"),
  dict = c(
    lag_log_pop = "Lag LN(Population)",
    lag_total_aid = "Lag LN(Total Aid)",
    cfa = "CFA Residuals"
  ),
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  fitstat = c("n", "r2"),
  replace = TRUE,
  tex = TRUE,
  file = tab_path("table2_cfe_u5m")
)

#------------------------------------------------------------------------------#
# Control function with frag indicator only
#------------------------------------------------------------------------------#

frag_high_admin1 <- perform_cfa_analysis_frag(
  high_admin1,
  "GID_1",
  "nl_growth",
  "IV_lag",
  "GID_0"
)
frag_low_admin1 <- perform_cfa_analysis_frag(
  low_admin1,
  "GID_1",
  "nl_growth",
  "IV_lag",
  "GID_0"
)
frag_high_admin2 <- perform_cfa_analysis_frag(
  high_admin2,
  "GID_2",
  "nl_growth",
  "IV_lag",
  "GID_0"
)
frag_low_admin2 <- perform_cfa_analysis_frag(
  low_admin2,
  "GID_2",
  "nl_growth",
  "IV_lag",
  "GID_0"
)

etable(
  frag_high_admin1,
  frag_low_admin1,
  frag_high_admin2,
  frag_low_admin2,
  headers = c("High Admin1", "Low Admin1", "High Admin2", "Low Admin2"),
  dict = c(
    lag_hhi = "Lag Frag Index",
    lag_log_pop = "Lag LN(Population)",
    lag_total_aid = "Lag LN(Total Aid)",
    cfa = "CFA Residuals"
  ),
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  fitstat = c("n", "r2"),
  replace = TRUE,
  tex = TRUE,
  file = tab_path("table2_cfe_frag_ind")
)

#------------------------------------------------------------------------------#
# Expanded control-function model with interaction
#------------------------------------------------------------------------------#

int_high_admin1 <- perform_cfa_analysis_interaction(
  high_admin1,
  "GID_1",
  "nl_growth",
  "IV_lag",
  "GID_0"
)
int_low_admin1 <- perform_cfa_analysis_interaction(
  low_admin1,
  "GID_1",
  "nl_growth",
  "IV_lag",
  "GID_0"
)
int_high_admin2 <- perform_cfa_analysis_interaction(
  high_admin2,
  "GID_2",
  "nl_growth",
  "IV_lag",
  "GID_0"
)
int_low_admin2 <- perform_cfa_analysis_interaction(
  low_admin2,
  "GID_2",
  "nl_growth",
  "IV_lag",
  "GID_0"
)

etable(
  int_high_admin1,
  int_low_admin1,
  int_high_admin2,
  int_low_admin2,
  headers = c("High Admin1", "Low Admin1", "High Admin2", "Low Admin2"),
  dict = c(
    `frag_index:lag_total_aid` = "Frag. x Lag LN(Total Aid)",
    lag_log_pop = "Lag LN(Population)",
    lag_total_aid = "Lag LN(Total Aid)",
    cfa = "CFA Residuals"
  ),
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  fitstat = c("n", "r2"),
  replace = TRUE,
  tex = TRUE,
  file = tab_path("table2_cfe_interaction")
)
