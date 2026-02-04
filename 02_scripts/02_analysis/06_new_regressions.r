### DiD ####
# Regression Analysis
source(here::here("02_scripts", "02_analysis", "_setup_new_regressions.R"))

panel_aid_admin1 %>%
  group_by(med_sgq_admin1) %>%
  filter(!is.na(nl_growth) & is.finite(nl_growth)) %>%
  summarise(
    mean.growth = mean(nl_growth, na.rm = TRUE),
    sd_growth = sd(nl_growth, na.rm = TRUE),
    n = n()
  )

summary(high_admin1)

# Update regressions to use lagged variables

#### Table 1 Panel B
perform_ols_analysis <- function(data, admin_level, outcome_var, cluster_var) {
  # Harmonize variables by admin level
  if (admin_level == "GID_1") {
    data <- data %>%
      dplyr::mutate(
        total_aid = total_aid_admin1,
        frag_index = lag_hhi_admin1,
        lag_log_pop = lag_pop_admin1,
        lag_donor_count = lag_donor_count_admin1,
        lag_total_proj = lag_total_proj_admin1,
        lag_total_aid = lag_total_aid_admin1
      )
  } else if (admin_level == "GID_2") {
    data <- data %>%
      dplyr::mutate(
        total_aid = total_aid_admin2,
        frag_index = lag_hhi_admin2,
        lag_log_pop = lag_pop_admin2,
        lag_donor_count = lag_donor_count_admin2,
        lag_total_proj = lag_total_proj_admin2,
        lag_total_aid = lag_total_aid_admin2
      )
  }

  # Regress the outcome variable on predictors
  stage_2_formula <- as.formula(
    paste0(
      outcome_var,
      " ~ frag_index + lag_total_aid + lag_log_pop"
    )
  )

  stage_2 <- fixest::feols(
    stage_2_formula,
    cluster = cluster_var,
    data = data
  )

  return(stage_2)
}


high_1_ols <- perform_ols_analysis(high_admin1, "GID_1", "nl_growth", "GID_0")
low_1_ols <- perform_ols_analysis(low_admin1, "GID_1", "nl_growth", "GID_0")
high_2_ols <- perform_ols_analysis(high_admin2, "GID_2", "nl_growth", "GID_0")
low_2_ols <- perform_ols_analysis(low_admin2, "GID_2", "nl_growth", "GID_0")

# Remove the LaTeX file for Table 1 OLS if it exists
if (file.exists(here("03_output", "tabs", "table1_ols.tex"))) {
  file.remove(here("03_output", "tabs", "table1_ols.tex"))
}

etable(
  high_1_ols,
  low_1_ols,
  high_2_ols,
  low_2_ols,
  headers = c("High Admin1", "Low Admin1", "High Admin2", "Low Admin2"),
  dict = c(
    lag_donor_count = "Lag Donor Count",
    lag_total_proj = "Lag Total Projects",
    lag_log_pop = "Lag LN(Population)",
    `lag_donor_count:lag_total_proj` = "Lag Donor × Lag Projects",
    lag_total_aid = "Lag LN(Total Aid)"
  ),
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1), # <-- proper format
  fitstat = c("n", "r2"),
  tex = TRUE, # Save as LaTeX
  file = here("03_output", "tabs", "table1_ols.tex") # Specify output file
)

#### Table 1 Panel B
perform_fe_analysis <- function(data, admin_level, outcome_var, cluster_var) {
  # Harmonize variables by admin level
  if (admin_level == "GID_1") {
    data <- data %>%
      dplyr::mutate(
        total_aid = total_aid_admin1,
        frag_index = lag_hhi_admin1,
        lag_log_pop = lag_pop_admin1,
        lag_donor_count = lag_donor_count_admin1,
        lag_total_proj = lag_total_proj_admin1,
        lag_total_aid = lag_total_aid_admin1
      )
  } else if (admin_level == "GID_2") {
    data <- data %>%
      dplyr::mutate(
        total_aid = total_aid_admin2,
        frag_index = lag_hhi_admin2,
        lag_log_pop = lag_pop_admin2,
        lag_donor_count = lag_donor_count_admin2,
        lag_total_proj = lag_total_proj_admin2,
        lag_total_aid = lag_total_aid_admin2
      )
  }
  # Regress the outcome variable on predictors
  stage_2_formula <- as.formula(
    paste0(
      outcome_var,
      " ~ frag_index + lag_total_aid + lag_log_pop | GID_0^year + ",
      admin_level
    )
  )

  stage_2 <- fixest::feols(
    stage_2_formula,
    cluster = cluster_var,
    data = data
  )

  return(stage_2)
}

# Remove the LaTeX file for Table 1 OLS if it exists
if (file.exists(here("03_output", "tabs", "table1_fe.tex"))) {
  file.remove(here("03_output", "tabs", "table1_fe.tex"))
}

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
    lag_donor_count = "Lag Donor Count",
    lag_total_proj = "Lag Total Projects",
    lag_log_pop = "Lag LN(Population)",
    `lag_donor_count:lag_total_proj` = "Lag Donor × Lag Projects",
    lag_total_aid = "Lag LN(Total Aid)"
  ),
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1), # <-- proper format
  fitstat = c("n", "r2"),
  tex = TRUE, # Save as LaTeX
  file = here("03_output", "tabs", "table1_fe.tex") # Specify output file
)

### CFA for country govs table 1 Panel C
perform_cfa_analysis <- function(
  data,
  admin_level,
  outcome_var,
  iv_var,
  cluster_var
) {
  # Harmonize variables by admin level
  if (admin_level == "GID_1") {
    data <- data %>%
      dplyr::mutate(
        total_aid = total_aid_admin1,
        frag_index = lag_hhi_admin1,
        lag_log_pop = lag_pop_admin1,
        lag_donor_count = lag_donor_count_admin1,
        lag_total_proj = lag_total_proj_admin1,
        lag_total_aid = lag_total_aid_admin1
      )
  } else if (admin_level == "GID_2") {
    data <- data %>%
      dplyr::mutate(
        total_aid = total_aid_admin2,
        frag_index = lag_hhi_admin2,
        lag_log_pop = lag_pop_admin2,
        lag_donor_count = lag_donor_count_admin2,
        lag_total_proj = lag_total_proj_admin2,
        lag_total_aid = lag_total_aid_admin2
      )
  }

  # Stage 1: Regress total aid on the instrumental variable
  stage_1_formula <- as.formula(
    paste0(
      "total_aid ~ ",
      iv_var,
      " + lag_log_pop | GID_0^year + ",
      admin_level
    )
  )

  stage_1 <- fixest::feols(
    stage_1_formula,
    cluster = cluster_var,
    data = data
  )

  # Add CFA residuals to the dataset
  data <- data %>%
    filter(!is.na(nl_growth)) %>%
    dplyr::mutate(cfa = resid(stage_1))

  # Stage 2: Regress the outcome variable on predictors and CFA residuals
  stage_2_formula <- as.formula(
    paste0(
      outcome_var,
      " ~ frag_index + lag_total_aid + lag_log_pop + cfa | GID_0^year + ",
      admin_level
    )
  )

  stage_2 <- fixest::feols(
    stage_2_formula,
    cluster = cluster_var,
    data = data
  )

  return(stage_2)
}


# Perform the analysis for high and low admin1 and admin2
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

# Remove the LaTeX file for Table 2 CFE if it exists
if (file.exists(here("03_output", "tabs", "table2_cfe.tex"))) {
  file.remove(here("03_output", "tabs", "table2_cfe.tex"))
}

etable(
  stage_2_high_admin1,
  stage_2_low_admin1,
  stage_2_high_admin2,
  stage_2_low_admin2,
  headers = c("High Admin1", "Low Admin1", "High Admin2", "Low Admin2"),
  dict = c(
    lag_donor_count = "Lag Donor Count",
    lag_total_proj = "Lag Total Projects",
    lag_log_pop = "Lag LN(Population)",
    `lag_donor_count:lag_total_proj` = "Lag Donor × Lag Projects",
    lag_total_aid = "Lag LN(Total Aid)",
    cfa = "CFA Residuals"
  ),
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1), # <-- proper format
  fitstat = c("n", "r2"),
  tex = TRUE, # Save as LaTeX
  file = here("03_output", "tabs", "table2_cfe.tex") # Specify output file
)

# Perform the analysis for high and low admin1 and admin2 HEALTH
stage_2_high_admin1 <- perform_cfa_analysis(
  high_admin1,
  "GID_1",
  "u5m",
  "IV_lag",
  "GID_1"
)
stage_2_low_admin1 <- perform_cfa_analysis(
  low_admin1,
  "GID_1",
  "u5m",
  "IV_lag",
  "GID_1"
)
stage_2_high_admin2 <- perform_cfa_analysis(
  high_admin2,
  "GID_2",
  "u5m",
  "IV_lag",
  "GID_2"
)
stage_2_low_admin2 <- perform_cfa_analysis(
  low_admin2,
  "GID_2",
  "u5m",
  "IV_lag",
  "GID_2"
)

#------------------------------------------------------------------------------#
#  Now we run with new dependent ----
#------------------------------------------------------------------------------#

etable(
  stage_2_high_admin1,
  stage_2_low_admin1,
  stage_2_high_admin2,
  stage_2_low_admin2,
  headers = c("High Admin1", "Low Admin1", "High Admin2", "Low Admin2"),
  dict = c(
    lag_donor_count = "Lag Donor Count",
    lag_total_proj = "Lag Total Projects",
    lag_log_pop = "Lag LN(Population)",
    `lag_donor_count:lag_total_proj` = "Lag Donor × Lag Projects",
    lag_total_aid = "Lag LN(Total Aid)",
    cfa = "CFA Residuals"
  ),
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1), # <-- proper format
  fitstat = c("n", "r2"),
  tex = TRUE, # Save as LaTeX
  file = here("03_output", "tabs", "table2_cfe_u5m.tex") # Specify output file
)


# Now we are just looking at frag indicators no interaction

### CFA for country govs table 1 Panel C
perform_cfa_analysis_frag <- function(
  data,
  admin_level,
  outcome_var,
  iv_var,
  cluster_var
) {
  # Harmonize variables by admin level
  if (admin_level == "GID_1") {
    data <- data %>%
      dplyr::mutate(
        total_aid = total_aid_admin1,
        lag_log_pop = lag_pop_admin1,
        lag_donor_count = lag_donor_count_admin1,
        lag_total_proj = lag_total_proj_admin1,
        lag_total_aid = lag_total_aid_admin1,
        lag_hhi = frag_index_admin1
      )
  } else if (admin_level == "GID_2") {
    data <- data %>%
      dplyr::mutate(
        total_aid = total_aid_admin2,
        lag_log_pop = lag_pop_admin2,
        lag_donor_count = lag_donor_count_admin2,
        lag_total_proj = lag_total_proj_admin2,
        lag_total_aid = lag_total_aid_admin2,
        lag_hhi = frag_index_admin2
      )
  }

  # Stage 1: Regress total aid on the instrumental variable
  stage_1_formula <- as.formula(
    paste0(
      "total_aid ~ ",
      iv_var,
      " + lag_log_pop | GID_0^year + ",
      admin_level
    )
  )

  stage_1 <- fixest::feols(
    stage_1_formula,
    cluster = cluster_var,
    data = data
  )

  # Add CFA residuals to the dataset
  data <- data %>%
    dplyr::mutate(cfa = resid(stage_1))

  # Stage 2: Regress the outcome variable on predictors and CFA residuals
  stage_2_formula <- as.formula(
    paste0(
      outcome_var,
      " ~ lag_hhi + lag_total_aid + lag_log_pop + cfa | GID_0^year + ",
      admin_level
    )
  )

  stage_2 <- fixest::feols(
    stage_2_formula,
    cluster = cluster_var,
    data = data
  )

  return(stage_2)
}

# Perform the analysis for high and low admin1 and admin2
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
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1), # <-- proper format
  fitstat = c("n", "r2"),
  tex = TRUE, # Save as LaTeX
  file = here("03_output", "tabs", "table2_cfe_frag_ind.tex") # Specify output file
)
