### DiD ####
# Regression Analysis 
if (!require("pacman")) 
  install.packages("pacman"); 

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
  did
)

panel_aid_admin1 <-  read_csv("01_panel_data/panel_aid_admin1.csv")
panel_aid_admin2 <- read_csv("01_panel_data/panel_aid_admin2.csv")

study_countries <- c("BEN", "BWA", "GHA", "MDG", "MLI", "MOZ", "MWI", "NAM", "SEN", 
"TZA", "ZAF", "ZMB", "ZWE")

# Ensure proper ordering before applying lag()
panel_aid_admin1 <- panel_aid_admin1 %>%
  arrange(GID_1, paymentyear) %>%  # Sort before grouping
  group_by(GID_1) %>%
  mutate(
    lag_mean_nl = dplyr::lag(mean_nl),
    # Only first observation per group will be NA
    nl_growth = case_when(
      is.na(lag_mean_nl) ~ NA_real_,
      lag_mean_nl == 0 & mean_nl == 0 ~ log(mean_nl + 0.01),  # No growth when both are zero
      lag_mean_nl == 0 ~ log(mean_nl + 0.01),  # Handle zero in previous period
      TRUE ~ log((mean_nl + 0.01) / (lag_mean_nl + 0.01))  # Regular case with small constant
    )
  ) %>%
  ungroup() %>%
  filter(GID_0 %in% study_countries)

panel_aid_admin2 <- panel_aid_admin2 %>%
  arrange(GID_2, paymentyear) %>%  # Sort before grouping
  group_by(GID_2) %>%
  mutate(
    lag_mean_nl = dplyr::lag(mean_nl),
    # Only first observation per group will be NA
    nl_growth = case_when(
      is.na(lag_mean_nl) ~ NA_real_,
      lag_mean_nl == 0 & mean_nl == 0 ~ log(mean_nl + 0.01),  # No growth when both are zero
      lag_mean_nl == 0 ~ log(mean_nl + 0.01),  # Handle zero in previous period
      TRUE ~ log((mean_nl + 0.01) / (lag_mean_nl + 0.01))  # Regular case with small constant
    )
  ) %>%
  ungroup() %>%
  filter(GID_0 %in% study_countries)


# Group by five year? 

# Create summary table for panel_aid_admin1
summary_table_admin1 <- panel_aid_admin1 %>%
  select(mean_nl, total_aid_admin1, frag_index_admin1, mean_sgq_admin1, nl_growth) %>%
  tbl_summary(
    statistic = all_continuous() ~ "{mean} ({sd})"
  ) %>%
  modify_header(label = "**Variable**")

# Create summary table for panel_aid_admin2
summary_table_admin2 <- panel_aid_admin2 %>%
  select(mean_nl, total_aid_admin2, frag_index_admin2, mean_sgq_admin2, nl_growth) %>%
  tbl_summary(
    statistic = all_continuous() ~ "{mean} ({sd})"
  ) %>%
  modify_header(label = "**Variable**")

# Print the summary tables
# Generate LaTeX output
latex_output_1 <- as_gt(summary_table_admin1) %>% gt::as_latex()

# Write to .tex files
writeLines(latex_output_1, "summary_table_admin1.tex")

# Generate LaTeX output for another table if needed
latex_output_2 <- as_gt(summary_table_admin2) %>% gt::as_latex()

# Write to a different .tex file
writeLines(latex_output_2, "summary_table_admin2.tex")

#### Table 1 Panel A
frag_admin1 <- fixest::feols(log(mean_nl + 0.01) ~ log(total_aid_admin1) + frag_index_admin1| GID_0^paymentyear + GID_1, panel_aid_admin1)
frag_admin2  <- fixest::feols(log(mean_nl + 0.01)  ~ log(total_aid_admin2) + frag_index_admin2| GID_0^paymentyear + GID_2, panel_aid_admin2)

#### Table 1 Panel B
fe_admin1 <- fixest::feols(log(mean_nl + 0.01) ~ log(total_aid_admin1) + frag_index_admin1*mean_sgq_admin1 | GID_0^paymentyear + GID_1, panel_aid_admin1)
fe_admin2  <- fixest::feols(log(mean_nl + 0.01) ~ log(total_aid_admin2) + frag_index_admin2*mean_sgq_admin2 | GID_0^paymentyear + GID_2, panel_aid_admin2)

etable(frag_admin1, frag_admin2, fe_admin1, fe_admin2, tex = TRUE, file = "fe_models.tex")

#### Table 2 OLS


#### Table 1 Panel B
fe_admin1 <- fixest::feols(log(nl_growth + 0.01) ~ log(lag(total_aid_admin1)) + lag(frag_index_admin1)*mean_sgq_admin1 | GID_0^paymentyear + GID_1, panel_aid_admin1)
fe_admin2  <- fixest::feols(log(nl_growth + 0.01) ~ log(lag(total_aid_admin2)) + lag(frag_index_admin2)*mean_sgq_admin2 | GID_0^paymentyear + GID_2, panel_aid_admin2)

etable(fe_admin1, fe_admin2, tex = TRUE, file = "fe_growth_models.tex")

# Function to run regressions
run_regression <- function(data, index_cols, aid_col, frag_cols, controls, fixed_effects = TRUE) {
  models <- list()
  model_type <- ifelse(fixed_effects, "within", "pooling") # Corrected plm model type
  effect_param <- if (fixed_effects) "twoways" else "individual"

  for (frag_col in frag_cols) {
    # Ensure frag variable is also lagged
    data <- data %>%
      mutate(!!paste0("lag_", frag_col) := lag(!!sym(frag_col)))

    # Construct formula
    if (length(controls) > 0) {
      control_str <- paste(controls, collapse = " + ")
      formula <- as.formula(paste("nl_growth ~ log(",aid_col, ") + lag_", frag_col, " * ", control_str, sep = ""))
    } else {
      formula <- as.formula(paste("nl_growth ~ log(", aid_col, ") + lag_", frag_col, sep = ""))
    }

    # Run panel regression
    models[[frag_col]] <- plm(
      formula,
      data = data,
      index = index_cols,
      effect = effect_param,
      model = model_type
    )
  }
  
  return(models)
}

# Define model parameters
frag_vars_admin1 <- c("frag_index_admin1", "frag_1_admin1", "frag_3_admin1", "frag_below10_admin1", "donor_count_admin1")
frag_vars_admin2 <- c("frag_index_admin2", "frag_1_admin2", "frag_3_admin2", "frag_below10_admin2", "donor_count_admin2")
controls_fix_admin1 <- c("mean_sgq_admin1", "pop_admin1", "distance_to_capital", "capital_region", "disaster_dummy", "ge_pct")
controls_fix_admin2 <- c("mean_sgq_admin2",  "pop_admin2", "distance_to_capital", "capital_region", "disaster_dummy", "ge_pct")

# Run Pooled OLS Models
fe_models_admin1 <- run_regression(panel_aid_admin1, c("GID_1", "paymentyear"), "lag(total_aid_admin1)", frag_vars_admin1, controls_fix_admin1, FALSE)
fe_models_admin2 <- run_regression(panel_aid_admin2, c("GID_2", "paymentyear"), "lag(total_aid_admin2)", frag_vars_admin2, controls_fix_admin2, FALSE)

# Output results to LaTeX tables

# Fixed Effects Models
stargazer(fe_models_admin1, type = "latex", single.row = TRUE, header = FALSE, column.sep.width = "2pt", style = "aer", out = "ols_models_admin1.tex")
stargazer(fe_models_admin2, type = "latex", single.row = TRUE, header = FALSE,column.sep.width = "2pt", style = "aer", out = "ols_models_admin2.tex")

# Run Fixed Effects Models
controls_fix_admin1 <- c("mean_sgq_admin1")
controls_fix_admin2 <- c("mean_sgq_admin2")

fe_models_admin1 <- run_regression(panel_aid_admin1, c("GID_1", "paymentyear"), "lag(total_aid_admin1)", frag_vars_admin1, controls_fix_admin1, TRUE)
fe_models_admin2 <- run_regression(panel_aid_admin2, c("GID_2", "paymentyear"), "lag(total_aid_admin2)", frag_vars_admin2, controls_fix_admin2, TRUE)

# Fixed Effects Models
stargazer(fe_models_admin1, type = "text", single.row = TRUE, header = FALSE,column.sep.width = "2pt", style = "aer", out = "twoway_models_admin1.tex")
stargazer(fe_models_admin2, type = "text", single.row = TRUE, header = FALSE,column.sep.width = "2pt", style = "aer", out = "twoway_models_admin2.tex")

# Run Fixed Effects Models (FAC)
controls_fix_admin1 <- c("mean_fac_admin1")
controls_fix_admin2 <- c("mean_fac_admin2")

fe_models_admin1 <- run_regression(panel_aid_admin1, c("GID_1", "paymentyear"), "lag(total_aid_admin1)", frag_vars_admin1, controls_fix_admin1, TRUE)
fe_models_admin2 <- run_regression(panel_aid_admin2, c("GID_2", "paymentyear"), "lag(total_aid_admin2)", frag_vars_admin2, controls_fix_admin2, TRUE)

# Fixed Effects Models
stargazer(fe_models_admin1, type = "latex", single.row = TRUE, header = FALSE,column.sep.width = "2pt", style = "aer", out = "twoway_fac_models_admin1.tex")
stargazer(fe_models_admin2, type = "latex", single.row = TRUE, header = FALSE,column.sep.width = "2pt", style = "aer", out = "twoway_fac_models_admin2.tex")

# Run Fixed Effects Models (SVC)
controls_fix_admin1 <- c("mean_svc_admin1")
controls_fix_admin2 <- c("mean_svc_admin2")

fe_models_admin1 <- run_regression(panel_aid_admin1, c("GID_1", "paymentyear"), "lag(total_aid_admin1)", frag_vars_admin1, controls_fix_admin1, TRUE)
fe_models_admin2 <- run_regression(panel_aid_admin2, c("GID_2", "paymentyear"), "lag(total_aid_admin2)", frag_vars_admin2, controls_fix_admin2, TRUE)

# Fixed Effects Models
stargazer(fe_models_admin1, type = "latex", single.row = TRUE, header = FALSE,column.sep.width = "2pt", style = "aer", out = "twoway_svc_models_admin1.tex")
stargazer(fe_models_admin2, type = "latex", single.row = TRUE, header = FALSE,column.sep.width = "2pt", style = "aer", out = "twoway_svc_models_admin2.tex")

