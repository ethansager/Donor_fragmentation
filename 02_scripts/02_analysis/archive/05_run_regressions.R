# Regression Analysis
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
  modelsummary,
  car,
  stargazer,
  broom
)

# Load required datasets
# full data
panel_aid_admin1 <- read_csv(here(
  "01_panel_data",
  "panel_aid_admin1_fin.csv"
)) %>%
  rename(mean_nl = mean)

panel_aid_admin2 <- read_csv(here(
  "01_panel_data",
  "panel_aid_admin2_fin.csv"
)) %>%
  rename(mean_nl = mean)

run_regression <- function(
  data,
  index_cols,
  aid_col,
  frag_cols,
  controls,
  fixed_effects = TRUE
) {
  models <- list()
  model_type <- ifelse(fixed_effects, "within", "pooled")
  effect_param <- ifelse(fixed_effects, "twoways", NULL)

  # Ensure data is sorted by time before applying lag function
  data <- data[order(data[[index_cols[2]]]), ]

  for (frag_col in frag_cols) {
    if (length(controls) > 0) {
      control_str <- paste(controls, collapse = " + ")
      formula <- as.formula(paste(
        "log(lag(mean_nl,1)+.01) ~ log(lag(",
        aid_col,
        ")) +",
        "lag(",
        frag_col,
        ")*",
        control_str
      ))
    } else {
      formula <- as.formula(paste(
        "lag(mean_nl,1) ~ log(",
        aid_col,
        ") +",
        frag_col
      ))
    }

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
frag_vars_admin1 <- c(
  "frag_index_admin1",
  "frag_1_admin1",
  "frag_3_admin1",
  "frag_below10_admin1",
  "donor_count_admin1",
  "total_proj_admin1"
)
frag_vars_admin2 <- c(
  "frag_index_admin2",
  "frag_1_admin2",
  "frag_3_admin2",
  "frag_below10_admin2",
  "donor_count_admin2",
  "total_proj_admin2"
)
controls_fix_admin1 <- c("mean_sgq_admin1")
controls_fix_admin2 <- c("mean_sgq_admin2")

controls_rand_admin1 <- c(
  "mean_sgq_admin1",
  "mean_svc_admin1",
  "mean_fac_admin1",
  "ge_pct",
  "distance_to_capital",
  "log_avg_pop_admin1",
  "nearest_city_dist",
  "avg_spei_admin1"
)
controls_rand_admin2 <- c(
  "mean_sgq_admin2",
  "mean_svc_admin2",
  "mean_fac_admin2",
  "ge_pct",
  "distance_to_capital",
  "log_avg_pop_admin2",
  "nearest_city_dist",
  "avg_spei_admin2"
)

# Run Fixed Effects Models
# Full
fe_models_admin1 <- run_regression(
  panel_aid_admin1,
  c("GID_1", "year"),
  "total_aid_admin1",
  frag_vars_admin1,
  controls_fix_admin1,
  TRUE
)

fe_models_admin2 <- run_regression(
  panel_aid_admin2,
  c("GID_2", "year"),
  "total_aid_admin2",
  frag_vars_admin2,
  controls_fix_admin2,
  TRUE
)


# Output results to LaTeX tables

# Fixed Effects Models
stargazer(
  fe_models_admin1,
  type = "text",
  out = here("03_output", "tabs", "fe_models_admin1.tex")
)
stargazer(
  fe_models_admin2,
  type = "text",
  out = here("03_output", "tabs", "fe_models_admin2.tex")
)


##############################################################################################
# New Regrression Analysis
##############################################################################################

# Load utility functions
source(here("02_scripts", "utils", "regression_utils.r"))

# Split data into high and low capacity groups and add growth variable
panel_aid_admin1 <- add_capacity_and_growth(
  data = panel_aid_admin1,
  admin_id = "GID_1",
  capacity_var = "mean_sgq_admin1"
)

panel_aid_admin2 <- add_capacity_and_growth(
  data = panel_aid_admin2,
  admin_id = "GID_2",
  capacity_var = "mean_sgq_admin2"
)

# Run panel regressions for both admin levels
admin1_models <- run_capacity_regressions(
  data = panel_aid_admin1,
  admin_id = "GID_1",
  aid_var = "total_aid_admin1",
  frag_var = "donor_count_admin1"
)

admin2_models <- run_capacity_regressions(
  data = panel_aid_admin2,
  admin_id = "GID_2",
  aid_var = "total_aid_admin2",
  frag_var = "donor_count_admin2"
)

# Extract individual models for compatibility
model_admin1_high <- admin1_models$high
model_admin1_low <- admin1_models$low
model_admin2_high <- admin2_models$high
model_admin2_low <- admin2_models$low

model_list <- list(
  "Admin1 High" = model_admin1_high,
  "Admin1 Low" = model_admin1_low,
  "Admin2 High" = model_admin2_high,
  "Admin2 Low" = model_admin2_low
)

modelsummary(
  model_list,
  output = "default",
  stars = TRUE,
  title = "Panel Regression Results by Admin Groups and Capacity",
  notes = "Dependent Variable: Lagged one year log(mean nl) "
)
