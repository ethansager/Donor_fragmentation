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
  car,
  stargazer,
  broom
)

# Load required datasets 
# full data
panel_aid_admin1 <- read_csv("01_panel_data/panel_aid_admin1_new.csv")
panel_aid_admin2 <- read_csv("01_panel_data/panel_aid_admin2_new.csv")

run_regression <- function(data, index_cols, aid_col, frag_cols, controls, fixed_effects = TRUE) {
  models <- list()
  model_type <- ifelse(fixed_effects, "within", "pooled")
  effect_param <- ifelse(fixed_effects, "twoways", NULL)
  
  # Ensure data is sorted by time before applying lag function
  data <- data[order(data[[index_cols[2]]]), ]
  
  for (frag_col in frag_cols) {
    if (length(controls) > 0) {
      control_str <- paste(controls, collapse = " + ")
      formula <- as.formula(paste("log(lag(mean_nl,1)+.01) ~ log(lag(", aid_col, ")) +", "lag(", frag_col,")*", control_str))
    } else {
      formula <- as.formula(paste("lag(mean_nl,1) ~ log(", aid_col, ") +", frag_col))
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
frag_vars_admin1 <- c("frag_index_admin1", "frag_1_admin1", "frag_3_admin1", "frag_below10_admin1", "donor_count_admin1", "total_proj_admin1")
frag_vars_admin2 <- c("frag_index_admin2", "frag_1_admin2", "frag_3_admin2", "frag_below10_admin2", "donor_count_admin2", "total_proj_admin2")
controls_fix_admin1 <- c("mean_sgq_admin1")
controls_fix_admin2 <- c("mean_sgq_admin2")
controls_rand_admin1 <- c("mean_sgq_admin1", "mean_svc_admin1", "mean_fac_admin1", "ge_pct", "distance_to_capital", "log_avg_pop_admin1", "nearest_city_dist", "avg_spei_admin1")
controls_rand_admin2 <- c("mean_sgq_admin2", "mean_svc_admin2", "mean_fac_admin2", "ge_pct", "distance_to_capital", "log_avg_pop_admin2", "nearest_city_dist", "avg_spei_admin2")

# Run Fixed Effects Models
# Full
fe_models_admin1 <- run_regression(panel_aid_admin1, c("GID_1", "paymentyear"), "total_aid_admin1", frag_vars_admin1, controls_fix_admin1, TRUE)
fe_models_admin2 <- run_regression(panel_aid_admin2, c("GID_2", "paymentyear"), "total_aid_admin2", frag_vars_admin2, controls_fix_admin2, TRUE)
# Early
fe_models_admin1_early <- run_regression(panel_aid_admin1_early, c("GID_1", "paymentyear"), "total_aid_admin1", frag_vars_admin1, controls_fix_admin1, TRUE)
fe_models_admin2_early <- run_regression(panel_aid_admin2_early, c("GID_2", "paymentyear"), "total_aid_admin2", frag_vars_admin2, controls_fix_admin2, TRUE)
# Late 
fe_models_admin1_late <- run_regression(panel_aid_admin1_late, c("GID_1", "paymentyear"), "total_aid_admin1", frag_vars_admin1, controls_fix_admin1, TRUE)
fe_models_admin2_late <- run_regression(panel_aid_admin2_late, c("GID_2", "paymentyear"), "total_aid_admin2", frag_vars_admin2, controls_fix_admin2, TRUE)

# Run Random Effects Models
random_models_admin1 <- run_regression(panel_aid_admin1, c("GID_1", "paymentyear"), "total_aid_admin1", frag_vars_admin1, controls_rand_admin1, FALSE)
random_models_admin2 <- run_regression(panel_aid_admin2, c("GID_2", "paymentyear"), "total_aid_admin2", frag_vars_admin2, controls_rand_admin2, FALSE)
# Early
random_models_admin1_early <- run_regression(panel_aid_admin1_early, c("GID_1", "paymentyear"), "total_aid_admin1", frag_vars_admin1, controls_rand_admin1, FALSE)
random_models_admin2_early <- run_regression(panel_aid_admin2_early, c("GID_2", "paymentyear"), "total_aid_admin2", frag_vars_admin2, controls_rand_admin2, FALSE)
# Late
random_models_admin1_late <- run_regression(panel_aid_admin1_late, c("GID_1", "paymentyear"), "total_aid_admin1", frag_vars_admin1, controls_rand_admin1, FALSE)
random_models_admin2_late <- run_regression(panel_aid_admin2_late, c("GID_2", "paymentyear"), "total_aid_admin2", frag_vars_admin2, controls_rand_admin2, FALSE)

# Output results to LaTeX tables

# Fixed Effects Models
stargazer(fe_models_admin1, type = "text", out = "fe_models_admin1.tex")
stargazer(fe_models_admin2, type = "text", out = "fe_models_admin2.tex")

stargazer(fe_models_admin1_early, type = "latex", out = "fe_models_admin1_early.tex")
stargazer(fe_models_admin2_early, type = "latex", out = "fe_models_admin2_early.tex")
stargazer(fe_models_admin1_late, type = "latex", out = "fe_models_admin1_late.tex")
stargazer(fe_models_admin2_late, type = "latex", out = "fe_models_admin2_late.tex")

# Random Effects Models
stargazer(random_models_admin1, type = "latex", out = "random_models_admin1.tex")
stargazer(random_models_admin2, type = "latex", out = "random_models_admin2.tex")
stargazer(random_models_admin1_early, type = "latex", out = "random_models_admin1_early.tex")
stargazer(random_models_admin2_early, type = "latex", out = "random_models_admin2_early.tex")
stargazer(random_models_admin1_late, type = "latex", out = "random_models_admin1_late.tex")
stargazer(random_models_admin2_late, type = "latex", out = "random_models_admin2_late.tex")


##############################################################################################
# New Regrression Analysis
##############################################################################################

# split data into high and low capacity groups
panel_aid_admin1 <- panel_aid_admin1 %>%
  mutate(
    threshold = mean(mean_sgq_admin1, na.rm = TRUE),
    high_capacity = ifelse(mean_sgq_admin1 > threshold, 1, 0))%>%  
    arrange(GID_1, paymentyear) %>%  # Sort before grouping
  group_by(GID_1) %>%
  mutate(
    lag_mean_nl = dplyr::lag(mean_nl),
    # Only first observation per group will be NA
    nl_growth = as.numeric(case_when(
      is.na(lag_mean_nl) ~ NA_real_,
      lag_mean_nl == 0 & mean_nl == 0 ~ log(mean_nl + 0.01),  # No growth when both are zero
      lag_mean_nl == 0 ~ log(mean_nl + 0.01),  # Handle zero in previous period
      TRUE ~ log((mean_nl + 0.01) / (lag_mean_nl + 0.01)))  # Regular case with small constant
    )
  )


panel_aid_admin2 <- panel_aid_admin2 %>%
  mutate(
    threshold = mean(mean_sgq_admin2, na.rm = TRUE),
    high_capacity = ifelse(mean_sgq_admin2 > threshold, 1, 0))%>%  
    arrange(GID_2, paymentyear) %>%  # Sort before grouping
  group_by(GID_2) %>%
  mutate(
    lag_mean_nl = dplyr::lag(mean_nl),
    # Only first observation per group will be NA
    nl_growth = as.numeric(case_when(
      is.na(lag_mean_nl) ~ NA_real_,
      lag_mean_nl == 0 & mean_nl == 0 ~ log(mean_nl + 0.01),  # No growth when both are zero
      lag_mean_nl == 0 ~ log(mean_nl + 0.01),  # Handle zero in previous period
      TRUE ~ log((mean_nl + 0.01) / (lag_mean_nl + 0.01)))  # Regular case with small constant
    )
  )

# Now we want to regress the lagged nl growth on aid variable X fragmentation, separately for high and low capacity groups
admin1_high <- panel_aid_admin1 %>% filter(high_capacity == 1)
admin1_low  <- panel_aid_admin1 %>% filter(high_capacity == 0)

admin2_high <- panel_aid_admin2 %>% filter(high_capacity == 1)
admin2_low  <- panel_aid_admin2 %>% filter(high_capacity == 0)

# Run panel regressions for Admin1 groups
model_admin1_high <- plm(
  log(lag(mean_nl,1)+.01) ~ log(total_aid_admin1) * donor_count_admin1,
  data = admin1_high,
  index = c("GID_1", "paymentyear"),
  effect = "twoways",
  model = "within"
)

model_admin1_low <- plm(
  log(lag(mean_nl,1)+.01) ~ log(total_aid_admin1) * donor_count_admin1,
  data = admin1_low,
  index = c("GID_1", "paymentyear"),
   effect = "twoways",
  model = "within"
)

# Run panel regressions for Admin2 groups
model_admin2_high <- plm(
  log(lag(mean_nl,1)+.01) ~ log(total_aid_admin2) * donor_count_admin2,
  data = admin2_high,
  index = c("GID_2", "paymentyear"),
   effect = "twoways",
  model = "within"
)

model_admin2_low <- plm(
  log(lag(mean_nl,1)+.01) ~ log(total_aid_admin2) * donor_count_admin2,
  data = admin2_low,
  index = c("GID_2", "paymentyear"),
   effect = "twoways",
  model = "within"
)

model_list <- list(
  "Admin1 High" = model_admin1_high,
  "Admin1 Low"  = model_admin1_low,
  "Admin2 High" = model_admin2_high,
  "Admin2 Low"  = model_admin2_low
)

modelsummary(
  model_list,
  output = "default",
  stars = TRUE,
  title = "Panel Regression Results by Admin Groups and Capacity",
  notes = "Dependent Variable: Lagged one year log(mean nl) "
)

