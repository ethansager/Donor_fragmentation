# Load required packages
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)

# Assuming your data is in a dataframe with variables averaged over 3-year paymentyears
# First create the growth variable
#full data
panel_aid_admin2 <- read.csv("01_panel_data/panel_aid_admin2.csv") %>%
  arrange(GID_2, paymentyear) %>%
  group_by(GID_2) %>%
  mutate(mean_nl_base = first(mean_nl)) %>%  # more explicit than mean_nl[1]
  ungroup() %>%
  pdata.frame(index = c("GID_2", "paymentyear")) %>%
  mutate(mean_nl_growth = mean_nl - lag(mean_nl))

panel_aid_admin1 <- read.csv("01_panel_data/panel_aid_admin1.csv") %>%
  arrange(GID_1, paymentyear) %>%
  group_by(GID_1) %>%
  mutate(mean_nl_base = first(mean_nl)) %>%  # more explicit than mean_nl[1]
  ungroup() %>%
  pdata.frame(index = c("GID_1", "paymentyear")) %>%
  mutate(mean_nl_growth = mean_nl - lag(mean_nl))

# Model 1 & 2: Total aid
model1_admin2_ols <- plm(
                  lag(mean_nl,1) ~ 
                  log(total_aid_admin2) +
                  log_avg_pop_admin2 +
                  frag_below10_admin2 *
                  mean_sgq_admin2 +
                  ge_pct +
                  log(distance_to_capital) +
                  avg_spei_admin2 +
                  log(nearest_city_dist),
                  data = panel_aid_admin2,
                  model = "pooling")

summary(model1_admin2_ols)

model1_admin2_ols <- plm(lag(mean_nl,1) ~ 
                           log(total_aid_admin2) +
                           frag_index_admin2 *
                           mean_sgq_admin2,
                         data = panel_aid_admin2,
                         model = "fd")

summary(model1_admin2_ols)


model1_admin1_ols <- plm(lag(mean_nl,1) ~ 
                           log(total_aid_admin1) +
                           log_avg_pop_admin1 +
                           log(distance_to_capital) +
                           frag_index_admin1 *
                           mean_sgq_admin1 +
                           mean_nl_base,
                         data = panel_aid_admin1,
                         model = "pooling",
                         effect = "time")

summary(model1_admin1_ols)


model1_admin1_ols <- plm(lag(mean_nl,1) ~ 
                           log(total_aid_admin1) +
                           log_avg_pop_admin1 +
                           log(distance_to_capital) +
                           frag_index_admin1 +
                           mean_nl_base,
                         data = panel_aid_admin1,
                         model = "pooling",
                         effect = "time")

summary(model1_admin1_ols)

model2_fe_fd  <- plm(lag(mean_nl,1) ~ 
                           log(total_aid_admin2) +
                           frag_index_admin2,
                         data = panel_aid_admin2,
                         model = "fd")

summary(model2_fe_fd)

model2_fe_fd <- plm(mean_nl ~ 
                   lag(log(total_aid_admin2),1) +
                   lag(log_avg_pop_admin2,1) +
                   log(distance_to_capital),
                 data = panel_data,
                 model = "fd")

summary(model2_fe)

# Model 3 & 4: Number of projects
model3_ols <- plm(mean_nl ~ 
                    lag(num_projects,1) +
                    lag(log_avg_pop_admin2,1) +
                    log(distance_to_capital),
                  data = panel_aid_admin2,
                  model = "pooling",
                  effect = "time")


model4_fe <- plm(lag(mean_nl,1) ~ 
                   total_aid_admin1 +
                   frag_index_admin1 +
                   mean_sgq_admin1,
                 data = panel_aid_admin1,
                 model = "within")

summary(model4_fe)

# Model 7 & 8: Number of early and late impact projects
model7_ols <- plm(mean_nl ~ 
                  lag(total_early_admin2) +
                  lag(total_late_admin2) +
                  lag(log_avg_pop_admin2,1) +
                  log(distance_to_capital),
                  data = panel_data,
                  model = "pooling",
                  effect = "time")

model8_fe <- lm(mean_nl ~ 
                   lag(total_early_admin2) +
                   lag(total_late_admin2) +
                   lag(log_avg_pop_admin2,1) +
                   log(distance_to_capital) +
                   factor(GID_2)-1,
                 data = panel_data)
summary
# Calculate clustered standard errors at region level
all_models <- 
                  
for(i in 1:length(all_models)) {
  all_models[[i]]$vcov <- vcovHC(all_models[[i]], cluster = "group")
}

# Create regression table
stargazer(all_models,
          type = "text",
          title = "OLS and region-fixed effects, ADM2, 2000-12")
          
column.labels = c("OLS", "FE", "OLS", "FE", "OLS", "FE"),
          dep.var.labels = "Growth in night-time light",
          covariate.labels = c("(Log) Total aid, t-1",
                              "(Log) No. of projects, t-1",
                              "(Log) Early-impact aid, t-1",
                              "(Log) Late-impact aid, t-1",
                              "(Log) No. of early-impact projects, t-1",
                              "(Log) No. of late-impact projects, t-1",
                              "(Log) Night-time light in 2000",
                              "(Log) Size of population, t-1",
                              "Population growth",
                              "(Log) Area",
                              "(Log) Distance to capital"),
          notes = "OLS and region-fixed-effects regressions (FE). Dependent variable: Growth in night-time light. Standard errors, clustered at the region-level, in parentheses: * p < 0.1, ** p < 0.05, *** p < 0.01.")

