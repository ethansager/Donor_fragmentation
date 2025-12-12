#marginal effects tests
# Regression Analysis 
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

panel_aid_admin1 <-  read_csv("01_panel_data/panel_aid_admin1_new.csv")
panel_aid_admin2 <- read_csv("01_panel_data/panel_aid_admin2_new.csv")

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
  ungroup()%>%
  filter(GID_0 %in% study_countries)%>%
  mutate(treat = 1,
         mean_sgq_admin1 = as.numeric(mean_sgq_admin1),
         lag_frag_index_admin1 = lag(as.numeric(frag_index_admin1)))%>%
         select(nl_growth, lag_frag_index_admin1, mean_sgq_admin1, GID_1, paymentyear)%>%
         as.data.frame()

panel_aid_admin2 <- panel_aid_admin2 %>%
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
  ) %>%
  ungroup() %>%
  filter(GID_0 %in% study_countries)%>%
  mutate(treat = 1,
         mean_sgq_admin2 = as.numeric(mean_sgq_admin2),
         lag_frag_index_admin2 = lag(as.numeric(frag_index_admin2)))%>%
         select(nl_growth, lag_frag_index_admin2, mean_sgq_admin2, GID_2, paymentyear)%>%
         as.data.frame()

interflex(Y = "nl_growth", D = "lag_frag_index_admin2", X = "mean_sgq_admin2", data = panel_aid_admin2, na.rm = TRUE, estimator = "binning", FE = c("GID_2", "paymentyear"), cl = "GID_2", treat.type = "continuous")
interflex(Y = "nl_growth", D = "lag_frag_index_admin1", X = "mean_sgq_admin1", data = panel_aid_admin1, na.rm = TRUE, estimator = "binning", FE = c("GID_1", "paymentyear"), cl = "GID_1", treat.type = "continuous")
