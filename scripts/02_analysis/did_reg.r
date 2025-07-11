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
  did,
  interflex
)


panel_aid_admin1 <-  read_csv("01_panel_data/panel_aid_admin1.csv")
panel_aid_admin2 <- read_csv("01_panel_data/panel_aid_admin2.csv")

admin1_afro <- read_csv("00_rawdata/ab_raw/processed/admin1_afro_panel.csv")%>%
  select(year, GID_0, GID_1, starts_with("mean_"), afro_count, wave)
 
admin2_afro <- read_csv("00_rawdata/ab_raw/processed/admin2_afro_panel.csv")%>%
  select(year, GID_0, GID_1, GID_2, starts_with("mean_"), afro_count, wave)


# merge in the panel 
panel_aid_admin1 <- panel_aid_admin1 %>%
select(-c("mean_sgq_admin1", "mean_svc_admin1", "mean_fac_admin1")) %>%
  left_join(admin1_afro,
  by= c("year", "GID_0", "GID_1"))

panel_aid_admin2 <- panel_aid_admin2 %>%
select(-c("mean_sgq_admin2", "mean_svc_admin2", "mean_fac_admin2")) %>%
  left_join(admin2_afro,
  by= c("year", "GID_0", "GID_1", "GID_2"))

# Ensure proper ordering before applying lag()
panel_aid_admin1 <- panel_aid_admin1 %>%
  arrange(GID_1, year) %>%  # Sort before grouping
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
  ungroup() 

panel_aid_admin2 <- panel_aid_admin2 %>%
  arrange(GID_2, year) %>%  # Sort before grouping
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
  ungroup() 


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

panel_aid_admin1 <- panel_aid_admin1 %>%
  mutate(
med_sgq_admin1 = if_else(mean_sgq_admin1 > (mean(mean_sgq_admin1, na.rm = TRUE) + sd(mean_sgq_admin1, na.rm = TRUE)), 1, 0),
  )

panel_aid_admin2 <- panel_aid_admin2 %>%
  mutate(
med_sgq_admin2 = if_else(mean_sgq_admin2 > (mean(mean_sgq_admin2, na.rm = TRUE) + sd(mean_sgq_admin2, na.rm = TRUE)), 1, 0),
  )

high_admin1 <- panel_aid_admin1[panel_aid_admin1$med_sgq_admin1 == 1, ]
low_admin1 <- panel_aid_admin1[panel_aid_admin1$med_sgq_admin1 != 1, ]

high_admin2 <- panel_aid_admin2[panel_aid_admin2$med_sgq_admin2 == 1, ]
low_admin2 <- panel_aid_admin2[panel_aid_admin2$med_sgq_admin2 != 1, ]

#### Table 1 Panel A
high_1 <- fixest::feols(log(lag_mean_nl + 0.01) ~ donor_count_admin1 * total_proj_admin1| GID_0^year + GID_1, data = high_admin1)
low_1 <- fixest::feols(log(lag_mean_nl + 0.01) ~ donor_count_admin1 * total_proj_admin1| GID_0^year + GID_1, data = low_admin1)

high_2  <- fixest::feols(log(lag_mean_nl + 0.01)  ~ donor_count_admin2 * total_proj_admin2 | GID_0^year + GID_2, high_admin2)
low_2 <- fixest::feols(log(lag_mean_nl + 0.01) ~ donor_count_admin2 * total_proj_admin2 | GID_0^year + GID_2, low_admin2)


etable(high_1, low_1, high_2, low_2, tex = TRUE, file = "fe_models.tex")


# Simple OLS regressions for comparison
ols_high_1 <- lm(log(lag_mean_nl + 0.01) ~ donor_count_admin1 * total_proj_admin1, data = high_admin1)
ols_low_1 <- lm(log(lag_mean_nl + 0.01) ~ donor_count_admin1 * total_proj_admin1, data = low_admin1)
ols_high_2 <- lm(log(lag_mean_nl + 0.01) ~ donor_count_admin2 * total_proj_admin2, data = high_admin2)
ols_low_2 <- lm(log(lag_mean_nl + 0.01) ~ donor_count_admin2 * total_proj_admin2, data = low_admin2)

# Summarize the OLS models
summary(ols_high_1)
summary(ols_low_1)
summary(ols_high_2)
summary(ols_low_2)
