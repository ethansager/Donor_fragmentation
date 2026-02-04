# Shared setup for regressions and QMD analyses

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
  gtsummary,
  car,
  stargazer,
  broom,
  fixest,
  did,
  interflex
)




admin1 <- grep("admin1", files)
admin2 <- grep("admin2", files)

build_dataset <-function(admin_list){
# Ensure proper ordering before applying lag()
panel_aid_admin1 <- panel_aid_admin1 %>%
  arrange(GID_1, year) %>%
  group_by(GID_1) %>%
  rename(mean_nl = mean) %>%
  mutate(
    lag_mean_nl = dplyr::lag(mean_nl),
    nl_growth = case_when(
      is.na(lag_mean_nl) ~ NA_real_,
      TRUE ~
        ((log(mean_nl + 0.01) - log(lag_mean_nl + 0.01)) /
          log(lag_mean_nl + 0.01)) *
        100
    ),
    lag_hhi_admin1 = dplyr::lag(frag_index_admin1),
    lag_pop_admin1 = dplyr::lag(ln_pop_admin1),
    lag_donor_count_admin1 = dplyr::lag(donor_count_admin1),
    lag_total_proj_admin1 = dplyr::lag(total_proj_admin1),
    lag_total_aid_admin1 = log(lag(total_aid_admin1) + .01)
  ) %>%
  filter(
    nl_growth > quantile(nl_growth, probs = .05, na.rm = TRUE) &
      nl_growth < quantile(nl_growth, probs = .95, na.rm = TRUE)
  ) %>%
  ungroup()

panel_aid_admin2 <- panel_aid_admin2 %>%
  arrange(GID_2, year) %>%
  group_by(GID_2) %>%
  rename(mean_nl = mean) %>%
  mutate(
    lag_mean_nl = dplyr::lag(mean_nl),
    nl_growth = case_when(
      is.na(lag_mean_nl) ~ NA_real_,
      TRUE ~
        ((log(mean_nl + 0.01) - log(lag_mean_nl + 0.01)) /
          log(lag_mean_nl + 0.01)) *
        100
    ),
    lag_hhi_admin2 = dplyr::lag(frag_index_admin2),
    lag_pop_admin2 = dplyr::lag(ln_pop_admin2),
    lag_donor_count_admin2 = dplyr::lag(donor_count_admin2),
    lag_total_proj_admin2 = dplyr::lag(total_proj_admin2),
    lag_total_aid_admin2 = log(lag(total_aid_admin2) + .01)
  ) %>%
  filter(
    nl_growth > quantile(nl_growth, probs = .05, na.rm = TRUE) &
      nl_growth < quantile(nl_growth, probs = .95, na.rm = TRUE)
  ) %>%
  ungroup()

# Split the sample into high and low SGQ
panel_aid_admin1 <- panel_aid_admin1 %>%
  mutate(
    med_sgq_admin1 = if_else(
      mean_sgq_admin1 > quantile(mean_sgq_admin1, 0.75, na.rm = TRUE),
      1,
      0
    )
  )

panel_aid_admin2 <- panel_aid_admin2 %>%
  mutate(
    med_sgq_admin2 = if_else(
      mean_sgq_admin2 > quantile(mean_sgq_admin2, 0.75, na.rm = TRUE),
      1,
      0
    )
  )

high_admin1 <- panel_aid_admin1 %>%
  filter(panel_aid_admin1$med_sgq_admin1 == 1)

low_admin1 <- panel_aid_admin1 %>%
  filter(panel_aid_admin1$med_sgq_admin1 == 0)

high_admin2 <- panel_aid_admin2 %>%
  filter(med_sgq_admin2 == 1)

low_admin2 <- panel_aid_admin2 %>%
  filter(med_sgq_admin2 == 0)
