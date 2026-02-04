# Ad hoc Analysis Script
# This script performs a specific data analysis task as requested.

adm2 <- read_csv("01_panel_data/panel_aid_admin2_health.csv")
adm1 <- read_csv("01_panel_data/panel_aid_admin1_health.csv")


skimr::skim(adm2)
skimr::skim(adm1)


# The more populated the more fragmented but the effect is small
summary(lm(hhi_admin1 ~ ln_pop_admin1 + as.factor(GID_0), data = adm1))
summary(lm(hhi_admin2 ~ ln_pop_admin2 + as.factor(GID_0), data = adm2))

# Same is true for facilites which would correlate with population
summary(lm(hhi_admin1 ~ mean_fac_admin1 + as.factor(GID_0), data = adm1))
summary(lm(hhi_admin2 ~ mean_fac_admin2 + as.factor(GID_0), data = adm2))
