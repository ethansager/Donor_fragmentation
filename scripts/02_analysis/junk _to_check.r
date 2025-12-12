dat <- read_csv("01_panel_data\\panel_aid_admin2_new.csv")

# First question did sgq change with urbanicity

fixest::feols(
    mean_sgq_admin2 ~ urban_share | GID_0,
    data = dat
)


fixest::feols(
    mean_sgq_admin2 ~ distance_to_capital | GID_0,
    data = dat
)

fixest::feols(
    mean_sgq_admin2 ~ afro_count | GID_0,
    data = dat
)

# Does it
dat <- read_csv("01_panel_data\\panel_aid_admin2_fin.csv")

fixest::feols(
    mean_sgq_admin2 ~ IV_lag | GID_0,
    data = dat
)

# Check at admin1 as well
dat <- read_csv("01_panel_data\\panel_aid_admin1_new.csv")


fixest::feols(
    mean_sgq_admin1 ~ urban_share | GID_0,
    data = dat
)

fixest::feols(
    mean_sgq_admin1 ~ distance_to_capital | GID_0,
    data = dat
)

fixest::feols(
    mean_sgq_admin1 ~ afro_count | GID_0,
    data = dat
)

fixest::feols(
    mean_sgq_admin1 ~ capital_region | GID_0,
    data = dat
)

# Check at admin1 as well
dat <- read_csv("01_panel_data\\panel_aid_admin1_fin.csv")

fixest::feols(
    mean ~
        total_early_admin1 +
            frag_index_admin1 * mean_sgq_admin1 +
            pop |
            GID_0^year + GID_1,
    data = dat
)
