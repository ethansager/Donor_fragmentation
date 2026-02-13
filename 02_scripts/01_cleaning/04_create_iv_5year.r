# Create IV for 5-year bucketed panels

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  data.table,
  tidyr,
  here,
  sf,
  zoo
)

map_to_5year_bucket <- function(year) {
  dplyr::case_when(
    year %in% 2005:2009 ~ 2005,
    year %in% 2010:2014 ~ 2010,
    TRUE ~ NA_real_
  )
}

# Get country isos for gadm
countries_iso3 <- read_csv(
  "00_rawdata/nightlights/topcodefix/processed_topcodefix_nl_admin2.csv"
) %>%
  select(GID_0) %>%
  distinct() %>%
  pull()

# Read in data
raw_aid_data <- read_csv(here("00_rawdata", "GODAD_projectlevel.csv")) %>%
  mutate(
    world_bank = ifelse(
      donor == "World Bank" & !precision_code %in% c("1", "2", "3"),
      1,
      0
    ),
    year = map_to_5year_bucket(paymentyear)
  ) %>%
  filter(
    gid_0 %in% countries_iso3,
    paymentyear >= 2005,
    paymentyear <= 2014,
    !is.na(latitude),
    !is.na(longitude),
    !is.na(year),
    world_bank == 0
  )

donor_countries <- c(
  "Austria",
  "Belgium",
  "Denmark",
  "Finland",
  "France",
  "Germany",
  "Greece",
  "Iceland",
  "India",
  "Ireland",
  "Italy",
  "Luxembourg",
  "Netherlands",
  "Norway",
  "Portugal",
  "Spain",
  "Sweden",
  "Switzerland",
  "United Kingdom",
  "United States",
  "World Bank"
)

# Combine all shapefiles into one
shp <- sf::read_sf("00_rawdata/shapefiles/gadm_admin2.shp") %>%
  mutate(GID_2 = ifelse(is.na(GID_2), GID_1, GID_2))

aid_points <- sf::st_as_sf(
  raw_aid_data,
  coords = c("longitude", "latitude"),
  crs = 4326
)

dat <- sf::st_join(aid_points, sf::st_make_valid(shp)) %>%
  filter(donor %in% donor_countries)

setDT(dat)

aid_data <- dat[,
  .(
    aid_amount = sum(abs(disb_loc_evensplit), na.rm = TRUE)
  ),
  by = .(GID_0, GID_1, GID_2, year, donor)
]

# Read in annual donor fractionalization data
frac_data_annual <- read_csv("00_rawdata/DPI2020.csv") %>%
  mutate(
    countryname = str_replace(countryname, "USA", "United States"),
    countryname = str_replace(countryname, "UK", "United Kingdom"),
    countryname = str_replace(countryname, "FRG/Germany", "Germany"),
    frac_full = if_else(
      countryname == "United States",
      frac,
      govfrac
    )
  ) %>%
  filter(countryname %in% donor_countries & year >= 2005 & year <= 2014) %>%
  select(donor = countryname, year, frac_full)

# Estimate weighted average donor fractionalization for World Bank
votes <- read_csv("00_rawdata/pdfs/wb_vote_shares.csv") %>%
  mutate(
    Country = str_replace(Country, "USA", "United States"),
    Country = str_replace(Country, "UK", "United Kingdom")
  ) %>%
  pivot_longer(
    cols = starts_with("Vote_"),
    names_to = "year",
    values_to = "vote_share"
  ) %>%
  mutate(year = str_replace(year, "Vote_", "")) %>%
  filter(year != "avg") %>%
  mutate(year = as.numeric(year)) %>%
  complete(Country = unique(Country), year = 2005:2014) %>%
  group_by(Country) %>%
  mutate(vote_share = na.approx(vote_share, na.rm = TRUE, rule = 2)) %>%
  ungroup() %>%
  left_join(
    frac_data_annual %>% select(donor, year, frac_full),
    by = c("Country" = "donor", "year")
  ) %>%
  group_by(year) %>%
  summarise(
    frac_full = weighted.mean(frac_full, vote_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(donor = "World Bank")

frac_data <- bind_rows(frac_data_annual, votes) %>%
  mutate(year = map_to_5year_bucket(year)) %>%
  filter(!is.na(year)) %>%
  group_by(donor, year) %>%
  summarise(frac_full = mean(frac_full, na.rm = TRUE), .groups = "drop")

construct_shift_share_iv_bucketed <- function(
    aid_data,
    frac_data,
    panel_data,
    region_id,
    aid_threshold = 0
) {
  periods <- sort(unique(panel_data$year))

  pji <- aid_data %>%
    complete(
      !!sym(region_id),
      donor,
      year = periods,
      fill = list(aid_amount = 0)
    ) %>%
    mutate(received_aid = ifelse(aid_amount > aid_threshold, 1, 0)) %>%
    group_by(!!sym(region_id), donor) %>%
    summarise(pji = mean(received_aid, na.rm = TRUE), .groups = "drop")

  names(pji)[names(pji) == region_id] <- "region_id"

  iv_components <- panel_data %>%
    select(region_id = !!sym(region_id), year) %>%
    distinct() %>%
    crossing(donor = unique(frac_data$donor)) %>%
    left_join(pji, by = c("region_id", "donor")) %>%
    left_join(frac_data, by = c("donor", "year")) %>%
    mutate(iv_component = pji * frac_full)

  iv_data <- iv_components %>%
    group_by(region_id, year) %>%
    summarise(IV = sum(iv_component, na.rm = TRUE), .groups = "drop") %>%
    arrange(region_id, year) %>%
    group_by(region_id) %>%
    mutate(IV_lag = dplyr::lag(IV, order_by = year)) %>%
    ungroup()

  return(iv_data)
}

# Create the IV (admin2)
panel_aid_admin2 <- read_csv(here("01_panel_data", "panel_aid_admin2_5year.csv"))

results <- construct_shift_share_iv_bucketed(
  aid_data = aid_data,
  frac_data = frac_data,
  panel_data = panel_aid_admin2,
  region_id = "GID_2"
)

panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(results, by = c("GID_2" = "region_id", "year"))

# Create the IV (admin1)
panel_aid_admin1 <- read_csv(here("01_panel_data", "panel_aid_admin1_5year.csv"))

results <- construct_shift_share_iv_bucketed(
  aid_data = aid_data,
  frac_data = frac_data,
  panel_data = panel_aid_admin1,
  region_id = "GID_1"
)

panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(results, by = c("GID_1" = "region_id", "year"))

# Save the data
write_csv(panel_aid_admin2, here("01_panel_data", "panel_aid_admin2_fin_5year.csv"))
write_csv(panel_aid_admin1, here("01_panel_data", "panel_aid_admin1_fin_5year.csv"))
