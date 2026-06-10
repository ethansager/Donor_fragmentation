# Create IV for 5-year bucketed panels (1995-2015)

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

source(here("02_scripts", "utils", "panel_time_windows.r"))

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
cfg <- get_5year_panel_config_1995_2015()
START_YEAR <- cfg$start_year
END_YEAR <- cfg$end_year
PANEL_TAG <- cfg$panel_tag
ALLOW_SYNTHETIC_EXTENSION <- TRUE

extract_years_from_paths <- function(paths) {
  extracted <- gsub(".*?(\\d{4}).*", "\\1", basename(paths))
  years <- suppressWarnings(as.integer(extracted))
  sort(unique(years[is.finite(years)]))
}

# -----------------------------------------------------------------------------
# Data readiness gate
# -----------------------------------------------------------------------------
nightlight_years <- c(
  list.files(
    here("00_rawdata", "nightlights", "africa"),
    pattern = "\\.tif$",
    full.names = TRUE
  ),
  list.files(
    here("00_rawdata", "nightlights", "topcodefix"),
    pattern = "^DMSP.*\\.tif$",
    full.names = TRUE
  )
) %>%
  extract_years_from_paths()
print_year_support("Nightlights raster year support", nightlight_years)
if (!ALLOW_SYNTHETIC_EXTENSION) {
  assert_year_coverage(
    "Nightlights raster support",
    nightlight_years,
    START_YEAR:END_YEAR
  )
}

u5m_raster <- here(
  "00_rawdata",
  "under_5",
  "IHME_LMICS_U5M_2000_2017_Q_UNDER5_MEAN_Y2019M10D16.TIF"
)
if (!file.exists(u5m_raster)) {
  stop("Missing U5M raster file: ", u5m_raster, call. = FALSE)
}
u5m_layers <- terra::nlyr(terra::rast(u5m_raster))
u5m_years <- 2000:(2000 + u5m_layers - 1L)
print_year_support("U5M raster year support", u5m_years)
if (!ALLOW_SYNTHETIC_EXTENSION) {
  assert_year_coverage(
    "U5M raster support",
    u5m_years,
    START_YEAR:END_YEAR
  )
}

admin1_afro_years <- readr::read_csv(
  here("00_rawdata", "ab_raw", "processed", "admin1_afro_panel.csv"),
  col_select = c(year),
  show_col_types = FALSE
) %>%
  pull(year)
admin2_afro_years <- readr::read_csv(
  here("00_rawdata", "ab_raw", "processed", "admin2_afro_panel.csv"),
  col_select = c(year),
  show_col_types = FALSE
) %>%
  pull(year)
print_year_support("Afro admin1 observed support", admin1_afro_years)
print_year_support("Afro admin2 observed support", admin2_afro_years)
if (!all(c(2005L, 2015L) %in% admin1_afro_years)) {
  stop(
    "Afro admin1 data must include years 2005 and 2015 for extrapolation bounds.",
    call. = FALSE
  )
}
if (!all(c(2005L, 2015L) %in% admin2_afro_years)) {
  stop(
    "Afro admin2 data must include years 2005 and 2015 for extrapolation bounds.",
    call. = FALSE
  )
}

# Get country isos for gadm
countries_iso3 <- read_csv(
  "00_rawdata/nightlights/processed/processed_topcodefix_nl_admin2.csv",
  show_col_types = FALSE
) %>%
  select(GID_0) %>%
  distinct() %>%
  pull()

# Read in aid data
raw_aid_data <- read_csv(
  here("00_rawdata", "GODAD_projectlevel.csv"),
  show_col_types = FALSE
)
print_year_support("GODAD paymentyear support", raw_aid_data$paymentyear)
assert_year_coverage(
  "GODAD paymentyear support",
  raw_aid_data$paymentyear,
  max(1996L, START_YEAR):END_YEAR
)

raw_aid_data <- raw_aid_data %>%
  mutate(
    world_bank = ifelse(
      donor == "World Bank" & !precision_code %in% c("1", "2", "3"),
      1,
      0
    ),
    year = map_to_5year_bucket_1995_2015(paymentyear)
  ) %>%
  filter(
    gid_0 %in% countries_iso3,
    paymentyear >= START_YEAR,
    paymentyear <= END_YEAR,
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

frac_data_annual <- read_csv(
  "00_rawdata/DPI2020.csv",
  show_col_types = FALSE
) %>%
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
  filter(
    countryname %in% donor_countries & year >= START_YEAR & year <= END_YEAR
  ) %>%
  select(donor = countryname, year, frac_full)

print_year_support("DPI fractionalization support", frac_data_annual$year)
assert_year_coverage(
  "DPI fractionalization support",
  frac_data_annual$year,
  START_YEAR:END_YEAR
)

votes <- read_csv(
  "00_rawdata/pdfs/wb_vote_shares.csv",
  show_col_types = FALSE
) %>%
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
  complete(Country = unique(Country), year = START_YEAR:END_YEAR) %>%
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
  mutate(year = map_to_5year_bucket_1995_2015(year)) %>%
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

  iv_data
}

panel_aid_admin2 <- read_csv(
  here("01_panel_data", paste0("panel_aid_admin2", PANEL_TAG, ".csv")),
  show_col_types = FALSE
)
panel_aid_admin1 <- read_csv(
  here("01_panel_data", paste0("panel_aid_admin1", PANEL_TAG, ".csv")),
  show_col_types = FALSE
)

admin2_results <- construct_shift_share_iv_bucketed(
  aid_data = aid_data,
  frac_data = frac_data,
  panel_data = panel_aid_admin2,
  region_id = "GID_2"
)
panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(admin2_results, by = c("GID_2" = "region_id", "year"))

admin1_results <- construct_shift_share_iv_bucketed(
  aid_data = aid_data,
  frac_data = frac_data,
  panel_data = panel_aid_admin1,
  region_id = "GID_1"
)
panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(admin1_results, by = c("GID_1" = "region_id", "year"))

write_csv(
  panel_aid_admin2,
  here("01_panel_data", paste0("panel_aid_admin2_fin", PANEL_TAG, ".csv"))
)
write_csv(
  panel_aid_admin1,
  here("01_panel_data", paste0("panel_aid_admin1_fin", PANEL_TAG, ".csv"))
)

message("Finished IV construction for ", PANEL_TAG, ".")
