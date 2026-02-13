# Cleaning datasets script (5-year buckets)
# Set up and packages ---------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  here,
  readr,
  janitor,
  scales,
  psych,
  tidylog,
  units,
  DescTools,
  zoo,
  data.table,
  sf,
  geodata,
  terra,
  nngeo
)

# Note the below functions are sourced from utils
source("02_scripts/utils/godad_processing_utils.r")

map_to_5year_bucket <- function(year) {
  dplyr::case_when(
    year %in% 2005:2009 ~ 2005,
    year %in% 2010:2014 ~ 2010,
    TRUE ~ NA_real_
  )
}

safe_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}

## Load data
raw_dat <- read_csv(here("00_rawdata", "GODAD_projectlevel.csv"))

# Get country isos for gadm
countries_iso3 <- read_sf("00_rawdata/shapefiles/gadm_admin2.shp") %>%
  st_drop_geometry() %>%
  select(GID_0) %>%
  distinct() %>%
  pull()

# Round 3 2005-2006
# Round 4 2008-2009
# Round 5 2011-2013
# Round 6 2014-2015

# Clean CRS data as it does not have IATI codes
raw_dat <- raw_dat %>%
  mutate(
    sector_main = as.numeric(str_split_fixed(sector_codes, "\\|", 2)[, 1]),
    early_impact = case_when(
      sector_main %in% 110:160 ~ 0,
      sector_main %in% 210:230 ~ 1,
      sector_main %in% 231 ~ 0,
      sector_main %in% 232:323 ~ 1,
      sector_main %in% 331:998 ~ 0,
      TRUE ~ NA
    ),
    bucket_5year = map_to_5year_bucket(paymentyear),
    world_bank = ifelse(
      donor == "World Bank" & !precision_code %in% c("1", "2", "3"),
      1,
      0
    )
  ) %>%
  filter(
    gid_0 %in% countries_iso3,
    paymentyear >= 2005,
    paymentyear <= 2014,
    !is.na(latitude),
    !is.na(longitude),
    !is.na(bucket_5year),
    world_bank == 0
  ) %>%
  mutate(paymentyear = bucket_5year)

# Attach study_group using mapping based on sector_main
sector_info <- apply_sector_mapping(raw_dat)
raw_dat <- sector_info$dat
aid_type_codes <- sector_info$aid_type_codes

shp <- read_sf("00_rawdata/shapefiles/gadm_admin2.shp") %>%
  st_make_valid() %>%
  st_transform(4326)

# Convert aid dataset to an sf object
aid_points <- st_as_sf(
  raw_dat,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Perform a spatial join to link aid points to admin-2 polygons
dat <- st_join(aid_points, st_make_valid(shp))

# Convert dat to data.table for speed
setDT(dat)

# Population grid GWP (bucketed)
admin1_pop <- read_csv("00_rawdata/population/admin1_population.csv") %>%
  pivot_longer(
    cols = starts_with("sum_"),
    names_to = "year",
    values_to = "pop"
  ) %>%
  select(GID_1, year, pop) %>%
  mutate(
    year = as.numeric(gsub("sum_", "", year)),
    year = map_to_5year_bucket(year)
  ) %>%
  filter(!is.na(year)) %>%
  group_by(GID_1, year) %>%
  summarise(pop = safe_mean(pop), .groups = "drop") %>%
  mutate(ln_pop = log(pop))

admin2_pop <- read_csv("00_rawdata/population/admin2_population.csv") %>%
  pivot_longer(
    cols = starts_with("sum_"),
    names_to = "year",
    values_to = "pop"
  ) %>%
  select(GID_2, year, pop) %>%
  mutate(
    year = as.numeric(gsub("sum_", "", year)),
    year = map_to_5year_bucket(year)
  ) %>%
  filter(!is.na(year)) %>%
  group_by(GID_2, year) %>%
  summarise(pop = safe_mean(pop), .groups = "drop") %>%
  mutate(ln_pop = log(pop))

###### Read in Afro data and create panel to match ########

admin1_afro <- read_csv("00_rawdata/ab_raw/processed/admin1_afro_panel.csv") %>%
  select(year, GID_0, GID_1, starts_with("mean_"), afro_count, wave) %>%
  mutate(year = map_to_5year_bucket(year)) %>%
  filter(!is.na(year)) %>%
  group_by(year, GID_0, GID_1) %>%
  summarise(
    across(starts_with("mean_"), ~ safe_mean(.x)),
    afro_count = safe_mean(afro_count),
    wave = safe_max(wave),
    .groups = "drop"
  )

admin2_afro <- read_csv("00_rawdata/ab_raw/processed/admin2_afro_panel.csv") %>%
  select(year, GID_0, GID_2, starts_with("mean_"), afro_count, wave) %>%
  mutate(year = map_to_5year_bucket(year)) %>%
  filter(!is.na(year)) %>%
  group_by(year, GID_0, GID_2) %>%
  summarise(
    across(starts_with("mean_"), ~ safe_mean(.x)),
    afro_count = safe_mean(afro_count),
    wave = safe_max(wave),
    .groups = "drop"
  )

##### Read in dependent variables (bucketed) #####

admin1_dep_vars <- read_csv("00_rawdata/processed_dep_vars_admin1.csv") %>%
  mutate(year = map_to_5year_bucket(year)) %>%
  filter(!is.na(year)) %>%
  group_by(GID_0, GID_1, year) %>%
  summarise(
    sum = safe_mean(sum),
    mean = safe_mean(mean),
    u5m = safe_mean(u5m),
    .groups = "drop"
  )

admin2_dep_vars <- read_csv("00_rawdata/processed_dep_vars_admin2.csv") %>%
  mutate(year = map_to_5year_bucket(year)) %>%
  filter(!is.na(year)) %>%
  group_by(GID_0, GID_1, GID_2, year) %>%
  summarise(
    sum = safe_mean(sum),
    mean = safe_mean(mean),
    u5m = safe_mean(u5m),
    .groups = "drop"
  )

###### WGI (bucketed) ######
wgi <- readxl::read_xlsx("00_rawdata/wgidataset.xlsx") %>%
  select(GID_0, year, ge_pct) %>%
  mutate(ge_pct = suppressWarnings(as.numeric(ge_pct))) %>%
  mutate(year = map_to_5year_bucket(year)) %>%
  filter(!is.na(year)) %>%
  group_by(GID_0, year) %>%
  summarise(ge_pct = safe_mean(ge_pct), .groups = "drop")

#------------------------------------------------------------------------------#
# Run data for all panels
#------------------------------------------------------------------------------#

build_panels_for_type(dat, "all", "_5year")

sector_admin1_list <- list()
sector_admin2_list <- list()

for (type_name in names(aid_type_codes)) {
  dat_type <- dat[study_group == type_name]
  sector_result <- build_panels_for_type(
    dat_type,
    type_name,
    paste0("_", type_name, "_5year")
  )

  sector_result$panel_aid_admin1_fin$study_group <- type_name
  sector_result$panel_aid_admin2_fin$study_group <- type_name

  sector_admin1_list[[type_name]] <- sector_result$panel_aid_admin1_fin
  sector_admin2_list[[type_name]] <- sector_result$panel_aid_admin2_fin
}

#------------------------------------------------------------------------------#
# Write out final long datasets
#------------------------------------------------------------------------------#

if (length(sector_admin1_list) > 0) {
  panel_aid_admin1_by_sector <- bind_rows(sector_admin1_list)
  write_csv(
    panel_aid_admin1_by_sector,
    "01_panel_data/panel_aid_admin1_by_sector_5year.csv"
  )
}

if (length(sector_admin2_list) > 0) {
  panel_aid_admin2_by_sector <- bind_rows(sector_admin2_list)
  write_csv(
    panel_aid_admin2_by_sector,
    "01_panel_data/panel_aid_admin2_by_sector_5year.csv"
  )
}
