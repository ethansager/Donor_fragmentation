# Cleaning datasets script
# Set up and packages  ----------------------------------------------------

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
  ### GEO Packages
  sf,
  geodata,
  terra,
  nngeo
)

## Load dat
dat <- read_csv(here("00_rawdata", "GODAD_projectlevel.csv"))

# Note the below functions are sourced from utils
source("02_scripts/utils/godad_processing_utils.r")

# get country isos for gadm
countries_iso3 <- read_sf("00_rawdata/shapefiles/gadm_admin2.shp") %>%
  st_drop_geometry() %>%
  select(GID_0) %>%
  distinct() %>%
  pull()

# Round 3	2005-2006	18
# Round 4	2008-2009	20
# Round 5	2011-2013	34
# Round 6	2014-2015	36

# Clean CRS data as it does not have IATI codes
dat <- dat %>%
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
    group_yr = case_when(
      paymentyear %in% 2005:2008 ~ 1,
      paymentyear %in% 2009:2011 ~ 2,
      paymentyear %in% 2012:2015 ~ 3
    )
  ) %>%
  mutate(
    world_bank = ifelse(
      donor == "World Bank" & !precision_code %in% c("1", "2", "3"),
      1,
      0
    )
  ) %>%
  filter(
    gid_0 %in%
      countries_iso3 &
      paymentyear >= 2005 &
      paymentyear <= 2015 &
      !is.na(latitude) &
      !is.na(longitude) &
      world_bank == 0
  )

# Attach study_group using mapping based on sector_main
sector_info <- apply_sector_mapping(dat)
dat <- sector_info$dat
aid_type_codes <- sector_info$aid_type_codes

summary(dat)


# This gives us 33139 aid projects

shp <- read_sf("00_rawdata/shapefiles/gadm_admin2.shp") %>%
  st_make_valid() %>% # Ensure geometries are valid
  st_transform(4326) # Transform to WGS84 CRS

shp <- sf::st_as_sf(shp)

# Convert aid dataset to an sf object
aid_points <- st_as_sf(
  dat,
  coords = c("longitude", "latitude"), # Specify longitude and latitude columns
  crs = 4326 # Set CRS (WGS84)
)

# Perform a spatial join to link aid points to admin-2 polygons
dat <- st_join(aid_points, st_make_valid(shp))

# Convert dat to data.table for speed
setDT(dat)

# population grid GWP

admin1_pop <- read_csv("00_rawdata/population/admin1_population.csv") %>%
  pivot_longer(
    cols = starts_with("sum_"),
    names_to = "year",
    values_to = "pop"
  ) %>%
  select(GID_1, year, pop) %>%
  mutate(
    year = as.numeric(gsub("sum_", "", year)),
    ln_pop = log(pop)
  )

admin2_pop <- read_csv("00_rawdata/population/admin2_population.csv") %>%
  pivot_longer(
    cols = starts_with("sum_"),
    names_to = "year",
    values_to = "pop"
  ) %>%
  select(GID_2, year, pop) %>%
  mutate(
    year = as.numeric(gsub("sum_", "", year)),
    ln_pop = log(pop)
  )

###### Read in Afro data and create panel to match ########

admin1_afro <- read_csv("00_rawdata/ab_raw/processed/admin1_afro_panel.csv") %>%
  select(year, GID_0, GID_1, starts_with("mean_"), afro_count, wave)

admin2_afro <- read_csv("00_rawdata/ab_raw/processed/admin2_afro_panel.csv") %>%
  select(year, GID_0, GID_2, starts_with("mean_"), afro_count, wave)

##### Read in dependent varaibles #####

admin1_dep_vars <- read_csv("00_rawdata/processed_dep_vars_admin1.csv")
admin2_dep_vars <- read_csv("00_rawdata/processed_dep_vars_admin2.csv")

###### WGI ######
wgi <- readxl::read_xlsx("00_rawdata/wgidataset.xlsx") %>%
  select(GID_0, year, ge_pct)


#------------------------------------------------------------------------------#
#  Run data for all panels ----
#------------------------------------------------------------------------------#

build_panels_for_type(dat, "all", "")

sector_admin1_list <- list()
sector_admin2_list <- list()


for (type_name in names(aid_type_codes)) {
  # Filter the data to the sector we want
  dat_type <- dat[study_group == type_name]
  sector_result <- build_panels_for_type(
    dat_type,
    type_name,
    paste0("_", type_name)
  )
  # Assign the type name
  sector_result$panel_aid_admin1_fin$study_group <- type_name
  sector_result$panel_aid_admin2_fin$study_group <- type_name
  # Assign to the list
  sector_admin1_list[[type_name]] <- sector_result$panel_aid_admin1_fin
  sector_admin2_list[[type_name]] <- sector_result$panel_aid_admin2_fin
}

#------------------------------------------------------------------------------#
#  Write out the final long dataset for ease of use----
#------------------------------------------------------------------------------#

if (length(sector_admin1_list) > 0) {
  panel_aid_admin1_by_sector <- bind_rows(sector_admin1_list)
  write_csv(
    panel_aid_admin1_by_sector,
    "01_panel_data/panel_aid_admin1_by_sector.csv"
  )
}

if (length(sector_admin2_list) > 0) {
  panel_aid_admin2_by_sector <- bind_rows(sector_admin2_list)
  write_csv(
    panel_aid_admin2_by_sector,
    "01_panel_data/panel_aid_admin2_by_sector.csv"
  )
}
