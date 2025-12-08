# Build Dependent Variables
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
  scales,
  psych,
  tidylog,
  ### GEO Packages
  sf,
  spdep,
  spatstat,
  geodata,
  terra,
  exactextractr,
  zoo
)

# Load utility functions
source(here("scripts", "utils", "spatial_processing_utils.r"))

# Process nightlight data for both admin levels
admin_results <- process_admin_levels(
  admin1_shp_path = "00_rawdata/shapefiles/gadm_admin1.shp",
  admin2_shp_path = "00_rawdata/shapefiles/gadm_admin2.shp",
  raster_dir = "00_rawdata/nightlights/africa",
  stats = c("sum", "mean"),
  crop_to_extent = FALSE
)

ssa_admin1 <- admin_results$admin1
ssa_admin2 <- admin_results$admin2

# Save processed nightlight data
ssa_admin1 %>%
  st_drop_geometry() %>%
  write_csv(., here("00_rawdata", "nightlights", "topcodefix", "processed_topcodefix_nl_admin1.csv"))

ssa_admin2 %>%
  st_drop_geometry() %>%
  write_csv(., here("00_rawdata", "nightlights", "topcodefix", "processed_topcodefix_nl_admin2.csv"))

### Now we estimate average under 5 mortality rates for each admin level
years <- 2005:2015

# Extract U5M data for admin1
ssa_admin1 <- extract_multiband_stats(
  raster_path = "00_rawdata/under_5/IHME_LMICS_U5M_2000_2017_Q_UNDER5_MEAN_Y2019M10D16.TIF",
  shapefile = ssa_admin1,
  years = years,
  start_year = 2000,
  stat = "mean",
  var_prefix = "u5m"
)

# Extract U5M data for admin2
ssa_admin2 <- extract_multiband_stats(
  raster_path = "00_rawdata/under_5/IHME_LMICS_U5M_2000_2017_Q_UNDER5_MEAN_Y2019M10D16.TIF",
  shapefile = ssa_admin2,
  years = years,
  start_year = 2000,
  stat = "mean",
  var_prefix = "u5m"
)

## Build the final admin 1 panel data
ssa_admin1_panel <- build_panel_data(
  data = ssa_admin1,
  id_cols = c("GID_0", "GID_1"),
  year_range = years,
  value_prefixes = c("mean", "u5m", "sum"),
  admin_suffix = "admin1"
)

ssa_admin1_panel %>%
  st_drop_geometry() %>%
  write_csv(., here("00_rawdata", "processed_dep_vars_admin1.csv"))

## Build the final admin 2 panel data
ssa_admin2_panel <- build_panel_data(
  data = ssa_admin2,
  id_cols = c("GID_0", "GID_1", "GID_2"),
  year_range = years,
  value_prefixes = c("mean", "u5m", "sum"),
  admin_suffix = "admin2"
)

ssa_admin2_panel %>%
  st_drop_geometry() %>%
  write_csv(., here("00_rawdata", "processed_dep_vars_admin2.csv"))
