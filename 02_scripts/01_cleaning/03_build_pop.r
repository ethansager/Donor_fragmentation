# population grid GWP
# Set up and packages  ----------------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  here,
  readr,
  data.table,
  ### GEO Packages
  sf,
  terra,
  exactextractr
)

# Load utility functions
source(here("02_scripts", "utils", "spatial_processing_utils.r"))

# Population grid WorldPop ----------------------------------------------
# Process population data for both admin levels using utility function
admin_results <- process_admin_levels(
  admin1_shp_path = "00_rawdata/shapefiles/gadm_admin1.shp",
  admin2_shp_path = "00_rawdata/shapefiles/gadm_admin2.shp",
  raster_dir = "00_rawdata/population",
  stats = c("sum"),
  crop_to_extent = TRUE
)

# Save processed population data
admin_results$admin1 %>%
  st_drop_geometry() %>%
  write_csv(., here("00_rawdata", "population", "admin1_population.csv"))

admin_results$admin2 %>%
  st_drop_geometry() %>%
  write_csv(., here("00_rawdata", "population", "admin2_population.csv"))
