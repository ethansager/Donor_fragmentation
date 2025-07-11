#population grid GWP
# Set up and packages  ----------------------------------------------------
setwd("C:/Users/eman7/Dropbox/GitHub/ra_work/Donor_fragmentation/")

if (!require("pacman")) 
  install.packages("pacman"); 

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
#### Admin 2 

# Population grid GWP
# Load population data for each year
# Define years of interest
yrs <- c(2005, 2010, 2015)

# Load administrative boundaries once instead of multiple times
shp_admin2 <- sf::st_as_sf(geodata::gadm(country = countries_iso3, level = 2, path = "00_rawdata/shapefiles/"))
shp_admin1 <- sf::st_as_sf(geodata::gadm(country = countries_iso3, level = 1, path = "00_rawdata/shapefiles/"))

# Union of boundaries for cropping

# Initialize list to store results
admin2_pop_list <- list()
admin1_pop_list <- list()

for (yr in yrs) {
  # Load population raster
  pop <- geodata::population(yr, res = "10", path = tempdir())
  
  # Crop and mask to relevant area
  admin_bounds <- sf::st_transform(shp_admin1, terra::crs(pop))
  pop <- terra::crop(pop, sf::st_bbox(admin_bounds))
  pop <- terra::mask(pop, admin_bounds)
  
  # Extract population for Admin2
  avg_pop_admin2 <- terra::extract(pop, shp_admin2, fun = mean, na.rm = TRUE, exact = TRUE)
  temp_admin2 <- data.frame(
    GID_2 = shp_admin2$GID_2,
    year = yr,
    pop = avg_pop_admin2[, 2]  # Extract correct column
  )
  admin2_pop_list[[as.character(yr)]] <- temp_admin2
  
  # Extract population for Admin1
  avg_pop_admin1 <- terra::extract(pop, shp_admin1, fun = mean, na.rm = TRUE, exact = TRUE)
  temp_admin1 <- data.frame(
    GID_1 = shp_admin1$GID_1,
    year = yr,
    pop = avg_pop_admin1[, 2]  # Extract correct column
  )
  admin1_pop_list[[as.character(yr)]] <- temp_admin1
}

# Bind all results into final data frames
admin2_pop <- bind_rows(admin2_pop_list)
admin1_pop <- bind_rows(admin1_pop_list)

# Interpolate population for missing years
# First interpolate population for missing years
admin2_pop <- admin2_pop %>%
  group_by(GID_2) %>%
  complete(year = 2005:2015) %>%
  mutate(
    pop = na.approx(pop, x = year, na.rm = FALSE, rule = 2),
    group_yr = case_when(
      year %in% 2005:2008 ~ 1,
      year %in% 2009:2011 ~ 2,
      year %in% 2012:2015 ~ 3
    )
  ) 

admin1_pop <- admin1_pop %>%
  group_by(GID_1) %>%
  complete(year = 2005:2015) %>%
  mutate(
    pop = na.approx(pop, x = year, na.rm = FALSE, rule = 2),
    group_yr = case_when(
      year %in% 2005:2008 ~ 1,
      year %in% 2009:2011 ~ 2,
      year %in% 2012:2015 ~ 3
    )
  )

# Admin1 
write_csv(admin1_pop, here("00_rawdata", "population", "admin1_population.csv"))
# Admin2
write_csv(admin2_pop, here("00_rawdata", "population", "admin2_population.csv"))
