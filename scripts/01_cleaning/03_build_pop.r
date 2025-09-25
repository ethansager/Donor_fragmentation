# population grid GWP
# Set up and packages  ----------------------------------------------------
setwd("C:/Users/eman7/Dropbox/GitHub/ra_work/Donor_fragmentation/")

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
  terra
)

# Population grid WorldPop ----------------------------------------------
# Load population data for each year
# Define years of interest
yrs <- c(2005:2015)


# Load administrative boundaries once instead of multiple times
# admin 2
shp_admin2 <- read_sf("00_rawdata/shapefiles/gadm_admin2.shp") %>%
  st_transform(4326)

# admin 1
shp_admin1 <- read_sf("00_rawdata/shapefiles/gadm_admin1.shp") %>%
  st_transform(4326)

# List all population raster files
pop_files <- list.files(here("00_rawdata", "population"), pattern = "\\.tif$", full.names = TRUE)

# Extract years from filenames (assuming year is a 4-digit number)
file_years <- as.numeric(gsub(".*?(\\d{4}).*", "\\1", pop_files))

# Combine files and years into a data frame, then sort by year
raster_data <- data.frame(file = pop_files, year = file_years)
raster_data <- raster_data[order(raster_data$year), ]

#-------------------------------------------------------------------------#
# admin 1
#-------------------------------------------------------------------------#
ssa_admin1 <- read_sf("00_rawdata/shapefiles/gadm_admin1.shp") %>%
  st_transform(4326)

for (i in seq_along(raster_data$file)) {
  raster_file <- raster_data$file[i]
  year_label <- raster_data$year[i]

  # Load the raster
  pop_raster <- rast(raster_file)
  # Crop the raster to the extent of ssa_admin1
  pop_raster <- crop(pop_raster, vect(ssa_admin1))

  # Compute zonal statistics
  zonal_stats <- exactextractr::exact_extract(
    pop_raster,
    ssa_admin1,
    "sum"
  )

  # Assign statistics to the shapefile columns
  ssa_admin1[[paste0("sum_", year_label)]] <- zonal_stats
}

# Admin1
ssa_admin1 %>%
  st_drop_geometry() %>%
  write_csv(., here("00_rawdata", "population", "admin1_population.csv"))


#-------------------------------------------------------------------------#
# admin 2
#-------------------------------------------------------------------------#
ssa_admin2 <- read_sf("00_rawdata/shapefiles/gadm_admin2.shp") %>%
  st_transform(4326)

for (i in seq_along(raster_data$file)) {
  raster_file <- raster_data$file[i]
  year_label <- raster_data$year[i]

  pop_raster <- rast(raster_file)
  # Crop the raster to the extent of ssa_admin2
  pop_raster <- crop(pop_raster, vect(ssa_admin2))

  # Compute zonal statistics
  zonal_stats <- exactextractr::exact_extract(
    pop_raster,
    ssa_admin2,
    "sum"
  )

  # Assign statistics to the shapefile columns
  ssa_admin2[[paste0("sum_", year_label)]] <- zonal_stats
}

# Admin2
ssa_admin2 %>%
  st_drop_geometry() %>%
  write_csv(., here("00_rawdata", "population", "admin2_population.csv"))
