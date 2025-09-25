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
  exactextractr
)


# admin 1
ssa_admin1 <- read_sf("00_rawdata/shapefiles/gadm_admin1.shp") %>%
  st_transform(4326)

raster_files <- list.files(here("00_rawdata", "nightlights", "africa"), pattern = "\\.tif$", full.names = TRUE)

# Extract years from filenames (assuming year is a 4-digit number)
file_years <- as.numeric(gsub(".*?(\\d{4}).*", "\\1", raster_files))

# Combine files and years into a data frame, then sort by year
raster_data <- data.frame(file = raster_files, year = file_years)
raster_data <- raster_data[order(raster_data$year), ]


for (i in seq_along(raster_data$file)) {
  raster_file <- raster_data$file[i]
  year_label <- raster_data$year[i]

  # Load the raster
  nightlight_raster <- rast(raster_file)

  # Compute zonal statistics
  zonal_stats <- exactextractr::exact_extract(
    nightlight_raster,
    ssa_admin1,
    "sum"
  )

  # Assign statistics to the shapefile columns
  ssa_admin1[[paste0("sum_", year_label)]] <- zonal_stats

  # Compute zonal statistics
  zonal_stats2 <- exactextractr::exact_extract(
    nightlight_raster,
    ssa_admin1,
    "mean"
  )

  # Assign statistics to the shapefile columns
  ssa_admin1[[paste0("mean_", year_label)]] <- zonal_stats2
}

ssa_admin1 %>%
  st_drop_geometry() %>%
  write_csv(., here("00_rawdata", "nightlights", "topcodefix", "processed_topcodefix_nl_admin1.csv"))

# admin 2
ssa_admin2 <- read_sf("00_rawdata/shapefiles/gadm_admin2.shp") %>%
  st_transform(4326)

for (i in seq_along(raster_data$file)) {
  raster_file <- raster_data$file[i]
  year_label <- raster_data$year[i]

  # Load the raster
  nightlight_raster <- rast(raster_file)

  # Compute zonal statistics
  zonal_stats <- exactextractr::exact_extract(
    nightlight_raster,
    ssa_admin2,
    "sum"
  )

  # Assign statistics to the shapefile columns
  ssa_admin2[[paste0("sum_", year_label)]] <- zonal_stats

  # Compute zonal statistics
  zonal_stats2 <- exactextractr::exact_extract(
    nightlight_raster,
    ssa_admin2,
    "mean"
  )

  # Assign statistics to the shapefile columns
  ssa_admin2[[paste0("mean_", year_label)]] <- zonal_stats2
}

ssa_admin2 %>%
  st_drop_geometry() %>%
  write_csv(., here("00_rawdata", "nightlights", "topcodefix", "processed_topcodefix_nl_admin2.csv"))

### Now we estimate average under 5 mortality rates for each admin level

u5m_raster <- terra::rast("00_rawdata/under_5/IHME_LMICS_U5M_2000_2017_Q_UNDER5_MEAN_Y2019M10D16.TIF")

years <- 2000:2015
u5m_values <- list()

for (year in years) {
  # Extract values for the specific year
  u5m_values[[paste0("u5m_", year)]] <- exactextractr::exact_extract(u5m_raster[[year - 2000 + 1]], ssa_admin1, "mean")
}

u5m_df <- as.data.frame(u5m_values)
# Bind GDP values to the main dataframe
ssa_admin1 <- cbind(ssa_admin1, u5m_df)

### Admin 2
u5m_values <- list()

for (year in years) {
  # Extract values for the specific year
  u5m_values[[paste0("u5m_", year)]] <- exactextractr::exact_extract(u5m_raster[[year - 2000 + 1]], ssa_admin2, "mean")
}

u5m_df <- as.data.frame(u5m_values)
# Bind u5m values to the main dataframe
ssa_admin2 <- cbind(ssa_admin2, u5m_df)

## Build the final admin 1 panel data
ssa_admin1 <- ssa_admin1 %>%
  select(
    GID_0, GID_1,
    mean_2005:mean_2015,
    u5m_2005:u5m_2015,
    sum_2005:sum_2015
  ) %>%
  pivot_longer(
    c(
      mean_2005:mean_2015,
      u5m_2005:u5m_2015,
      sum_2005:sum_2015
    ),
    names_to = c(".value", "year"),
    names_pattern = "(mean|u5m|sum)_(\\d+)",
    values_to = c("mean_nl", "u5m_admin1", "sum_admin1")
  ) %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(year))

ssa_admin1 %>%
  st_drop_geometry() %>%
  write_csv(., here("00_rawdata", "processed_dep_vars_admin1.csv"))

## Build the final admin 1 panel data
ssa_admin2 <- ssa_admin2 %>%
  st_drop_geometry() %>%
  select(
    GID_0, GID_1, GID_2,
    mean_2005:mean_2015,
    u5m_2005:u5m_2015,
    sum_2005:sum_2015
  ) %>%
  pivot_longer(
    c(
      mean_2005:mean_2015,
      u5m_2005:u5m_2015,
      sum_2005:sum_2015
    ),
    names_to = c(".value", "year"),
    names_pattern = "(mean|u5m|sum)_(\\d+)",
    values_to = c("mean_nl", "u5m_admin2", "sum_admin2")
  ) %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(year))

ssa_admin2 %>%
  st_drop_geometry() %>%
  write_csv(., here("00_rawdata", "processed_dep_vars_admin2.csv"))
