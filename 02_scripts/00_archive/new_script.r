# Run cluster analysis for radius
setwd("C:/Users/eman7/Dropbox/GitHub/ra_work/Donor_fragmentation/")

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  here,
  tidylog,
  furrr,
  digest,
  ### GEO Packages
  sf,
  terra,
  exactextractr
)

## Load dat
dat <- read_csv(here("00_rawdata", "GODAD_projectlevel.csv"))

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
      paymentyear %in% 2012:2015 ~ 3,
    )
  ) %>%
  mutate(world_bank = ifelse(donor == "World Bank" & !precision_code %in% c("1", "2", "3"), 1, 0)) %>%
  filter(
    gid_0 %in%
      countries_iso3 &
      paymentyear >= 2005 &
      paymentyear <= 2015 &
      !is.na(latitude) &
      !is.na(longitude) &
      world_bank == 0
  )


# Convert aid dataset to an sf object
aid_points <- st_as_sf(
  dat,
  coords = c("longitude", "latitude"), # Specify longitude and latitude columns
  crs = 4326 # Set CRS (WGS84)
)

# # Load Afrobarometer data
# afro_points <- read_csv(here("00_rawdata", "ab_raw", "processed", "afrobarometer_w3_w6_geomerged_new.csv"))

# afro_points <- st_as_sf(
#   afro_points,
#   coords = c("longitude", "latitude"), # Specify longitude and latitude columns
#   crs = 4326 # Set CRS (WGS84)
# )


# # Convert projection to work in meters
# afro_points <- st_transform(afro_points, crs = 3395)

# # Add a 50 km buffer around each aid project
# afro_points$buffer_50km <- st_buffer(afro_points$geometry, dist = 50000)

# raster_files <- list.files(here("00_rawdata", "nightlights", "africa"), pattern = "\\.tif$", full.names = TRUE)

# # Extract years from filenames (assuming year is a 4-digit number)
# file_years <- as.numeric(gsub(".*?(\\d{4}).*", "\\1", raster_files))

# # Combine files and years into a data frame, then sort by year
# raster_data <- data.frame(file = raster_files, year = file_years)
# raster_data <- raster_data[order(raster_data$year), ]

# combined_buffer <- st_union(afro_points$buffer_50km)
# bbox_combined <- st_bbox(combined_buffer)

# process_raster <- function(raster_path, year, buffers, bbox) {
#   message("Processing year: ", year)

#   r <- terra::rast(raster_path)
#   r_crop <- terra::crop(r, bbox)

#   stats <- exactextractr::exact_extract(r_crop, buffers, c("sum", "mean"))

#   tibble(
#     id = seq_along(buffers),
#     year = year,
#     sum = stats$sum,
#     mean = stats$mean
#   )
# }


# # --- Run Raster Processing Sequentially ---
# results_list <- lapply(
#   seq_along(raster_data$file),
#   function(i) process_raster(raster_data$file[i], raster_data$year[i], buffers = afro_points$buffer_50km, bbox = bbox_combined)
# )

# # --- Combine to Long Format ---
# nightlight_stats <- bind_rows(results_list)

# # --- Pivot to Wide Format ---
# wide_stats <- nightlight_stats %>%
#   pivot_wider(
#     names_from = year,
#     values_from = c(sum, mean),
#     names_sep = "_"
#   )

# # --- Join Back to Aid Points ---
# afro_points <- bind_cols(afro_points, wide_stats[, -1]) # drop id

# # --- Optional: Save Output ---
# # st_write(afro_points, here("01_processed", "afro_points_with_nightlights.gpkg"))
# write_csv(afro_points, here("afro_points_with_nightlights.csv"))

# message("✅ Nightlight extraction complete.")

afro_points <- read_csv(here("afro_points_with_nightlights.csv"))

# Now we need to count the number of aid projects within each 50km buffer per year
afro_points <- st_transform(afro_points, crs = 4326)
aid_points <- st_transform(aid_points, crs = 4326)

# Ensure the buffer geometry has the same CRS as aid_points
afro_points$buffer_50km <- st_transform(afro_points$buffer_50km, crs = st_crs(aid_points))


afro_points <- afro_points %>%
  rowwise() %>%
  mutate(
    buffer_hash = digest(c(year, st_as_binary(buffer_50km)), algo = "xxhash64")
  ) %>%
  ungroup()

# Check
afro_points %>%
  group_by(year, buffer_50km) %>%
  select(year, buffer_50km) %>%
  n_distinct()


year_chunks <- split(afro_points, afro_points$year)


### TODO: Put the on for a project until the closing date in the dataset.
### TODO: We would split the size of the disbursement by the number of years it was active.

# Compute project counts per buffer-year combo
results <- map_dfr(names(year_chunks), function(y_str) {
  y <- as.integer(y_str)
  this_afro <- year_chunks[[y_str]]

  this_aid <- aid_points %>%
    filter(paymentyear == y) %>%
    mutate(GID_0 = gid_0)

  # Filter to only GID_0s present in afro_points this year
  valid_gid0s <- unique(this_afro$GID_0)
  this_aid <- this_aid %>% filter(GID_0 %in% valid_gid0s)

  # Get unique buffers with hash, and retain GID_0 for filtering
  unique_buffers <- this_afro %>%
    select(buffer_hash, buffer_50km, GID_0) %>%
    distinct(buffer_hash, .keep_all = TRUE)

  # For each unique buffer, count aid points in the same GID_0
  n_projects <- map2_int(
    unique_buffers$buffer_50km,
    unique_buffers$GID_0,
    ~ sum(st_intersects(.x, this_aid[this_aid$GID_0 == .y, ], sparse = FALSE))
  )
  # Count how many donors are in each buffer
  n_donors <- map2_int(
    unique_buffers$buffer_50km,
    unique_buffers$GID_0,
    ~ sum(st_intersects(.x, this_aid[this_aid$GID_0 == .y, ], sparse = FALSE))
  )

  # Add counts and year to unique_buffers
  unique_buffers <- unique_buffers %>%
    mutate(
      n_projects = n_projects,
      any_projects = n_projects > 0,
      year = y
    )

  # Join back to full afro set
  left_join(sf::st_drop_geometry(this_afro), sf::st_drop_geometry(unique_buffers), by = c("buffer_hash", "year"))
})


# Join results back to the full afro_points
afro_points_with_counts <- results %>%
  mutate(
    n_projects = coalesce(n_projects, 0),
    any_projects = coalesce(any_projects, FALSE)
  )

# Summary statistics for n_projects and any_projects
summary_stats <- afro_points_with_counts %>%
  group_by(year) %>%
  distinct(buffer_hash, .keep_all = TRUE) %>%
  summarise(
    total_buffers = n(),
    total_projects = sum(n_projects, na.rm = TRUE),
    buffers_with_projects = sum(any_projects, na.rm = TRUE),
    avg_projects_per_buffer = mean(n_projects, na.rm = TRUE),
    median_projects_per_buffer = median(n_projects, na.rm = TRUE),
    max_projects_in_buffer = max(n_projects, na.rm = TRUE)
  )

# Print summary statistics
print(summary_stats)


# Randomly sample a project

set.seed(1235) # For reproducibility
random_project <- afro_points %>%
  slice_sample(n = 3)

# Find points that fall within the buffer of the sampled project
random_project <- random_project %>%
  mutate(count = sum(st_intersects(buffer_50km, aid_points[aid_points$paymentyear == year, ], sparse = FALSE)))

points_within_buffer <- aid_points %>%
  filter(paymentyear == random_project$year) %>%
  filter(st_intersects(geometry, random_project$buffer_50km, sparse = FALSE))

# Make a map
library(leaflet)

# Create a leaflet map
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = random_project,
    color = "red",
    radius = 5,
    label = ~ paste("Random Project"),
    popup = ~ paste("Random Project")
  ) %>%
  addCircleMarkers(
    data = points_within_buffer,
    color = "blue",
    radius = 3,
    label = ~ paste("Aid Project"),
    popup = ~ paste("Aid Project")
  ) %>%
  addPolygons(
    data = st_as_sf(random_project$buffer_50km),
    color = "green",
    weight = 2,
    fillOpacity = 0.2,
    label = ~ paste("Buffer Zone"),
    popup = ~ paste("Buffer Zone")
  )

# Optional: Save summary statistics to a CSV file
write_csv(summary_stats, here("summary_statistics.csv"))
