# Spatial Processing Utility Functions
# Functions for processing raster and shapefile data with zonal statistics

#' Load and transform administrative shapefile
#'
#' @param shapefile_path Path to the shapefile
#' @param crs Target coordinate reference system (default: 4326)
#' @return sf object with transformed CRS
load_admin_shapefile <- function(shapefile_path, crs = 4326) {
  read_sf(shapefile_path) %>%
    st_transform(crs)
}

#' Extract zonal statistics from rasters for multiple years
#'
#' @param shapefile sf object with administrative boundaries
#' @param raster_dir Directory containing raster files
#' @param pattern File pattern to match raster files (default: "\\.tif$")
#' @param stats Vector of statistics to compute (e.g., c("sum", "mean"))
#' @param crop_to_extent Whether to crop raster to shapefile extent (default: FALSE)
#' @return sf object with added columns for each year and statistic
extract_zonal_stats <- function(shapefile, raster_dir, pattern = "\\.tif$", stats = c("sum", "mean"), crop_to_extent = FALSE) {
  # Ensure required packages are loaded
  require(terra)
  require(exactextractr)
  require(here)
  
  # List all raster files
  raster_files <- list.files(here(raster_dir), pattern = pattern, full.names = TRUE)
  
  # Extract years from filenames (assuming year is a 4-digit number)
  file_years <- as.numeric(gsub(".*?(\\d{4}).*", "\\1", raster_files))
  
  # Combine files and years into a data frame, then sort by year
  raster_data <- data.frame(file = raster_files, year = file_years)
  raster_data <- raster_data[order(raster_data$year), ]
  
  # Process each raster file
  for (i in seq_along(raster_data$file)) {
    raster_file <- raster_data$file[i]
    year_label <- raster_data$year[i]
    
    # Load the raster
    raster_obj <- rast(raster_file)
    
    # Optionally crop the raster to the extent of shapefile
    if (crop_to_extent) {
      raster_obj <- crop(raster_obj, vect(shapefile))
    }
    
    # Compute zonal statistics for each requested statistic
    for (stat in stats) {
      zonal_result <- exactextractr::exact_extract(
        raster_obj,
        shapefile,
        stat
      )
      
      # Assign statistics to the shapefile columns
      shapefile[[paste0(stat, "_", year_label)]] <- zonal_result
    }
  }
  
  return(shapefile)
}

#' Process both admin1 and admin2 shapefiles with raster data
#'
#' @param admin1_shp_path Path to admin1 shapefile
#' @param admin2_shp_path Path to admin2 shapefile
#' @param raster_dir Directory containing raster files
#' @param pattern File pattern to match raster files
#' @param stats Vector of statistics to compute
#' @param crop_to_extent Whether to crop raster to shapefile extent
#' @return List with admin1 and admin2 sf objects
process_admin_levels <- function(admin1_shp_path, admin2_shp_path, raster_dir, 
                                 pattern = "\\.tif$", stats = c("sum", "mean"), 
                                 crop_to_extent = FALSE) {
  # Load shapefiles
  admin1 <- load_admin_shapefile(admin1_shp_path)
  admin2 <- load_admin_shapefile(admin2_shp_path)
  
  # Process admin1
  admin1_processed <- extract_zonal_stats(
    admin1, 
    raster_dir, 
    pattern, 
    stats, 
    crop_to_extent
  )
  
  # Process admin2
  admin2_processed <- extract_zonal_stats(
    admin2, 
    raster_dir, 
    pattern, 
    stats, 
    crop_to_extent
  )
  
  return(list(admin1 = admin1_processed, admin2 = admin2_processed))
}

#' Extract multi-band raster values for a range of years
#'
#' @param raster_path Path to multi-band raster file
#' @param shapefile sf object with administrative boundaries
#' @param years Vector of years to extract
#' @param start_year Starting year of the raster bands (e.g., 2000)
#' @param stat Statistic to compute (default: "mean")
#' @param var_prefix Prefix for output column names (e.g., "u5m")
#' @return sf object with added columns for each year
extract_multiband_stats <- function(raster_path, shapefile, years, start_year, stat = "mean", var_prefix = "var") {
  # Validate inputs
  if (!is.numeric(years) || length(years) == 0) {
    stop("years must be a non-empty numeric vector")
  }
  if (!is.numeric(start_year) || length(start_year) != 1) {
    stop("start_year must be a single numeric value")
  }
  
  # Load the multi-band raster
  raster_obj <- terra::rast(raster_path)
  
  # Validate band indices
  band_indices <- years - start_year + 1
  min_band <- min(band_indices)
  max_band <- max(band_indices)
  
  if (min_band < 1) {
    stop(sprintf("All years must be >= start_year (%d). Minimum year provided: %d", start_year, min(years)))
  }
  if (max_band > terra::nlyr(raster_obj)) {
    stop(sprintf("Requested band index %d exceeds available bands (%d)", max_band, terra::nlyr(raster_obj)))
  }
  
  # Extract values for each year
  values_list <- list()
  for (year in years) {
    band_index <- year - start_year + 1
    values_list[[paste0(var_prefix, "_", year)]] <- exactextractr::exact_extract(
      raster_obj[[band_index]], 
      shapefile, 
      stat
    )
  }
  
  # Convert to data frame and bind to shapefile
  values_df <- as.data.frame(values_list)
  result <- cbind(shapefile, values_df)
  
  return(result)
}

#' Build panel data from wide format with year-specific columns
#'
#' @param data Data frame in wide format with year-specific columns
#' @param id_cols Vector of ID column names to keep
#' @param year_range Vector of years to include (e.g., 2005:2015)
#' @param value_prefixes Vector of value prefixes to pivot (e.g., c("mean", "u5m", "sum"))
#' @param admin_suffix Suffix for admin-level specific output columns (e.g., "admin1")
#' @return Data frame in long format
build_panel_data <- function(data, id_cols, year_range, value_prefixes, admin_suffix = NULL) {
  require(tidyverse)
  
  # Extract regex patterns to avoid duplication
  col_pattern <- paste0("^(", paste(value_prefixes, collapse = "|"), ")_\\d{4}$")
  name_pattern <- paste0("(", paste(value_prefixes, collapse = "|"), ")_(\\d+)")
  
  # Select and pivot
  result <- data %>%
    select(all_of(id_cols), matches(col_pattern)) %>%
    pivot_longer(
      cols = matches(col_pattern),
      names_to = c(".value", "year"),
      names_pattern = name_pattern
    ) %>%
    mutate(year = as.numeric(year)) %>%
    filter(!is.na(year))
  
  return(result)
}
