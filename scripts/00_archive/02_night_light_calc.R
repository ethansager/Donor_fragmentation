if (!require("pacman")) 
  install.packages("pacman"); 

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

# Create lookup tables for each wave
wave3_countries <- setNames(
  c("Benin", "Botswana", "Cape Verde", "Ghana", "Kenya", "Lesotho", "Madagascar", 
    "Malawi", "Mali", "Mozambique", "Namibia", "Nigeria", "Senegal", "South Africa",
    "Tanzania", "Uganda", "Zambia", "Zimbabwe"), 1:18)

wave4_countries <- setNames(
  c("Benin", "Botswana", "Burkina Faso", "Cape Verde", "Ghana", "Kenya", "Lesotho",
    "Liberia", "Madagascar", "Malawi", "Mali", "Mozambique", "Namibia", "Nigeria",
    "Senegal", "South Africa", "Tanzania", "Uganda", "Zambia", "Zimbabwe"), 1:20)

wave5_countries <- setNames(
  c("Algeria", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", 
    "Cape Verde", "Cote d'Ivoire", "Egypt", NA, "Ghana", "Guinea", "Kenya",
    "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritius", "Morocco",
    "Mozambique", "Namibia", "Niger", "Nigeria", "Senegal", "Sierra Leone",
    "South Africa", "Sudan", "Swaziland", "Tanzania", "Togo", "Tunisia",
    "Uganda", "Zambia", "Zimbabwe"), 1:35)

wave6_countries <- setNames(
  c("Algeria", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon",
    "Cape Verde", "Cote d'Ivoire", "Egypt", "Gabon", "Ghana", "Guinea", "Kenya",
    "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritius", "Morocco",
    "Mozambique", "Namibia", "Niger", "Nigeria", "São Tomé and Príncipe",
    "Senegal", "Sierra Leone", "South Africa", "Sudan", "Swaziland", "Tanzania",
    "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe"), 1:36)

# Get country names for GADM
gadm_countries <- unique(unlist(list(wave3_countries, wave4_countries, wave5_countries, wave6_countries)))
countries <- gadm_countries[!is.na(gadm_countries)]

# admin 1 
ssa_admin1<-sf::st_as_sf(geodata::gadm(country = countries, level = 1, path = "C:/Users/eman7/OneDrive/Desktop/shapefiles/"))

raster_files <- list.files(here("00_rawdata", "nightlights", "clean_dmsp"), pattern = "\\.tif$", full.names = TRUE)

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

ssa_admin1%>%
  st_drop_geometry()%>%
  write_csv(., here("00_rawdata", "nightlights", "topcodefix", "processed_topcodefix_nl_admin1.csv"))

# admin 2
ssa_admin2<-sf::st_as_sf(geodata::gadm(country = countries, level = 2, path = "C:/Users/eman7/OneDrive/Desktop/shapefiles/"))


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

ssa_admin2%>%
  st_drop_geometry()%>%
  write_csv(., here("00_rawdata", "nightlights", "topcodefix", "processed_topcodefix_nl_admin2.csv"))

