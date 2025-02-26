setwd("C:/Users/eman7/OneDrive/Desktop/nightlights/topcodefix")

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

countries <- c("Nigeria", "Mozambique", "Malawi", "Kenya", "Ghana")

# admin 1 
ssa_admin1<-sf::st_as_sf(geodata::gadm(country = countries, level = 1, path = "C:/Users/eman7/OneDrive/Desktop/shapefiles/"))

raster_files <- list.files()

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
}

ssa_admin1%>%
  st_drop_geometry()%>%
  write_csv(., "processed_topcodefix_nl_admin1_sum.csv")

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
}

ssa_admin2%>%
  st_drop_geometry()%>%
  write_csv(., "processed_topcodefix_nl_admin2_sum.csv")
