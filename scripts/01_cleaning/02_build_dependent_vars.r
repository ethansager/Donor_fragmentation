# Build Dependent Variables 
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

### Now we estimate average under 5 mortality rates for each admin level

u5m_raster <- terra::rast("00_rawdata/under_5/IHME_LMICS_U5M_2000_2017_Q_UNDER5_MEAN_Y2019M10D16.TIF")

years <- 2000:2017
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

#### Now do the same for GDP per capita

gdp_raster <- terra::rast("00_rawdata/gdp/rast_gdpTot_1990_2022_5arcmin.tif")

years <- 1990:2022

gdp_values <- list()

for (year in years) {
    # Extract values for the specific year using "sum" to aggregate GDP at the admin level
    gdp_values[[paste0("gdp_", year)]] <- exactextractr::exact_extract(gdp_raster[[year - 1990 + 1]], ssa_admin1, "sum")
}

gdp_df <- as.data.frame(gdp_values)

# Bind GDP values to the main dataframe
ssa_admin1 <- cbind(ssa_admin1, gdp_df)  

### Admin 2
gdp_values <- list()

for (year in years) {
    # Extract values for the specific year
    gdp_values[[paste0("gdp_", year)]] <- exactextractr::exact_extract(gdp_raster[[year - 1990 + 1]], ssa_admin2, "sum")
}

gdp_df <- as.data.frame(gdp_values)

# Bind GDP values to the main dataframe
ssa_admin2 <- cbind(ssa_admin2, gdp_df)  

## Build the final admin 1 panel data
ssa_admin1 <- ssa_admin1 %>%
  select(GID_0, GID_1, 
         mean_2005:mean_2015,
         u5m_2005:u5m_2015, 
         gdp_2005:gdp_2015)%>%
  pivot_longer(c(mean_2005:mean_2015, 
                 u5m_2005:u5m_2015, 
                 gdp_2005:gdp_2015), 
               names_to = c(".value", "year"),
               names_pattern = "(mean|u5m|gdp)_(\\d+)",
               values_to = c("mean_nl", "u5m_admin1", "gdp_admin1"))%>%
  mutate(year = as.numeric(year))

ssa_admin1%>%
  st_drop_geometry()%>%
  write_csv(., here("00_rawdata", "processed_dep_vars_admin1.csv"))

## Build the final admin 1 panel data
ssa_admin2 <- ssa_admin2 %>%
  distinct(GID_0, GID_1, GID_2, .keep_all = TRUE) %>%
  select(GID_0, GID_1, GID_2,
         mean_2005:mean_2015,
         u5m_2005 : u5m_2015,
         gdp_2005 : gdp_2015) %>%
  pivot_longer(c(mean_2005 : mean_2015,
                 u5m_2005 : u5m_2015,
                 gdp_2005 : gdp_2015),
               names_to = c(".value", "year"),
               names_pattern = "(mean|u5m|gdp)_(\\d+)",
               values_to = c("mean_nl", "u5m_admin2", "gdp_admin2")) %>%
  mutate(year = as.numeric(year))

# Summing GDP by GID_0
gdp_by_gid0 <- ssa_admin2 %>%
  st_drop_geometry()%>%
  filter(year == 2015) %>%
    group_by(GID_0) %>%
    summarize(total_gdp = sum(gdp, na.rm = TRUE), .groups = "drop")


ssa_admin2%>%
  st_drop_geometry()%>%
  write_csv(., here("00_rawdata", "processed_dep_vars_admin2.csv"))