# Cleaning datasets script 
# Set up and packages  ----------------------------------------------------
setwd("C:/Users/eman7/Dropbox/GitHub/ra_work/Donor_fragmentation/")

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
  units,
  DescTools,
  zoo,
  data.table,
  ### GEO Packages
  sf,
  spdep,
  spatstat,
  geodata,
  terra,
  giscoR,
  nngeo
)

## Load dat
dat<-read_csv(here("00_rawdata", "GODAD_projectlevel.csv"))

# get country isos for gadm 
countries_iso3 <- read_csv("00_rawdata/nightlights/topcodefix/processed_topcodefix_nl_admin1.csv")%>%
  select(GID_0)%>%
  distinct()%>%
  pull()

# Round 3	2005-2006	18	
# Round 4	2008-2009	20	
# Round 5	2011-2013	34	
# Round 6	2014-2015	36


# ============================================================================
# PREPARING GODAD DATA
# ============================================================================

# Clean CRS data as it does not have IATI codes
dat <- dat %>% 
  mutate(
    sector_main = as.numeric(str_split_fixed(sector_codes, "\\|", 2)[,1]),
    early_impact = case_when(
      sector_main %in% 110:160 ~ 0,
      sector_main %in% 210:230 ~ 1,
      sector_main %in% 231 ~ 0,
      sector_main %in% 232:323 ~ 1,
      sector_main %in% 331:998 ~ 0,
      TRUE ~ NA
    ),
    group_yr = case_when(
      paymentyear %in% 2000:2004 ~ 1,
      paymentyear %in% 2005:2009 ~ 2,
      paymentyear %in% 2010:2015 ~ 3,
      ))%>%
  filter(
           !is.na(gid_2)& 
           !is.na(disb_loc_evensplit) & 
           !is.na(paymentyear) & 
           disb != 0 & 
           gid_0 %in% countries_iso3 & 
           paymentyear >= 2000 & paymentyear <= 2015
         )
# This gives us 24000 aid projects 

shp <- geodata::gadm(country = countries_iso3, level = 2, path = "00_rawdata/shapefiles/")

shp <- sf::st_as_sf(shp)

# Step 3: Convert your aid dataset to an sf object
aid_points <- st_as_sf(
  dat,
  coords = c("longitude", "latitude"), # Specify longitude and latitude columns
  crs = 4326                           # Set CRS (WGS84)
)

# Step 4: Perform a spatial join to link aid points to admin-2 polygons
dat <- st_join(aid_points, st_make_valid(shp))

# Convert dat to data.table for speed
setDT(dat)

# Create HHI for admin2 level
hhi_results_admin2 <- dat[, .(
  total_disb_admin2 = sum(abs(disb_loc_evensplit), na.rm = TRUE),
  total_early_projects = sum(early_impact == 1, na.rm = TRUE),
  total_late_projects = sum(early_impact == 0, na.rm = TRUE)
  ), by = .(GID_0, GID_2, group_yr, donor)]

hhi_results_admin2 <- hhi_results_admin2[, {
  total_aid = sum(total_disb_admin2, na.rm = TRUE)
  list(
    total_early_admin2 = sum(total_early_projects),
    total_late_admin2 = sum(total_late_projects), 
    total_proj_admin2 = sum(total_early_projects + total_late_projects),
    total_aid_admin2 = total_aid,
    donor_count_admin2 = uniqueN(donor),
    hhi_admin2 = sum((total_disb_admin2 / total_aid)^2, na.rm = TRUE),
    frag_index_admin2 = 1 - sum((total_disb_admin2 / total_aid)^2, na.rm = TRUE),
    frag_1_admin2 = 1 - max(total_disb_admin2 / total_aid, na.rm = TRUE),
    frag_3_admin2 = 1 - sum(head(sort(total_disb_admin2 / total_aid, decreasing = TRUE), 3), na.rm = TRUE),
    frag_below10_admin2 = sum((total_disb_admin2 / total_aid) < 0.10, na.rm = TRUE)
  )
  }, by = .(GID_0, GID_2, group_yr)]

# Create HHI for admin1 level 
hhi_results_admin1 <- dat[, .(
  total_disb_admin1 = sum(abs(disb_loc_evensplit), na.rm = TRUE),
  total_early_projects = sum(early_impact == 1, na.rm = TRUE),
  total_late_projects = sum(early_impact == 0, na.rm = TRUE)
  ), by = .(GID_0, GID_1, group_yr, donor)]

hhi_results_admin1 <- hhi_results_admin1[, {
  total_aid = sum(total_disb_admin1, na.rm = TRUE)
  list(
    total_early_admin1 = sum(total_early_projects),
    total_late_admin1 = sum(total_late_projects),
    total_proj_admin1 = sum(total_early_projects + total_late_projects),
    total_aid_admin1 = total_aid,
    donor_count_admin1 = uniqueN(donor),
    hhi_admin1 = sum((total_disb_admin1 / total_aid)^2, na.rm = TRUE),
    frag_index_admin1 = 1 - sum((total_disb_admin1 / total_aid)^2, na.rm = TRUE),
    frag_1_admin1 = 1 - max(total_disb_admin1 / total_aid, na.rm = TRUE),
    frag_3_admin1 = 1 - sum(head(sort(total_disb_admin1 / total_aid, decreasing = TRUE), 3), na.rm = TRUE),
    frag_below10_admin1 = sum((total_disb_admin1 / total_aid) < 0.10, na.rm = TRUE)
  )
  }, by = .(GID_0, GID_1, group_yr)]

# Descriptives region count by admin level  

n_distinct(hhi_results_admin1$GID_1)
n_distinct(hhi_results_admin2$GID_2)

# Step 2: Merge the HHI results back into the main dataset

panel_aid_admin1 <- dat %>%
  sf::st_drop_geometry()%>%
  filter(!is.na(GID_1))%>%
  tidylog::left_join(hhi_results_admin1, by = c("GID_0", "GID_1", "group_yr"))%>%
  select(GID_0, GID_1, group_yr, ends_with("_admin1"), -contains("total_disb"))%>%
  distinct()

check <- panel_aid_admin1 %>%
  group_by(GID_0, GID_1, group_yr)%>%
  summarise(count = n())%>%
  filter(count > 1)

panel_aid_admin2 <- dat %>%
  sf::st_drop_geometry()%>%
  filter(!is.na(GID_1))%>%
  tidylog::left_join(hhi_results_admin2, by = c("GID_0", "GID_2", "group_yr"))%>%
  select(GID_0, GID_1, GID_2, group_yr, ends_with("_admin2"), -contains("total_disb"))%>%
  distinct()

check <- panel_aid_admin2 %>%
  group_by(GID_0, GID_1, GID_2, group_yr)%>%
  summarise(count = n())%>%
  filter(count > 1)
  
n_distinct(panel_aid_admin1$GID_1)
n_distinct(panel_aid_admin2$GID_2)

#population grid GWP
#### Admin 2 

# Population grid GWP
# Load population data for each year
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
      year %in% 2005:2009 ~ 2,
      year %in% 2010:2015 ~ 3
    )
  )%>%
  group_by(GID_2, group_yr)%>%
  mutate(avg_pop = mean(pop, na.rm = TRUE))%>%
  select(GID_2, group_yr, avg_pop)%>%
  distinct()

admin1_pop <- admin1_pop %>%
  group_by(GID_1) %>%
  complete(year = 2005:2015) %>%
  mutate(
    pop = na.approx(pop, x = year, na.rm = FALSE, rule = 2),
    group_yr = case_when(
      year %in% 2005:2009 ~ 2,
      year %in% 2010:2015 ~ 3
    )
  )%>%
  group_by(GID_1, group_yr)%>%
  mutate(avg_pop = mean(pop, na.rm = TRUE))%>%
  select(GID_1, group_yr, avg_pop)%>%
  distinct()

# Merge with panel data
panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(admin1_pop, by = c("GID_1", "group_yr")) %>%
  rename(pop_admin1 = avg_pop)

panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(admin2_pop, by = c("GID_2", "group_yr")) %>%
  rename(pop_admin2 = avg_pop)

# ============================================================================
# PREPARING AFROBAROMETER DATA
# ============================================================================
###### Read in Afro data and create panel to match ########

afro_merged <- read_csv("00_rawdata/ab_raw/processed/afrobarometer_w3_w6_geomerged.csv")
 
admin1_afro <-afro_merged%>%
  mutate(wave = if_else(wave %in% 3:4 , 1, 2))%>%
  group_by(GID_1, wave)%>%
  filter(!is.na(GID_1) & !is.na(wave))%>%
  sf::st_drop_geometry()%>%
  mutate(
            afro_count = n(),
            mean_sgq_admin1 = mean(sgqi, na.rm = TRUE),
            mean_svc_admin1 = mean(ea_svc_index, na.rm = TRUE),
            mean_fac_admin1 = mean(ea_fac_index, na.rm = TRUE)
  )%>%
  select(
    afro_count,
    wave,
    GID_0,
    GID_1,
    starts_with("mean")
  )%>%
  distinct()


# Create a complete panel of years for each GID_1.
admin1_afro <- admin1_afro %>%
  mutate(group_yr = case_when(
      wave == 1 ~ 2,
      wave == 2 ~ 3
      ))

### admin 2 
admin2_afro <-afro_merged%>%
  mutate(wave = if_else(wave %in% 3:4 , 1, 2))%>%
  group_by(GID_2, wave)%>%
  filter(!is.na(GID_2) & !is.na(wave))%>%
  sf::st_drop_geometry()%>%
  mutate(
    afro_count = n(),
    mean_sgq_admin2 = mean(sgqi, na.rm = TRUE),
    mean_svc_admin2 = mean(ea_svc_index, na.rm = TRUE),
    mean_fac_admin2 = mean(ea_fac_index, na.rm = TRUE)
  )%>%
  select(
    afro_count,
    wave,
    GID_0,
    GID_1,
    GID_2,
    starts_with("mean")
  )%>%
  distinct()


# Create a complete panel of years for each GID_1.
admin2_afro <- admin2_afro %>%
  mutate(group_yr = case_when(
      wave == 1 ~ 2,
      wave == 2 ~ 3
      ))

# Round 3	2005-2006	18	
# Round 4	2008-2009	20	
# Round 5	2011-2013	34	
# Round 6	2014-2015	36

panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(admin1_afro%>%select(group_yr, GID_0, GID_1, starts_with("mean_"), 
  afro_count, wave), by= c("group_yr", "GID_0", "GID_1"))

panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(admin2_afro%>%select(group_yr, GID_0, GID_1, GID_2, starts_with("mean_"), 
  afro_count, wave), by= c("group_yr", "GID_0", "GID_1", "GID_2"))

### Read in and compute night lights 
admin1_nl <- read_csv("00_rawdata/nightlights/topcodefix/processed_topcodefix_nl_admin1.csv")%>%
  select(GID_0, GID_1, mean_2005:mean_2015, sum_2005:sum_2015)%>%
  pivot_longer(
    cols = c(mean_2005:mean_2015, sum_2005:sum_2015), 
    names_to = c(".value", "year"),
    names_pattern = "(mean|sum)_(\\d+)"
  ) %>%
 arrange(GID_1, year) %>% 
 mutate(group_yr = case_when(
      year %in% 2005:2009 ~ 2,
      year %in% 2010:2015 ~ 3
      ),
    lagged_mean = dplyr::lag(mean),
    nl_growth = if_else(lagged_mean > 0, (mean - lagged_mean) / lagged_mean, NA_real_)  # Avoid division by zero
  ) %>%
  group_by(GID_0, GID_1, group_yr) %>%
  summarise(
    mean_nl = mean(mean, na.rm = TRUE),
    nl_growth = mean(nl_growth, na.rm = TRUE),  # Aggregating growth rate
    sum_nl = sum(sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  distinct()


panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(admin1_nl, by= c("group_yr", "GID_0", "GID_1"))

admin2_nl <- read_csv("00_rawdata/nightlights/topcodefix/processed_topcodefix_nl_admin2.csv")%>%
  select(GID_0, GID_1, GID_2, mean_2005:mean_2015, sum_2005:sum_2015)%>%
  pivot_longer(c(mean_2005:mean_2015, sum_2005:sum_2015), 
               names_to = c(".value", "year"),
               names_pattern = "(mean|sum)_(\\d+)")%>%
 arrange(GID_2, year) %>% 
 mutate(group_yr = case_when(
      year %in% 2005:2009 ~ 2,
      year %in% 2010:2015 ~ 3
      ),
    lagged_mean = dplyr::lag(mean),
    nl_growth = if_else(lagged_mean > 0, (mean - lagged_mean) / lagged_mean, NA_real_)  # Avoid division by zero
  ) %>%
  group_by(GID_0, GID_1, GID_2, group_yr) %>%
  summarise(
    mean_nl = mean(mean, na.rm = TRUE),
    nl_growth = mean(nl_growth, na.rm = TRUE),  # Aggregating growth rate
    sum_nl = sum(sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  distinct()

panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(admin2_nl, by= c("group_yr", "GID_0", "GID_1", "GID_2"))

### calculate some final control variables 
# Calculate distances from centroids to captial_city 

# Read the capital city coordinates
cap_city <- read_csv("00_rawdata/country-capital-lat.csv")

cap_city <- cap_city %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_join(st_make_valid(shp)) %>%
  mutate(Longitude = st_coordinates(.)[,1],  # Extract longitude
         Latitude = st_coordinates(.)[,2]) %>% # Extract latitude
  select(GID_0, Longitude, Latitude) %>%
  st_drop_geometry()%>%
  distinct()

# Process the administrative units shapefile
shp_admin2 <- st_make_valid(shp_admin2) %>%
  st_transform(4326) %>%
  group_by(GID_0) %>%
  mutate(
    centroid = st_centroid(geometry))%>%
  left_join(cap_city, by = "GID_0")%>%
  rowwise() %>%
  mutate(
    capital_point = st_sfc(st_point(c(Longitude, Latitude)), crs = 4326),
    distance_to_capital = drop_units(st_distance(centroid, capital_point, by_element = TRUE) / 1000),  # Convert to kilometers
    capital_region = as.integer(st_intersects(geometry, capital_point, sparse = FALSE)[1])
  )

descr(shp_admin2$distance_to_capital)

# Same for admin1 
shp_admin1 <- st_make_valid(shp_admin1) %>%
  st_transform(4326) %>%
  group_by(GID_0) %>%
  mutate(
    centroid = st_centroid(geometry))%>%
  left_join(cap_city, by = "GID_0")%>%
  rowwise() %>%
  mutate(
    capital_point = st_sfc(st_point(c(Longitude, Latitude)), crs = 4326),
    distance_to_capital = drop_units(st_distance(centroid, capital_point, by_element = TRUE) / 1000),  # Convert to kilometers
    capital_region = as.integer(st_intersects(geometry, capital_point, sparse = FALSE)[1])  # 1 if capital is in region, 0 otherwise
  )

panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(st_drop_geometry(shp_admin1) %>% select(distance_to_capital, capital_region, GID_1), by = "GID_1")

panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(st_drop_geometry(shp_admin2) %>% select(distance_to_capital, capital_region, GID_2), by = "GID_2")


# ============================================================================
# PREPARING SPEI INDEX
# ============================================================================
# spei_file <- "00_rawdata/spei/spei12.nc"

# if(file.exists(spei_file)) {
#   # Read the SPEI data
#   spei_rast <- terra::rast(spei_file)
  
#   # Extract time information
#   spei_times <- terra::time(spei_rast)
#   years <- as.numeric(format(as.Date(spei_times), "%Y"))
  
#   # Identify target years
#   target_years <- 2005:2015
#   year_indices <- which(years %in% target_years)
  
#   if(length(year_indices) > 0) {
#     # Transform shapefiles to match raster projection
#     shp_admin1_trans <- sf::st_transform(shp_admin1, terra::crs(spei_rast))
#     shp_admin2_trans <- sf::st_transform(shp_admin2, terra::crs(spei_rast))

#     # Extract SPEI for all years in one batch
#     spei_values_admin1 <- terra::extract(spei_rast[[year_indices]], shp_admin1_trans, fun = mean, na.rm = TRUE)
#     spei_values_admin2 <- terra::extract(spei_rast[[year_indices]], shp_admin2_trans, fun = mean, na.rm = TRUE)

#     # Convert to data frame
#     admin1_spei <- as.data.frame(spei_values_admin1)
#     admin2_spei <- as.data.frame(spei_values_admin2)

#     # Add GID and reshape
#     admin1_spei <- admin1_spei %>%
#       mutate(GID_1 = shp_admin1$GID_1) %>%
#       pivot_longer(-GID_1, names_to = "year", values_to = "spei") %>%
#       mutate(year = target_years[as.numeric(gsub("lyr", "", year))])

#     admin2_spei <- admin2_spei %>%
#       mutate(GID_2 = shp_admin2$GID_2) %>%
#       pivot_longer(-GID_2, names_to = "year", values_to = "spei") %>%
#       mutate(year = target_years[as.numeric(gsub("lyr", "", year))])

#     # Group SPEI values by period
#     admin1_spei <- admin1_spei %>%
#       mutate(group_yr = case_when(year %in% 2005:2009 ~ 2, year %in% 2010:2015 ~ 3)) %>%
#       group_by(GID_1, group_yr) %>%
#       summarise(spei_admin1 = mean(spei, na.rm = TRUE), .groups = "drop")

#     admin2_spei <- admin2_spei %>%
#       mutate(group_yr = case_when(year %in% 2005:2009 ~ 2, year %in% 2010:2015 ~ 3)) %>%
#       group_by(GID_2, group_yr) %>%
#       summarise(spei_admin2 = mean(spei, na.rm = TRUE), .groups = "drop")

#     # Join SPEI values to panel data
#     panel_aid_admin1 <- panel_aid_admin1 %>%
#       mutate(group_yr = case_when(paymentyear %in% 2005:2009 ~ 2, paymentyear %in% 2010:2015 ~ 3)) %>%
#       left_join(admin1_spei, by = c("GID_1", "group_yr"))

#     panel_aid_admin2 <- panel_aid_admin2 %>%
#       mutate(group_yr = case_when(paymentyear %in% 2005:2009 ~ 2, paymentyear %in% 2010:2015 ~ 3)) %>%
#       left_join(admin2_spei, by = c("GID_2", "group_yr"))
      
#   } else {
#     warning("No matching years found in SPEI data")
#     panel_aid_admin1$spei_admin1 <- NA
#     panel_aid_admin2$spei_admin2 <- NA
#   }
# } else {
#   warning("SPEI file not found. Setting spei_admin1 and spei_admin2 to NA.")
#   panel_aid_admin1$spei_admin1 <- NA
#   panel_aid_admin2$spei_admin2 <- NA
# }
# ============================================================================

### Distance to nearest town of 100k 
cities <- st_read("00_rawdata/Africapolis_GIS_2024.gpkg")

cities_100k <- cities %>%
  filter(Population_2010 > 100000 & Select_Geometry_Year == 2015)%>%
  select(1:5, Population_2010, Population_2015, Built.up_2015)%>%
  st_drop_geometry()%>%
  st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4326)

# Compute admin1 distances between projects and cities
distance_matrix1 <- st_nn(shp_admin1$centroid, cities_100k, k = 1, returnDist = TRUE, progress = TRUE)
  
# Find nearest city for each project
shp_admin1$nearest_city_dist <- sapply(distance_matrix1$dist, function(x) x[1]/1000)# Convert to KM

# Compute admin2 distances between projects and cities
distance_matrix2 <- st_nn(shp_admin2$centroid, cities_100k, k = 1, returnDist = TRUE, progress = TRUE)

# Find nearest city for each project
shp_admin2$nearest_city_dist <- sapply(distance_matrix2$dist, function(x) x[1]/1000)  # Convert to KM


panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(st_drop_geometry(shp_admin1) %>% select(nearest_city_dist, GID_1), by = "GID_1")

panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(st_drop_geometry(shp_admin2) %>% select(nearest_city_dist, GID_2), by = "GID_2")



### Degree of urbanization 
# process_file <- function(file_name) {
#   # Extract year from the filename
#   year <- stringr::str_extract(file_name, "(?<=_GADM41_)\\d{4}(?=_level)")
#   # Read in the file (assuming CSV format)
#   data <- read.csv(file_name)
  
#   # Add a new column with the year
#   data <- data %>%
#     mutate(Year = as.numeric(year))%>%
#     filter(GID_0GHSL %in% countries_iso3 & Year >= 2005)%>%
#     select(any_of(c("GID_1", "GID_2")), Urban_share, Year)
  
#   return(data)
# }

# urb_files_admin1 <- list.files("00_rawdata/degurba", pattern = "level1", full.names = TRUE)

# # Apply the function to each file and combine them
# urb_admin1 <- lapply(urb_files_admin1, process_file) %>%
#   bind_rows()

# urb_admin1 <- urb_admin1 %>%
#     group_by(GID_1) %>% 
#     complete(Year = 2005:2015) %>%
#     arrange(GID_1, Year) %>%
#     mutate(
#       Urban_share = na.approx(Urban_share, x = Year, na.rm = FALSE, rule = 3)
#     ) %>%
#     ungroup()

# panel_aid_admin1 <- panel_aid_admin1 %>%
#   left_join(urb_admin1, by =  c("paymentyear" = "Year","GID_1"))


# urb_files_admin2 <- list.files("00_rawdata/degurba", pattern = "level2", full.names = TRUE)

# # Apply the function to each file and combine them
# urb_admin2 <- lapply(urb_files_admin2, process_file) %>%
#   bind_rows()

# urb_admin2 <- urb_admin2 %>%
#     group_by(GID_2) %>%
#     complete(Year = 2005:2015) %>%
#     arrange(GID_2, Year) %>%
#     mutate(
#       Urban_share = na.approx(Urban_share, x = Year, na.rm = FALSE, rule = 3)
#     ) %>%
#     ungroup()%>%
#     mutate(group_yr =)


# panel_aid_admin2 <- panel_aid_admin2 %>%
#   left_join(urb_admin2, by = c("paymentyear" = "Year", "GID_2"))


### okay last one WGI governace effectiveness as a control 
wgi <- readxl::read_xlsx("00_rawdata/wgidataset.xlsx")%>%
  select(GID_0, year, ge_pct)%>%
  mutate(group_yr = case_when(
      year %in% 2005:2009 ~ 2,
      year %in% 2010:2015 ~ 3
      ))%>%
  group_by(GID_0, group_yr)%>%
  summarize(
    ge_pct = mean(ge_pct)
  )


panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(wgi, by = c("group_yr", "GID_0"))

panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(wgi, by = c("group_yr", "GID_0"))


# Select final data
panel_aid_admin1_fin <- panel_aid_admin1 %>%
  select(
    GID_0 = GID_0.x, GID_1, group_yr,
    starts_with("frag_"), starts_with("mean_"),
    mean_nl, sum_nl, nl_growth, capital_region,
    total_early_admin1, total_late_admin1, total_proj_admin1,
    total_aid_admin1, donor_count_admin1, hhi_admin1,
    pop_admin1, afro_count, wave,
    distance_to_capital, capital_region, 
    #spei_admin1, urban_share = Urban_share,
    nearest_city_dist, , ge_pct
  )

panel_aid_admin2_fin <- panel_aid_admin2 %>%
  select(
    GID_0 = GID_0.x, GID_1, GID_2, group_yr,
    starts_with("frag_"), starts_with("mean_"),
    mean_nl, sum_nl, nl_growth, capital_region,
    total_early_admin2, total_late_admin2, total_proj_admin2,
    total_aid_admin2, donor_count_admin2, hhi_admin2,
    pop_admin2, afro_count, wave,
    distance_to_capital, capital_region, 
    #spei_admin2, urban_share = Urban_share,
    nearest_city_dist,  ge_pct
  )


#Winsorize the top and bottom 5 as per others did% #### TODO ####

### SAVE OUT FINAL PROCESSED DATA ### 
write_csv(panel_aid_admin1_fin, "01_panel_data/panel_aid_admin1_grp5.csv")
write_csv(panel_aid_admin2_fin, "01_panel_data/panel_aid_admin2_grp5.csv")
