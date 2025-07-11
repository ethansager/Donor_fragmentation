# Cleaning datasets script 
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

## Load dat
dat<-read_csv(here("00_rawdata", "GODAD_projectlevel.csv"))

# get country isos for gadm 
countries_iso3 <- read_csv("00_rawdata/nightlights/topcodefix/processed_topcodefix_nl_admin2.csv")%>%
  select(GID_0)%>%
  distinct()%>%
  pull()

# Round 3	2005-2006	18	
# Round 4	2008-2009	20	
# Round 5	2011-2013	34	
# Round 6	2014-2015	36

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
      paymentyear %in% 2005:2008 ~ 1,
      paymentyear %in% 2009:2011 ~ 2,
      paymentyear %in% 2012:2015 ~ 3,
      ))%>%
  filter(
           !is.na(gid_2)& 
           !is.na(disb_loc_evensplit) & 
           !is.na(paymentyear) & 
           disb != 0 & 
           gid_0 %in% countries_iso3 & 
           paymentyear >= 2005 & paymentyear <= 2015
         )


# This gives us 33139 aid projects 

shp <- geodata::gadm(country = countries_iso3, level = 2, version = 3.6, path = "00_rawdata/shapefiles/")

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
  ), by = .(GID_0, GID_2, paymentyear, donor)]

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
  }, by = .(GID_0, GID_2, paymentyear)]

# Create HHI for admin1 level 
hhi_results_admin1 <- dat[, .(
  total_disb_admin1 = sum(abs(disb_loc_evensplit), na.rm = TRUE),
  total_early_projects = sum(early_impact == 1, na.rm = TRUE),
  total_late_projects = sum(early_impact == 0, na.rm = TRUE)
  ), by = .(GID_0, GID_1, paymentyear, donor)]

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
  }, by = .(GID_0, GID_1, paymentyear)]

# Descriptives region count by admin level  

n_distinct(hhi_results_admin1$GID_1)
n_distinct(hhi_results_admin2$GID_2)

# Step 2: Merge the HHI results back into the main dataset

panel_aid_admin1 <- dat %>%
  sf::st_drop_geometry()%>%
  filter(!is.na(GID_1))%>%
  tidylog::left_join(hhi_results_admin1, by = c("GID_0", "GID_1", "paymentyear"))%>%
  select(GID_0, GID_1, paymentyear, ends_with("_admin1"), -contains("total_disb"))%>%
  distinct()

check <- panel_aid_admin1 %>%
  group_by(GID_0, GID_1, paymentyear)%>%
  summarise(count = n())%>%
  filter(count > 1)

panel_aid_admin2 <- dat %>%
  sf::st_drop_geometry()%>%
  filter(!is.na(GID_1))%>%
  tidylog::left_join(hhi_results_admin2, by = c("GID_0", "GID_2", "paymentyear"))%>%
  select(GID_0, GID_1, GID_2, paymentyear, ends_with("_admin2"), -contains("total_disb"))%>%
  distinct()

check <- panel_aid_admin2 %>%
  group_by(GID_0, GID_1, GID_2, paymentyear)%>%
  summarise(count = n())%>%
  filter(count > 1)
  
n_distinct(panel_aid_admin1$GID_1)
n_distinct(panel_aid_admin2$GID_2)

#population grid GWP

admin1_pop <- read_csv("00_rawdata/population/admin1_population.csv") %>%
  select(GID_1, year, pop) %>%
  mutate(year = as.numeric(year))

admin2_pop <- read_csv("00_rawdata/population/admin2_population.csv") %>%
  select(GID_2, year, pop) %>%
  mutate(year = as.numeric(year))

# Merge with panel data
panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(admin1_pop, by = c("GID_1", "paymentyear" = "year")) %>%
  rename(pop_admin1 = pop)

panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(admin2_pop, by = c("GID_2", "paymentyear" = "year")) %>%
  rename(pop_admin2 = pop)

###### Read in Afro data and create panel to match ########

admin1_afro <- read_csv("00_rawdata/ab_raw/processed/admin1_afro_panel.csv")%>%
  select(year, GID_0, GID_1, starts_with("mean_"), afro_count, wave)
 
admin2_afro <- read_csv("00_rawdata/ab_raw/processed/admin2_afro_panel.csv")%>%
  select(year, GID_0, GID_1, GID_2, starts_with("mean_"), afro_count, wave)

# merge in the panel 
panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(admin1_afro,
  by= c("paymentyear"="year", "GID_0", "GID_1"))

panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(admin2_afro,
  by= c("paymentyear"="year", "GID_0", "GID_1", "GID_2"))

### Read in and compute night lights 
admin1_dep_vars <- read_csv("00_rawdata/processed_dep_vars_admin1.csv")

panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(admin1_dep_vars, by= c("paymentyear" = "year", "GID_0", "GID_1"))

admin2_dep_vars <- read_csv("00_rawdata/processed_dep_vars_admin2.csv")

panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(admin2_dep_vars, by= c("paymentyear" = "year", "GID_0", "GID_1", "GID_2"))

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
process_file <- function(file_name) {
  # Extract year from the filename
  year <- stringr::str_extract(file_name, "(?<=_GADM41_)\\d{4}(?=_level)")
  # Read in the file (assuming CSV format)
  data <- read.csv(file_name)
  
  # Add a new column with the year
  data <- data %>%
    mutate(Year = as.numeric(year))%>%
    filter(GID_0GHSL %in% countries_iso3 & Year >= 2005)%>%
    select(any_of(c("GID_1", "GID_2")), Urban_share, Year)
  
  return(data)
}

urb_files_admin1 <- list.files("00_rawdata/degurba", pattern = "level1", full.names = TRUE)

# Apply the function to each file and combine them
urb_admin1 <- lapply(urb_files_admin1, process_file) %>%
  bind_rows()

urb_admin1 <- urb_admin1 %>%
    group_by(GID_1) %>% 
    complete(Year = 2005:2015) %>%
    arrange(GID_1, Year) %>%
    mutate(
      Urban_share = na.approx(Urban_share, x = Year, na.rm = FALSE, rule = 3)
    ) %>%
    ungroup()

panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(urb_admin1, by =  c("paymentyear" = "Year","GID_1"))


urb_files_admin2 <- list.files("00_rawdata/degurba", pattern = "level2", full.names = TRUE)

# Apply the function to each file and combine them
urb_admin2 <- lapply(urb_files_admin2, process_file) %>%
  bind_rows()

urb_admin2 <- urb_admin2 %>%
    group_by(GID_2) %>%
    complete(Year = 2005:2015) %>%
    arrange(GID_2, Year) %>%
    mutate(
      Urban_share = na.approx(Urban_share, x = Year, na.rm = FALSE, rule = 3)
    ) %>%
    ungroup()


panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(urb_admin2, by = c("paymentyear" = "Year", "GID_2"))


### okay  WGI governace effectiveness as a control 
wgi <- readxl::read_xlsx("00_rawdata/wgidataset.xlsx")%>%
  select(GID_0, year, ge_pct)


panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(wgi, by = c("paymentyear" = "year", "GID_0"))

panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(wgi, by = c("paymentyear" = "year", "GID_0"))


# Select final data
panel_aid_admin1_fin <- panel_aid_admin1 %>%
  select(
    GID_0 = GID_0.x, GID_1, paymentyear,
    starts_with("frag_"), starts_with("mean_"),
    mean_nl = mean, capital_region,
    total_early_admin1, total_late_admin1, total_proj_admin1,
    total_aid_admin1, donor_count_admin1, hhi_admin1,
    pop_admin1, afro_count, wave,
    distance_to_capital, capital_region, #spei_admin1, 
    nearest_city_dist, urban_share = Urban_share, ge_pct

  )

panel_aid_admin2_fin <- panel_aid_admin2 %>%
  select(
    GID_0 = GID_0.x, GID_1, GID_2, paymentyear,
    starts_with("frag_"), starts_with("mean_"),
    mean_nl = mean, sum_nl = sum, capital_region,
    total_early_admin2, total_late_admin2, total_proj_admin2,
    total_aid_admin2, donor_count_admin2, hhi_admin2,
    pop_admin2, afro_count, wave,
    distance_to_capital, capital_region, #spei_admin2, 
    nearest_city_dist, urban_share = Urban_share, ge_pct
  )

# Read in the disaster information file
disaster_data1 <- read_csv("01_panel_data/panel_aid_admin1_with_disasters.csv")
disaster_data2 <- read_csv("01_panel_data/panel_aid_admin2_with_disasters.csv")

# Merge disaster info into the admin1 panel based on GID_0, GID_1, and paymentyear
panel_aid_admin1_fin <- panel_aid_admin1_fin%>%
  left_join(
    disaster_data1 %>% 
    select(GID_1, paymentyear, disaster_dummy, disaster_count),
    by = c("GID_1", "paymentyear")
  )

# Merge disaster info into the admin2 panel based on GID_0, GID_1, GID_2, and paymentyear
panel_aid_admin2_fin <- panel_aid_admin2_fin %>%
  left_join(
    disaster_data2 %>% select(GID_2, paymentyear, disaster_dummy, disaster_count),
    by = c("GID_2", "paymentyear")
  )

### SAVE OUT FINAL PROCESSED DATA ### 
write_csv(panel_aid_admin1_fin, "01_panel_data/panel_aid_admin1.csv")
write_csv(panel_aid_admin2_fin, "01_panel_data/panel_aid_admin2.csv")
