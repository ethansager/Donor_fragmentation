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
  sjmisc,
  plm,
  scales,
  psych,
  tidylog,
  units,
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
dat<-read_csv("00_rawdata/GODAD_projectlevel.csv")

# Key missing info by donor 

dat%>% 
  group_by(donor)%>%
  mutate(miss_time = if_else(is.na(paymentyear) & is.na(closingyear) & is.na(startyear), 1, 0))%>%
  summarise(admin2_missing = sum(is.na(gid_2))/n(),
            percison_code = sum(precision_code ==12, na.rm = TRUE)/n(),
            notime_var = sum(miss_time)/n(),
            no_disb = sum(disb>0, na.rm = TRUE)/n(),
            no_comm = sum(comm>0, na.rm = TRUE)/n(),
            no_monies = sum(comm> 0 | disb > 0, na.rm =TRUE)/n(),
            yr_covered = paste0(min(startyear, na.rm = TRUE), "-", max(startyear, na.rm = TRUE)))%>%
  print(n=30)



countries_iso3 <- c(
  "NGA", "MOZ", "MWI", "KEN", "GHA"
)

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
    )
  )%>%
  filter(!is.na(gid_2)& !is.na(disb_loc_evensplit) & 
           !is.na(paymentyear) & 
           disb != 0 & gid_0 %in% countries_iso3 & 
           paymentyear >= 2005 & paymentyear <= 2015,
         #Adding filter to select only early impact aid
         early_impact == 1
         )

# This gives us 24000 aid projects 
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
dat <- st_join(aid_points, shp)


## Create a hhi to merge back 
hhi_results_admin2 <- dat %>%
  
  # Aggregate total disbursements per donor within each group
  group_by(gid_0, GID_2, paymentyear, donor) %>%
  summarise(
    total_disb_admin2 = sum(abs(disb_loc_evensplit), na.rm = TRUE), 
    total_early_projects = sum(early_impact == 1, na.rm = TRUE),
    total_late_projects = sum(early_impact == 0, na.rm = TRUE),
    .groups = "drop") %>%
  
  # Group by region and year to calculate HHI and other metrics
  group_by(gid_0, GID_2, paymentyear) %>%
  summarise(
    # Count of project types 
    total_early_admin2 = sum(total_early_projects),
    total_late_admin2 = sum(total_late_projects),
    # Sum total aid across all donors in the group
    total_aid_admin2 = sum(total_disb_admin2, na.rm = TRUE),
    
    # Count the distinct number of donors
    donor_count_admin2 = n_distinct(donor),
    
    # Calculate the HHI (summing squares of all percentage contributions)
    hhi_admin2 = sum((total_disb_admin2 / total_aid_admin2)^2, na.rm = TRUE),
    
    # Calculate the fragility index as 1 minus HHI
    frag_index_admin2 = 1 - hhi_admin2,
    
    # Largest total_disb / total_aid from 1
    frag_1_admin2 = 1 -  max(total_disb_admin2 / total_aid_admin2, na.rm = TRUE),
    
    # Largest 3 total_disb / total_aid from 1
    frag_3_admin2 = sum(head(sort(total_disb_admin2 / total_aid_admin2, decreasing = TRUE), 3), na.rm = TRUE),
    
    # Count the number of donors below 10% of total_disb / total_aid
    frag_below10_admin2 = sum((total_disb_admin2 / total_aid_admin2) < 0.10, na.rm = TRUE),
    
    .groups = "drop"
  )

## Create a hhi to merge back 
hhi_results_admin1 <- dat %>%
  
  # Aggregate total disbursements per donor within each group
  group_by(gid_0, GID_1, paymentyear, donor) %>%
  summarise(
    total_disb_admin1 = sum(abs(disb_loc_evensplit), na.rm = TRUE), 
    total_early_projects = sum(early_impact == 1, na.rm = TRUE),
    total_late_projects = sum(early_impact == 0, na.rm = TRUE),
    .groups = "drop") %>%
  
  # Group by region and year to calculate HHI and other metrics
  group_by(gid_0, GID_1, paymentyear) %>%
  summarise(
    # Count of project types 
    total_early_admin1 = sum(total_early_projects),
    total_late_admin1 = sum(total_late_projects),
    # Sum total aid across all donors in the group
    total_aid_admin1 = sum(total_disb_admin1, na.rm = TRUE),
    
    # Count the distinct number of donors
    donor_count_admin1 = n_distinct(donor),
    
    # Calculate the HHI (summing squares of all percentage contributions)
    hhi_admin1 = sum((total_disb_admin1 / total_aid_admin1)^2, na.rm = TRUE),
    
    # Calculate the fragility index as 1 minus HHI
    frag_index_admin1 = 1 - hhi_admin1,
    
    # Largest total_disb / total_aid from 1
    frag_1_admin1 = 1 -  max(total_disb_admin1 / total_aid_admin1, na.rm = TRUE),
    
    # Largest 3 total_disb / total_aid from 1
    frag_3_admin1 = sum(head(sort(total_disb_admin1 / total_aid_admin1, decreasing = TRUE), 3), na.rm = TRUE),
    
    # Count the number of donors below 10% of total_disb / total_aid
    frag_below10_admin1 = sum((total_disb_admin1 / total_aid_admin1) < 0.10, na.rm = TRUE),
    
    .groups = "drop"
  )


# Descriptives region count by admin level  

n_distinct(hhi_results_admin1$GID_1)
n_distinct(hhi_results_admin2$GID_2)

# Step 2: Merge the HHI results back into the main dataset
target_years <- c(2005, 2010, 2015)

panel_aid_admin1 <- dat %>%
  sf::st_drop_geometry()%>%
  filter(!is.na(GID_1))%>%
  tidylog::left_join(hhi_results_admin1, by = c("gid_0", "GID_1", "paymentyear"))%>%
  select(GID_0, GID_1, paymentyear, ends_with("_admin1"), -contains("total_disb"))%>%
  distinct()

check <- panel_aid_admin1 %>%
  group_by(GID_0, GID_1, paymentyear)%>%
  summarise(count = n())%>%
  filter(count > 1)

panel_aid_admin2 <- dat %>%
  sf::st_drop_geometry()%>%
  filter(!is.na(GID_1))%>%
  tidylog::left_join(hhi_results_admin2, by = c("gid_0", "GID_2", "paymentyear"))%>%
  select(GID_0, GID_1, GID_2, paymentyear, ends_with("_admin2"), -contains("total_disb"))%>%
  distinct()

check <- panel_aid_admin2 %>%
  group_by(GID_0, GID_1, GID_2, paymentyear)%>%
  summarise(count = n())%>%
  filter(count > 1)

n_distinct(panel_aid_admin1$GID_1)
n_distinct(panel_aid_admin2$GID_2)

#population grid GWP
#### Admin 2 
pop_10 <-geodata::population(2010, res = "10", path = tempdir())

shp_admin2 <- sf::st_as_sf(geodata::gadm(country = countries_iso3, level = 2, path = "00_rawdata/shapefiles/"))

avg_pop_admin2_10  <- terra::extract(
  pop_10,
  shp_admin2,
  fun = sum,
  na.rm = TRUE,
  exact = TRUE
)

shp_admin2$log_avg_pop_admin2 <- log(avg_pop_admin2_10[, 2])

panel_aid_admin2 <- panel_aid_admin2 %>%
  tidylog::left_join(select(shp_admin2, log_avg_pop_admin2, GID_2), by = "GID_2")

##### Admin 1
shp_admin1 <- sf::st_as_sf(geodata::gadm(country = countries_iso3, level = 1, path = "00_rawdata/shapefiles/"))


avg_pop_admin1_10  <- terra::extract(
  pop_10,
  shp_admin1,
  fun = sum,
  na.rm = TRUE,
  exact = TRUE
)

shp_admin1$log_avg_pop_admin1 <- log(avg_pop_admin1_10[, 2])

panel_aid_admin1 <- panel_aid_admin1 %>%
  tidylog::left_join(select(shp_admin1, log_avg_pop_admin1, GID_1), by = "GID_1")

###### Read in Afro data and create panel to match ########

afro_merged <- haven::read_sav("00_rawdata/ab_raw/processed/afrobarometer_w3_w6_geomerged.sav")

table(afro_merged$country) 

afro_points <- st_as_sf(
  afro_merged,
  coords = c("longitude", "latitude"), # Specify longitude and latitude columns
  crs = 4326                # Set CRS (WGS84)
)

afro_points <- st_join(afro_points, shp_admin2)



admin1_afro <-afro_points%>%
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
  mutate(mean_svc_admin1 = if_else(wave ==3, NA, mean_svc_admin1),
         mean_fac_admin1 = if_else(wave ==3, NA, mean_fac_admin1),
  )%>%
  distinct()

### admin 2 
admin2_afro <-afro_points%>%
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
  mutate(mean_svc_admin2 = if_else(wave ==3, NA, mean_svc_admin2),
         mean_fac_admin2 = if_else(wave ==3, NA, mean_fac_admin2),
  )%>%
  distinct()

# Round 3	2005-2006	18	
# Round 4	2008-2009	20	
# Round 5	2011-2013	34	
# Round 6	2014-2015	36

panel_aid_admin1 <- panel_aid_admin1 %>%
  mutate(wave = case_when(
    paymentyear %in% 2005:2007 ~ 3,
    paymentyear %in% 2008:2010 ~ 4,
    paymentyear %in% 2011:2013 ~ 5,
    paymentyear %in% 2014:2015 ~ 6,
  ))%>%
  left_join(admin1_afro, by= c("wave", "GID_0", "GID_1"))

panel_aid_admin2 <- panel_aid_admin2 %>%
  mutate(wave = case_when(
    paymentyear %in% 2005:2007 ~ 3,
    paymentyear %in% 2008:2010 ~ 4,
    paymentyear %in% 2011:2013 ~ 5,
    paymentyear %in% 2014:2015 ~ 6,
  ))%>%
  left_join(admin2_afro, by= c("wave", "GID_0", "GID_1", "GID_2"))

### Read in and compute night lights 
admin1_nl <- read_csv("00_rawdata/nightlights/topcodefix/processed_topcodefix_nl_admin1.csv")%>%
  select(GID_0, GID_1, starts_with("mean"))%>%
  pivot_longer(mean_2005:mean_2016, 
               names_to = "year",
               names_pattern = "mean_(\\d+)",
               values_to = "mean_nl")%>%
  mutate(year = as.numeric(year))

panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(admin1_nl, by= c("paymentyear" = "year", "GID_0", "GID_1"))


admin2_nl <- read_csv("00_rawdata/nightlights/topcodefix/processed_topcodefix_nl_admin2.csv")%>%
  select(GID_0, GID_1, GID_2, starts_with("mean"))%>%
  pivot_longer(mean_2005:mean_2016, 
               names_to = "year",
               names_pattern = "mean_(\\d+)",
               values_to = "mean_nl")%>%
  mutate(year = as.numeric(year))

panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(admin2_nl, by= c("paymentyear" = "year", "GID_0", "GID_1", "GID_2"))

### calculate some final control variables 
# Calculate distances from centroids to captial_city 

# Read the capital city coordinates
cap_city <- read_csv("00_rawdata/country-capital-lat.csv")

# Process the administrative units shapefile
shp_admin2 <- shp_admin2 %>%
  st_transform(4326) %>%
  group_by(GID_0) %>%
  mutate(
    centroid = st_centroid(geometry))%>%
  left_join(cap_city, by = "GID_0")%>%
  rowwise() %>%
  mutate(
    capital_point = st_sfc(st_point(c(Longitude, Latitude)), crs = 4326),
    distance_to_capital = drop_units(st_distance(centroid, capital_point, by_element = TRUE) / 1000)  # Convert to kilometers
  )

descr(shp_admin2$distance_to_capital)

# Same for admin1 
shp_admin1 <- shp_admin1 %>%
  st_transform(4326) %>%
  group_by(GID_0) %>%
  mutate(
    centroid = st_centroid(geometry))%>%
  left_join(cap_city, by = "GID_0")%>%
  rowwise() %>%
  mutate(
    capital_point = st_sfc(st_point(c(Longitude, Latitude)), crs = 4326),
    distance_to_capital = drop_units(st_distance(centroid, capital_point, by_element = TRUE) / 1000)  # Convert to kilometers
  )

panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(st_drop_geometry(shp_admin1) %>% select(distance_to_capital, GID_1), by = "GID_1")

panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(st_drop_geometry(shp_admin2) %>% select(distance_to_capital, GID_2), by = "GID_2")
#### SPEI Index 

spei <- terra::rast("00_rawdata/spei_yearly_12mnth.tif")

spei_admin1  <- terra::extract(
  spei,
  shp_admin1,
  fun = mean,
  na.rm = TRUE,
  exact = TRUE
)

shp_admin1$avg_spei_admin1 <- spei_admin1[, 2]

spei_admin2  <- terra::extract(
  spei,
  shp_admin2,
  fun = mean,
  na.rm = TRUE,
  exact = TRUE
)

shp_admin2$avg_spei_admin2 <- spei_admin2[, 2]

panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(st_drop_geometry(shp_admin1) %>% select(avg_spei_admin1, GID_1), by = "GID_1")

panel_aid_admin2 <- panel_aid_admin2 %>%
  
  left_join(st_drop_geometry(shp_admin2) %>% select(avg_spei_admin2, GID_2), by = "GID_2")


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
    filter(GID_0GHSL %in% countries_iso3 & Year == 2010)%>%
    select(any_of(c("GID_1", "GID_2")), Urban_share)
  
  return(data)
}

urb_files_admin1 <- list.files("00_rawdata/degurba", pattern = "level1", full.names = TRUE)

# Apply the function to each file and combine them
urb_admin1 <- lapply(urb_files_admin1, process_file) %>%
  bind_rows()

panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(urb_admin1, by =  "GID_1")


urb_files_admin2 <- list.files("00_rawdata/degurba", pattern = "level2", full.names = TRUE)

# Apply the function to each file and combine them
urb_admin2 <- lapply(urb_files_admin2, process_file) %>%
  bind_rows()

panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(urb_admin2, by = "GID_2")

### okay last one WGI governace effectiveness as a control 
wgi <- readxl::read_xlsx("00_rawdata/wgidataset.xlsx")%>%
  select(GID_0, year, ge_pct)


panel_aid_admin1 <- panel_aid_admin1 %>%
  select(-mean_2004, -mean_2017, -GID_0.y, -GID_0.x.x, -GID_0.y.y)%>%
  rename(GID_0 = GID_0.x)%>%
  left_join(wgi, by = c("paymentyear" = "year", "GID_0"))

panel_aid_admin2 <- panel_aid_admin2 %>%
  select(-mean_2004, -mean_2017, -GID_0.y, -GID_0.x.x, -GID_0.y.y)%>%
  rename(GID_0 = GID_0.x)%>%
  left_join(wgi, by = c("paymentyear" = "year", "GID_0"))

panel_aid_admin2 <- panel_aid_admin2 %>%
  select(-geometry)%>%
  st_drop_geometry()

panel_aid_admin1 <- panel_aid_admin1 %>%
  select(-geometry)%>%
  st_drop_geometry()

### SAVE OUT FINAL PROCESSED DATA ### 
write_csv(panel_aid_admin1, "01_panel_data/panel_aid_admin1_earlyimpact.csv")
write_csv(panel_aid_admin2, "01_panel_data/panel_aid_admin2_earlyimpact.csv")
