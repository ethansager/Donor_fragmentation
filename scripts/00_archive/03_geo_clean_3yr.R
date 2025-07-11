# Cleaning datasets script 
# Set up and packages  ----------------------------------------------------
setwd("C:/Users/eman7/Dropbox/GitHub/ra_work/Donor_fragmentation/")

if (!require("pacman")) 
  install.packages("pacman")

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
  sf,
  spdep,
  spatstat,
  geodata,
  terra,
  giscoR,
  nngeo
)

# Load data
admin2 <- read_csv("00_rawdata/GODAD_adm2_filtered.csv")

admin2 <- admin2 %>%
  mutate(group_yr = case_when(
    year %in% 2005:2008 ~ 1,
    year %in% 2009:2011 ~ 2,
    year %in% 2012:2015 ~ 3
  ))

admin2%>%
  filter(!is.na(group_yr))%>%
  group_by(gid_0, group_yr)%>%
  summarise(
    aid = sum(total_aid),
    total_proj = sum(total_project)
  )

admin2_panel_with_disasters%>%
  mutate(group_yr = case_when(
      paymentyear %in% 2005:2008 ~ 1,
      paymentyear %in% 2009:2011 ~ 2,
      paymentyear %in% 2012:2015 ~ 3
    ))%>%
  filter(!is.na(group_yr))%>%
  group_by(GID_0, group_yr)%>%
  summarise(
    aid = sum(total_aid_admin2),
    total_proj = sum(total_early_admin2, total_late_admin2)
  )


dat <- read_csv("00_rawdata/GODAD_projectlevel.csv")

# Key missing info by donor 
countries_iso3 <- c("NGA", "MOZ", "MWI", "KEN", "GHA")

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
      paymentyear %in% 2012:2015 ~ 3
    )
  ) %>%
  filter(
    !is.na(gid_2) & 
    !is.na(disb_loc_evensplit) & 
    !is.na(paymentyear) & 
    disb != 0 & 
    gid_0 %in% countries_iso3 & 
    paymentyear >= 2005 & paymentyear <= 2015
  )

# Load shapefiles
shp <- geodata::gadm(country = countries_iso3, level = 2, path = "00_rawdata/shapefiles/")

shp <- sf::st_as_sf(shp)

# Convert aid dataset to an sf object
aid_points <- st_as_sf(
  dat,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Perform a spatial join to link aid points to admin-2 polygons
dat <- st_join(aid_points, shp)

# Create HHI to merge back
hhi_results_admin2 <- dat %>%
  group_by(gid_0, GID_2, group_yr, donor) %>%
  summarise(
    total_disb_admin2 = sum(abs(disb_loc_evensplit), na.rm = TRUE), 
    total_early_projects = sum(early_impact == 1, na.rm = TRUE),
    total_late_projects = sum(early_impact == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(gid_0, GID_2, group_yr) %>%
  summarise(
    total_early_admin2 = n_distinct(case_when(early_impact == 1 ~ project_id)),
    total_late_admin2 = n_distinct(case_when(early_impact == 0 ~ project_id)), 
    total_proj_admin2 = n_distinct(project_id),
    total_aid_admin2 = sum(total_disb_admin2, na.rm = TRUE),
    donor_count_admin2 = n_distinct(donor),
    hhi_admin2 = sum((total_disb_admin2 / total_aid_admin2)^2, na.rm = TRUE),
    frag_index_admin2 = 1 - hhi_admin2,
    frag_1_admin2 = 1 - max(total_disb_admin2 / total_aid_admin2, na.rm = TRUE),
    frag_3_admin2 = sum(head(sort(total_disb_admin2 / total_aid_admin2, decreasing = TRUE), 3), na.rm = TRUE),
    frag_below10_admin2 = sum((total_disb_admin2 / total_aid_admin2) < 0.10, na.rm = TRUE),
    .groups = "drop"
  )

hhi_results_admin1 <- dat %>%
  group_by(gid_0, GID_1, group_yr, donor) %>%
  summarise(
    total_disb_admin1 = sum(abs(disb_loc_evensplit), na.rm = TRUE), 
    total_early_projects = sum(early_impact == 1, na.rm = TRUE),
    total_late_projects = sum(early_impact == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(gid_0, GID_1, group_yr) %>%
  summarise(
    total_early_admin1 = sum(total_early_projects),
    total_late_admin1 = sum(total_late_projects),
    total_aid_admin1 = sum(total_disb_admin1, na.rm = TRUE),
    donor_count_admin1 = n_distinct(donor),
    hhi_admin1 = sum((total_disb_admin1 / total_aid_admin1)^2, na.rm = TRUE),
    frag_index_admin1 = 1 - hhi_admin1,
    frag_1_admin1 = 1 - max(total_disb_admin1 / total_aid_admin1, na.rm = TRUE),
    frag_3_admin1 = sum(head(sort(total_disb_admin1 / total_aid_admin1, decreasing = TRUE), 3), na.rm = TRUE),
    frag_below10_admin1 = sum((total_disb_admin1 / total_aid_admin1) < 0.10, na.rm = TRUE),
    .groups = "drop"
  )

# Descriptives region count by admin level  
n_distinct(hhi_results_admin1$GID_1)
n_distinct(hhi_results_admin2$GID_2)

# Merge the HHI results back into the main dataset
panel_aid_admin1 <- dat %>%
  sf::st_drop_geometry() %>%
  filter(!is.na(GID_1)) %>%
  tidylog::left_join(hhi_results_admin1, by = c("gid_0", "GID_1", "group_yr")) %>%
  select(GID_0, GID_1, group_yr, ends_with("_admin1"), -contains("total_disb")) %>%
  distinct()

panel_aid_admin2 <- dat %>%
  sf::st_drop_geometry() %>%
  filter(!is.na(GID_1)) %>%
  tidylog::left_join(hhi_results_admin2, by = c("gid_0", "GID_2", "group_yr")) %>%
  select(GID_0, GID_1, GID_2, group_yr, ends_with("_admin2"), -contains("total_disb")) %>%
  distinct()

# Population grid GWP
# Load population data for each year
yrs <- c(2005, 2010, 2015)

admin2_pop <- data.frame()
admin1_pop <- data.frame()

for (yr in yrs) {
  pop <- geodata::population(yr, res = "10", path = tempdir())
  
  shp_admin2 <- sf::st_as_sf(geodata::gadm(country = countries_iso3, level = 2, path = "00_rawdata/shapefiles/"))
  avg_pop_admin2 <- terra::extract(pop, shp_admin2, fun = sum, na.rm = TRUE, exact = TRUE)
  temp_admin2 <- data.frame(
    GID_2 = shp_admin2$GID_2,
    year = yr,
    pop = avg_pop_admin2[, 2]
  )
  admin2_pop <- rbind(admin2_pop, temp_admin2)
  
  shp_admin1 <- sf::st_as_sf(geodata::gadm(country = countries_iso3, level = 1, path = "00_rawdata/shapefiles/"))
  avg_pop_admin1 <- terra::extract(pop, shp_admin1, fun = sum, na.rm = TRUE, exact = TRUE)
  temp_admin1 <- data.frame(
    GID_1 = shp_admin1$GID_1,
    year = yr,
    pop = avg_pop_admin1[, 2]
  )
  admin1_pop <- rbind(admin1_pop, temp_admin1)
}

# Interpolate population for missing years
admin2_pop <- admin2_pop %>%
  group_by(GID_2) %>%
  complete(year = 2005:2015) %>%
  mutate(
    pop = na.approx(pop, x = year, na.rm = FALSE, rule = 2),
    group_yr = case_when(
      year %in% 2005:2008 ~ 1,
      year %in% 2009:2011 ~ 2,
      year %in% 2012:2015 ~ 3
    )
  ) %>%
  group_by(GID_2, group_yr) %>%
  summarise(avg_pop_admin2 = mean(pop, na.rm = TRUE))

admin1_pop <- admin1_pop %>%
  group_by(GID_1) %>%
  complete(year = 2005:2015) %>%
  mutate(
    pop = na.approx(pop, x = year, na.rm = FALSE, rule = 2),
    group_yr = case_when(
      year %in% 2005:2008 ~ 1,
      year %in% 2009:2011 ~ 2,
      year %in% 2012:2015 ~ 3
    )
  ) %>%
  group_by(GID_1, group_yr) %>%
  summarise(avg_pop_admin1 = mean(pop, na.rm = TRUE))

# Merge with panel data
panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(admin2_pop, by = c("GID_2", "group_yr"))

panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(admin1_pop, by = c("GID_1", "group_yr"))

# Read in Afro data and create panel to match
afro_merged <- haven::read_sav("00_rawdata/ab_raw/processed/afrobarometer_w3_w6_geomerged.sav")

afro_points <- st_as_sf(
  afro_merged,
  coords = c("longitude", "latitude"),
  crs = 4326
)

afro_points <- st_join(afro_points, shp_admin2)

admin1_afro <- afro_points %>%
  #adjust wave so that wave is 3-4 5-6
  mutate(
    wave= if_else(wave %in% c(3,4), 1, 2)
  )%>%
  group_by(GID_1, wave) %>%
  filter(!is.na(GID_1) & !is.na(wave)) %>%
  sf::st_drop_geometry() %>%
  mutate(
    afro_count = n(),
    year = substr(date_of_interview, 1, 4),
    mean_sgq_admin1 = mean(sgqi, na.rm = TRUE),
    mean_svc_admin1 = mean(ea_svc_index, na.rm = TRUE),
    mean_fac_admin1 = mean(ea_fac_index, na.rm = TRUE)
  ) %>%
  select(
    afro_count,
    year,
    wave,
    GID_0,
    GID_1,
    starts_with("mean")
  ) %>%
  mutate(
    mean_svc_admin1 = if_else(wave == 3, NA, mean_svc_admin1),
    mean_fac_admin1 = if_else(wave == 3, NA, mean_fac_admin1)
  ) %>%
  distinct()

admin1_afro <- admin1_afro %>%
  mutate(year = as.numeric(year))

admin1_afro <- admin1_afro %>%
  group_by(GID_1) %>%
  complete(year = seq(min(year), max(year), by = 1)) %>%
  arrange(GID_1, year) %>%
  fill(wave, GID_0, afro_count, .direction = "down") %>%
  mutate(group_yr = case_when(
    year %in% 2005:2008 ~ 1,
    year %in% 2009:2011 ~ 2,
    year %in% 2012:2015 ~ 3
  ))

admin1_afro <- admin1_afro %>%
  group_by(GID_1) %>%
  mutate(
    mean_sgq_admin1 = na.approx(mean_sgq_admin1, x = year, na.rm = FALSE, rule = 2),
    mean_svc_admin1 = na.approx(mean_svc_admin1, x = year, na.rm = FALSE, rule = 2),
    mean_fac_admin1 = na.approx(mean_fac_admin1, x = year, na.rm = FALSE, rule = 2)
  ) %>%
  ungroup() %>%
  group_by(GID_1, group_yr) %>%
  mutate(
    mean_sgq_admin1 = mean(mean_sgq_admin1, na.rm = TRUE),
    mean_svc_admin1 = mean(mean_svc_admin1, na.rm = TRUE),
    mean_fac_admin1 = mean(mean_fac_admin1, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  distinct()

admin2_afro <- afro_points %>%
  group_by(GID_2, wave) %>%
  filter(!is.na(GID_2) & !is.na(wave)) %>%
  sf::st_drop_geometry() %>%
  mutate(
    afro_count = n(),
    year = substr(date_of_interview, 1, 4),
    mean_sgq_admin2 = mean(sgqi, na.rm = TRUE),
    mean_svc_admin2 = mean(ea_svc_index, na.rm = TRUE),
    mean_fac_admin2 = mean(ea_fac_index, na.rm = TRUE)
  ) %>%
  select(
    afro_count,
    year,
    wave,
    GID_0,
    GID_1,
    GID_2,
    starts_with("mean")
  ) %>%
  mutate(
    mean_svc_admin2 = if_else(wave == 3, NA, mean_svc_admin2),
    mean_fac_admin2 = if_else(wave == 3, NA, mean_fac_admin2)
  ) %>%
  distinct()

admin2_afro <- admin2_afro %>%
  mutate(year = as.numeric(year))

admin2_afro <- admin2_afro %>%
  group_by(GID_2) %>%
  complete(year = seq(min(year), max(year) + 1, by = 1)) %>%
  arrange(GID_2, year) %>%
  fill(wave, GID_0, GID_1, afro_count, .direction = "down") %>%
  mutate(group_yr = case_when(
    year %in% 2005:2008 ~ 1,
    year %in% 2009:2011 ~ 2,
    year %in% 2012:2015 ~ 3
  )) %>%
  distinct(GID_1, year, .keep_all = TRUE)

admin2_afro <- admin2_afro %>%
  group_by(GID_1) %>%
  mutate(
    mean_sgq_admin2 = na.approx(mean_sgq_admin2, x = year, na.rm = FALSE, rule = 2),
    mean_svc_admin2 = na.approx(mean_svc_admin2, x = year, na.rm = FALSE, rule = 2),
    mean_fac_admin2 = na.approx(mean_fac_admin2, x = year, na.rm = FALSE, rule = 2)
  ) %>%
  ungroup() %>%
  group_by(GID_2, group_yr) %>%
  mutate(
    mean_sgq_admin2 = mean(mean_sgq_admin2, na.rm = TRUE),
    mean_svc_admin2 = mean(mean_svc_admin2, na.rm = TRUE),
    mean_fac_admin2 = mean(mean_fac_admin2, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  distinct()

panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(admin1_afro, by = c("group_yr", "GID_0", "GID_1"))

panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(admin2_afro, by = c("group_yr", "GID_0", "GID_1", "GID_2"))

# Read in and compute night lights
admin1_nl <- read_csv("00_rawdata/nightlights/topcodefix/processed_topcodefix_nl_admin1.csv") %>%
  select(GID_0, GID_1, starts_with("mean")) %>%
  pivot_longer(mean_2005:mean_2016, 
               names_to = "year",
               names_pattern = "mean_(\\d+)",
               values_to = "mean_nl") %>%
  mutate(year = as.numeric(year)) %>%
  mutate(group_yr = case_when(
    year %in% 2005:2008 ~ 1,
    year %in% 2009:2011 ~ 2,
    year %in% 2012:2015 ~ 3
  )) %>%
  group_by(GID_1, group_yr) %>%
  mutate(mean_nl = mean(mean_nl, na.rm = TRUE))

panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(admin1_nl, by = c("group_yr", "GID_0", "GID_1"))

admin2_nl <- read_csv("00_rawdata/nightlights/topcodefix/processed_topcodefix_nl_admin2.csv") %>%
  select(GID_0, GID_1, GID_2, starts_with("mean")) %>%
  pivot_longer(mean_2005:mean_2016, 
               names_to = "year",
               names_pattern = "mean_(\\d+)",
               values_to = "mean_nl") %>%
  mutate(year = as.numeric(year)) %>%
  mutate(group_yr = case_when(
    year %in% 2005:2008 ~ 1,
    year %in% 2009:2011 ~ 2,
    year %in% 2012:2015 ~ 3
  )) %>%
  group_by(GID_2, group_yr) %>%
  mutate(mean_nl = mean(mean_nl, na.rm = TRUE))

panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(admin2_nl, by = c("group_yr", "GID_0", "GID_1", "GID_2"))

# Calculate distances from centroids to capital city
cap_city <- read_csv("00_rawdata/country-capital-lat.csv")

shp_admin2 <- shp_admin2 %>%
  st_transform(4326) %>%
  group_by(GID_0) %>%
  mutate(
    centroid = st_centroid(geometry)
  ) %>%
  left_join(cap_city, by = "GID_0") %>%
  rowwise() %>%
  mutate(
    capital_point = st_sfc(st_point(c(Longitude, Latitude)), crs = 4326),
    distance_to_capital = drop_units(st_distance(centroid, capital_point, by_element = TRUE) / 1000)
  )

shp_admin1 <- shp_admin1 %>%
  st_transform(4326) %>%
  group_by(GID_0) %>%
  mutate(
    centroid = st_centroid(geometry)
  ) %>%
  left_join(cap_city, by = "GID_0") %>%
  rowwise() %>%
  mutate(
    capital_point = st_sfc(st_point(c(Longitude, Latitude)), crs = 4326),
    distance_to_capital = drop_units(st_distance(centroid, capital_point, by_element = TRUE) / 1000)
  )

panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(st_drop_geometry(shp_admin1) %>% select(distance_to_capital, GID_1), by = "GID_1")

panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(st_drop_geometry(shp_admin2) %>% select(distance_to_capital, GID_2), by = "GID_2")

# SPEI Index
spei <- terra::rast("00_rawdata/spei_yearly_12mnth.tif")

spei_admin1 <- terra::extract(
  spei,
  shp_admin1,
  fun = mean,
  na.rm = TRUE,
  exact = TRUE
)

shp_admin1$avg_spei_admin1 <- spei_admin1[, 2]

spei_admin2 <- terra::extract(
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

# Distance to nearest town of 100k
cities <- st_read("00_rawdata/Africapolis_GIS_2024.gpkg")

cities_100k <- cities %>%
  filter(Population_2010 > 100000 & Select_Geometry_Year == 2015) %>%
  select(1:5, Population_2010, Population_2015, Built.up_2015) %>%
  st_drop_geometry() %>%
  st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4326)

# Compute admin1 distances between projects and cities
distance_matrix1 <- st_nn(shp_admin1$centroid, cities_100k, k = 1, returnDist = TRUE, progress = TRUE)

# Find nearest city for each project
shp_admin1$nearest_city_dist <- sapply(distance_matrix1$dist, function(x) x[1] / 1000)

# Compute admin2 distances between projects and cities
distance_matrix2 <- st_nn(shp_admin2$centroid, cities_100k, k = 1, returnDist = TRUE, progress = TRUE)

# Find nearest city for each project
shp_admin2$nearest_city_dist <- sapply(distance_matrix2$dist, function(x) x[1] / 1000)

panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(st_drop_geometry(shp_admin1) %>% select(nearest_city_dist, GID_1), by = "GID_1")

panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(st_drop_geometry(shp_admin2) %>% select(nearest_city_dist, GID_2), by = "GID_2")

# Degree of urbanization
process_file <- function(file_name) {
  year <- stringr::str_extract(file_name, "(?<=_GADM41_)\\d{4}(?=_level)")
  data <- read.csv(file_name)
  data <- data %>%
    mutate(Year = as.numeric(year)) %>%
    filter(GID_0GHSL %in% countries_iso3 & Year == 2010) %>%
    select(any_of(c("GID_1", "GID_2")), Urban_share)
  return(data)
}

urb_files_admin1 <- list.files("00_rawdata/degurba", pattern = "level1", full.names = TRUE)

urb_admin1 <- lapply(urb_files_admin1, process_file) %>%
  bind_rows()

panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(urb_admin1, by = "GID_1")

urb_files_admin2 <- list.files("00_rawdata/degurba", pattern = "level2", full.names = TRUE)

urb_admin2 <- lapply(urb_files_admin2, process_file) %>%
  bind_rows()

panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(urb_admin2, by = "GID_2")

# WGI governance effectiveness as a control
wgi <- readxl::read_xlsx("00_rawdata/wgidataset.xlsx") %>%
  select(GID_0, year, ge_pct)%>%
  mutate(
    group_yr = case_when(
    year %in% 2005:2008 ~ 1,
    year %in% 2009:2011 ~ 2,
    year %in% 2012:2015 ~ 3
  )
  )%>%
  group_by(group_yr) %>%
  mutate(mean_ge = mean(ge_pct, na.rm = TRUE))

panel_aid_admin1 <- panel_aid_admin1 %>%
  select(-mean_2004, -mean_2017, -GID_0.y, -GID_0.x.x, -GID_0.y.y) %>%
  rename(GID_0 = GID_0.x) %>%
  left_join(wgi, by = c("group_yr", "GID_0")) 

panel_aid_admin2 <- panel_aid_admin2 %>%
  select(-mean_2004, -mean_2017, -GID_0.y, -GID_0.x.x, -GID_0.y.y) %>%
  rename(GID_0 = GID_0.x) %>%
  left_join(wgi, by = c("group_yr" = "year", "GID_0"))

panel_aid_admin2 <- panel_aid_admin2 %>%
  select(-geometry) %>%
  st_drop_geometry()

panel_aid_admin1 <- panel_aid_admin1 %>%
  select(-geometry) %>%
  st_drop_geometry()


panel_aid_admin1 <- panel_aid_admin1 %>%
    select(-starts_with("year"), 
    #-ge_pct, 
    #-wave,
    -afro_count)%>%
    distinct()

panel_aid_admin2 <- panel_aid_admin2 %>%
    select(-starts_with("year"), #-ge_pct, 
    #-wave
    -afro_count)%>%
    distinct()



# Save final processed data
write_csv(panel_aid_admin1, "01_panel_data/panel_aid_admin1_grouped.csv")
write_csv(panel_aid_admin2, "01_panel_data/panel_aid_admin2_grouped.csv")
