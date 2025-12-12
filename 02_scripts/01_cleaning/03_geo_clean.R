# Cleaning datasets script
# Set up and packages  ----------------------------------------------------
setwd("C:/Users/eman7/Dropbox/GitHub/ra_work/Donor_fragmentation/")

if (!require("pacman")) {
  install.packages("pacman")
}
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

summary(dat)


# This gives us 33139 aid projects

shp <- read_sf("00_rawdata/shapefiles/gadm_admin2.shp") %>%
  st_make_valid() %>% # Ensure geometries are valid
  st_transform(4326) # Transform to WGS84 CRS

shp <- sf::st_as_sf(shp)

# Step 3: Convert your aid dataset to an sf object
aid_points <- st_as_sf(
  dat,
  coords = c("longitude", "latitude"), # Specify longitude and latitude columns
  crs = 4326 # Set CRS (WGS84)
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

hhi_results_admin2 <- hhi_results_admin2[,
  {
    total_aid <- sum(total_disb_admin2, na.rm = TRUE)
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
  },
  by = .(GID_0, GID_2, paymentyear)
]

# Create HHI for admin1 level
hhi_results_admin1 <- dat[, .(
  total_disb_admin1 = sum(abs(disb_loc_evensplit), na.rm = TRUE),
  total_early_projects = sum(early_impact == 1, na.rm = TRUE),
  total_late_projects = sum(early_impact == 0, na.rm = TRUE)
), by = .(GID_0, GID_1, paymentyear, donor)]

hhi_results_admin1 <- hhi_results_admin1[,
  {
    total_aid <- sum(total_disb_admin1, na.rm = TRUE)
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
  },
  by = .(GID_0, GID_1, paymentyear)
]



# Descriptives region count by admin level

n_distinct(hhi_results_admin1$GID_1)
n_distinct(hhi_results_admin2$GID_2)

# Step 2: Merge the HHI results back into the main dataset

panel_aid_admin1 <- dat %>%
  sf::st_drop_geometry() %>%
  filter(!is.na(GID_1)) %>%
  tidylog::inner_join(hhi_results_admin1, by = c("GID_0", "GID_1", "paymentyear")) %>%
  select(GID_0, GID_1, paymentyear, ends_with("_admin1"), -contains("total_disb")) %>%
  distinct()

check <- panel_aid_admin1 %>%
  group_by(GID_0, GID_1, paymentyear) %>%
  summarise(count = n()) %>%
  filter(count > 1)

panel_aid_admin2 <- dat %>%
  sf::st_drop_geometry() %>%
  filter(!is.na(GID_1)) %>%
  tidylog::left_join(hhi_results_admin2, by = c("GID_0", "GID_2", "paymentyear")) %>%
  select(GID_0, GID_1, GID_2, paymentyear, ends_with("_admin2"), -contains("total_disb")) %>%
  distinct()

check <- panel_aid_admin2 %>%
  group_by(GID_0, GID_1, GID_2, paymentyear) %>%
  summarise(count = n()) %>%
  filter(count > 1)

n_distinct(panel_aid_admin1$GID_1)
n_distinct(panel_aid_admin2$GID_2)

# population grid GWP

admin1_pop <- read_csv("00_rawdata/population/admin1_population.csv") %>%
  pivot_longer(
    cols = starts_with("sum_"),
    names_to = "year",
    values_to = "pop"
  ) %>%
  select(GID_1, year, pop) %>%
  mutate(
    year = as.numeric(gsub("sum_", "", year)),
    ln_pop = log(pop)
  )

admin2_pop <- read_csv("00_rawdata/population/admin2_population.csv") %>%
  pivot_longer(
    cols = starts_with("sum_"),
    names_to = "year",
    values_to = "pop"
  ) %>%
  select(GID_2, year, pop) %>%
  mutate(
    year = as.numeric(gsub("sum_", "", year)),
    ln_pop = log(pop)
  )

# Merge with panel data
panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(admin1_pop, by = c("GID_1", "paymentyear" = "year")) %>%
  rename(ln_pop_admin1 = ln_pop)

panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(admin2_pop, by = c("GID_2", "paymentyear" = "year")) %>%
  rename(ln_pop_admin2 = ln_pop)

###### Read in Afro data and create panel to match ########

admin1_afro <- read_csv("00_rawdata/ab_raw/processed/admin1_afro_panel.csv") %>%
  select(year, GID_0, GID_1, starts_with("mean_"), afro_count, wave)

admin2_afro <- read_csv("00_rawdata/ab_raw/processed/admin2_afro_panel.csv") %>%
  select(year, GID_0, GID_1, GID_2, starts_with("mean_"), afro_count, wave)

# Merge the Afro data into the panel data
panel_aid_admin1 <- panel_aid_admin1 %>%
  dplyr::inner_join(
    admin1_afro,
    by = c("paymentyear" = "year", "GID_0", "GID_1")
  ) %>%
  rename(year = paymentyear)

panel_aid_admin2 <- panel_aid_admin2 %>%
  dplyr::inner_join(
    admin2_afro,
    by = c("paymentyear" = "year", "GID_0", "GID_1", "GID_2")
  ) %>%
  rename(year = paymentyear)

### Read in and compute night lights
admin1_dep_vars <- read_csv("00_rawdata/processed_dep_vars_admin1.csv")

panel_aid_admin1 <- panel_aid_admin1 %>%
  fill(GID_0, .direction = "up") %>%
  left_join(admin1_dep_vars, by = c("year", "GID_0", "GID_1"))

admin2_dep_vars <- read_csv("00_rawdata/processed_dep_vars_admin2.csv")

panel_aid_admin2 <- panel_aid_admin2 %>%
  fill(GID_0, GID_1, .direction = "up") %>%
  left_join(admin2_dep_vars, by = c("year", "GID_0", "GID_1", "GID_2"))


### okay  WGI governace effectiveness as a control
wgi <- readxl::read_xlsx("00_rawdata/wgidataset.xlsx") %>%
  select(GID_0, year, ge_pct)


panel_aid_admin1 <- panel_aid_admin1 %>%
  left_join(wgi, by = c("year", "GID_0"))

panel_aid_admin2 <- panel_aid_admin2 %>%
  left_join(wgi, by = c("year", "GID_0"))


# Select final data
panel_aid_admin1_fin <- panel_aid_admin1 %>%
  select(
    GID_0, GID_1, year,
    starts_with("frag_"), starts_with("mean_"),
    mean_nl = mean,
    sum_nl = sum,
    u5m,
    # capital_region,
    total_early_admin1, total_late_admin1, total_proj_admin1,
    total_aid_admin1, donor_count_admin1, hhi_admin1,
    ln_pop_admin1, afro_count, wave,
    # distance_to_capital, capital_region, #spei_admin1,
    # nearest_city_dist, urban_share = Urban_share,
    # ge_pct
  )

panel_aid_admin2_fin <- panel_aid_admin2 %>%
  select(
    GID_0, GID_1, GID_2, year,
    starts_with("frag_"), starts_with("mean_"),
    mean_nl = mean,
    sum_nl = sum,
    u5m,
    # capital_region,
    total_early_admin2, total_late_admin2, total_proj_admin2,
    total_aid_admin2, donor_count_admin2, hhi_admin2,
    ln_pop_admin2, afro_count, wave,
    # distance_to_capital, capital_region, #spei_admin2,
    # nearest_city_dist, urban_share = Urban_share, ge_pct
  )

# # Read in the disaster information file
# disaster_data1 <- read_csv("01_panel_data/panel_aid_admin1_with_disasters.csv")
# disaster_data2 <- read_csv("01_panel_data/panel_aid_admin2_with_disasters.csv")

# # Merge disaster info into the admin1 panel based on GID_0, GID_1, and paymentyear
# panel_aid_admin1_fin <- panel_aid_admin1_fin%>%
#   left_join(
#     disaster_data1 %>%
#     select(GID_1, year = paymentyear, disaster_dummy, disaster_count),
#     by = c("GID_1", "year")
#   )

# # Merge disaster info into the admin2 panel based on GID_0, GID_1, GID_2, and paymentyear
# panel_aid_admin2_fin <- panel_aid_admin2_fin %>%
#   left_join(
#     disaster_data2 %>% select(GID_2, paymentyear, disaster_dummy, disaster_count),
#     by = c("GID_2", "paymentyear")
#   )

### SAVE OUT FINAL PROCESSED DATA ###
write_csv(panel_aid_admin1, "01_panel_data/panel_aid_admin1.csv")
write_csv(panel_aid_admin2, "01_panel_data/panel_aid_admin2.csv")
