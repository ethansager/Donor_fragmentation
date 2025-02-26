# Cleaning Crisis
if (!require("pacman")) {
    install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  here,
  readr,
  ### GEO Packages
  sf,
  spdep,
  spatstat,
  geodata,
  terra,
  tmap
)

sf::sf_use_s2(FALSE)

emdat <- openxlsx::read.xlsx("00_rawdata/public_emdat_custom_request_2025-02-17_c70922f5-cb0d-4162-ad6f-83809e87c7c0.xlsx")

pend <- st_read("00_rawdata/pend-gdis-1960-2018-disasterlocations.gpkg")

pend <- pend %>%
  mutate(DisNo. = paste0(disasterno, "-", iso3))

pend <- pend %>%
  left_join(., emdat, by = "DisNo.")

table(pend$Start.Year, pend$disastertype)

# Read admin1 panel data
admin1_panel <- read.csv("01_panel_data/panel_aid_admin1.csv")

countries_iso3 <- c(
  "NGA", "MOZ", "MWI", "KEN", "GHA"
)

shp <- geodata::gadm(country = countries_iso3, level = 1, path = "00_rawdata/shapefiles/")

shp <- sf::st_as_sf(shp)

shp <- shp %>%
  select(GID_1, geometry)

# Add geometry to admin1_panel
admin1_panel <- admin1_panel %>%
  left_join(shp, by = "GID_1") %>%
  sf::st_as_sf()


# Convert disaster dates to proper format if needed
pend <- pend %>%
  mutate(
    start_date = as.Date(Start.Year),
    end_date = as.Date(End.Year)
  )

# Fix geometries and perform spatial join of disasters with admin1 boundaries
admin1_panel <- admin1_panel %>% st_make_valid()
pend <- pend %>% st_make_valid()

admin1_disasters <- st_join(admin1_panel, pend)

# Count disasters by admin1 and year
disaster_counts <- admin1_disasters %>%
  group_by(GID_1, paymentyear) %>%
  summarize(
    disaster_count = sum(
        (paymentyear >= start_date & paymentyear <= end_date), 
      na.rm = TRUE
    ),
    disaster_dummy = if_else(disaster_count > 0, 1, 0)
  )
  
disaster_counts <- disaster_counts%>%
  st_drop_geometry()

# Merge back with original panel
admin1_panel_with_disasters <- admin1_panel %>%
  left_join(disaster_counts, by = c("GID_1", "paymentyear"))%>%
  st_drop_geometry()

# Write admin1 panel with disasters
write.csv(admin1_panel_with_disasters, "01_panel_data/panel_aid_admin1_with_disasters.csv", row.names = FALSE)

# Read admin2 panel data
admin2_panel <- read.csv("01_panel_data/panel_aid_admin2.csv")

# Get admin2 shapefile
shp_admin2 <- geodata::gadm(country = countries_iso3, level = 2, path = "00_rawdata/shapefiles/")

shp_admin2 <- sf::st_as_sf(shp_admin2)
shp_admin2 <- shp_admin2 %>%
    select(GID_2, geometry)

# Add geometry to admin2_panel
admin2_panel <- admin2_panel %>%
    left_join(shp_admin2, by = "GID_2") %>%
    sf::st_as_sf()

# Fix geometries and perform spatial join
admin2_panel <- admin2_panel %>% st_make_valid()
admin2_disasters <- st_join(admin2_panel, pend)

# Count disasters by admin2 and year
disaster_counts_admin2 <- admin2_disasters %>%
    group_by(GID_2, paymentyear) %>%
    summarize(
        disaster_count = sum(
            (paymentyear >= start_date & paymentyear <= end_date),
            na.rm = TRUE
        ),
        disaster_dummy = if_else(disaster_count > 0, 1, 0)
    ) %>%
    st_drop_geometry()

# Merge back with original panel
admin2_panel_with_disasters <- admin2_panel %>%
    left_join(disaster_counts_admin2, by = c("GID_2", "paymentyear")) %>%
    st_drop_geometry()


# Write admin2 panel with disasters
write.csv(admin2_panel_with_disasters, "01_panel_data/panel_aid_admin2_with_disasters.csv", row.names = FALSE)
