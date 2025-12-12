#Make figure 1 and 2
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
  geodata
)


### Make Figure for Fragmentation
study_countries <- c(
  "BEN",
  "BWA",
  "GHA",
  "MDG",
  "MLI",
  "MOZ",
  "MWI",
  "NAM",
  "SEN",
  "TZA",
  "ZAF",
  "ZMB",
  "ZWE"
)

panel_aid <- read_csv("01_panel_data/panel_aid_admin1.csv") %>%
  filter(GID_0 %in% study_countries & year == 2015)

african_iso3_ssa <- c(
  "AGO",
  "BEN",
  "BWA",
  "BFA",
  "BDI",
  "CMR",
  "CAF",
  "TCD",
  "COG",
  "COD",
  "DJI",
  "GNQ",
  "ERI",
  "SWZ",
  "ETH",
  "GAB",
  "GMB",
  "GHA",
  "GIN",
  "GNB",
  "CIV",
  "KEN",
  "LSO",
  "LBR",
  "MDG",
  "MWI",
  "MLI",
  "MRT",
  "MOZ",
  "NAM",
  "NER",
  "NGA",
  "RWA",
  "SEN",
  "SLE",
  "SOM",
  "ZAF",
  "SSD",
  "SDN",
  "TZA",
  "TGO",
  "UGA",
  "ZMB",
  "ZWE"
)

africa_admin1 <- sf::st_make_valid(
  sf::st_as_sf(geodata::gadm(
    country = african_iso3_ssa,
    level = 1,
    path = "00_rawdata/shapefiles/"
  ))
)

# Join while ensuring unmatched regions are retained
admin1_sf <- africa_admin1 %>%
  left_join(panel_aid, by = c("GID_0", "GID_1")) # Keep all regions

# Plot
ggplot(data = admin1_sf) +
  geom_sf(aes(fill = mean_sgq_admin1)) +
  scale_fill_viridis_c(
    option = "viridis",
    name = "Mean Value",
    na.value = "grey90"
  ) +
  theme_minimal()

panel_aid <- read_csv("01_panel_data/panel_aid_admin1_fin.csv")

# Calculate country-level averages
country_avgs <- panel_aid %>%
  group_by(GID_1, year) %>%
  summarize(
    country_mean_sgq = mean(mean_sgq_admin1, na.rm = TRUE),
    country_mean_fragmentation = mean(frag_index_admin1, na.rm = TRUE)
  ) %>%
  ungroup()

# Create scatter plot with regression line
ggplot(
  country_avgs,
  aes(x = country_mean_fragmentation, y = country_mean_sgq)
) +
  geom_point(size = 3, color = "black", alpha = 0.7) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(
    x = "Mean Fragmentation",
    y = "Mean Quality Score (SGQ)"
  ) +
  theme_minimal()


# Calculate country-level averages
country_avgs <- panel_aid %>%
  group_by(GID_2, year) %>%
  filter(frag_index_admin2 != 0 & frag_index_admin2 != 1) %>%
  summarize(
    country_mean_sgq = mean(mean_sgq_admin2, na.rm = TRUE),
    country_mean_fragmentation = mean(frag_index_admin2, na.rm = TRUE)
  ) %>%
  ungroup()

# Create scatter plot with regression line
ggplot(
  country_avgs,
  aes(x = country_mean_fragmentation, y = country_mean_sgq)
) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  geom_point(size = 3, color = "black", alpha = 0.7) +
  labs(
    x = "Mean Fragmentation",
    y = "Mean Quality Score (SGQ)"
  ) +
  theme_minimal()
