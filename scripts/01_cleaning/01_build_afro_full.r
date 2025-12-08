setwd("C:/Users/eman7/Dropbox/GitHub/ra_work/Donor_fragmentation/")

pacman::p_load(
  tidyverse,
  haven,
  sjmisc,
  labelled,
  here,
  readr,
  zoo,
  data.table,
  scales,
  psych,
  retroharmonize,
  readxl,
  assertr,
  lubridate,
  ### GEO Packages
  sf,
  spdep,
  spatstat,
  geodata,
  terra
)


data_list <- list()

files <- list.files(here("00_rawdata", "ab_raw_geo"), pattern = ".csv", full.names = TRUE, recursive = TRUE)


# Read each CSV and append to the list
data_list <- c(data_list, lapply(files, function(x) {
  read_csv(x) %>%
    mutate(wave = as.numeric(str_extract(x, "r(\\d+)", group = 1)))
}))


# Combine all dataframes into one
afro_merged <- do.call(bind_rows, data_list)

# Create variables consolidated across waves using case_when
afro_merged <- afro_merged %>%
  mutate(
    trust_your_elected_local_government_council = case_when(
      wave == 3 ~ q55d,
      wave == 4 ~ q49d,
      wave == 5 ~ q59e,
      wave == 6 ~ q59e
    ),
    performance_local_government_councilor = case_when(
      wave == 3 ~ q68c,
      wave == 4 ~ q70c,
      wave == 5 ~ q71c,
      wave == 6 ~ q68c
    ),
    local_govt_handling_maintaining_roads = case_when(
      wave == 3 ~ q67a,
      wave == 4 ~ q59a,
      wave == 5 ~ q66a,
      wave == 6 ~ q67a
    ),
    local_govt_handling_maintaining_local_markets = case_when(
      wave == 4 ~ q59b,
      wave == 5 ~ q66b,
      wave == 6 ~ q67b
    ),
    contact_local_government_councilor = case_when(
      wave == 3 ~ as.numeric(q32a), # Note issue of #!null from sql database
      wave == 4 ~ q25a,
      wave == 5 ~ q30a,
      wave == 6 ~ q24a
    ),
    local_government_councilors_listen = case_when(
      wave == 3 ~ q62b,
      wave == 4 ~ q54b,
      wave == 5 ~ q62b,
      wave == 6 ~ q59b
    ),
    corruption_local_government_councilors = case_when(
      wave == 3 ~ q56c,
      wave == 4 ~ q50c,
      wave == 5 ~ q60d,
      wave == 6 ~ q53d
    )
  )


# Load utility function for recoding
source(here("scripts", "utils", "afro_processing_utils.r"))

afro_merged <- afro_merged %>%
  tidylog::mutate(
    # corruption should be reverse coded
    corruption_rec = recode_four_point(corruption_local_government_councilors, reverse = TRUE),
    trust_rec = recode_four_point(trust_your_elected_local_government_council),
    contact_rec = recode_four_point(contact_local_government_councilor),
    maintian_road_rec = recode_four_point(local_govt_handling_maintaining_roads),
    maintian_market_rec = recode_four_point(local_govt_handling_maintaining_local_markets),
    preformance_rec = recode_four_point(performance_local_government_councilor),
    listen_rec = recode_four_point(local_government_councilors_listen)
  )

# Descriptive
descrips <- afro_merged %>%
  group_by(wave, country) %>%
  summarise(across(ends_with("_rec"),
    list(
      mean = ~ mean(., na.rm = TRUE),
      missing_count = ~ sum(is.na(.))
    ),
    .names = "{.col}_{.fn}"
  ))

# construct index subgovernemnt qaulity index
# Convert to data.table
afro_merged <- as.data.table(afro_merged)

# Calculate index
afro_merged[, `:=`(
  sub_gov_qual = rowSums(.SD, na.rm = TRUE) / rowSums(!is.na(.SD)),
  non_miss = rowSums(!is.na(.SD))
), .SDcols = patterns("_rec$")]

# Rescale
range_vals <- range(afro_merged$sub_gov_qual, na.rm = TRUE)
afro_merged[, sgqi := (sub_gov_qual - range_vals[1]) / diff(range_vals) * 100]

summary(afro_merged$sgqi, na.rm = TRUE)

# clean ea items
afro_merged <- afro_merged %>%
  tidylog::mutate(across(
    starts_with("ea_"), # Select variables starting with "ea_"
    ~ if_else(. == 1, 1, 0), # Recode: 1 for "Yes", 0 for others
    .names = "{.col}_rec" # Create new variables with "_rec" suffix
  ))

descrips2 <- afro_merged %>%
  group_by(wave, country) %>%
  summarise(across(
    starts_with("ea_") & ends_with("_rec"), # Select variables starting with "ea_" and ending with "_rec"
    list(
      mean = ~ mean(., na.rm = TRUE), # Calculate mean, ignoring NA values
      missing_count = ~ sum(is.na(.)) # Count missing values
    ),
    .names = "{.col}_{.fn}" # Append function name to new column names
  ))

# construct ea indexes
# Convert to data.table if not already
afro_merged <- as.data.table(afro_merged)

# Calculate EA indices
afro_merged[, `:=`(
  ea_svc_index = rowSums(.SD[, .SD, .SDcols = patterns("^ea_svc.*_rec$")], na.rm = TRUE),
  ea_fac_index = rowSums(.SD[, .SD, .SDcols = patterns("^ea_fac.*_rec$")], na.rm = TRUE),
  non_miss_ea = rowSums(!is.na(.SD[, .SD, .SDcols = patterns("^ea_.*_rec$")]))
)]


afro_merged[, country_code := str_sub(respno, 1, 3)]

table(afro_merged$country, afro_merged$country_code)

# #Wave 3
# 1=Benin, 2=Botswana, 3=Cape Verde, 4=Ghana, 5=Kenya, 6=Lesotho, 7=Madagascar, 8=Malawi,
# 9=Mali, 10=Mozambique, 11=Namibia, 12=Nigeria, 13=Senegal, 14=South Africa, 15=Tanzania, 16=Uganda,
# 17=Zambia, 18=Zimbabwe
# #Wave 4
# 1=Benin, 2=Botswana, 3=Burkina Faso, 4=Cape Verde, 5=Ghana, 6=Kenya, 7=Lesotho, 8=Liberia,
# 9=Madagascar, 10=Malawi, 11=Mali, 12=Mozambique, 13=Namibia, 14=Nigeria, 15=Senegal, 16=South Africa,
# 17=Tanzania, 18=Uganda, 19=Zambia, 20=Zimbabwe
# #Wave 5
# 1=Algeria, 2=Benin, 3=Botswana, 4=Burkina Faso, 5=Burundi, 6=Cameroon, 7=Cape Verde,
# 8=Cote d’Ivoire, 9=Egypt, 11=Ghana, 12=Guinea, 13=Kenya, 14=Lesotho, 15=Liberia, 16=Madagascar,
# 17=Malawi, 18=Mali, 19=Mauritius, 20=Morocco, 21=Mozambique, 22=Namibia, 23=Niger, 24=Nigeria,
# 25=Senegal, 26=Sierra Leone, 27=South Africa, 28=Sudan, 29=Swaziland, 30=Tanzania, 31=Togo, 32=Tunisia,
# 33=Uganda, 34=Zambia, 35=Zimbabwe
# #Wave 6
# 1=Algeria, 2=Benin, 3=Botswana, 4=Burkina Faso, 5=Burundi, 6=Cameroon, 7=Cape Verde,
# 8=Cote dIvoire, 9=Egypt, 10=Gabon, 11=Ghana, 12=Guinea, 13=Kenya, 14=Lesotho, 15=Liberia,
# 16=Madagascar, 17=Malawi, 18=Mali, 19=Mauritius, 20=Morocco, 21=Mozambique, 22=Namibia, 23=Niger,
# 24=Nigeria, 25=São Tomé and Príncipe, 26=Senegal, 27=Sierra Leone, 28=South Africa, 29=Sudan,
# 30=Swaziland, 31=Tanzania, 32=Togo, 33=Tunisia, 34=Uganda, 35=Zambia, 36=Zimbabwe

# Create country_name based on wave and country_code
# Create lookup tables for each wave
wave3_countries <- setNames(
  c(
    "Benin", "Botswana", "Cape Verde", "Ghana", "Kenya", "Lesotho", "Madagascar",
    "Malawi", "Mali", "Mozambique", "Namibia", "Nigeria", "Senegal", "South Africa",
    "Tanzania", "Uganda", "Zambia", "Zimbabwe"
  ), 1:18
)

wave4_countries <- setNames(
  c(
    "Benin", "Botswana", "Burkina Faso", "Cape Verde", "Ghana", "Kenya", "Lesotho",
    "Liberia", "Madagascar", "Malawi", "Mali", "Mozambique", "Namibia", "Nigeria",
    "Senegal", "South Africa", "Tanzania", "Uganda", "Zambia", "Zimbabwe"
  ), 1:20
)

wave5_countries <- setNames(
  c(
    "Algeria", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon",
    "Cape Verde", "Cote d'Ivoire", "Egypt", NA, "Ghana", "Guinea", "Kenya",
    "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritius", "Morocco",
    "Mozambique", "Namibia", "Niger", "Nigeria", "Senegal", "Sierra Leone",
    "South Africa", "Sudan", "Swaziland", "Tanzania", "Togo", "Tunisia",
    "Uganda", "Zambia", "Zimbabwe"
  ), 1:35
)

wave6_countries <- setNames(
  c(
    "Algeria", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon",
    "Cape Verde", "Cote d'Ivoire", "Egypt", "Gabon", "Ghana", "Guinea", "Kenya",
    "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritius", "Morocco",
    "Mozambique", "Namibia", "Niger", "Nigeria", "São Tomé and Príncipe",
    "Senegal", "Sierra Leone", "South Africa", "Sudan", "Swaziland", "Tanzania",
    "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe"
  ), 1:36
)

# Get country names for GADM
gadm_countries <- unique(unlist(list(wave3_countries, wave4_countries, wave5_countries, wave6_countries)))
gadm_countries <- gadm_countries[!is.na(gadm_countries)]

# Create lookup for different country name variations
country_lookup <- c(
  "Cape Verde" = "Cabo Verde",
  "Cote d'Ivoire" = "Côte d'Ivoire",
  "Swaziland" = "Eswatini"
)

# Download and process GADM data
gadm_list <- list()
for (country in gadm_countries) {
  tryCatch(
    {
      # Use lookup table if needed
      gadm_name <- ifelse(country %in% names(country_lookup),
        country_lookup[country],
        country
      )

      # Get GADM data
      gadm_data <- gadm(country = gadm_name, level = 2, version = "3.6", path = "00_rawdata/shapefiles/")

      # Convert to sf
      gadm_sf <- st_as_sf(gadm_data)
      gadm_list[[country]] <- gadm_sf
    },
    error = function(e) {
      message(sprintf("Error processing %s: %s", country, e$message))
    }
  )
}

# Combine all GADM data
gadm_combined <- do.call(rbind, gadm_list)

# Convert survey points to sf
afro_sf <- afro_merged %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Spatial join
afro_sf <- st_join(afro_sf, sf::st_make_valid(gadm_combined)) %>%
  st_drop_geometry() %>%
  select(respno, wave, GID_0, GID_1, GID_2, COUNTRY = NAME_0) %>%
  distinct()

afro_merged <- afro_merged %>%
  distinct(respno, wave, .keep_all = TRUE) %>%
  left_join(.,
    afro_sf,
    by = c("respno", "wave")
  )



# Ad-hoc fix for Tanzania and Swaziland
look <- afro_merged %>%
  filter(country_code %in% c("SWZ", "TAN"))

# Get the most common GID_0 for each country code
most_common_gid <- look %>%
  group_by(country_code) %>%
  count(GID_0) %>%
  slice_max(n) %>%
  select(country_code, GID_0)

# Filter look to keep only remove rows not matching the most common GID_0 for each country_code
remove <- look %>%
  filter(!GID_0 %in% c("SWZ", "TZA")) %>%
  select(respno, wave)


#### Create final dataset #####
# Clean and standardize date formats to extract year
if ("dateintr" %in% colnames(afro_merged)) {
  # First check date formats
  date_check <- afro_merged %>%
    mutate(date_char = as.character(dateintr)) %>%
    summarise(
      min_date = min(dateintr, na.rm = TRUE),
      max_date = max(dateintr, na.rm = TRUE),
      na_count = sum(is.na(dateintr)),
      invalid_values = list(unique(date_char[
        !is.na(date_char) &
          !grepl("^\\d{4}-\\d{2}-\\d{2}$", date_char)
      ]))
    )

  print("Date of interview summary:")
  print(date_check)

  # Extract year from dates in various formats
  afro_merged <- afro_merged %>%
    mutate(
      dateintr = as.character(dateintr),
      year = case_when(
        # For standard date format (YYYY-MM-DD)
        !is.na(dateintr) & grepl("^\\d{4}-\\d{2}-\\d{2}$", dateintr) ~
          year(ymd(dateintr, quiet = TRUE)),

        # For other date formats with 4-digit year
        !is.na(dateintr) & grepl("\\d{4}", dateintr) ~
          as.numeric(str_extract(dateintr, "\\d{4}")),

        # For years such as 1-Apr-05
        !is.na(dateintr) & grepl("[a-z]-\\d{2}", dateintr) ~
          as.numeric(paste0("20", str_extract(dateintr, "(?<=-)[0-9]{2}"))),

        # Default to wave year estimate when date is unavailable
        !is.na(wave) ~ case_when(
          wave == 3 ~ 2005,
          wave == 4 ~ 2008,
          wave == 5 ~ 2012,
          wave == 6 ~ 2015,
          TRUE ~ NA_real_
        ),

        # Default case
        TRUE ~ NA_real_
      )
    )


  # Check the results
  year_summary <- afro_merged %>%
    group_by(wave) %>%
    summarise(
      year_min = min(year, na.rm = TRUE),
      year_max = max(year, na.rm = TRUE),
      year_na_count = sum(is.na(year)),
      year_counts = list(table(year))
    )

  print("Survey year summary by wave:")
  print(year_summary)
} else {
  print("dateintr column not found in afro_merged dataset")
}

afro_fin <- afro_merged %>%
  as.data.frame() %>%
  # Ensure `remove` is a data frame
  anti_join(as.data.frame(remove)) %>%
  # Grab the columns of interest
  select(respno,
    country = COUNTRY, wave, dateintr, year, latitude, longitude,
    starts_with("GID_"),
    sub_gov_qual, non_miss, sgqi,
    ea_svc_index, ea_fac_index, non_miss_ea,
    ends_with("_rec")
  )

write_csv(afro_fin, paste0(here("00_rawdata", "ab_raw", "processed"), "/afrobarometer_w3_w6_geomerged_new.csv"))


###### create panel to match ########

# Load utility functions
source(here("scripts", "utils", "afro_processing_utils.r"))

# Process admin1 panel using utility function
admin1_afro <- process_afro_panel(
  afro_data = afro_merged,
  admin_level = "admin1",
  year_range = c(2005, 2015)
)

summary(admin1_afro$mean_sgq_admin1, na.rm = TRUE)

### admin 2
# Process admin2 panel using utility function
admin2_afro <- process_afro_panel(
  afro_data = afro_merged,
  admin_level = "admin2",
  year_range = c(2005, 2015)
)

summary(admin1_afro$mean_sgq_admin1, na.rm = TRUE)
summary(admin2_afro$mean_sgq_admin2, na.rm = TRUE)

# Admin1
write_csv(admin1_afro, paste0(here("00_rawdata", "ab_raw", "processed"), "/admin1_afro_panel.csv"))
# Admin2
write_csv(admin2_afro, paste0(here("00_rawdata", "ab_raw", "processed"), "/admin2_afro_panel.csv"))
