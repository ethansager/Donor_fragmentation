# Cleaning datasets script (5-year buckets: 1995-2015)

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
  sf,
  geodata,
  terra,
  nngeo
)

source(here("02_scripts", "utils", "godad_processing_utils.r"))
source(here("02_scripts", "utils", "panel_time_windows.r"))

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
cfg <- get_5year_panel_config_1995_2015()
START_YEAR <- cfg$start_year
END_YEAR <- cfg$end_year
PANEL_TAG <- cfg$panel_tag
PERIOD_KEYS <- cfg$period_keys
TARGET_YEARS <- START_YEAR:END_YEAR
ALLOW_SYNTHETIC_EXTENSION <- TRUE

safe_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}

extract_years_from_paths <- function(paths) {
  extracted <- gsub(".*?(\\d{4}).*", "\\1", basename(paths))
  years <- suppressWarnings(as.integer(extracted))
  sort(unique(years[is.finite(years)]))
}

fill_series_with_edges <- function(values, years) {
  values <- as.numeric(values)
  years <- as.integer(years)

  observed_idx <- which(is.finite(values))
  if (length(observed_idx) == 0) {
    return(rep(NA_real_, length(values)))
  }

  out <- zoo::na.approx(values, x = years, na.rm = FALSE, rule = 1)

  first_idx <- observed_idx[[1]]
  last_idx <- observed_idx[[length(observed_idx)]]
  first_year <- years[[first_idx]]
  last_year <- years[[last_idx]]

  out[years < first_year] <- values[[first_idx]]
  out[years > last_year] <- values[[last_idx]]

  out
}

build_annual_afro <- function(
  afro_raw,
  admin_id,
  sgq_col,
  status_col
) {
  id_cols <- c("GID_0", admin_id)
  mean_cols <- names(afro_raw)[grepl("^mean_", names(afro_raw))]

  prepped <- afro_raw %>%
    select(all_of(id_cols), year, all_of(mean_cols), afro_count, wave) %>%
    group_by(across(all_of(c(id_cols, "year")))) %>%
    summarise(
      across(all_of(mean_cols), safe_mean),
      afro_count = safe_mean(afro_count),
      wave = safe_max(wave),
      .groups = "drop"
    ) %>%
    group_by(across(all_of(id_cols))) %>%
    tidyr::complete(year = TARGET_YEARS) %>%
    ungroup()

  split_key <- interaction(
    prepped$GID_0,
    prepped[[admin_id]],
    drop = TRUE,
    lex.order = TRUE
  )
  by_unit <- split(prepped, split_key)

  filled <- lapply(by_unit, function(unit_df) {
    unit_df <- unit_df %>% arrange(year)

    for (mean_col in mean_cols) {
      unit_df[[mean_col]] <- fill_series_with_edges(
        unit_df[[mean_col]],
        unit_df$year
      )
    }

    observed_years <- unit_df$year[
      !is.na(unit_df$wave) & is.finite(unit_df[[sgq_col]])
    ]
    if (length(observed_years) == 0) {
      observed_years <- unit_df$year[is.finite(unit_df[[sgq_col]])]
    }

    if (length(observed_years) == 0) {
      unit_df[[status_col]] <- NA_character_
    } else {
      first_obs_year <- min(observed_years)
      last_obs_year <- max(observed_years)
      unit_df[[status_col]] <- dplyr::case_when(
        unit_df$year < first_obs_year ~ "backfilled_pre2005",
        unit_df$year > last_obs_year ~ "carry_forward_post2015",
        !is.na(unit_df$wave) ~ "observed",
        TRUE ~ "interpolated"
      )
    }

    unit_df
  })

  bind_rows(filled)
}

bucket_afro <- function(
  annual_afro,
  admin_id,
  status_col
) {
  annual_afro %>%
    mutate(
      year = map_to_5year_bucket_1995_2015(year),
      sgq_status_year = .data[[status_col]]
    ) %>%
    filter(!is.na(year)) %>%
    group_by(across(all_of(c("GID_0", admin_id, "year")))) %>%
    summarise(
      across(starts_with("mean_"), safe_mean),
      afro_count = safe_mean(afro_count),
      wave = safe_max(wave),
      sgq_status_observed_share = mean(
        sgq_status_year == "observed",
        na.rm = TRUE
      ),
      sgq_status_interpolated_share = mean(
        sgq_status_year == "interpolated",
        na.rm = TRUE
      ),
      sgq_status_backfilled_share = mean(
        sgq_status_year == "backfilled_pre2005",
        na.rm = TRUE
      ),
      sgq_status_carry_forward_share = mean(
        sgq_status_year == "carry_forward_post2015",
        na.rm = TRUE
      ),
      sgq_status = {
        statuses <- sgq_status_year[!is.na(sgq_status_year)]
        if (length(statuses) == 0) {
          NA_character_
        } else if (dplyr::n_distinct(statuses) == 1) {
          statuses[[1]]
        } else {
          "mixed"
        }
      },
      .groups = "drop"
    ) %>%
    mutate(
      sgq_status_observed_share = if_else(
        is.nan(sgq_status_observed_share),
        NA_real_,
        sgq_status_observed_share
      ),
      sgq_status_interpolated_share = if_else(
        is.nan(sgq_status_interpolated_share),
        NA_real_,
        sgq_status_interpolated_share
      ),
      sgq_status_backfilled_share = if_else(
        is.nan(sgq_status_backfilled_share),
        NA_real_,
        sgq_status_backfilled_share
      ),
      sgq_status_carry_forward_share = if_else(
        is.nan(sgq_status_carry_forward_share),
        NA_real_,
        sgq_status_carry_forward_share
      )
    )
}

# -----------------------------------------------------------------------------
# Data readiness gate
# -----------------------------------------------------------------------------
nightlight_years <- c(
  list.files(
    here("00_rawdata", "nightlights", "africa"),
    pattern = "\\.tif$",
    full.names = TRUE
  ),
  list.files(
    here("00_rawdata", "nightlights", "topcodefix"),
    pattern = "^DMSP.*\\.tif$",
    full.names = TRUE
  )
) %>%
  extract_years_from_paths()
print_year_support("Nightlights raster year support", nightlight_years)
if (!ALLOW_SYNTHETIC_EXTENSION) {
  assert_year_coverage(
    "Nightlights raster support",
    nightlight_years,
    START_YEAR:END_YEAR
  )
} else {
  missing_nl <- setdiff(START_YEAR:END_YEAR, nightlight_years)
  if (length(missing_nl) > 0) {
    message(
      "Nightlights missing years accepted under synthetic extension: ",
      paste(missing_nl, collapse = ", ")
    )
  }
}

u5m_raster <- here(
  "00_rawdata",
  "under_5",
  "IHME_LMICS_U5M_2000_2017_Q_UNDER5_MEAN_Y2019M10D16.TIF"
)
if (!file.exists(u5m_raster)) {
  stop("Missing U5M raster file: ", u5m_raster, call. = FALSE)
}
u5m_layers <- terra::nlyr(terra::rast(u5m_raster))
u5m_years <- 2000:(2000 + u5m_layers - 1L)
print_year_support("U5M raster year support", u5m_years)
if (!ALLOW_SYNTHETIC_EXTENSION) {
  assert_year_coverage(
    "U5M raster support",
    u5m_years,
    START_YEAR:END_YEAR
  )
} else {
  missing_u5m <- setdiff(START_YEAR:END_YEAR, u5m_years)
  if (length(missing_u5m) > 0) {
    message(
      "U5M missing years accepted under synthetic extension: ",
      paste(missing_u5m, collapse = ", ")
    )
  }
}

raw_dat <- read_csv(
  here("00_rawdata", "GODAD_projectlevel.csv"),
  show_col_types = FALSE
)
print_year_support("GODAD paymentyear support", raw_dat$paymentyear)
assert_year_coverage(
  "GODAD paymentyear support",
  raw_dat$paymentyear,
  max(1996L, START_YEAR):END_YEAR
)

admin1_afro_raw <- read_csv(
  here("00_rawdata", "ab_raw", "processed", "admin1_afro_panel.csv"),
  show_col_types = FALSE
)
admin2_afro_raw <- read_csv(
  here("00_rawdata", "ab_raw", "processed", "admin2_afro_panel.csv"),
  show_col_types = FALSE
)
print_year_support("Afro admin1 observed support", admin1_afro_raw$year)
print_year_support("Afro admin2 observed support", admin2_afro_raw$year)
if (!all(c(2005L, 2015L) %in% admin1_afro_raw$year)) {
  stop(
    "Afro admin1 data must include years 2005 and 2015 for extrapolation bounds.",
    call. = FALSE
  )
}
if (!all(c(2005L, 2015L) %in% admin2_afro_raw$year)) {
  stop(
    "Afro admin2 data must include years 2005 and 2015 for extrapolation bounds.",
    call. = FALSE
  )
}

admin1_dep_vars_annual <- read_csv(
  here("00_rawdata", "processed_dep_vars_admin1.csv"),
  show_col_types = FALSE
)
admin2_dep_vars_annual <- read_csv(
  here("00_rawdata", "processed_dep_vars_admin2.csv"),
  show_col_types = FALSE
)
print_year_support("Dependent vars admin1 support", admin1_dep_vars_annual$year)
print_year_support("Dependent vars admin2 support", admin2_dep_vars_annual$year)
assert_year_coverage(
  "Dependent vars admin1 support",
  admin1_dep_vars_annual$year,
  START_YEAR:END_YEAR
)
assert_year_coverage(
  "Dependent vars admin2 support",
  admin2_dep_vars_annual$year,
  START_YEAR:END_YEAR
)

# -----------------------------------------------------------------------------
# Build aid + covariates in 2000-2020 mapped periods
# -----------------------------------------------------------------------------
countries_iso3 <- read_sf("00_rawdata/shapefiles/gadm_admin2.shp") %>%
  st_drop_geometry() %>%
  select(GID_0) %>%
  distinct() %>%
  pull()

raw_dat <- raw_dat %>%
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
    bucket_5year = map_to_5year_bucket_1995_2015(paymentyear),
    world_bank = ifelse(
      donor == "World Bank" & !precision_code %in% c("1", "2", "3"),
      1,
      0
    )
  ) %>%
  filter(
    gid_0 %in% countries_iso3,
    paymentyear >= START_YEAR,
    paymentyear <= END_YEAR,
    !is.na(latitude),
    !is.na(longitude),
    !is.na(bucket_5year),
    world_bank == 0
  ) %>%
  mutate(paymentyear = bucket_5year)

sector_info <- apply_sector_mapping(raw_dat)
raw_dat <- sector_info$dat
aid_type_codes <- sector_info$aid_type_codes

shp <- read_sf("00_rawdata/shapefiles/gadm_admin2.shp") %>%
  st_make_valid() %>%
  st_transform(4326)

aid_points <- st_as_sf(
  raw_dat,
  coords = c("longitude", "latitude"),
  crs = 4326
)

dat <- st_join(aid_points, st_make_valid(shp))
setDT(dat)

admin1_pop <- read_csv(
  "00_rawdata/population/admin1_population.csv",
  show_col_types = FALSE
) %>%
  pivot_longer(
    cols = starts_with("sum_"),
    names_to = "year",
    values_to = "pop"
  ) %>%
  select(GID_1, year, pop) %>%
  mutate(
    year = as.numeric(gsub("sum_", "", year)),
    year = map_to_5year_bucket_1995_2015(year)
  ) %>%
  filter(!is.na(year)) %>%
  group_by(GID_1, year) %>%
  summarise(pop = safe_mean(pop), .groups = "drop") %>%
  mutate(ln_pop = log(pop))

admin2_pop <- read_csv(
  "00_rawdata/population/admin2_population.csv",
  show_col_types = FALSE
) %>%
  pivot_longer(
    cols = starts_with("sum_"),
    names_to = "year",
    values_to = "pop"
  ) %>%
  select(GID_2, year, pop) %>%
  mutate(
    year = as.numeric(gsub("sum_", "", year)),
    year = map_to_5year_bucket_1995_2015(year)
  ) %>%
  filter(!is.na(year)) %>%
  group_by(GID_2, year) %>%
  summarise(pop = safe_mean(pop), .groups = "drop") %>%
  mutate(ln_pop = log(pop))

admin1_afro <- build_annual_afro(
  afro_raw = admin1_afro_raw,
  admin_id = "GID_1",
  sgq_col = "mean_sgq_admin1",
  status_col = "sgq_status_admin1"
) %>%
  bucket_afro(
    admin_id = "GID_1",
    status_col = "sgq_status_admin1"
  )

admin2_afro <- build_annual_afro(
  afro_raw = admin2_afro_raw,
  admin_id = "GID_2",
  sgq_col = "mean_sgq_admin2",
  status_col = "sgq_status_admin2"
) %>%
  bucket_afro(
    admin_id = "GID_2",
    status_col = "sgq_status_admin2"
  )

admin1_dep_vars <- admin1_dep_vars_annual %>%
  mutate(year = map_to_5year_bucket_1995_2015(year)) %>%
  filter(!is.na(year)) %>%
  group_by(GID_0, GID_1, year) %>%
  summarise(
    sum = safe_mean(sum),
    mean = safe_mean(mean),
    u5m = safe_mean(u5m),
    .groups = "drop"
  )

admin2_dep_vars <- admin2_dep_vars_annual %>%
  mutate(year = map_to_5year_bucket_1995_2015(year)) %>%
  filter(!is.na(year)) %>%
  group_by(GID_0, GID_1, GID_2, year) %>%
  summarise(
    sum = safe_mean(sum),
    mean = safe_mean(mean),
    u5m = safe_mean(u5m),
    .groups = "drop"
  )

wgi <- readxl::read_xlsx("00_rawdata/wgidataset.xlsx") %>%
  select(GID_0, year, ge_pct) %>%
  mutate(ge_pct = suppressWarnings(as.numeric(ge_pct))) %>%
  mutate(year = map_to_5year_bucket_1995_2015(year)) %>%
  filter(!is.na(year)) %>%
  group_by(GID_0, year) %>%
  summarise(ge_pct = safe_mean(ge_pct), .groups = "drop")

period_metadata <- bucket_metadata(PERIOD_KEYS)

# -----------------------------------------------------------------------------
# Run data for all panels
# -----------------------------------------------------------------------------
build_panels_for_type(
  dat,
  "all",
  PANEL_TAG,
  period_metadata_tbl = period_metadata
)

sector_admin1_list <- list()
sector_admin2_list <- list()

for (type_name in names(aid_type_codes)) {
  dat_type <- dat[study_group == type_name]
  sector_result <- build_panels_for_type(
    dat_type,
    type_name,
    paste0("_", type_name, PANEL_TAG),
    period_metadata_tbl = period_metadata
  )

  sector_result$panel_aid_admin1_fin$study_group <- type_name
  sector_result$panel_aid_admin2_fin$study_group <- type_name

  sector_admin1_list[[type_name]] <- sector_result$panel_aid_admin1_fin
  sector_admin2_list[[type_name]] <- sector_result$panel_aid_admin2_fin
}

# -----------------------------------------------------------------------------
# Write out final long datasets
# -----------------------------------------------------------------------------
if (length(sector_admin1_list) > 0) {
  panel_aid_admin1_by_sector <- bind_rows(sector_admin1_list)
  write_csv(
    panel_aid_admin1_by_sector,
    here(
      "01_panel_data",
      paste0("panel_aid_admin1_by_sector", PANEL_TAG, ".csv")
    )
  )
}

if (length(sector_admin2_list) > 0) {
  panel_aid_admin2_by_sector <- bind_rows(sector_admin2_list)
  write_csv(
    panel_aid_admin2_by_sector,
    here(
      "01_panel_data",
      paste0("panel_aid_admin2_by_sector", PANEL_TAG, ".csv")
    )
  )
}

message("Finished panel build for ", PANEL_TAG, ".")
