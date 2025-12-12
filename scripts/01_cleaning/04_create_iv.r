# Create IV
pacman::p_load(
    tidyverse,
    data.table,
    tidyr,
    here,
    sf,
    zoo
)

# get country isos for gadm
countries_iso3 <- read_csv(
    "00_rawdata/nightlights/topcodefix/processed_topcodefix_nl_admin2.csv"
) %>%
    select(GID_0) %>%
    distinct() %>%
    pull()


# Read in data
aid_data <- read_csv(here("00_rawdata", "GODAD_projectlevel.csv")) %>%
    mutate(
        world_bank = ifelse(
            donor == "World Bank" & !precision_code %in% c("1", "2", "3"),
            1,
            0
        )
    ) %>%
    filter(
        gid_0 %in%
            countries_iso3 &
            paymentyear >= 2005 &
            paymentyear <= 2015 &
            !is.na(latitude) &
            !is.na(longitude) &
            world_bank == 0
    )


donor_countries <- c(
    "Austria",
    "Belgium",
    "Denmark",
    "Finland",
    "France",
    "Germany",
    "Greece",
    "Iceland",
    "India",
    "Ireland",
    "Italy",
    "Luxembourg",
    "Netherlands",
    "Norway",
    "Portugal",
    "Spain",
    "Sweden",
    "Switzerland",
    "United Kingdom",
    "United States",
    "World Bank"
)


# Combine all shapefiles into one
shp <- sf::read_sf("00_rawdata/shapefiles/gadm_admin2.shp")


shp <- shp %>%
    # Assign GID_1 for those countries that don't have GID_2
    mutate(GID_2 = ifelse(is.na(GID_2), GID_1, GID_2))


aid_points <- sf::st_as_sf(
    aid_data,
    coords = c("longitude", "latitude"), # Specify longitude and latitude columns
    crs = 4326 # Set CRS (WGS84)
)

dat <- sf::st_join(aid_points, sf::st_make_valid(shp))

dat <- dat %>%
    filter(donor %in% donor_countries)

# Convert dat to data.table for speed
setDT(dat)

# Create HHI for admin2 level

aid_data <- dat[,
    .(
        aid_amount = sum(abs(disb_loc_evensplit), na.rm = TRUE)
    ),
    by = .(GID_0, GID_1, GID_2, year = paymentyear, donor)
]

# Read in the fractionization data for
frac_data <- read_csv("00_rawdata/DPI2020.csv") %>%
    mutate(
        countryname = str_replace(countryname, "USA", "United States"),
        countryname = str_replace(countryname, "UK", "United Kingdom"),
        countryname = str_replace(countryname, "FRG/Germany", "Germany"),
        frac_full = if_else(
            countryname == "United States",
            frac,
            govfrac
        )
    ) %>%
    filter(countryname %in% donor_countries & year >= 2005 & year <= 2015) %>%
    select(donor = countryname, year, frac_full)
# Estimate weighted average of donor fraction for World Bank

votes <- read_csv("00_rawdata/pdfs/wb_vote_shares.csv") %>%
    mutate(
        Country = str_replace(Country, "USA", "United States"),
        Country = str_replace(Country, "UK", "United Kingdom")
    ) %>%
    pivot_longer(
        cols = starts_with("Vote_"),
        names_to = "year",
        values_to = "vote_share"
    ) %>%
    mutate(year = str_replace(year, "Vote_", "")) %>%
    filter(year != "avg") %>%
    mutate(year = as.numeric(year)) %>%
    complete(Country = unique(Country), year = 2005:2015) %>%
    group_by(Country) %>%
    mutate(vote_share = na.approx(vote_share, na.rm = TRUE, rule = 2)) %>%
    ungroup() %>%
    left_join(
        frac_data %>% select(donor, year, frac_full),
        by = c("Country" = "donor", "year")
    ) %>%
    group_by(year) %>%
    summarise(
        frac_full = weighted.mean(frac_full, vote_share, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(donor = "World Bank")


frac_data <- bind_rows(frac_data, votes)

donor_countries_look <- frac_data %>%
    select(donor) %>%
    distinct() %>%
    pull()

construct_shift_share_iv_strict <- function(
    aid_data, # columns: region_id, year, donor, aid_amount
    frac_data, # columns: donor, year, frac
    panel_data, # columns: region_id, year, country, pop_it (optional)
    aid_threshold = 0, # threshold for considering aid as received
    region_id, # region ID column in aid_data
    start_year = 2005, # start year for the analysis
    end_year = 2015 # end year for the analysis
) {
    # Step 1: Define p_ji = avg(1(aid_ijt > 0)) over the specified time period
    pji <- aid_data %>%
        complete(
            !!sym(region_id),
            donor,
            year = start_year:end_year,
            fill = list(aid_amount = 0, received_aid = 0)
        ) %>%
        mutate(received_aid = ifelse(aid_amount > aid_threshold, 1, 0)) %>%
        # Use the dynamic region_id parameter instead of hardcoded value
        group_by(!!sym(region_id), donor) %>%
        summarise(pji = sum(received_aid, na.rm = TRUE) / 11, .groups = "drop") # fixed across time

    # Rename the column to ensure consistent joining
    names(pji)[names(pji) == region_id] <- "region_id"

    # Step 2: Create full donor-region-year panel
    iv_components <- panel_data %>%
        # Use the dynamic region_id parameter
        select(region_id = !!sym(region_id), year) %>%
        filter(year >= start_year, year <= end_year) %>%
        crossing(donor = unique(frac_data$donor)) %>%
        left_join(pji, by = c("region_id", "donor")) %>%
        left_join(frac_data, by = c("donor", "year")) %>%
        mutate(iv_component = pji * frac_full)

    # Step 3: Collapse to IVit = sum_j (pji * frac_jt)
    iv_data <- iv_components %>%
        group_by(region_id, year) %>%
        summarise(IV = sum(iv_component, na.rm = TRUE), .groups = "drop") %>%
        arrange(region_id, year) %>%
        group_by(region_id) %>%
        mutate(IV_lag = dplyr::lag(IV, order_by = year)) %>% # enforce year order
        ungroup()

    return(iv_data)
}

# Create the IV admin2
panel_aid_admin2 <- read_csv("01_panel_data/panel_aid_admin2.csv")

results <- construct_shift_share_iv_strict(
    aid_data = aid_data,
    frac_data = frac_data,
    panel_data = panel_aid_admin2,
    region_id = "GID_2"
)

# Merge the IV with the panel data
panel_aid_admin2 <- panel_aid_admin2 %>%
    left_join(results, by = c("GID_2" = "region_id", "year"))

# Create the IV admin1

panel_aid_admin1 <- read_csv("01_panel_data/panel_aid_admin1.csv")

results <- construct_shift_share_iv_strict(
    aid_data = aid_data,
    frac_data = frac_data,
    panel_data = panel_aid_admin1,
    region_id = "GID_1"
)

# Merge the IV with the panel data
panel_aid_admin1 <- panel_aid_admin1 %>%
    left_join(results, by = c("GID_1" = "region_id", "year"))

# Save the data

write_csv(panel_aid_admin2, "01_panel_data/panel_aid_admin2_fin.csv")
write_csv(panel_aid_admin1, "01_panel_data/panel_aid_admin1_fin.csv")
