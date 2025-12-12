# Build shape file for analysis
if (!require("pacman")) {
    install.packages("pacman")
}

pacman::p_load(
    tidyverse,
    here,
    readr,
    janitor,
    sf,
    geodata
)

countries_iso3 <- c(
    "DZA",
    "BEN",
    "BWA",
    "BFA",
    "BDI",
    "CMR",
    "CPV",
    "CIV",
    "EGY",
    "GAB",
    "GHA",
    "GIN",
    "KEN",
    "LSO",
    "LBR",
    "MDG",
    "MWI",
    "MLI",
    "MUS",
    "MAR",
    "MOZ",
    "NAM",
    "NER",
    "NGA",
    "STP",
    "SEN",
    "SLE",
    "ZAF",
    "SDN",
    "SWZ",
    "TZA",
    "TGO",
    "TUN",
    "UGA",
    "ZMB",
    "ZWE"
)


# Try to download GADM data, first attempting level 2 then falling back to level 1 if needed
shp_list <- lapply(countries_iso3, function(country) {
    result <- geodata::gadm(
        country = country,
        version = "3.6",
        level = 2,
        path = "00_rawdata/shapefiles/"
    )
    if (is.null(result)) {
        # If level 2 fails, return level 1
        message(paste0("Level 2 failed for ", country))
        result <- geodata::gadm(
            country = country,
            version = "3.6",
            level = 1,
            path = "00_rawdata/shapefiles/"
        )
    } else {
        message(paste0("Level 2 succeeded for ", country))
    }
    return(result)
})

# Combine all shapefiles into one
shp <- do.call(rbind, shp_list)
shp <- sf::st_as_sf(shp)
# Save the combined shapefile
sf::st_write(shp, "00_rawdata/shapefiles/gadm_admin2.shp", delete_dsn = TRUE)

### Same thing for admin1 level

shp_list <- lapply(countries_iso3, function(country) {
    result <- geodata::gadm(
        country = country,
        version = "3.6",
        level = 1,
        path = "00_rawdata/shapefiles/"
    )
    if (is.null(result)) {
        # If level 1 fails, return message
        message(paste0("Level 1 failed for ", country))
    } else {
        message(paste0("Level 1 succeeded for ", country))
    }
    return(result)
})

# Combine all shapefiles into one
shp <- do.call(rbind, shp_list)
shp <- sf::st_as_sf(shp)
# Save the combined shapefile
sf::st_write(shp, "00_rawdata/shapefiles/gadm_admin1.shp", delete_dsn = TRUE)
