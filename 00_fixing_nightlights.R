if (!require("pacman")) 
  install.packages("pacman"); 

pacman::p_load(
  tidyverse,
  here,
  readr,
  janitor,
  sjmisc,
  ### GEO Packages
  sf,
  ncdf4,
  spdep,
  spatstat,
  geodata,
  terra
)

file <- c("C:/Users/eman7/Downloads/spei12.nc")

sepi  <- rast(file)

# Check the structure of the raster
time_values <- time(sepi) # Extract the time values

print(time_values)

# Define the date threshold
threshold_date <- as.Date("2005-01-01")

# Find indices of layers with time greater than the threshold
indices <- which(as.Date(time_values) > threshold_date)

# Subset the raster using the indices
sepi_subset <- subset(sepi, indices)

# Extract year and month
time_values <- time(sepi_subset) # Extract the time values
years <- format(time_values, "%Y") # Format as "YYYY-MM"

# Compute monthly averages
yearly_avg <- tapp(sepi_subset, index = years, fun = mean, na.rm = TRUE)

terra::writeRaster(yearly_avg, filename = "C:/Users/eman7/Dropbox/GitHub/ra_work/Donor_fragmentation/spei_yearly_12mnth.tif", overwrite = TRUE)

plot(yearly_avg)


# Build afropolis from source 
cities <- st_read("C:/Users/eman7/OneDrive/Desktop/Africapolis_GIS_2024.gpkg")

cities_100k <- cities %>%
  filter(Population_2010 > 100000 & Select_Geometry_Year == 2015)%>%
  select(1:5, Population_2010, Population_2015, Built.up_2015)

table(unique(cities$Select_Geometry_Year))
