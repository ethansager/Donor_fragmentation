# Load necessary libraries
library(dplyr)
library(ggplot2)

# Read in the dataset
data <- read.csv("00_rawdata/nightlights/topcodefix/processed_topcodefix_nl_admin1.csv")

# Select one country for time series visualization

country_data <- data %>% filter(GID_0 == "BDI")

country_data <- country_data %>%
    pivot_longer(
        cols = starts_with("sum_"),
        names_to = "Year",
        values_to = "sum_nl"
    ) %>%
    mutate(Year = as.numeric(gsub("sum_", "", Year))) # Convert Year to numeric for proper ordering

# Plot the time series with each GID_1 as its own line
ggplot(country_data, aes(x = Year, y = sum_nl, color = GID_1, group = GID_1)) +
    geom_line() +
    geom_vline(xintercept = 2013.5, linetype = "dashed", color = "red") +
    labs(
        title = paste("Time Series for", "Ghana"),
        x = "Year",
        y = "Average Growth Rate",
        color = "GID_1"
    ) +
    theme_minimal()
