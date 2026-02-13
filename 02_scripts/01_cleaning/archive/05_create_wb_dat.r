# Load packages
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# Step 1: Download the dataset from Finances One
url <- "https://financesone.worldbank.org/api/views/DS00051/rows.json?accessType=DOWNLOAD"

response <- GET(url)
stop_for_status(response)

# Step 2: Parse JSON
json_data <- content(response, as = "text")
parsed <- fromJSON(json_data)

# Extract column names
columns <- sapply(parsed$meta$view$columns, function(col) col$name)
data_raw <- parsed$data

# Convert to data.frame
df <- as.data.frame(do.call(rbind, data_raw), stringsAsFactors = FALSE)
names(df) <- columns

# Step 3: Select and clean relevant columns
df_clean <- df %>%
  select(
    country = `Country Name`,
    iso3 = `Country Code`,
    date = `Date`,
    votes = `Number of Votes`,
    vote_share = `% of Total Votes`,
    subscribed_capital = `Subscribed Capital (US$ millions)`
  ) %>%
  mutate(
    date = ymd(date),
    year = year(date),
    votes = as.numeric(gsub(",", "", votes)),
    vote_share = as.numeric(str_remove(vote_share, "%")),
    subscribed_capital = as.numeric(gsub(",", "", subscribed_capital))
  ) %>%
  filter(!is.na(vote_share)) %>%
  arrange(country, year)

# Step 4: Aggregate (if needed, e.g., latest per year)
df_panel <- df_clean %>%
  group_by(country, year) %>%
  filter(date == max(date)) %>%
  ungroup()

# Step 5: Save to CSV
write.csv(df_panel, "ibrd_voting_shares_timeseries.csv", row.names = FALSE)

# Optional: Plot
library(ggplot2)
top_countries <- df_panel %>%
  filter(year == max(year)) %>%
  slice_max(order_by = vote_share, n = 10) %>%
  pull(country)

ggplot(df_panel %>% filter(country %in% top_countries), 
       aes(x = year, y = vote_share, color = country)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "IBRD Voting Share Over Time",
       y = "Vote Share (%)", x = "Year")
