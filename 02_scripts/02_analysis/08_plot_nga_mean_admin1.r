# =============================================================================
# 08_plot_nga_mean_admin1.r
#
# Line chart for Nigeria (NGA):
#   1) mean for each admin1 unit (GID_1) by year
#   2) country-wide average across all GID_1 by year
# =============================================================================

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, here, readr)

country_code <- "BWA"

dep <- read_csv(
  here("00_rawdata", "processed_dep_vars_admin1.csv"),
  show_col_types = FALSE
)

required_cols <- c("GID_0", "GID_1", "year", "mean")
missing_cols <- setdiff(required_cols, names(dep))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

nga_dep <- dep %>%
  mutate(year = as.integer(year)) %>%
  filter(is.finite(mean))

if (nrow(nga_dep) == 0) {
  stop("No rows found for country code: ", country_code)
}

nga_avg <- nga_dep %>%
  group_by(year) %>%
  summarise(mean = mean(mean, na.rm = TRUE), .groups = "drop")

p <- ggplot() +
  geom_line(
    data = nga_dep,
    aes(x = year, y = mean, group = GID_1),
    color = "#5a8db8",
    alpha = 0.35,
    linewidth = 0.4
  ) +
  geom_line(
    data = nga_avg,
    aes(x = year, y = mean),
    color = "#c9302c",
    linewidth = 1.2
  ) +
  labs(
    title = "Nigeria: Nightlights Mean by Admin1 and Country Average",
    subtitle = paste0(
      "Country average is the yearly mean across all GID_1 units (n = ",
      n_distinct(nga_dep$GID_1),
      ")"
    ),
    x = "Year",
    y = "Mean nightlights",
    caption = "Data: 00_rawdata/processed_dep_vars_admin1.csv"
  ) +
  scale_x_continuous(
    breaks = seq(
      min(nga_dep$year, na.rm = TRUE),
      max(nga_dep$year, na.rm = TRUE),
      by = 2
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title.position = "plot",
    panel.grid.minor = element_blank()
  )

fig_dir <- here("03_output", "figs")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

png_path <- file.path(fig_dir, "nga_admin1_mean_by_year.png")
pdf_path <- file.path(fig_dir, "nga_admin1_mean_by_year.pdf")

ggsave(filename = png_path, plot = p, width = 11, height = 6, dpi = 300)
ggsave(filename = pdf_path, plot = p, width = 11, height = 6)

message("Wrote figure files:")
message("  ", png_path)
message("  ", pdf_path)
